{-# LANGUAGE OverloadedStrings #-}

module Wiki
  ( WikiNode (..)
  , WikiConcept (..)
  , RawWikiItem (..)
  , buildWikiTree
  , flattenConcepts
  , latestNConcepts
  , gitModTime
  , wikiRootsContext
  , wikiSidebarLecturasContext
  , wikiSidebarWikiEntriesContext
  , wikiSidebarObraContext
  , wikiConceptListField
  , wikiConceptFlowField
  , firstImageFromHtml
  , globalSidebarContext
  , wikiCategoryContext
  , findNodeByPath
  , wikiWelcomeImageContext
  , metaTableContext
  ) where

import Control.Exception (SomeException, try)
import Data.List (intercalate, sortOn, isPrefixOf, tails)
import Data.Ord (Down (..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath (splitDirectories, dropExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (..))
import Text.Read (readMaybe)

import Hakyll

data WikiNode = WikiNode
  { wnSlug          :: String
  , wnLabel         :: String
  , wnPath          :: [String]
  , wnIcon          :: Maybe String
  , wnIndexItem     :: Maybe (Item String)
  , wnSubcategorias :: [WikiNode]
  , wnConceptos     :: [WikiConcept]
  }

data WikiConcept = WikiConcept
  { wcTitle   :: String
  , wcSlug    :: String
  , wcCatPath :: [String]
  , wcUrl     :: String
  , wcDate    :: UTCTime
  , wcMeta    :: [(String, String)]
  , wcItem    :: Item String
  }

data RawWikiItem = RawWikiItem
  { rwPath  :: FilePath
  , rwTitle :: Maybe String
  , rwIcon  :: Maybe String
  , rwMeta  :: [(String, String)]
  , rwDate  :: UTCTime
  , rwItem  :: Item String
  }

gitModTime :: FilePath -> IO UTCTime
gitModTime path = do
  result <- try $ readProcessWithExitCode
              "git" ["log", "-1", "--format=%ct", "--", path] ""
              :: IO (Either SomeException (ExitCode, String, String))
  now <- getCurrentTime
  return $ case result of
    Right (ExitSuccess, out, _) ->
      case readMaybe (trim out) :: Maybe Integer of
        Just secs | secs > 0 -> posixSecondsToUTCTime (fromIntegral secs)
        _                    -> now
    _ -> now
  where
    trim = f . f where f = reverse . dropWhile (`elem` (" \n\r\t" :: String))

splitWikiPath :: FilePath -> [String]
splitWikiPath fp =
  case splitDirectories fp of
    ("wiki":rest) -> rest
    rest          -> rest

isIndexFile :: FilePath -> Bool
isIndexFile fp = takeFileName fp == "_index.md"

buildWikiTree :: [RawWikiItem] -> [WikiNode]
buildWikiTree items =
  map (buildNode []) (M.toList rootGroups)
  where
    grouped :: [RawWikiItem]
    grouped = filter (not . isArchivoBorrador . rwPath) items

    isArchivoBorrador fp = "_archivos" `isPrefixOf` intercalate "/" (splitWikiPath fp)

    rootGroups :: Map String [RawWikiItem]
    rootGroups = M.fromListWith (++)
      [ (head (splitWikiPath (rwPath ri)), [ri])
      | ri <- grouped
      , not (null (splitWikiPath (rwPath ri)))
      ]

    buildNode :: [String] -> (String, [RawWikiItem]) -> WikiNode
    buildNode parentPath (slug, entries) =
      WikiNode
        { wnSlug          = slug
        , wnLabel         = label
        , wnPath          = fullPath
        , wnIcon          = indexEntry >>= rwIcon
        , wnIndexItem     = indexItem
        , wnSubcategorias = subNodes
        , wnConceptos     = concepts
        }
      where
        fullPath = parentPath ++ [slug]
        depth    = length fullPath

        relEntries :: [([String], RawWikiItem)]
        relEntries =
          [ (drop depth (splitWikiPath (rwPath ri)), ri)
          | ri <- entries
          ]

        indexEntry :: Maybe RawWikiItem
        indexEntry = listToMaybe
          [ ri | (rel, ri) <- relEntries, null rel, isIndexFile (rwPath ri) ]

        indexItem = rwItem <$> indexEntry

        label = fromMaybe (prettifySlug slug) (indexEntry >>= rwTitle)

        directConceptEntries =
          [ (rel, ri)
          | (rel, ri) <- relEntries
          , length rel == 1
          , not (isIndexFile (rwPath ri))
          ]

        concepts =
          [ WikiConcept
              { wcTitle   = fromMaybe (prettifySlug slugName) (rwTitle ri)
              , wcSlug    = slugName
              , wcCatPath = fullPath
              , wcUrl     = "/" ++ intercalate "/" ("wiki" : fullPath) ++ "/"
                             ++ slugName ++ ".html"
              , wcDate    = rwDate ri
              , wcMeta    = rwMeta ri
              , wcItem    = rwItem ri
              }
          | (rel, ri) <- directConceptEntries
          , let slugName = dropExtension (head rel)
          ]

        childGroups :: Map String [RawWikiItem]
        childGroups = M.fromListWith (++)
          [ (head rel, [ri])
          | (rel, ri) <- relEntries
          , length rel > 1
          ]

        subNodes = map (buildNode fullPath) (M.toList childGroups)

prettifySlug :: String -> String
prettifySlug = capitalize . map (\c -> if c == '_' || c == '-' then ' ' else c)
  where
    capitalize (c:cs) = toUpperChar c : cs
    capitalize []      = []
    toUpperChar c = if c >= 'a' && c <= 'z'
                    then toEnum (fromEnum c - 32)
                    else c

flattenConcepts :: WikiNode -> [WikiConcept]
flattenConcepts node =
  wnConceptos node ++ concatMap flattenConcepts (wnSubcategorias node)

latestNConcepts :: Int -> WikiNode -> [WikiConcept]
latestNConcepts n = take n . sortOn (Down . wcDate) . flattenConcepts

wikiConceptContext :: Context WikiConcept
wikiConceptContext =
  field "title" (return . wcTitle . itemBody) <>
  field "url"   (return . wcUrl   . itemBody) <>
  field "date"  (return . formatTime defaultTimeLocale "%-d %b %Y" . wcDate . itemBody)

wikiConceptListField :: String -> [WikiConcept] -> Context a
wikiConceptListField name concepts =
  listField name wikiConceptContext (mapM makeItem concepts)

wikiConceptFlowField :: String -> String -> [WikiConcept] -> Compiler (Context a)
wikiConceptFlowField name sep concepts = do
  rendered <- mapM renderOne concepts
  return $ constField name (intercalate sep rendered)
  where
    renderOne wc =
      return $ "<a href=\"" ++ wcUrl wc ++ "\">" ++ wcTitle wc ++ "</a>"

wikiRootsContext :: [String] -> [WikiNode] -> Context a
wikiRootsContext wantedSlugs allRoots =
  listField "roots" rootCtx (mapM makeItem orderedRoots)
  where
    orderedRoots :: [WikiNode]
    orderedRoots = mapMaybeKeepOrder (\slug -> findRoot slug allRoots) wantedSlugs

    mapMaybeKeepOrder f = foldr (\x acc -> maybe acc (:acc) (f x)) []

    rootCtx :: Context WikiNode
    rootCtx =
      field "label" (return . wnLabel . itemBody) <>
      field "slug"  (return . wnSlug  . itemBody) <>
      field "url"   (return . rootUrl . itemBody)

    rootUrl :: WikiNode -> String
    rootUrl node = "/" ++ intercalate "/" ("wiki" : wnPath node) ++ "/index.html"

wikiSidebarLecturasContext :: [WikiNode] -> Int -> Compiler (Context a)
wikiSidebarLecturasContext roots n =
  case findRoot "lecturas" roots of
    Just node -> wikiConceptFlowField "sidebar_lecturas_flow" sep (latestNConcepts n node)
    Nothing   -> wikiConceptFlowField "sidebar_lecturas_flow" sep []
  where
    sep = " &bull; "

wikiSidebarWikiEntriesContext :: [String] -> [WikiNode] -> Int -> Compiler (Context a)
wikiSidebarWikiEntriesContext areaSlugs roots n =
  wikiConceptFlowField "sidebar_wiki_entries_flow" sep combined
  where
    sep = " &bull; "
    combined = take n
             . sortOn (Down . wcDate)
             . concatMap flattenConcepts
             $ mapMaybeKeepOrder (\slug -> findRoot slug roots) areaSlugs
    mapMaybeKeepOrder f = foldr (\x acc -> maybe acc (:acc) (f x)) []

wikiSidebarObraContext :: [WikiNode] -> Context a
wikiSidebarObraContext roots =
  case findRoot "obras" roots >>= listToMaybe . latestNConcepts 1 of
    Just c ->
      constField "obra_title" (wcTitle c) <>
      constField "obra_url"   (wcUrl c)   <>
      boolField  "has_obra"   (const True) <>
      (case firstImageFromHtml (itemBody (wcItem c)) of
         Just (src, alt) ->
           constField "obra_img"     src <>
           constField "obra_img_alt" (if null alt then wcTitle c else alt) <>
           boolField  "has_obra_img" (const True)
         Nothing ->
           boolField "has_obra_img" (const False))
    Nothing ->
      boolField "has_obra" (const False)

findRoot :: String -> [WikiNode] -> Maybe WikiNode
findRoot slug = listToMaybe . filter ((== slug) . wnSlug)

firstImageFromHtml :: String -> Maybe (String, String)
firstImageFromHtml text =
  firstImageMarkdown text `orElse` firstImageHtmlTag text
  where
    orElse (Just x) _ = Just x
    orElse Nothing  y = y

firstImageMarkdown :: String -> Maybe (String, String)
firstImageMarkdown text =
  case findAfter "![" text of
    Nothing -> Nothing
    Just afterBang ->
      let alt = takeWhile (/= ']') afterBang
          rest = drop (length alt) afterBang
      in case rest of
           (']':'(':afterParen) ->
             let src = takeWhile (/= ')') afterParen
             in if null src then Nothing else Just (trim src, alt)
           _ -> Nothing
  where
    trim = takeWhile (/= ' ')

firstImageHtmlTag :: String -> Maybe (String, String)
firstImageHtmlTag html =
  case findAfter "<img" html of
    Nothing  -> Nothing
    Just after ->
      let tag = takeWhile (/= '>') after
          src = extractAttr "src" tag
          alt = extractAttr "alt" tag
      in case src of
           Just s  -> Just (s, fromMaybe "" alt)
           Nothing -> Nothing
  where
    extractAttr :: String -> String -> Maybe String
    extractAttr attr tag =
      case findAfter (attr ++ "=\"") tag of
        Nothing    -> Nothing
        Just after -> Just (takeWhile (/= '\"') after)

findAfter :: String -> String -> Maybe String
findAfter needle haystack =
  listToMaybe [ drop (length needle) t
              | t <- tails haystack
              , needle `isPrefixOf` t
              ]

globalSidebarContext :: [WikiNode] -> Compiler (Context a)
globalSidebarContext roots = do
  lecturasCtx <- wikiSidebarLecturasContext roots 5
  wikiEntriesCtx <- wikiSidebarWikiEntriesContext ["filosofia", "informatica", "matematicas"] roots 5
  return $ lecturasCtx <> wikiEntriesCtx <> wikiSidebarObraContext roots

wikiCategoryContext :: WikiNode -> Context a
wikiCategoryContext node =
  constField "wiki_label" (wnLabel node) <>
  constField "wiki_entry_count" (show (length (wnConceptos node) + length (wnSubcategorias node))) <>
  (case wnIcon node of
     Just ic -> constField "wiki_icon" ic <> boolField "has_wiki_icon" (const True)
     Nothing -> boolField "has_wiki_icon" (const False)) <>
  wikiConceptListField "wiki_direct_conceptos" (wnConceptos node) <>
  listField "wiki_direct_subcategorias" subcatCtx (mapM makeItem (wnSubcategorias node))
  where
    subcatCtx :: Context WikiNode
    subcatCtx =
      field "label" (return . wnLabel . itemBody) <>
      field "url"   (return . subcatUrl . itemBody)
    subcatUrl n = "/" ++ intercalate "/" ("wiki" : wnPath n) ++ "/index.html"

findNodeByPath :: [String] -> [WikiNode] -> Maybe WikiNode
findNodeByPath []     _     = Nothing
findNodeByPath [slug] roots = findRoot slug roots
findNodeByPath (slug:rest) roots =
  findRoot slug roots >>= findNodeByPath rest . wnSubcategorias

wikiWelcomeImageContext :: [WikiNode] -> Context a
wikiWelcomeImageContext allRoots =
  case findRoot "_index.md" allRoots >>= wnIndexItem of
    Just item ->
      case firstImageFromHtml (itemBody item) of
        Just (src, alt) ->
          constField "wiki_welcome_img" src <>
          constField "wiki_welcome_img_alt" (if null alt then "" else alt) <>
          boolField  "has_wiki_welcome_img" (const True)
        Nothing ->
          boolField "has_wiki_welcome_img" (const False)
    Nothing ->
      boolField "has_wiki_welcome_img" (const False)

metaTableContext :: WikiConcept -> Context a
metaTableContext wc =
  if null rows
    then boolField "has_meta_table" (const False)
    else constField "meta_table" tableHtml <> boolField "has_meta_table" (const True)
  where
    knownFields :: [(String, String)]
    knownFields =
      [ ("autor", "Autor")
      , ("año", "Año")
      , ("tecnica", "Técnica")
      , ("url", "Url")
      ]

    rows :: [(String, String)]
    rows = [ (label, value)
           | (key, label) <- knownFields
           , Just value <- [lookup key (wcMeta wc)]
           ]

    renderRow (label, value) =
      "<tr><td class=\"meta-label\">" ++ label ++ "</td><td class=\"meta-value\">"
      ++ renderValue label value ++ "</td></tr>"

    renderValue "Url" value =
      "<a href=\"" ++ value ++ "\" target=\"_blank\" rel=\"noopener\">" ++ value ++ "</a>"
    renderValue _ value = value

    tableHtml =
      "<table class=\"meta-table\"><tbody>"
      ++ concatMap renderRow rows
      ++ "</tbody></table>"
