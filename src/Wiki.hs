{-# LANGUAGE OverloadedStrings #-}

module Wiki
  ( WikiNode (..)
  , WikiConcept (..)
  , RawWikiItem (..)
  , buildWikiTree
  , wikiRootsContext
  , wikiCategoryContext
  , findNodeByPath
  , metaTableContext
  ) where

import Data.List (intercalate, isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import System.FilePath (splitDirectories, dropExtension, takeFileName)

import Hakyll

data WikiNode = WikiNode
  { wnSlug          :: String
  , wnLabel         :: String
  , wnPath          :: [String]
  , wnIndexItem     :: Maybe (Item String)
  , wnSubcategorias :: [WikiNode]
  , wnConceptos     :: [WikiConcept]
  }

data WikiConcept = WikiConcept
  { wcTitle   :: String
  , wcSlug    :: String
  , wcCatPath :: [String]
  , wcUrl     :: String
  , wcMeta    :: [(String, String)]
  , wcItem    :: Item String
  }

data RawWikiItem = RawWikiItem
  { rwPath  :: FilePath
  , rwTitle :: Maybe String
  , rwMeta  :: [(String, String)]
  , rwItem  :: Item String
  }

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
        , wnIndexItem     = indexItem
        , wnSubcategorias = subNodes
        , wnConceptos     = sortOn wcTitle concepts
        }
      where
        fullPath = parentPath ++ [slug]
        depth    = length fullPath

        relEntries :: [([String], RawWikiItem)]
        relEntries =
          [ (drop depth (splitWikiPath (rwPath ri)), ri)
          | ri <- entries
          ]

        -- El _index.md de esta carpeta: o bien el propio archivo raíz
        -- (wiki/_index.md, rel == []), o bien <carpeta>/_index.md (rel == ["_index.md"]).
        indexEntry :: Maybe RawWikiItem
        indexEntry = listToMaybe
          [ ri
          | (rel, ri) <- relEntries
          , isIndexFile (rwPath ri)
          , null rel || rel == ["_index.md"]
          ]

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

nodeUrl :: WikiNode -> String
nodeUrl node = "/" ++ intercalate "/" ("wiki" : wnPath node) ++ "/index.html"

wikiConceptContext :: Context WikiConcept
wikiConceptContext =
  field "title" (return . wcTitle . itemBody) <>
  field "url"   (return . wcUrl   . itemBody)

wikiNodeContext :: Context WikiNode
wikiNodeContext =
  field "label" (return . wnLabel . itemBody) <>
  field "slug"  (return . wnSlug  . itemBody) <>
  field "url"   (return . nodeUrl . itemBody)

wikiRootsContext :: [String] -> [WikiNode] -> Context a
wikiRootsContext wantedSlugs allRoots =
  listField "roots" wikiNodeContext (mapM makeItem orderedRoots)
  where
    orderedRoots = [ n | slug <- wantedSlugs, Just n <- [findRoot slug allRoots] ]

findRoot :: String -> [WikiNode] -> Maybe WikiNode
findRoot slug = listToMaybe . filter ((== slug) . wnSlug)

wikiCategoryContext :: WikiNode -> Context a
wikiCategoryContext node =
  constField "wiki_label" (wnLabel node) <>
  constField "wiki_entry_count" (show (length (wnConceptos node) + length (wnSubcategorias node))) <>
  -- Las listas solo se definen si no están vacías: en Hakyll, $if(lista)$ es
  -- verdadero para cualquier listField, aunque esté vacía. Los has_* se
  -- mantienen por compatibilidad con plantillas que los usen.
  boolField "has_wiki_conceptos"     (const (not (null (wnConceptos node)))) <>
  boolField "has_wiki_subcategorias" (const (not (null (wnSubcategorias node)))) <>
  nonEmptyList "wiki_direct_conceptos" wikiConceptContext (wnConceptos node) <>
  nonEmptyList "wiki_direct_subcategorias" wikiNodeContext (wnSubcategorias node)
  where
    nonEmptyList :: String -> Context b -> [b] -> Context a
    nonEmptyList _    _   [] = mempty
    nonEmptyList name ctx xs = listField name ctx (mapM makeItem xs)

findNodeByPath :: [String] -> [WikiNode] -> Maybe WikiNode
findNodeByPath []     _     = Nothing
findNodeByPath [slug] roots = findRoot slug roots
findNodeByPath (slug:rest) roots =
  findRoot slug roots >>= findNodeByPath rest . wnSubcategorias

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
