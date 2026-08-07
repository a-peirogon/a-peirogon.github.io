{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc (Pandoc(..), Block(..), Inline(..), Format(..), readMarkdown, runIOorExplode, writeHtml5String)
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Templates (compileTemplate)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (sortOn, groupBy)
import Data.Ord (Down (..))
import Data.Function (on)
import Control.Applicative ((<|>))
import Data.Char (isUpper, isAlpha)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException)
import System.FilePath ((</>), takeBaseName, makeRelative, takeExtension, replaceExtension, takeDirectory, splitDirectories)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS

import Wiki

main :: IO ()
main = do
    wikiFiles <- findMarkdownFilesRecursive "wiki"
    rawItems  <- mapM loadRawWikiItem wikiFiles
    let wikiTree = buildWikiTree rawItems

    hakyll $ do
        match ("css/*" .||. "js/*" .||. "img/**" .||. "fonts/**" .||. "favicon.ico" .||. "generated/**") $ do
            route   idRoute
            compile copyFileCompiler

        match "templates/*" $ compile templateCompiler

        match "posts/*.md" $ do
            route $ setExtension "html"
            compile $ do
                tocCtx <- getTocCtx postCtx
                sidebarCtx <- globalSidebarContext wikiTree
                customPandocCompiler
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/post.html"    tocCtx
                    >>= loadAndApplyTemplate "templates/default.html" (sidebarCtx <> tocCtx)
                    >>= relativizeUrls

        match "wiki/**/*.md" $ do
            route $ customRoute wikiConceptRoute
            compile $ do
                ident <- getUnderlying
                let path = toFilePath ident
                sidebarCtx <- globalSidebarContext wikiTree
                if isIndexPath path
                    then do
                        let catPath = drop 1 (splitDirectories (takeDirectory path))
                            mNode = findNodeByPath catPath wikiTree
                            catCtx = case mNode of
                                       Just node -> wikiCategoryContext node
                                       Nothing   -> mempty
                            catCtx2 = catCtx <> constField "bodyclass" "wikiConcept" <> defaultContext
                        customPandocCompiler
                            >>= loadAndApplyTemplate "templates/wiki-category.html" catCtx2
                            >>= loadAndApplyTemplate "templates/default.html"       (sidebarCtx <> catCtx2)
                            >>= relativizeUrls
                    else do
                        let catPath  = drop 1 (splitDirectories (takeDirectory path))
                            slugName = takeBaseName path
                            metaCtx = case findNodeByPath catPath wikiTree of
                                Just node ->
                                    case filter ((== slugName) . wcSlug) (wnConceptos node) of
                                        (wc:_) -> metaTableContext wc
                                        []     -> mempty
                                Nothing -> mempty
                            conceptCtx = metaCtx <> wikiConceptCtx wikiTree
                        customPandocCompiler
                            >>= loadAndApplyTemplate "templates/wiki-concept.html" conceptCtx
                            >>= loadAndApplyTemplate "templates/default.html"      (sidebarCtx <> conceptCtx)
                            >>= relativizeUrls

        create ["wiki/index.html"] $ do
            route idRoute
            compile $ do
                sidebarCtx <- globalSidebarContext wikiTree
                welcomeCtx <- case findNodeByPath ["_index.md"] wikiTree >>= wnIndexItem of
                    Just item -> unsafeCompiler $ do
                        Pandoc _ blocks <- runIOorExplode (readMarkdown readerOptions (T.pack (itemBody item)))
                        let framedBlocks = map wrapWelcomeImage blocks
                        html <- runIOorExplode (writeHtml5String writerOptions (Pandoc mempty framedBlocks))
                        return $ constField "wiki_welcome_html" (T.unpack html)
                    Nothing -> return mempty
                let ctx =
                        wikiRootsContext ["filosofia", "informatica", "matematicas"] wikiTree <>
                        welcomeCtx <>
                        constField "title" "Wiki" <>
                        constField "bodyclass" "wikiIndex" <>
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/wiki-index.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html"    (sidebarCtx <> ctx)
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- fmap (take 10) $ recentFirst =<< loadAll "posts/*.md"
                sidebarCtx <- globalSidebarContext wikiTree
                let indexCtx =
                        listField "posts" postCtx (return posts) <>
                        constField "bodyclass" "siteIndex"       <>
                        sidebarCtx                                <>
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "pages/about.md" $ do
            route $ customRoute (const "about.html")
            compile $ do
                sidebarCtx <- globalSidebarContext wikiTree
                customPandocCompiler
                    >>= loadAndApplyTemplate "templates/default.html" (sidebarCtx <> defaultContext)
                    >>= relativizeUrls

        match "books/*.md" $ do
            route $ setExtension "html"
            compile $ do
                bookMetaCtx <- bookMetaTableContext
                sidebarCtx <- globalSidebarContext wikiTree
                let bookCtx = bookMetaCtx <> constField "bodyclass" "wikiConcept" <> defaultContext
                customPandocCompiler
                    >>= loadAndApplyTemplate "templates/wiki-concept.html" bookCtx
                    >>= loadAndApplyTemplate "templates/default.html" (sidebarCtx <> bookCtx)
                    >>= relativizeUrls

        create ["books.html"] $ do
            route idRoute
            compile $ do
                books <- loadAll "books/*.md" :: Compiler [Item String]
                booksWithYear <- mapM (\b -> do
                        yr <- getMetadataField (itemIdentifier b) "fecha_lectura"
                        return (fromMaybe "" yr, b)
                    ) books
                let grouped = groupByYear booksWithYear
                sidebarCtx <- globalSidebarContext wikiTree
                let yearsCtx = listField "years" yearContext (mapM makeItem grouped)
                    booksCtx =
                        yearsCtx <>
                        constField "title" "Lecturas" <>
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/books-index.html" booksCtx
                    >>= loadAndApplyTemplate "templates/default.html" (sidebarCtx <> booksCtx)
                    >>= relativizeUrls

        match "pages/research.md" $ do
            route $ customRoute (const "research.html")
            compile $ do
                sidebarCtx <- globalSidebarContext wikiTree
                customPandocCompiler
                    >>= loadAndApplyTemplate "templates/default.html" (sidebarCtx <> defaultContext)
                    >>= relativizeUrls

        create ["feed.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx = postCtx <> bodyField "description"
                posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*.md" "content"
                renderAtom feedConfig feedCtx posts

findMarkdownFilesRecursive :: FilePath -> IO [FilePath]
findMarkdownFilesRecursive dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            entries <- listDirectory dir
            fmap concat $ mapM (classify . (dir </>)) entries
    where
        classify p = do
            isDir <- doesDirectoryExist p
            if isDir
                then findMarkdownFilesRecursive p
                else return [p | takeExtension p == ".md"]

loadRawWikiItem :: FilePath -> IO RawWikiItem
loadRawWikiItem fp = do
    contents <- readFile fp
    date     <- gitModTime fp
    let relPath = makeRelative "wiki" fp
        meta    = extractFrontmatter contents
        title   = lookup "title" meta
        icon    = lookup "icon" meta
        ident   = fromFilePath fp
    return RawWikiItem
        { rwPath  = relPath
        , rwTitle = title
        , rwIcon  = icon
        , rwMeta  = meta
        , rwDate  = date
        , rwItem  = Item ident contents
        }

extractFrontmatter :: String -> [(String, String)]
extractFrontmatter contents =
    case lines contents of
        ("---" : rest) -> go (takeWhile (/= "---") rest)
        _              -> []
    where
        go [] = []
        go (l:ls) = case break (== ':') l of
            (key, ':':val) | not (null (trim key)) -> (trim key, trim val) : go ls
            _                                        -> go ls
        trim = f . f where f = reverse . dropWhile (== ' ')

isIndexPath :: FilePath -> Bool
isIndexPath p = takeBaseName p == "_index"

wikiConceptRoute :: Identifier -> FilePath
wikiConceptRoute ident =
    let p   = toFilePath ident
        dir = takeDirectory p
    in if isIndexPath p
       then dir </> "index.html"
       else replaceExtension p "html"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

wikiConceptCtx :: [WikiNode] -> Context String
wikiConceptCtx _wikiTree =
    constField "bodyclass" "wikiConcept" <>
    defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Mi Sitio - Feed"
    , feedDescription = "Últimas publicaciones"
    , feedAuthorName  = "Tu Nombre"
    , feedAuthorEmail = "tu@email.com"
    , feedRoot        = "https://tu-sitio.com"
    }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
    pandoc <- readPandocWith readerOptions =<< getResourceBody
    let applyTransforms :: Pandoc -> IO Pandoc
        applyTransforms p = do
            let withSmallCaps = smallCapsTransform p
            withTikz <- tikzTransform withSmallCaps
            withCaja <- cajaTransform withTikz
            pygmentsTransform withCaja
    transformed <- unsafeCompiler $ traverse applyTransforms pandoc
    return $ fmap wrapFootnotesInDetails (writePandocWith writerOptions transformed)

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions
    { readerExtensions = enableExtension Ext_footnotes $
                        enableExtension Ext_inline_notes $
                        enableExtension Ext_smart $
                        enableExtension Ext_tex_math_dollars $
                        enableExtension Ext_fenced_code_attributes $
                        enableExtension Ext_backtick_code_blocks $
                        readerExtensions defaultHakyllReaderOptions
    }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
    { writerExtensions = enableExtension Ext_footnotes $
                         enableExtension Ext_inline_notes $
                         enableExtension Ext_smart $
                         writerExtensions defaultHakyllWriterOptions
    , writerHTMLMathMethod = MathJax ""
    , writerReferenceLinks = False
    , writerSectionDivs = True
    , writerNumberSections = False
    }

smallCapsTransform :: Pandoc -> Pandoc
smallCapsTransform = walk mapInline
  where
    mapInline :: Inline -> Inline
    mapInline (Str s)
        | T.length s > 1 && T.all isUpper s && T.all isAlpha s =
            Span ("", ["smallcaps"], []) [Str s]
        | otherwise = Str s
    mapInline x = x

pygmentsTransform :: Pandoc -> IO Pandoc
pygmentsTransform = walkM mapBlock
  where
    mapBlock :: Block -> IO Block
    mapBlock (CodeBlock (ident, classes, keyvals) content)
        | not (null classes) = do
            let lang = head classes
            let code = T.unpack content
            result <- try $ readProcess "pygmentize"
                ["-l", T.unpack lang, "-f", "html", "-O", "cssclass=sourceCode"]
                code :: IO (Either SomeException String)
            case result of
                Right highlighted ->
                    return $ RawBlock (Format "html") (T.pack highlighted)
                Left _ ->
                    return $ CodeBlock (ident, "sourceCode" : classes, keyvals) content
    mapBlock x = return x

cajaTransform :: Pandoc -> IO Pandoc
cajaTransform = walkM mapBlock
  where
    mapBlock :: Block -> IO Block
    mapBlock (CodeBlock (_, classes, keyvals) content)
        | "caja" `elem` classes = do
            let titulo = fromMaybe "" (lookup "título" keyvals <|> lookup "titulo" keyvals)
                colorKey = maybe "dorado" T.unpack (lookup "color" keyvals)
                color = cajaColor colorKey
            Pandoc _ innerBlocks <- runIOorExplode (readMarkdown readerOptions content)
            innerHtmlItem <- runIOorExplode (writeHtml5String writerOptions (Pandoc mempty innerBlocks))
            let innerHtml = T.unpack innerHtmlItem
                tituloHtml = if T.null titulo
                              then ""
                              else "<div class=\"portal-box-title\">" ++ T.unpack titulo ++ "</div>"
                html = "<div class=\"portal-box\" style=\"--portal-box-color:" ++ color ++ ";\">"
                    ++ tituloHtml
                    ++ "<div class=\"portal-box-body\">" ++ innerHtml ++ "</div>"
                    ++ "</div>"
            return $ RawBlock (Format "html") (T.pack html)
    mapBlock x = return x

cajaColor :: String -> String
cajaColor "dorado" = "#c8b88a"
cajaColor "azul"   = "#8aaccc"
cajaColor "verde"  = "#8ac8a0"
cajaColor "rojo"   = "#c88a8a"
cajaColor _        = "#c8b88a"

wrapWelcomeImage :: Block -> Block
wrapWelcomeImage b
    | isImageOnlyBlock b = Div ("", ["wiki-welcome-img-frame"], []) [b]
    | otherwise           = b
  where
    isImageOnlyBlock (Figure _ _ _)    = True
    isImageOnlyBlock (Para [Image {}]) = True
    isImageOnlyBlock (Plain [Image {}]) = True
    isImageOnlyBlock _                  = False

tikzTransform :: Pandoc -> IO Pandoc
tikzTransform = walkM mapBlock
  where
    mapBlock :: Block -> IO Block
    mapBlock (CodeBlock (_, classes, keyvals) content)
        | "tikzpicture" `elem` classes = do
            let contentHash = BS.unpack $ B16.encode $ hash $ BS.pack $ T.unpack content
                svgPath = "generated/tikz/" ++ contentHash ++ ".svg"
                svgUrl = "/" ++ svgPath
                width = lookup "width" keyvals
                height = lookup "height" keyvals
                caption = lookup "caption" keyvals
            createDirectoryIfMissing True "generated/tikz"
            svgContent <- compileTikz (T.unpack content) svgPath
            case svgContent of
                Just _ -> do
                    let imgTag = "<img src=\"" ++ svgUrl ++ "\""
                               ++ maybe "" (\w -> " width=\"" ++ T.unpack w ++ "\"") width
                               ++ maybe "" (\h -> " height=\"" ++ T.unpack h ++ "\"") height
                               ++ " alt=\"TikZ diagram\" class=\"tikz-image\">"
                        figureHtml = case caption of
                            Just cap -> "<figure class=\"tikz-figure\">" ++ imgTag
                                     ++ "<figcaption>" ++ T.unpack cap ++ "</figcaption></figure>"
                            Nothing -> "<div class=\"tikz-container\">" ++ imgTag ++ "</div>"
                    return $ RawBlock (Format "html") (T.pack figureHtml)
                Nothing -> do
                    putStrLn $ "WARNING: Could not compile TikZ diagram " ++ contentHash
                    return $ CodeBlock ("", ["tikzpicture-error"], keyvals) content
    mapBlock x = return x

compileTikz :: String -> FilePath -> IO (Maybe String)
compileTikz code outputPath = do
    let contentHash = BS.unpack $ B16.encode $ hash $ BS.pack code
        tempDir = "generated/tikz/temp"
        texFile = tempDir </> (contentHash ++ ".tex")
        pdfFile = tempDir </> (contentHash ++ ".pdf")
    createDirectoryIfMissing True tempDir
    let latexSrc = unlines
            [ "\\documentclass[tikz,border=2pt]{standalone}"
            , "\\usepackage{tikz}"
            , "\\usetikzlibrary{arrows,positioning,shapes}"
            , "\\begin{document}"
            , code
            , "\\end{document}"
            ]
    result <- try $ do
        writeFile texFile latexSrc
        (exitCode, _, stderr) <- readProcessWithExitCode "pdflatex"
            [ "-interaction=nonstopmode"
            , "-output-directory=" ++ tempDir
            , texFile
            ] ""
        case exitCode of
            ExitSuccess -> do
                (exitCode2, _, stderr2) <- readProcessWithExitCode "pdf2svg"
                    [pdfFile, outputPath] ""
                case exitCode2 of
                    ExitSuccess -> return $ Just "success"
                    ExitFailure _ -> do
                        putStrLn $ "PDF2SVG Error: " ++ stderr2
                        return Nothing
            ExitFailure _ -> do
                putStrLn $ "LaTeX Error: " ++ stderr
                return Nothing
      :: IO (Either SomeException (Maybe String))
    case result of
        Right val -> return val
        Left ex -> do
            putStrLn $ "System Error (Missing pdflatex/pdf2svg?): " ++ show ex
            return Nothing

getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
    underlying <- getUnderlying
    noTocMeta <- getMetadataField underlying "no-toc"
    let noToc = noTocMeta == Just "true"
    if noToc
        then return $ ctx <> boolField "no-toc" (const True)
        else do
            writerOpts <- mkTocWriter writerOptions underlying
            toc <- renderPandocWith readerOptions writerOpts =<< getResourceBody
            let tocBody = killLinkIds (itemBody toc)
                finalToc = if null (trim tocBody)
                          then ""
                          else "<div id=\"TOC\" class=\"TOC\">" ++ tocBody ++ "</div>"
            return $ ctx <> constField "toc" finalToc
  where
    mkTocWriter :: WriterOptions -> Identifier -> Compiler WriterOptions
    mkTocWriter opts ident = do
        tmpl <- either (const Nothing) Just <$>
                unsafeCompiler (compileTemplate "" "$toc$")
        depthMeta <- getMetadataField ident "toc-depth"
        let depth = fromMaybe 3 (depthMeta >>= readMaybe)
        return $ opts
            { writerTableOfContents = True
            , writerTOCDepth = depth
            , writerTemplate = tmpl
            }
    readMaybe s = case reads s of
        [(val, "")] -> Just val
        _           -> Nothing
    trim = T.unpack . T.strip . T.pack
    killLinkIds = asTxt (T.concat . go . T.splitOn "id=\"toc-")
      where
        go [] = []
        go (x:xs) = x : map (T.drop 1 . T.dropWhile (/= '\"')) xs

asTxt :: (T.Text -> T.Text) -> String -> String
asTxt f = T.unpack . f . T.pack

groupByYear :: [(String, Item String)] -> [(String, [Item String])]
groupByYear books =
    map (\grp -> (fst (head grp), map snd grp))
    . groupBy ((==) `on` fst)
    . sortOn (Down . fst)
    $ books

yearContext :: Context (String, [Item String])
yearContext =
    field "year" (return . fst . itemBody) <>
    Context (\k _ item -> case k of
        "posts" -> unContext
                     (listField "posts" defaultContext (return (snd (itemBody item))))
                     "posts" [] item
        _       -> unContext missingField k [] item)

bookMetaTableContext :: Compiler (Context String)
bookMetaTableContext = do
    ident <- getUnderlying
    autor <- getMetadataField ident "autor"
    anio  <- getMetadataField ident "año"
    url   <- getMetadataField ident "url"
    let rows = [ ("Autor", v) | Just v <- [autor] ]
            ++ [ ("Año", v)   | Just v <- [anio] ]
            ++ [ ("Url", v)   | Just v <- [url] ]
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
    return $ if null rows
        then boolField "has_meta_table" (const False)
        else constField "meta_table" tableHtml <> boolField "has_meta_table" (const True)

wrapFootnotesInDetails :: String -> String
wrapFootnotesInDetails html =
    case breakOnSubstring marker html of
        Nothing -> html
        Just (before, atMarker) ->
            case findMatchingClose atMarker of
                Nothing -> html
                Just (section, after) ->
                    before ++ "<details class=\"footnotes-details\"><summary>Notas al pie</summary>" ++ section ++ "</details>" ++ after
  where
    marker = "<section id=\"footnotes\""

    breakOnSubstring :: String -> String -> Maybe (String, String)
    breakOnSubstring needle haystack = go "" haystack
      where
        go acc rest
            | needle `isPrefixOfStr` rest = Just (reverse acc, rest)
            | null rest = Nothing
            | otherwise = go (head rest : acc) (tail rest)

    isPrefixOfStr [] _ = True
    isPrefixOfStr _ [] = False
    isPrefixOfStr (x:xs) (y:ys) = x == y && isPrefixOfStr xs ys

    findMatchingClose :: String -> Maybe (String, String)
    findMatchingClose s = go s 0 ""
      where
        openTag = "<section"
        closeTag = "</section>"
        go rest depth acc
            | openTag `isPrefixOfStr` rest =
                let (tag, rest') = splitAt (length openTag) rest
                in go rest' (depth + 1) (acc ++ tag)
            | closeTag `isPrefixOfStr` rest =
                let (tag, rest') = splitAt (length closeTag) rest
                in if depth == 1
                   then Just (acc ++ tag, rest')
                   else go rest' (depth - 1) (acc ++ tag)
            | null rest = Nothing
            | otherwise = go (tail rest) depth (acc ++ [head rest])