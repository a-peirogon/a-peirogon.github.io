{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc (readMarkdown, runIOorExplode, writeHtml5String)
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Templates (compileTemplate)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (sortOn, groupBy)
import Data.Ord (Down (..))
import Data.Function (on)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Char (isUpper, isAlpha)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException)
import System.FilePath ((</>), takeBaseName, makeRelative, takeExtension, replaceExtension, takeDirectory, splitDirectories)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding, utf8)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS

import Wiki

main :: IO ()
main = do
    -- Forzar UTF-8 aunque el locale del sistema no lo sea (p. ej. Debian mínimo o CI)
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8

    hakyll $ do
        -- El árbol de la wiki se reconstruye en cada pasada de reglas (también
        -- en `site watch`), y las páginas que lo usan dependen de todos los
        -- .md de la wiki: si se agrega, borra o renombra una entrada, las
        -- listas de categorías y de áreas se regeneran solas.
        wikiTree <- preprocess loadWikiTree
        wikiDeps <- makePatternDependency "wiki/**.md"

        match ("css/*" .||. "js/*" .||. "img/**" .||. "fonts/**" .||. "favicon.ico" .||. "generated/**") $ do
            route   idRoute
            compile copyFileCompiler

        match "templates/*" $ compile templateCompiler

        match "posts/*.md" $ do
            route $ setExtension "html"
            compile $ do
                toc <- getTocHtml
                customPandocCompiler
                    >>= saveSnapshot "content"
                    >>= return . fmap (insertToc toc)
                    >>= loadAndApplyTemplate "templates/post.html"    postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

        rulesExtraDependencies [wikiDeps] $ match "wiki/**/*.md" $ do
            route $ customRoute wikiConceptRoute
            compile $ do
                ident <- getUnderlying
                let path = toFilePath ident
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
                            >>= loadAndApplyTemplate "templates/default.html"       catCtx2
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
                            >>= loadAndApplyTemplate "templates/default.html"      conceptCtx
                            >>= relativizeUrls

        rulesExtraDependencies [wikiDeps] $ create ["wiki/index.html"] $ do
            route idRoute
            compile $ do
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
                    >>= loadAndApplyTemplate "templates/default.html"    ctx
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- fmap (take 10) $ recentFirst =<< loadAll "posts/*.md"
                let indexCtx =
                        listField "posts" postCtx (return posts) <>
                        constField "bodyclass" "siteIndex"       <>
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "pages/about.md" $ do
            route $ customRoute (const "about.html")
            compile $ do
                customPandocCompiler
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

        -- Los libros son solo datos (title, autor y opcionalmente fecha_lectura
        -- y portada): no generan página propia, solo alimentan la galería.
        match "books/*.md" $ compile getResourceBody

        -- Bookshelf se regenera también cuando se agrega o cambia una portada.
        coverDeps <- makePatternDependency "img/libros/*"
        rulesExtraDependencies [coverDeps] $ create ["books.html"] $ do
            route idRoute
            compile $ do
                books <- loadAll "books/*.md" :: Compiler [Item String]
                booksWithYear <- mapM (\b -> do
                        yr <- getMetadataField (itemIdentifier b) "fecha_lectura"
                        return (fromMaybe "" yr, b)
                    ) books
                let grouped = groupByYear booksWithYear
                let yearsCtx = listField "years" yearContext (mapM makeItem grouped)
                    booksCtx =
                        yearsCtx <>
                        constField "title" "Books" <>
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/books-index.html" booksCtx
                    >>= loadAndApplyTemplate "templates/default.html" booksCtx
                    >>= relativizeUrls

        match "pages/research.md" $ do
            route $ customRoute (const "research.html")
            compile $ do
                customPandocCompiler
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

loadWikiTree :: IO [WikiNode]
loadWikiTree = do
    files <- findMarkdownFilesRecursive "wiki"
    buildWikiTree <$> mapM loadRawWikiItem files

loadRawWikiItem :: FilePath -> IO RawWikiItem
loadRawWikiItem fp = do
    contents <- readFile fp
    length contents `seq` return ()   -- leer completo y cerrar el archivo
    let relPath = makeRelative "wiki" fp
        meta    = extractFrontmatter contents
        title   = lookup "title" meta
        ident   = fromFilePath fp
    return RawWikiItem
        { rwPath  = relPath
        , rwTitle = title
        , rwMeta  = meta
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
            (key, ':':val) | not (null (strip key)) -> (strip key, strip val) : go ls
            _                                        -> go ls
        strip = f . f where f = reverse . dropWhile (== ' ')

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
    { feedTitle       = "a-peirogon"
    , feedDescription = "Ensayos de a-peirogon"
    , feedAuthorName  = "a-peirogon"
    , feedAuthorEmail = ""
    , feedRoot        = "https://a-peirogon.github.io"
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
            -- El atributo `color` se acepta pero se ignora: todas las cajas
            -- usan la misma paleta neutra del sitio.
            -- Las cajas son plegables ("Click to expand") salvo `plegable="no"`.
            let titulo = fromMaybe "" (lookup "título" keyvals <|> lookup "titulo" keyvals)
                plegable = lookup "plegable" keyvals /= Just "no"
            Pandoc _ innerBlocks <- runIOorExplode (readMarkdown readerOptions content)
            innerHtmlItem <- runIOorExplode (writeHtml5String writerOptions (Pandoc mempty innerBlocks))
            let innerHtml = T.unpack innerHtmlItem
                tituloHtml = if T.null titulo
                              then ""
                              else "<div class=\"portal-box-title\">" ++ T.unpack titulo ++ "</div>"
                toggleHtml =
                    "<button type=\"button\" class=\"portal-box-toggle\" aria-expanded=\"true\">"
                    ++ "<span class=\"part bottom\"><span class=\"label\">Click to expand</span>"
                    ++ "<span class=\"icon\"><svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 320 512\"><path d=\"M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z\"></path></svg></span></span></button>"
                html = "<div class=\"portal-box" ++ (if plegable then " portal-box-collapsible" else "") ++ "\">"
                    ++ tituloHtml
                    ++ "<div class=\"portal-box-body\">" ++ innerHtml ++ "</div>"
                    ++ (if plegable then toggleHtml else "")
                    ++ "</div>"
            return $ RawBlock (Format "html") (T.pack html)
    mapBlock x = return x

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
            -- Si el SVG ya existe (está versionado), no recompilar: así el CI
            -- no necesita LaTeX para diagramas que no han cambiado.
            svgExists <- doesFileExist svgPath
            svgContent <- if svgExists
                then return (Just "cached")
                else compileTikz (T.unpack content) svgPath
            -- Copiar también al sitio generado: Hakyll arma la lista de archivos
            -- a copiar antes de que este SVG exista, así que en el primer build
            -- (o en `watch`) no llegaría a _site por sí solo.
            case svgContent of
                Just _ -> do
                    let dest = destinationDirectory defaultConfiguration </> svgPath
                    createDirectoryIfMissing True (takeDirectory dest)
                    copyFile svgPath dest
                Nothing -> return ()
            case svgContent of
                Just _ -> do
                    -- Sin width/height explícitos, se muestra a 1,6 veces su
                    -- tamaño natural: pdf2svg lo da en puntos y queda pequeño.
                    natural <- svgNaturalWidth svgPath
                    let autoWidth = case (width, height, natural) of
                            (Nothing, Nothing, Just w) -> Just (T.pack (show (round (w * 1.6) :: Int)))
                            _                          -> width
                        imgTag = "<img src=\"" ++ svgUrl ++ "\""
                               ++ maybe "" (\w -> " width=\"" ++ T.unpack w ++ "\"") autoWidth
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

-- | Ancho natural de un SVG (atributo width de la etiqueta <svg>), si se puede leer.
svgNaturalWidth :: FilePath -> IO (Maybe Double)
svgNaturalWidth path = do
    r <- try (readFile path) :: IO (Either SomeException String)
    return $ case r of
        Left _ -> Nothing
        Right contents ->
            let (_, rest) = T.breakOn "<svg" (T.pack (take 2000 contents))
                (_, w)    = T.breakOn "width=\"" rest
                digits    = T.takeWhile (\c -> c `elem` ("0123456789." :: String)) (T.drop 7 w)
            in case reads (T.unpack digits) of
                [(v, "")] -> Just v
                _         -> Nothing

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

-- | Tabla de contenidos del documento actual, como HTML listo para insertar.
--   Vacía si el documento tiene `no-toc: true` o menos de dos encabezados.
getTocHtml :: Compiler String
getTocHtml = do
    underlying <- getUnderlying
    noTocMeta <- getMetadataField underlying "no-toc"
    if noTocMeta == Just "true"
        then return ""
        else do
            writerOpts <- mkTocWriter writerOptions underlying
            toc <- renderPandocWith readerOptions writerOpts =<< getResourceBody
            let tocBody = killLinkIds (itemBody toc)
                entries = length (T.breakOnAll "<li" (T.pack tocBody))
            return $ if entries < 2
                then ""
                else "<nav id=\"TOC\" class=\"TOC\" aria-label=\"Contenido\">"
                     ++ "<div class=\"TOC-title\">Contenido</div>"
                     ++ tocBody ++ "</nav>"
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
    killLinkIds = asTxt (T.concat . go . T.splitOn "id=\"toc-")
      where
        go [] = []
        go (x:xs) = x : map (T.drop 1 . T.dropWhile (/= '\"')) xs

-- | Inserta la tabla de contenidos justo después del primer <h2> del cuerpo
--   (o al principio, si no hay ninguno), para que flote junto al primer párrafo.
insertToc :: String -> String -> String
insertToc "" html = html
insertToc toc html =
    let txt = T.pack html
        (before, after) = T.breakOn "</h2>" txt
    in if T.null after
        then toc ++ html
        else T.unpack (before <> "</h2>" <> T.pack toc <> T.drop 5 after)

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
    field "year" (\i -> let y = fst (itemBody i)
                        in if null y then noResult "sin año" else return y) <>
    Context (\k _ item -> case k of
        "posts" -> unContext
                     (listField "posts" bookCardCtx (return (snd (itemBody item))))
                     "posts" [] item
        _       -> unContext missingField k [] item)

-- | Contexto de cada libro en la galería de Bookshelf.
--   `portada`: el campo del frontmatter si existe; si no, la primera imagen
--   img/libros/<nombre-del-archivo>.{jpg,jpeg,png,webp} que exista.
--   Si no hay ninguna, el campo queda sin definir y la plantilla dibuja
--   una portada tipográfica.
bookCardCtx :: Context String
bookCardCtx = field "portada" cover <> defaultContext
  where
    cover item = do
        let ident = itemIdentifier item
        explicit <- getMetadataField ident "portada"
        case explicit of
            Just p  -> return p
            Nothing -> do
                let slug = takeBaseName (toFilePath ident)
                    candidates = [ "img/libros/" ++ slug ++ "." ++ ext
                                 | ext <- ["jpg", "jpeg", "png", "webp"] ]
                found <- unsafeCompiler (filterM doesFileExist candidates)
                case found of
                    (p:_) -> return ("/" ++ p)
                    []    -> noResult "sin portada"

wrapFootnotesInDetails :: String -> String
wrapFootnotesInDetails html =
    case firstJust [ wrapWith tag | tag <- ["aside", "section"] ] of
        Just wrapped -> T.unpack wrapped
        Nothing      -> html
  where
    txt = T.pack html

    firstJust xs = case [ x | Just x <- xs ] of
        (x:_) -> Just x
        []    -> Nothing

    -- Pandoc 3 emite <aside id="footnotes">; versiones anteriores, <section id="footnotes">.
    wrapWith :: T.Text -> Maybe T.Text
    wrapWith tag = do
        let openTag  = "<" <> tag
            closeTag = "</" <> tag <> ">"
            marker   = openTag <> " id=\"footnotes\""
            (before, atMarker) = T.breakOn marker txt
        if T.null atMarker then Nothing else do
            (block, after) <- matchClose openTag closeTag atMarker
            return $ before
                  <> "<details class=\"footnotes-details\" open><summary>Notas al pie</summary>"
                  <> block <> "</details>" <> after

    -- Devuelve el bloque completo (desde la etiqueta de apertura hasta su cierre
    -- correspondiente, respetando anidamiento) y el resto del documento.
    matchClose :: T.Text -> T.Text -> T.Text -> Maybe (T.Text, T.Text)
    matchClose openTag closeTag = go 0 ""
      where
        go :: Int -> T.Text -> T.Text -> Maybe (T.Text, T.Text)
        go depth acc rest
            | T.null rest = Nothing
            | openTag `T.isPrefixOf` rest =
                let (t, r) = T.splitAt (T.length openTag) rest
                in go (depth + 1) (acc <> t) r
            | closeTag `T.isPrefixOf` rest =
                let (t, r) = T.splitAt (T.length closeTag) rest
                in if depth == 1 then Just (acc <> t, r) else go (depth - 1) (acc <> t) r
            | otherwise =
                let (chunk, r) = T.break (== '<') (T.drop 1 rest)
                in go depth (acc <> T.take 1 rest <> chunk) r
