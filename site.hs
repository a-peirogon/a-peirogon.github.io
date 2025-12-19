{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc (Pandoc(..), Block(..), Inline(..), Format(..))
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Templates (compileTemplate)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Data.Char (isUpper, isAlpha)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException)
import System.FilePath ((</>), takeBaseName)
import System.Directory (createDirectoryIfMissing)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = hakyll $ do
    -- 1. Recursos estáticos
    match ("css/*" .||. "js/*" .||. "img/*" .||. "fonts/**" .||. "favicon.ico" .||. "generated/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- 2. Plantillas
    match "templates/*" $ compile templateCompiler

    -- 3. Posts
    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ do
            tocCtx <- getTocCtx postCtx
            customPandocCompiler
                >>= saveSnapshot "content"  -- Para feeds
                >>= loadAndApplyTemplate "templates/post.html"    tocCtx
                >>= loadAndApplyTemplate "templates/default.html" tocCtx
                >>= relativizeUrls

    -- 4. Index
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

    -- 5. Otras páginas
    match "about.md" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "books.md" $ do
        route $ constRoute "books/index.html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- 6. Feed Atom
    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*.md" "content"
            renderAtom feedConfig feedCtx posts

--------------------------------------------------------------------------------
-- CONTEXTOS
--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

-- Configuración del feed
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
    -- 1. Leemos el Markdown a formato Pandoc
    pandoc <- readPandocWith readerOptions =<< getResourceBody

    -- 2. Aplicamos transformaciones al AST en IO
    let applyTransforms :: Pandoc -> IO Pandoc
        applyTransforms p = do
            -- Primero smallcaps (pura)
            let withSmallCaps = smallCapsTransform p
            -- IMPORTANTE: TikZ ANTES de Pygments para que no intente resaltarlo
            withTikz <- tikzTransform withSmallCaps
            -- Luego pygments (IO) - ya no verá los bloques tikzpicture
            pygmentsTransform withTikz

    -- 3. Ejecutamos las transformaciones en IO usando unsafeCompiler
    transformed <- unsafeCompiler $ traverse applyTransforms pandoc

    -- 4. Creamos el Item transformado y escribimos a HTML
    return $ writePandocWith writerOptions transformed

-- Opciones de lectura
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

-- Opciones de escritura (KaTeX activado)
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

--------------------------------------------------------------------------------
-- TRANSFORMACIONES
--------------------------------------------------------------------------------

-- 1. SMALL CAPS INTELIGENTES
smallCapsTransform :: Pandoc -> Pandoc
smallCapsTransform = walk mapInline
  where
    mapInline :: Inline -> Inline
    mapInline (Str s)
        | T.length s > 1 && T.all isUpper s && T.all isAlpha s =
            Span ("", ["smallcaps"], []) [Str s]
        | otherwise = Str s
    mapInline x = x

-- 2. PYGMENTS (Resaltado de sintaxis externo)
pygmentsTransform :: Pandoc -> IO Pandoc
pygmentsTransform = walkM mapBlock
  where
    mapBlock :: Block -> IO Block
    mapBlock (CodeBlock (ident, classes, keyvals) content)
        | not (null classes) = do
            let lang = head classes
            let code = T.unpack content

            -- Intenta usar pygmentize con manejo de errores
            result <- try $ readProcess "pygmentize"
                ["-l", T.unpack lang, "-f", "html", "-O", "cssclass=sourceCode"]
                code :: IO (Either SomeException String)

            case result of
                Right highlighted ->
                    return $ RawBlock (Format "html") (T.pack highlighted)
                Left _ ->
                    return $ CodeBlock (ident, "sourceCode" : classes, keyvals) content
    mapBlock x = return x

-- 3. TIKZ (Compilación de gráficos LaTeX con caché y DEBUG)
tikzTransform :: Pandoc -> IO Pandoc
tikzTransform = walkM mapBlock
  where
    mapBlock :: Block -> IO Block
    mapBlock (CodeBlock (_, classes, keyvals) content)
        | "tikzpicture" `elem` classes = do
            -- Genera hash del contenido para caché
            let contentHash = BS.unpack $ B16.encode $ hash $ BS.pack $ T.unpack content
                svgPath = "generated/tikz/" ++ contentHash ++ ".svg"
                svgUrl = "/" ++ svgPath
                width = lookup "width" keyvals
                height = lookup "height" keyvals
                caption = lookup "caption" keyvals

            createDirectoryIfMissing True "generated/tikz"

            -- Intenta compilar TikZ
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
                    -- Si falla, imprime aviso en consola para que el usuario lo vea
                    putStrLn $ "WARNING: Could not compile TikZ diagram " ++ contentHash
                    return $ CodeBlock ("", ["tikzpicture-error"], keyvals) content
    mapBlock x = return x

-- Función auxiliar para compilar TikZ a SVG
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

--------------------------------------------------------------------------------
-- LÓGICA DE TOC
--------------------------------------------------------------------------------

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
            let finalToc = if null (trim tocBody)
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

asTxt f = T.unpack . f . T.pack
