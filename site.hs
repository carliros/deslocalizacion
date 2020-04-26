--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (liftM)
import           Hakyll
import           Data.Time.LocalTime (getZonedTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting (tango)

--------------------------------------------------------------------------------
main :: IO ()
main =
  do utcTime <- getZonedTime
     let stringUtcTime = formatTime defaultTimeLocale "%Y/%m/%d %H:%M" utcTime
     hakyll $ do
        match "biblio/*.bib" $ compile biblioCompiler

        match "biblio/*.csl" $ compile cslCompiler

        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match (fromList ["about.rst", "recursos.markdown"]) $ do
            route   $ setExtension "html"
            compile $ do
              let customCtx = buildDateCtx stringUtcTime defaultContext
              pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" customCtx
                >>= relativizeUrls

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ do
              let postsCtx = buildDateCtx stringUtcTime postCtx
              customBiblioCompiler
                  >>= loadAndApplyTemplate "templates/post.html"    postsCtx
                  >>= loadAndApplyTemplate "templates/default.html" postsCtx
                  >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        buildDateCtx stringUtcTime defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Bienvenida"          `mappend`
                        buildDateCtx stringUtcTime defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

buildDateCtx :: String -> Context String -> Context String
buildDateCtx stringUtcTime baseCtx =
    constField "currentUTCDate" stringUtcTime `mappend`
    baseCtx

chicagoCsl = "biblio/chicago-fullnote-bibliography-with-ibid.csl"
mainBib = "biblio/main.bib"
customBiblioCompiler :: Compiler (Item String)
customBiblioCompiler = do
    csl <- load $ fromFilePath chicagoCsl
    bib <- load $ fromFilePath mainBib
    liftM (writePandocWith wopt)
            (getResourceBody >>= readPandocBiblio ropt csl bib)
  where wopt = defaultHakyllWriterOptions
                  {   -- enable tango format highlight
                      writerHighlightStyle = Just tango
                  }
        ropt = defaultHakyllReaderOptions
                  {   -- enable citation rendering
                      readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHakyllReaderOptions
                  }
