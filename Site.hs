--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll

--------------------------------------------------------------------------------
import           Cook
--------------------------------------------------------------------------------
papersCompiler :: Compiler (Item String)
papersCompiler = do
  csl <- load "papers/acm_citation_style.csl"
  bib <- load "papers/biblio.bib"
  getResourceBody
    >>= readPandocBiblio defaultHakyllReaderOptions csl bib
    >>= return . writePandoc
    
main :: IO ()
main = hakyll $ do
    match "recipes/*" $ do
        route $ setExtension "html"
        compile $ Cook.store_to_html_compiler
            >>= loadAndApplyTemplate "templates/recipe.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext 

    create ["recipes.html"] $ do
        route idRoute
        compile $ do
            recipes <- loadAll "recipes/*"
            let archiveCtx =
                    constField "title" "Recipes"
                    <> listField "recipes" defaultContext (return recipes)
                    <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/recipe_list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "papers/*.markdown" $ do
        route $ setExtension "html"
        compile $ papersCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

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
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    defaultContext
    <> dateField "date" "%B %e, %Y"
