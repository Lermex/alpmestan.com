-- A significant part of this code has been borrowed from other
-- hakyll users, mostly Jasper through his site and hakyll's,
-- but also skybluetrades.net and chromaticleaves.com

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Char
import Data.Maybe (catMaybes)
import Data.Monoid (mappend, (<>), mconcat, mempty)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import Hakyll
import Hakyll.Web.Tags
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import Text.Pandoc
import Text.Pandoc.Options


main :: IO ()
main = do 
    year <- getCurrentYear

    hakyllWith config $ do

        match "favicon.ico" $ do
            route idRoute
            compile copyFileCompiler

        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "servant/*" $ do
            route idRoute
            compile copyFileCompiler

        match "js/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*.css" $ do
            route   idRoute
            compile compressCssCompiler

        match "css/*.ttf" $ do
            route   idRoute
            compile copyFileCompiler

        match "templates/*" $ compile templateCompiler

        -- build tags
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ myPandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" (defaultContext `mappend` yearCtx year)
                >>= relativizeUrls

        match "pages/*" $ do
            route $ gsubRoute "pages/" (const "") `composeRoutes`
                setExtension ".html"
            compile $ myPandocCompiler
                  >>= loadAndApplyTemplate "templates/page.html" defaultContext
                  >>= loadAndApplyTemplate "templates/default.html" (defaultContext `mappend` yearCtx year)
                  >>= relativizeUrls

        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots "posts/*" "content"
                    >>= fmap (take 10) . recentFirst
                    >>= renderRss myFeedConfiguration feedCtx

        -- create a listing of all posts, most recent first
        create ["posts.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let ctx = constField "title" "Posts" <>
                            listField "posts" (postCtx tags) (return posts) <>
                            defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" (ctx `mappend` yearCtx year)
                    >>= relativizeUrls

        -- Post tags
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged " ++ tag

            -- Copied from posts, need to refactor
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title <>
                            listField "posts" (postCtx tags) (return posts) <>
                            defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" (ctx `mappend` yearCtx year)
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
                let indexCtx = 
                      listField "posts" (postCtx tags) (return posts) <>
                      field "tagcloud" (\_ -> myTagCloud tags) <>
                      defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls


-- -----------------------------------------------------------------------------
-- * Contexts

-- | Creates a "year" context from a string representation of the current year
yearCtx :: String -> Context String
yearCtx year = field "year" $ \item -> return year

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%e %b %Y" 
    , dateField "date" "%B %e, %Y"
    , myTagsField "tags" tags 
    , defaultContext 
    ]

pageCtx :: Context String
pageCtx = mconcat
    [ modificationTimeField "mtime" "%e %b %Y" 
    , defaultContext 
    ]

-- -----------------------------------------------------------------------------
-- * Feed configuration

-- | Holds my feed's configuration
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Alp Mestanogullari's blog"
    , feedDescription = "From Hask Till Dawn"
    , feedAuthorName  = "Alp Mestanogullari"
    , feedAuthorEmail = "alpmestan@gmail.com"
    , feedRoot        = "http://alpmestan.com"
    }

-- -----------------------------------------------------------------------------
-- * Compilers

-- | Creates a compiler to render a list of posts for a given pattern, context,
-- and sorting/filtering function
postList :: Pattern
         -> Context String
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern postCtx sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts

-- -----------------------------------------------------------------------------
-- * Helpers
-- 
getCurrentYear :: IO String
getCurrentYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

myTagCloud :: Tags -> Compiler String
myTagCloud tags = 
    renderTagCloud 80 250 tags

myTagsField :: String -> Tags -> Context a
myTagsField =
    tagsFieldWith getTags renderOneTag $ \tagLinks -> do
        H.ul ! A.class_ "list-inline" $ do 
            H.li $ H.i ! A.class_ "fa fa-tags" $ mempty
            sequence_ tagLinks


renderOneTag :: String -> Maybe FilePath -> Maybe H.Html
renderOneTag _ Nothing           = Nothing
renderOneTag tag (Just filepath) = 
    Just $ H.li $ 
        H.a ! A.href (toValue $ toUrl filepath) $ toHtml tag

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync -avz -e ssh ./_site/ \ 
                       \ alp@alpmestan.com:public_html/" 
    }


myPandocCompiler' :: Maybe String -> Compiler (Item String)
myPandocCompiler' withToc = 
    pandocCompilerWith defaultHakyllReaderOptions $
        case withToc of
            Just x | map toLower x `elem` ["true", "yes"] -> writerWithToc
                   | otherwise                            -> writerOpts
            Nothing                                       -> writerOpts

    where writerOpts = defaultHakyllWriterOptions 
                           { writerReferenceLinks = True
                           , writerSectionDivs = True 
                           , writerHtml5 = True
                           , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                           , writerColumns = 100 
                           }
          writerWithToc = 
            writerOpts { writerTableOfContents = True 
                       , writerTemplate = "$if(toc)$<div id=\"toc\"><h3>Table of contents</h3>$toc$</div>$endif$\n$body$" 
                       , writerStandalone = True 
                       }

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
    ident <- getUnderlying
    myPandocCompiler' =<< getMetadataField ident "toc"

--------------------------------------------------------------------------------

