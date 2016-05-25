{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Model.CoreTypes
-- import Model.ResponseTypes
-- import Web.Utils
-- import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Configurator as C
import Data.ByteString (pack)
import Data.Monoid
import Data.Aeson hiding (json) -- (ToJSON, FromJSON, object, toJSON, (.=))

import Web.Spock.Safe (SpockActionCtx, SpockCtxM, runSpock, spock, defaultSpockCfg, PoolOrConn (PCPool) , getState, prehook, middleware, get, getpost, writeSession, redirect, getContext, setStatus, preferredFormat, json, ClientPreferredFormat(PrefJSON), readSession, file, root, (<//>), var, text, SpockCtxT, WebStateM, WebStateM())
-- SpockT, params, post, getpost, ActionT
-- import Web.Spock.Safe hiding (SessionId)
-- import Web.Spock.Internal.Wire (ActionCtxT)
import Web.Spock.Shared (lazyBytes, runQuery, HasSpock, SpockConn, ActionCtxT)

import Data.HVect
import Network.Wai.Middleware.RequestLogger -- (logStdoutDev)
import Network.Wai.Middleware.Static
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlConn)

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Web.Views.Site (SiteView(..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

type SessionVal = Maybe SessionId
type MyApp ctx = SpockCtxM ctx SqlBackend SessionVal MyState ()
type MyAction ctx a = SpockActionCtx ctx SqlBackend SessionVal MyState a
-- type App = SpockM SqlBackend SessionVal () ()

data MyState = MyState
   { bscfg :: MyCfg }

data MyCfg
   = MyCfg
   { bcfgdb   :: T.Text
   , bcfgport :: Int
   , bcfgname :: T.Text
   , bcfgdesc :: T.Text
   , bcfgconn :: ConnectionString}

data Person = Person {
      name :: String
    , age  :: Int
    } deriving Show

instance ToJSON Person where
    toJSON (Person name age) =
        object ["name" .= name, "age" .= age]

joe = (Person {name = "Joe", age = 12})

parseConfig :: FilePath -> IO MyCfg
parseConfig cfgFile =
    do cfg  <- C.load [C.Required cfgFile]
       db   <- C.require cfg "db"
       port <- C.require cfg "port"
       name <- C.require cfg "blogName"
       desc <- C.require cfg "blogDescription"
       conn <- C.require cfg "connStr"
       return (MyCfg db port name desc conn)

connStr :: ConnectionString
connStr = "host=localhost dbname=funblog user=test password=test port=5432"

kaffeH :: T.Text -> T.Text -> MyAction ctx a
kaffeH d n = text ("Drick mycke " <> d <> " " <> n <> "!!")

jsonnameH :: T.Text -> MyAction ctx a
jsonnameH name = json $ object [ "name" .= name]

jsontextH :: MyAction ctx a
jsontextH = jsonnameH "Tjoho"

gethtml :: String -> MyAction ctx a
gethtml name = file "text/html" $ "public/" <> name

indexH :: MyAction ctx a
indexH = gethtml "index.html"

-- blaze :: MonadIO m => Html ->  ctx m a
blaze :: MonadIO m => Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml
{-# INLINE blaze #-}

siteView :: SiteView -> H.Html -> H.Html
siteView sv body =
    H.html $
    do H.head $
        do H.title (H.toHtml $ sv_blogName sv)
           H.meta ! A.charset "utf-8"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
           H.link ! A.href "/css/blog.css" ! A.rel "stylesheet"
       H.body $
        do H.div ! A.class_ "blog-masthead" $
            do H.span body

mkSite :: (SiteView -> Html) -> MyAction ctx a
mkSite content =
    -- maybeUser $ \mUser ->
    do blogSt <- getState
       let cfg = 1 -- bs_cfg blogSt
           sv =
               SiteView
               { sv_blogName = "Haskell" -- bcfg_name cfg
               , sv_blogDesc = "Spock" -- bcfg_desc cfg
               , sv_user     = Nothing -- fmap snd mUser
               }
       blaze $ siteView sv (content sv)

-- mkSite' :: Html -> MyAction ctx a
-- mkSite' content = mkSite (const content)

homeView :: [Entity Post] -> SiteView -> H.Html
homeView posts sv =
    do H.div ! A.class_ "blog-header" $
        do H.h1 ! A.class_ "blog-title" $ H.toHtml $ sv_blogName sv
           H.p ! A.class_ "lead blog-description" $ H.toHtml $ sv_blogDesc sv
       H.div ! A.class_ "row" $
        do H.div ! A.class_ "col-sm-8 blog-main" $
            forM_ posts $ \post ->
            H.div ! A.class_ "blog-post" $
                do H.h2 ! A.class_ "blog-post-title" $ H.toHtml $ postTitle (entityVal post)
                   H.p ! A.class_ "blog-post-meta" $ H.toHtml $ show $ postDate (entityVal post)
                   H.p (H.toHtml $ postContent (entityVal post))

mkRawSite :: Html -> MyAction ctx a
mkRawSite content = blaze content

-- mkJsonSite :: json -> MyAction ctx a
-- mkJsonSite json = json

rawView :: H.Html
rawView = H.h1 "Min HTML"

dbView :: [Entity Post] -> H.Html
dbView posts =
    do H.div $
        forM_ posts $ \post ->
        H.div ! A.class_ "blog-post" $
            do H.h2 ! A.class_ "blog-post-title" $ H.toHtml $ postTitle (entityVal post)

-- jsonView :: [Entity Post] -> json
-- jsonView = jsonnameH "Tjoho"

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}

appMiddleware :: MyApp ctx
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "public")

appRoutes :: SpockCtxT ctx (WebStateM SqlBackend SessionVal MyState) ()
appRoutes = do
  get root $
    do allPosts <- runSQL $ selectList [] [Desc PostDate]
       mkSite (homeView allPosts)
  get ("json") $ json $ joe
  get ("raw") $ mkRawSite rawView
  get ("db") $
    do allPosts <- runSQL $ selectList [] [Desc PostDate]
       mkRawSite $ dbView allPosts
  get ("dbjson") $
    do allPosts <- runSQL $ selectList [] [Desc PostDate]
       -- jsonnameH allPosts
       -- jsonnameH "dbjson"
       mkRawSite $ dbView allPosts
  get ("html") indexH
  get ("jsonname" <//> var) jsonnameH
  get "jsontext" jsontextH
  get ("drick" <//> var <//> var) kaffeH

blogApp :: MyApp ()
blogApp =
  prehook baseHook $
  appMiddleware >> appRoutes

baseHook :: MyAction () (HVect '[])
baseHook = return HNil

runBlog :: MyCfg -> IO ()
runBlog bcfg = do
    pool <- runNoLoggingT $ createPostgresqlPool connStr 1
    runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
    runSpock 3001 $ spock (spockCfg pool) blogApp
  where
    spockCfg pool = defaultSpockCfg Nothing (PCPool pool) (MyState bcfg)

main :: IO ()
main = do
  cfg <- parseConfig "mini.cfg"
  runBlog cfg
