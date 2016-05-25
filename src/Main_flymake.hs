{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Model.CoreTypes
import Model.ResponseTypes
-- import Web.Utils

-- import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Configurator as C
import Data.ByteString (pack)
import Data.Monoid
import Data.Aeson hiding (json) -- (ToJSON, FromJSON, object, toJSON, (.=))
-- import Web.Spock.Safe (file, SpockT, params, runSpock, spockT, text, post, get, root, SpockActionCtx, SpockCtxM, spock, defaultSpockCfg, PoolOrConn (PCPool) , getState, prehook, middleware, getpost, writeSession, redirect, getContext, setStatus, preferredFormat, json, ClientPreferredFormat(PrefJSON), readSession, var, (<//>), ActionT)
import Web.Spock.Safe hiding (SessionId)
-- import Web.Spock.Internal.Wire (ActionCtxT)

import Data.HVect
import Network.Wai.Middleware.RequestLogger -- (logStdoutDev)
import Network.Wai.Middleware.Static
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
-- import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)
-- import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlConn)
-- import Database.Persist.TH
-- import Web.Spock.Shared

-- import Database.Persist
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Web.Views.Site (SiteView(..))


type SessionVal = Maybe SessionId
type MyApp ctx = SpockCtxM ctx SqlBackend SessionVal MyState ()
-- type App = SpockM SqlBackend SessionVal () ()
type MyAction ctx a = SpockActionCtx ctx SqlBackend SessionVal MyState a

-- type HandlerM = ActionT IO
-- type HandlerM = MyAction ctx a

data MyState = MyState
   { bscfg :: MyCfg }

data MyCfg
   = MyCfg
   { bcfgdb   :: T.Text
   , bcfgport :: Int
   , bcfgname :: T.Text
   , bcfgdesc :: T.Text
   , bcfgconn :: ConnectionString}

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

-- homeH :: HandlerM ()
homeH :: MyAction ctx a
homeH = text "Spocky!!"

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

testH :: MyAction ctx a
testH = text "Test"

-- {-# INLINE runSQL #-}
-- runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
-- runSQL action =
--     runQuery $ \conn ->
--         runResourceT $ runNoLoggingT $ runSqlConn action conn

-- dbget :: HandlerM ()
-- dbget =
  -- do
  -- res  :: [Entity Post] <- selectList [] [Desc PostDate, LimitTo 1]
  -- allPosts <- runSQL $ selectList [] [Desc PostDate]
  -- text "tjo"

-- postH = do allPosts <- runSQL $ selectList [] [Desc PostDate]
--            allPosts

mkSite' :: Html -> MyAction ctx a
mkSite' content = mkSite (const content)

mkSite :: (SiteView -> Html) -> MyAction ctx a
mkSite content =
    maybeUser $ \mUser ->
    do blogSt <- getState
       let cfg = bs_cfg blogSt
           sv =
               SiteView
               { sv_blogName = bcfg_name cfg
               , sv_blogDesc = bcfg_desc cfg
               , sv_user     = fmap snd mUser
               }
       blaze $ siteView sv (content sv)

homeView :: [Entity Post] -> SiteView -> H.Html
homeView posts sv =
    do H.li $ H.a ! A.href "#" $ "Fooo"
    -- do H.div ! A.class_ "blog-header" $

-- appRoutes :: SpockT IO ()
appRoutes = -- do
  get root indexH
  -- get root $
  --     do allPosts <- runSQL $ selectList [] [Desc PostDate]
  --        text "Tjo"
        -- mkSite (homeView allPosts)
 -- get "spock" homeH
 -- get ("drick" <//> var <//> var) kaffeH
  -- get "/users" $
  --     do allPosts <- runSQL $ selectList [] [Desc PostDate]
  --        allPosts

--   get ("jsonname"  <//> var) jsonnameH
--   get "jsontext" jsontextH

  -- get "/posts" $
  --     do allPosts <- runSQL $ selectList [] [Desc PostDate]
  --       mkSite (homeView allPosts)


  -- get ("user" <//> var) $ \userId -> do
  --   user <- withDb $ get (UserKey $ SqlBackendKey userId)
  --   case user of
  --     Nothing -> setStatus status404
  --     Just u  -> text (userFirstname u)
  -- get ("user" <//> "new" <//> var <//> var) $
  --   \fname lname -> do
  --     user <- withDb $ insertUnique (User fname lname)
  --     case user of
  --       Nothing -> text "Duplicate user"
  --       Just k -> text ("New user id " <> pack (show k))

-- blogApp :: MyApp ()
-- blogApp =
--   prehook baseHook $
--   do middleware (staticPolicy (addBase "static"))
--     get "/" $
--       do allPosts <- runSQL $ selectList [] [Desc PostDate]
--         text "hej"

baseHook :: MyAction () (HVect '[])
baseHook = return HNil

appMiddleware :: MyApp ctx
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "public")

server :: MyApp ()
server = prehook baseHook $
  appMiddleware >> appRoutes

runBlog :: MyCfg -> IO ()
runBlog bcfg =
    do
       pool <- runNoLoggingT $ createPostgresqlPool connStr 1
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       runSpock 3001 $ spock (spockCfg pool) blogApp -- server
    where
      spockCfg pool = defaultSpockCfg Nothing (PCPool pool) (MyState bcfg)

main :: IO ()
main = do cfg <- parseConfig "mini.cfg"
          runBlog cfg

-- main = do
  -- Data.appRoutes
