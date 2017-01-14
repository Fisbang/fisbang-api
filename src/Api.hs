{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators   #-}

module Api
    ( app
    , generateJavaScript
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models

import           Api.User
import           Api.Appliance

type FisbangAPI =  UserAPI :<|> ApplianceAPI

fisbangAPIServer = userServer :<|> applianceServer

-- | This is the function we export to run our 'FisbangAPI'. Given
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
fisbangApi :: Config -> Application
fisbangApi cfg = serve (Proxy :: Proxy FisbangAPI) (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server FisbangAPI
appToServer cfg = enter (convertApp cfg) fisbangAPIServer

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Application
files = serveDirectory "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type FisbangAPP = FisbangAPI :<|> Raw

fisbangApp :: Proxy FisbangAPP
fisbangApp = Proxy

-- | Finally, this function takes a configuration and runs our 'FisbangApp'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: Config -> Application
app cfg =
    serve fisbangApp (appToServer cfg :<|> files)

-- | Generates JavaScript to query the API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy FisbangAPI) vanillaJS "./assets/api.js"
