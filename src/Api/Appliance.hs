{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Appliance where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, get,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models

type ApplianceAPI =
         "appliances" :> Get '[JSON] [Entity Appliance]
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> Get '[JSON] (Appliance)
    :<|> "appliances" :> ReqBody '[JSON] Appliance :> Post '[JSON] Int64

-- | The server that runs the ApplianceAPI
applianceServer :: ServerT ApplianceAPI App
applianceServer = allAppliances :<|> singleAppliance :<|> createAppliance

-- | Returns all appliances in the database.
allAppliances :: App [Entity Appliance]
allAppliances =
    runDb (selectList [] [])

-- | Returns an appliance by name or throws a 404 error.
singleAppliance :: Key Appliance -> App (Appliance)
singleAppliance applianceId = do
    maybeAppliance <- runDb (get applianceId)
    case maybeAppliance of
         Nothing ->
            throwError err404
         Just appliance ->
            return appliance

-- | Creates an appliance in the database.
createAppliance :: Appliance -> App Int64
createAppliance p = do
    maybeUser <- runDb (selectFirst [UserName ==. "Ricky Hariady"] [])
    case maybeUser of
      Nothing ->
        throwError err401
      Just user -> do
        newAppliance <- runDb (insert (Appliance (entityKey user) (applianceName p)))
        return $ fromSqlKey newAppliance

