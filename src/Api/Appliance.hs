{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Appliance where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, get, delete, update,
                                              selectFirst, selectList, (==.), (=.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models

type ApplianceAPI =
         "appliances" :> Get '[JSON] [Entity Appliance]
    :<|> "appliances" :> ReqBody '[JSON] Appliance :> Post '[JSON] Int64
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> Get '[JSON] (Appliance)
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> Delete '[JSON] (String)
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> ReqBody '[JSON] Appliance :> Put '[JSON] (Appliance)
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> "devices" :> Get '[JSON] [Entity Device]
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> "devices" :> ReqBody '[JSON] (Entity Device) :> Post '[JSON] [Entity Device]
    :<|> "appliances" :> Capture "applianceId" (Key Appliance) :> "devices" :> Capture "deviceId" (Key Device) :> Delete '[JSON] [Entity Device]

-- | The server that runs the ApplianceAPI
applianceServer :: ServerT ApplianceAPI App
applianceServer = allAppliances :<|> createAppliance :<|> singleAppliance :<|> deleteAppliance :<|> updateAppliance :<|> getApplianceDevice :<|> postApplianceDevice :<|> deleteApplianceDevice

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

-- | Returns an appliance by name or throws a 404 error.
deleteAppliance :: Key Appliance -> App (String)
deleteAppliance applianceId = do
    runDb (delete applianceId)
    return "OK"


-- | Creates an appliance in the database.
createAppliance :: Appliance -> App Int64
createAppliance p = do
    maybeUser <- runDb (selectFirst [UserName ==. "Ricky Hariady"] [])
    case maybeUser of
      Nothing ->
        throwError err401
      Just user -> do
        newAppliance <- runDb (insert (Appliance (entityKey user) (applianceEnvironmentId p) (applianceName p)))
        return $ fromSqlKey newAppliance

-- | Returns an appliance by name or throws a 404 error.
updateAppliance :: Key Appliance -> Appliance -> App (Appliance)
updateAppliance applianceId newAppliance = do
    maybeAppliance <- runDb (get applianceId)
    case maybeAppliance of
         Nothing ->
            throwError err404
         Just appliance -> do
            runDb (update applianceId [ApplianceEnvironmentId =. (applianceEnvironmentId newAppliance), ApplianceName =. (applianceName newAppliance)])
            return newAppliance

getApplianceDevice :: Key Appliance -> App [Entity Device]
getApplianceDevice applianceId = do
    maybeAppliance <- runDb (get applianceId)
    case maybeAppliance of
         Nothing ->
            throwError err404
         Just appliance -> do
           runDb (selectList [DeviceApplianceId ==. (Just applianceId)] [])

postApplianceDevice :: Key Appliance -> Entity Device -> App [Entity Device]
postApplianceDevice applianceId device = do
  maybeAppliance <- runDb (get applianceId)
  case maybeAppliance of
    Nothing ->
      throwError err404
    Just appliance -> do
      maybeDevice <- runDb (get (entityKey device))
      case maybeDevice of
        Nothing ->
          throwError err404
        Just _ -> do
          runDb (update (entityKey device) [DeviceApplianceId =. (Just applianceId), DeviceEnvironmentId =. Nothing])
          runDb (selectList [DeviceApplianceId ==. (Just applianceId)] [])

deleteApplianceDevice :: Key Appliance -> Key Device -> App [Entity Device]
deleteApplianceDevice applianceId deviceId = do
  maybeAppliance <- runDb (get applianceId)
  case maybeAppliance of
    Nothing ->
      throwError err404
    Just appliance -> do
      maybeDevice <- runDb (selectFirst [DeviceId ==. deviceId, DeviceApplianceId ==. (Just applianceId)] [])
      case maybeDevice of
        Nothing ->
          throwError err404
        Just device -> do
          runDb (update (deviceId) [DeviceApplianceId =. Nothing])
          runDb (selectList [DeviceApplianceId ==. (Just applianceId)] [])
          
             
            
  
