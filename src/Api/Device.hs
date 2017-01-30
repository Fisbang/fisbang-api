{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Device where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, get,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models

type DeviceAPI =
         "devices" :> Get '[JSON] [Entity Device]
    :<|> "devices" :> Capture "deviceId" (Key Device) :> Get '[JSON] (Device)
    :<|> "devices" :> ReqBody '[JSON] Device :> Post '[JSON] Int64

-- | The server that runs the DeviceAPI
deviceServer :: ServerT DeviceAPI App
deviceServer = allDevices :<|> singleDevice :<|> createDevice

-- | Returns all devices in the database.
allDevices :: App [Entity Device]
allDevices =
    runDb (selectList [] [])

-- | Returns an device by Id or throws a 404 error.
singleDevice :: Key Device -> App (Device)
singleDevice deviceId = do
    maybeDevice <- runDb (get deviceId)
    case maybeDevice of
         Nothing ->
            throwError err404
         Just device ->
            return device

-- | Creates an device in the database.
createDevice :: Device -> App Int64
createDevice p = do
    maybeUser <- runDb (selectFirst [UserName ==. "Ricky Hariady"] [])
    case maybeUser of
      Nothing ->
        throwError err401
      Just user -> do
        newDevice <- runDb (insert (Device (entityKey user) (deviceToken p) (Nothing) (Nothing)))
        return $ fromSqlKey newDevice

