{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Environment where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, get, delete,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models

type EnvironmentAPI =
         "environments" :> Get '[JSON] [Entity Environment]
    :<|> "environments" :> Capture "environmentId" (Key Environment) :> Get '[JSON] (Environment)
    :<|> "environments" :> Capture "environmentId" (Key Environment) :> Delete '[JSON] (String)
    :<|> "environments" :> ReqBody '[JSON] Environment :> Post '[JSON] Int64
    :<|> "environments" :> Capture "environtmentId" (Key Environment) :> "appliances" :> Get '[JSON] [Entity Appliance]

-- | The server that runs the EnvironmentAPI
environmentServer :: ServerT EnvironmentAPI App
environmentServer = allEnvironments :<|> singleEnvironment :<|> deleteEnvironment :<|> createEnvironment :<|> singleEnvironmentAppliances

-- | Returns all environments in the database.
allEnvironments :: App [Entity Environment]
allEnvironments = do
    maybeUser <- runDb (selectFirst [UserName ==. "Ricky Hariady"] [])
    case maybeUser of
      Nothing ->
        throwError err401
      Just user -> do
        runDb (selectList [EnvironmentUserId ==. (entityKey user)] [])

-- | Returns an environment by name or throws a 404 error.
singleEnvironment :: Key Environment -> App (Environment)
singleEnvironment environmentId = do
    maybeEnvironment <- runDb (get environmentId)
    case maybeEnvironment of
         Nothing ->
            throwError err404
         Just environment ->
            return environment

-- | Returns an appliance by name or throws a 404 error.
deleteEnvironment :: Key Environment -> App (String)
deleteEnvironment environmentId = do
    runDb (delete environmentId)
    return "OK"

-- | Creates an environment in the database.
createEnvironment :: Environment -> App Int64
createEnvironment p = do
    maybeUser <- runDb (selectFirst [UserName ==. "Ricky Hariady"] [])
    case maybeUser of
      Nothing ->
        throwError err401
      Just user -> do
        newEnvironment <- runDb (insert (Environment (entityKey user) (environmentParentId p) (environmentName p)))
        return $ fromSqlKey newEnvironment

-- | Returns an environment by name or throws a 404 error.
singleEnvironmentAppliances :: Key Environment -> App [Entity Appliance]
singleEnvironmentAppliances environmentId = do
    maybeEnvironment <- runDb (get environmentId)
    case maybeEnvironment of
         Nothing ->
            throwError err404
         Just environment ->
            runDb (selectList [ApplianceEnvironmentId ==. (environmentId) ] [])
