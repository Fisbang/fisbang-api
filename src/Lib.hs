{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Environment = Environment
  { environmentId       :: Int
  , environmentName     :: String
  , environmentParent   :: Maybe Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Environment)

data Appliance = Appliance
  { applianceId         :: Int
  , applianceName       :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Appliance)

data Device = Device
  { deviceId    :: Int
  , deviceType  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Device)

type FisbangAPI = "environments" :> Get '[JSON] [Environment]
             :<|> "appliances" :> Get '[JSON] [Appliance]
             :<|> "devices" :> Get '[JSON] [Device]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy FisbangAPI
api = Proxy

server :: Server FisbangAPI
server = return environments
    :<|> return appliances
    :<|> return devices

environments :: [Environment]
environments = [ Environment 1 "My Home" Nothing
               , Environment 2 "Living Room" $ Just 1
               , Environment 3 "Bed Room" $ Just 1
               ]

appliances :: [Appliance]
appliances = [ Appliance 1 "Television"
             , Appliance 2 "Refrigerator"
             , Appliance 3 "Air Conditioner"
             ]

devices :: [Device]
devices = [ Device 1 "Main Power"
          , Device 2 "Socket"
          , Device 3 "Socket"
          ]

getEnvironments :: [Environment]
getEnvironments = 