{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Network.Owm.Internal.Weather where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Rain = Rain { 
    rain_1h :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Rain where
  parseJSON (Object v) = Rain <$> v .:   "1h"
  parseJSON _          = mzero


instance ToJSON Rain where
  toJSON     (Rain {..}) = object ["1h" .= rain_1h]
  toEncoding (Rain {..}) = pairs  ("1h" .= rain_1h)


data Wind = Wind { 
    windDeg :: (Maybe (Double:|:[(Maybe Value)])),
    windSpeed :: Double,
    windGust :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Wind where
  parseJSON (Object v) = Wind <$> v .:?? "deg" <*> v .:   "speed" <*> v .:?? "gust"
  parseJSON _          = mzero


instance ToJSON Wind where
  toJSON     (Wind {..}) = object ["deg" .= windDeg, "speed" .= windSpeed, "gust" .= windGust]
  toEncoding (Wind {..}) = pairs  ("deg" .= windDeg<>"speed" .= windSpeed<>"gust" .= windGust)


data Coord = Coord { 
    coordLat :: Double,
    coordLon :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Coord where
  parseJSON (Object v) = Coord <$> v .:   "lat" <*> v .:   "lon"
  parseJSON _          = mzero


instance ToJSON Coord where
  toJSON     (Coord {..}) = object ["lat" .= coordLat, "lon" .= coordLon]
  toEncoding (Coord {..}) = pairs  ("lat" .= coordLat<>"lon" .= coordLon)


data Main = Main { 
    mainSeaLevel :: (Maybe (Double:|:[(Maybe Value)])),
    mainHumidity :: Double,
    mainTempMin :: Double,
    mainTemp :: Double,
    mainTempMax :: Double,
    mainPressure :: Double,
    mainGrndLevel :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Main where
  parseJSON (Object v) = Main <$> v .:?? "sea_level" <*> v .:   "humidity" <*> v .:   "temp_min" <*> v .:   "temp" <*> v .:   "temp_max" <*> v .:   "pressure" <*> v .:?? "grnd_level"
  parseJSON _          = mzero


instance ToJSON Main where
  toJSON     (Main {..}) = object ["sea_level" .= mainSeaLevel, "humidity" .= mainHumidity, "temp_min" .= mainTempMin, "temp" .= mainTemp, "temp_max" .= mainTempMax, "pressure" .= mainPressure, "grnd_level" .= mainGrndLevel]
  toEncoding (Main {..}) = pairs  ("sea_level" .= mainSeaLevel<>"humidity" .= mainHumidity<>"temp_min" .= mainTempMin<>"temp" .= mainTemp<>"temp_max" .= mainTempMax<>"pressure" .= mainPressure<>"grnd_level" .= mainGrndLevel)


data Sys = Sys { 
    sysSunset :: Double,
    sysCountry :: (Maybe (Text:|:[(Maybe Value)])),
    sysId :: (Maybe (Double:|:[(Maybe Value)])),
    sysSunrise :: Double,
    sysType :: (Maybe (Double:|:[(Maybe Value)])),
    sysMessage :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Sys where
  parseJSON (Object v) = Sys <$> v .:   "sunset" <*> v .:?? "country" <*> v .:?? "id" <*> v .:   "sunrise" <*> v .:?? "type" <*> v .:   "message"
  parseJSON _          = mzero


instance ToJSON Sys where
  toJSON     (Sys {..}) = object ["sunset" .= sysSunset, "country" .= sysCountry, "id" .= sysId, "sunrise" .= sysSunrise, "type" .= sysType, "message" .= sysMessage]
  toEncoding (Sys {..}) = pairs  ("sunset" .= sysSunset<>"country" .= sysCountry<>"id" .= sysId<>"sunrise" .= sysSunrise<>"type" .= sysType<>"message" .= sysMessage)


data Clouds = Clouds { 
    cloudsAll :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Clouds where
  parseJSON (Object v) = Clouds <$> v .:   "all"
  parseJSON _          = mzero


instance ToJSON Clouds where
  toJSON     (Clouds {..}) = object ["all" .= cloudsAll]
  toEncoding (Clouds {..}) = pairs  ("all" .= cloudsAll)


data WeatherElt = WeatherElt { 
    weatherEltIcon :: Text,
    weatherEltMain :: Text,
    weatherEltId :: Double,
    weatherEltDescription :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON WeatherElt where
  parseJSON (Object v) = WeatherElt <$> v .:   "icon" <*> v .:   "main" <*> v .:   "id" <*> v .:   "description"
  parseJSON _          = mzero


instance ToJSON WeatherElt where
  toJSON     (WeatherElt {..}) = object ["icon" .= weatherEltIcon, "main" .= weatherEltMain, "id" .= weatherEltId, "description" .= weatherEltDescription]
  toEncoding (WeatherElt {..}) = pairs  ("icon" .= weatherEltIcon<>"main" .= weatherEltMain<>"id" .= weatherEltId<>"description" .= weatherEltDescription)


data Weather = Weather { 
    weatherRain :: (Maybe (Rain:|:[(Maybe Value)])),
    weatherWind :: Wind,
    weatherBase :: Text,
    weatherCoord :: Coord,
    weatherCod :: Double,
    weatherDt :: Double,
    weatherMain :: Main,
    weatherVisibility :: (Maybe (Double:|:[(Maybe Value)])),
    weatherSys :: Sys,
    weatherName :: Text,
    weatherClouds :: Clouds,
    weatherId :: Double,
    weatherWeather :: [WeatherElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Weather where
  parseJSON (Object v) = Weather <$> v .:?? "rain" <*> v .:   "wind" <*> v .:   "base" <*> v .:   "coord" <*> v .:   "cod" <*> v .:   "dt" <*> v .:   "main" <*> v .:?? "visibility" <*> v .:   "sys" <*> v .:   "name" <*> v .:   "clouds" <*> v .:   "id" <*> v .:   "weather"
  parseJSON _          = mzero


instance ToJSON Weather where
  toJSON     (Weather {..}) = object ["rain" .= weatherRain, "wind" .= weatherWind, "base" .= weatherBase, "coord" .= weatherCoord, "cod" .= weatherCod, "dt" .= weatherDt, "main" .= weatherMain, "visibility" .= weatherVisibility, "sys" .= weatherSys, "name" .= weatherName, "clouds" .= weatherClouds, "id" .= weatherId, "weather" .= weatherWeather]
  toEncoding (Weather {..}) = pairs  ("rain" .= weatherRain<>"wind" .= weatherWind<>"base" .= weatherBase<>"coord" .= weatherCoord<>"cod" .= weatherCod<>"dt" .= weatherDt<>"main" .= weatherMain<>"visibility" .= weatherVisibility<>"sys" .= weatherSys<>"name" .= weatherName<>"clouds" .= weatherClouds<>"id" .= weatherId<>"weather" .= weatherWeather)




parse :: FilePath -> IO Weather
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just _  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: Weather)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


