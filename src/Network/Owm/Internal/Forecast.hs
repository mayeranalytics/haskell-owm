{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Network.Owm.Internal.Forecast where

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


data Snow = Snow { 
    snow_3h :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Snow where
  parseJSON (Object v) = Snow <$> v .:?? "3h"
  parseJSON _          = mzero


instance ToJSON Snow where
  toJSON     (Snow {..}) = object ["3h" .= snow_3h]
  toEncoding (Snow {..}) = pairs  ("3h" .= snow_3h)


data Wind = Wind { 
    windDeg :: Double,
    windSpeed :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Wind where
  parseJSON (Object v) = Wind <$> v .:   "deg" <*> v .:   "speed"
  parseJSON _          = mzero


instance ToJSON Wind where
  toJSON     (Wind {..}) = object ["deg" .= windDeg, "speed" .= windSpeed]
  toEncoding (Wind {..}) = pairs  ("deg" .= windDeg<>"speed" .= windSpeed)


data Main = Main { 
    mainSeaLevel :: Double,
    mainTempKf :: Double,
    mainHumidity :: Double,
    mainTempMin :: Double,
    mainTemp :: Double,
    mainTempMax :: Double,
    mainPressure :: Double,
    mainGrndLevel :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Main where
  parseJSON (Object v) = Main <$> v .:   "sea_level" <*> v .:   "temp_kf" <*> v .:   "humidity" <*> v .:   "temp_min" <*> v .:   "temp" <*> v .:   "temp_max" <*> v .:   "pressure" <*> v .:   "grnd_level"
  parseJSON _          = mzero


instance ToJSON Main where
  toJSON     (Main {..}) = object ["sea_level" .= mainSeaLevel, "temp_kf" .= mainTempKf, "humidity" .= mainHumidity, "temp_min" .= mainTempMin, "temp" .= mainTemp, "temp_max" .= mainTempMax, "pressure" .= mainPressure, "grnd_level" .= mainGrndLevel]
  toEncoding (Main {..}) = pairs  ("sea_level" .= mainSeaLevel<>"temp_kf" .= mainTempKf<>"humidity" .= mainHumidity<>"temp_min" .= mainTempMin<>"temp" .= mainTemp<>"temp_max" .= mainTempMax<>"pressure" .= mainPressure<>"grnd_level" .= mainGrndLevel)


data Sys = Sys { 
    sysPod :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Sys where
  parseJSON (Object v) = Sys <$> v .:   "pod"
  parseJSON _          = mzero


instance ToJSON Sys where
  toJSON     (Sys {..}) = object ["pod" .= sysPod]
  toEncoding (Sys {..}) = pairs  ("pod" .= sysPod)


data Temp = Temp { 
    tempMax :: Double,
    tempDay :: Double,
    tempNight :: Double,
    tempMin :: Double,
    tempMorn :: Double,
    tempEve :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Temp where
  parseJSON (Object v) = Temp <$> v .:   "max" <*> v .:   "day" <*> v .:   "night" <*> v .:   "min" <*> v .:   "morn" <*> v .:   "eve"
  parseJSON _          = mzero


instance ToJSON Temp where
  toJSON     (Temp {..}) = object ["max" .= tempMax, "day" .= tempDay, "night" .= tempNight, "min" .= tempMin, "morn" .= tempMorn, "eve" .= tempEve]
  toEncoding (Temp {..}) = pairs  ("max" .= tempMax<>"day" .= tempDay<>"night" .= tempNight<>"min" .= tempMin<>"morn" .= tempMorn<>"eve" .= tempEve)


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


data ListElt = ListElt { 
    listEltRain :: (Maybe (Double:|:Snow:|:[(Maybe Value)])),
    listEltWind :: (Maybe (Wind:|:[(Maybe Value)])),
    listEltDt :: Double,
    listEltMain :: (Maybe (Main:|:[(Maybe Value)])),
    listEltDeg :: (Maybe (Double:|:[(Maybe Value)])),
    listEltSys :: (Maybe (Sys:|:[(Maybe Value)])),
    listEltHumidity :: (Maybe (Double:|:[(Maybe Value)])),
    listEltTemp :: (Maybe (Temp:|:[(Maybe Value)])),
    listEltSpeed :: (Maybe (Double:|:[(Maybe Value)])),
    listEltDtTxt :: (Maybe (Text:|:[(Maybe Value)])),
    listEltClouds :: Double:|:Clouds:|:[(Maybe Value)],
    listEltSnow :: (Maybe (Double:|:Snow:|:[(Maybe Value)])),
    listEltPressure :: (Maybe (Double:|:[(Maybe Value)])),
    listEltWeather :: [WeatherElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ListElt where
  parseJSON (Object v) = ListElt <$> v .:?? "rain" <*> v .:?? "wind" <*> v .:   "dt" <*> v .:?? "main" <*> v .:?? "deg" <*> v .:?? "sys" <*> v .:?? "humidity" <*> v .:?? "temp" <*> v .:?? "speed" <*> v .:?? "dt_txt" <*> v .:   "clouds" <*> v .:?? "snow" <*> v .:?? "pressure" <*> v .:   "weather"
  parseJSON _          = mzero


instance ToJSON ListElt where
  toJSON     (ListElt {..}) = object ["rain" .= listEltRain, "wind" .= listEltWind, "dt" .= listEltDt, "main" .= listEltMain, "deg" .= listEltDeg, "sys" .= listEltSys, "humidity" .= listEltHumidity, "temp" .= listEltTemp, "speed" .= listEltSpeed, "dt_txt" .= listEltDtTxt, "clouds" .= listEltClouds, "snow" .= listEltSnow, "pressure" .= listEltPressure, "weather" .= listEltWeather]
  toEncoding (ListElt {..}) = pairs  ("rain" .= listEltRain<>"wind" .= listEltWind<>"dt" .= listEltDt<>"main" .= listEltMain<>"deg" .= listEltDeg<>"sys" .= listEltSys<>"humidity" .= listEltHumidity<>"temp" .= listEltTemp<>"speed" .= listEltSpeed<>"dt_txt" .= listEltDtTxt<>"clouds" .= listEltClouds<>"snow" .= listEltSnow<>"pressure" .= listEltPressure<>"weather" .= listEltWeather)


data Coord = Coord { 
    coordLat :: (Maybe (Double:|:[(Maybe Value)])),
    coordLon :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Coord where
  parseJSON (Object v) = Coord <$> v .:?? "lat" <*> v .:?? "lon"
  parseJSON _          = mzero


instance ToJSON Coord where
  toJSON     (Coord {..}) = object ["lat" .= coordLat, "lon" .= coordLon]
  toEncoding (Coord {..}) = pairs  ("lat" .= coordLat<>"lon" .= coordLon)


data City = City { 
    cityCoord :: Coord,
    cityCountry :: (Maybe (Text:|:[(Maybe Value)])),
    cityName :: (Maybe (Text:|:[(Maybe Value)])),
    cityId :: (Maybe (Double:|:[(Maybe Value)])),
    cityPopulation :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON City where
  parseJSON (Object v) = City <$> v .:   "coord" <*> v .:?? "country" <*> v .:?? "name" <*> v .:?? "id" <*> v .:?? "population"
  parseJSON _          = mzero


instance ToJSON City where
  toJSON     (City {..}) = object ["coord" .= cityCoord, "country" .= cityCountry, "name" .= cityName, "id" .= cityId, "population" .= cityPopulation]
  toEncoding (City {..}) = pairs  ("coord" .= cityCoord<>"country" .= cityCountry<>"name" .= cityName<>"id" .= cityId<>"population" .= cityPopulation)


data Forecast = Forecast { 
    forecastList :: [ListElt],
    forecastCod :: Text,
    forecastCity :: City,
    forecastMessage :: Double,
    forecastCnt :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Forecast where
  parseJSON (Object v) = Forecast <$> v .:   "list" <*> v .:   "cod" <*> v .:   "city" <*> v .:   "message" <*> v .:   "cnt"
  parseJSON _          = mzero


instance ToJSON Forecast where
  toJSON     (Forecast {..}) = object ["list" .= forecastList, "cod" .= forecastCod, "city" .= forecastCity, "message" .= forecastMessage, "cnt" .= forecastCnt]
  toEncoding (Forecast {..}) = pairs  ("list" .= forecastList<>"cod" .= forecastCod<>"city" .= forecastCity<>"message" .= forecastMessage<>"cnt" .= forecastCnt)




parse :: FilePath -> IO Forecast
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just _  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: Forecast)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


