{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Network.Owm.Internal.ForecastDaily where

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
    listEltRain :: (Maybe (Double:|:[(Maybe Value)])),
    listEltDt :: Double,
    listEltDeg :: Double,
    listEltHumidity :: Double,
    listEltTemp :: Temp,
    listEltSpeed :: Double,
    listEltClouds :: Double,
    listEltSnow :: (Maybe (Double:|:[(Maybe Value)])),
    listEltPressure :: Double,
    listEltWeather :: [WeatherElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ListElt where
  parseJSON (Object v) = ListElt <$> v .:?? "rain" <*> v .:   "dt" <*> v .:   "deg" <*> v .:   "humidity" <*> v .:   "temp" <*> v .:   "speed" <*> v .:   "clouds" <*> v .:?? "snow" <*> v .:   "pressure" <*> v .:   "weather"
  parseJSON _          = mzero


instance ToJSON ListElt where
  toJSON     (ListElt {..}) = object ["rain" .= listEltRain, "dt" .= listEltDt, "deg" .= listEltDeg, "humidity" .= listEltHumidity, "temp" .= listEltTemp, "speed" .= listEltSpeed, "clouds" .= listEltClouds, "snow" .= listEltSnow, "pressure" .= listEltPressure, "weather" .= listEltWeather]
  toEncoding (ListElt {..}) = pairs  ("rain" .= listEltRain<>"dt" .= listEltDt<>"deg" .= listEltDeg<>"humidity" .= listEltHumidity<>"temp" .= listEltTemp<>"speed" .= listEltSpeed<>"clouds" .= listEltClouds<>"snow" .= listEltSnow<>"pressure" .= listEltPressure<>"weather" .= listEltWeather)


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


data City = City { 
    cityCoord :: Coord,
    cityCountry :: Text,
    cityName :: Text,
    cityId :: Double,
    cityPopulation :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON City where
  parseJSON (Object v) = City <$> v .:   "coord" <*> v .:   "country" <*> v .:   "name" <*> v .:   "id" <*> v .:   "population"
  parseJSON _          = mzero


instance ToJSON City where
  toJSON     (City {..}) = object ["coord" .= cityCoord, "country" .= cityCountry, "name" .= cityName, "id" .= cityId, "population" .= cityPopulation]
  toEncoding (City {..}) = pairs  ("coord" .= cityCoord<>"country" .= cityCountry<>"name" .= cityName<>"id" .= cityId<>"population" .= cityPopulation)


data ForecastDaily = ForecastDaily { 
    forecastDailyList :: [ListElt],
    forecastDailyCod :: Text,
    forecastDailyCity :: City,
    forecastDailyMessage :: Double,
    forecastDailyCnt :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ForecastDaily where
  parseJSON (Object v) = ForecastDaily <$> v .:   "list" <*> v .:   "cod" <*> v .:   "city" <*> v .:   "message" <*> v .:   "cnt"
  parseJSON _          = mzero


instance ToJSON ForecastDaily where
  toJSON     (ForecastDaily {..}) = object ["list" .= forecastDailyList, "cod" .= forecastDailyCod, "city" .= forecastDailyCity, "message" .= forecastDailyMessage, "cnt" .= forecastDailyCnt]
  toEncoding (ForecastDaily {..}) = pairs  ("list" .= forecastDailyList<>"cod" .= forecastDailyCod<>"city" .= forecastDailyCity<>"message" .= forecastDailyMessage<>"cnt" .= forecastDailyCnt)




parse :: FilePath -> IO ForecastDaily
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just _  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: ForecastDaily)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


