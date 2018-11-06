{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Network.Owm.Forecast (
    Rain,
    Wind,
    MainForecast,
    Sys,
    Temp,
    Clouds,
    Weather,
    ForecastElt,
    Coord,
    City,
    Forecast,
    parse
) where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), decode, object,
                                                  pairs, (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative ((:|:))
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid
import           Data.Text                       (Text)
import           GHC.Generics
import qualified Network.Owm
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Rain = Rain {
    rain_3h :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON Rain where
  parseJSON (Object v) = Rain <$> v .:?? "3h"
  parseJSON _          = mzero


instance ToJSON Rain where
  toJSON     (Rain {..}) = object ["3h" .= rain_3h]
  toEncoding (Rain {..}) = pairs  ("3h" .= rain_3h)


data Wind = Wind {
    windDeg   :: Double,
    windSpeed :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Wind where
  parseJSON (Object v) = Wind <$> v .:   "deg" <*> v .:   "speed"
  parseJSON _          = mzero


instance ToJSON Wind where
  toJSON     (Wind {..}) = object ["deg" .= windDeg, "speed" .= windSpeed]
  toEncoding (Wind {..}) = pairs  ("deg" .= windDeg<>"speed" .= windSpeed)


data MainForecast = MainForecast {
    mainSeaLevel  :: Double,
    mainTempKf    :: Double,
    mainHumidity  :: Double,
    mainTempMin   :: Double,
    mainTemp      :: Double,
    mainTempMax   :: Double,
    mainPressure  :: Double,
    mainGrndLevel :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON MainForecast where
  parseJSON (Object v) = MainForecast <$> v .:   "sea_level" <*> v .:   "temp_kf" <*> v .:   "humidity" <*> v .:   "temp_min" <*> v .:   "temp" <*> v .:   "temp_max" <*> v .:   "pressure" <*> v .:   "grnd_level"
  parseJSON _          = mzero


instance ToJSON MainForecast where
  toJSON     (MainForecast {..}) = object ["sea_level" .= mainSeaLevel, "temp_kf" .= mainTempKf, "humidity" .= mainHumidity, "temp_min" .= mainTempMin, "temp" .= mainTemp, "temp_max" .= mainTempMax, "pressure" .= mainPressure, "grnd_level" .= mainGrndLevel]
  toEncoding (MainForecast {..}) = pairs  ("sea_level" .= mainSeaLevel<>"temp_kf" .= mainTempKf<>"humidity" .= mainHumidity<>"temp_min" .= mainTempMin<>"temp" .= mainTemp<>"temp_max" .= mainTempMax<>"pressure" .= mainPressure<>"grnd_level" .= mainGrndLevel)


data Sys = Sys {
    sysPopulation :: (Maybe (Double:|:[(Maybe Value)])),
    sysPod        :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON Sys where
  parseJSON (Object v) = Sys <$> v .:?? "population" <*> v .:?? "pod"
  parseJSON _          = mzero


instance ToJSON Sys where
  toJSON     (Sys {..}) = object ["population" .= sysPopulation, "pod" .= sysPod]
  toEncoding (Sys {..}) = pairs  ("population" .= sysPopulation<>"pod" .= sysPod)


data Temp = Temp {
    max   :: Double,
    day   :: Double,
    night :: Double,
    min   :: Double,
    morn  :: Double,
    eve   :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Temp where
  parseJSON (Object v) = Temp <$> v .:   "max" <*> v .:   "day" <*> v .:   "night" <*> v .:   "min" <*> v .:   "morn" <*> v .:   "eve"
  parseJSON _          = mzero


instance ToJSON Temp where
  toJSON     (Temp {..}) = object ["max" .= max, "day" .= day, "night" .= night, "min" .= min, "morn" .= morn, "eve" .= eve]
  toEncoding (Temp {..}) = pairs  ("max" .= max<>"day" .= day<>"night" .= night<>"min" .= min<>"morn" .= morn<>"eve" .= eve)


data Clouds = Clouds {
    cloudsAll :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Clouds where
  parseJSON (Object v) = Clouds <$> v .:   "all"
  parseJSON _          = mzero


instance ToJSON Clouds where
  toJSON     (Clouds {..}) = object ["all" .= cloudsAll]
  toEncoding (Clouds {..}) = pairs  ("all" .= cloudsAll)


data Weather = Weather {
    icon         :: Text,
    weather_main :: Text,
    id_          :: Double,
    description  :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Weather where
  parseJSON (Object v) = Weather <$> v .:   "icon" <*> v .:   "main" <*> v .:   "id" <*> v .:   "description"
  parseJSON _          = mzero


instance ToJSON Weather where
  toJSON     (Weather {..}) = object ["icon" .= icon, "main" .= weather_main, "id" .= id_, "description" .= description]
  toEncoding (Weather {..}) = pairs  ("icon" .= icon<>"main" .= weather_main<>"id" .= id_<>"description" .= description)


data ForecastElt = ForecastElt {
    fc_rain     :: (Maybe (Double:|:Rain:|:[(Maybe Value)])),
    fc_wind     :: (Maybe (Wind:|:[(Maybe Value)])),
    fc_dt       :: Double,
    fc_main     :: (Maybe (MainForecast:|:[(Maybe Value)])),
    fc_deg      :: (Maybe (Double:|:[(Maybe Value)])),
    fc_sys      :: (Maybe (Sys:|:[(Maybe Value)])),
    fc_humidity :: (Maybe (Double:|:[(Maybe Value)])),
    fc_temp     :: (Maybe (Temp:|:[(Maybe Value)])),
    fc_speed    :: (Maybe (Double:|:[(Maybe Value)])),
    fc_dtTxt    :: (Maybe (Text:|:[(Maybe Value)])),
    fc_clouds   :: Double:|:Clouds:|:[(Maybe Value)],
    fc_snow     :: (Maybe (Double:|:[(Maybe Value)])),
    fc_pressure :: (Maybe (Double:|:[(Maybe Value)])),
    fc_weather  :: [Weather]
  } deriving (Show,Eq,Generic)


instance FromJSON ForecastElt where
  parseJSON (Object v) = ForecastElt <$> v .:?? "rain" <*> v .:?? "wind" <*> v .:   "dt" <*> v .:?? "main" <*> v .:?? "deg" <*> v .:?? "sys" <*> v .:?? "humidity" <*> v .:?? "temp" <*> v .:?? "speed" <*> v .:?? "dt_txt" <*> v .:   "clouds" <*> v .:?? "snow" <*> v .:?? "pressure" <*> v .:   "weather"
  parseJSON _          = mzero


instance ToJSON ForecastElt where
  toJSON     (ForecastElt {..}) = object ["rain" .= fc_rain, "wind" .= fc_wind, "dt" .= fc_dt, "main" .= fc_main, "deg" .= fc_deg, "sys" .= fc_sys, "humidity" .= fc_humidity, "temp" .= fc_temp, "speed" .= fc_speed, "dt_txt" .= fc_dtTxt, "clouds" .= fc_clouds, "snow" .= fc_snow, "pressure" .= fc_pressure, "weather" .= fc_weather]
  toEncoding (ForecastElt {..}) = pairs  ("rain" .= fc_rain<>"wind" .= fc_wind<>"dt" .= fc_dt<>"main" .= fc_main<>"deg" .= fc_deg<>"sys" .= fc_sys<>"humidity" .= fc_humidity<>"temp" .= fc_temp<>"speed" .= fc_speed<>"dt_txt" .= fc_dtTxt<>"clouds" .= fc_clouds<>"snow" .= fc_snow<>"pressure" .= fc_pressure<>"weather" .= fc_weather)


data Coord = Coord {
    coordLat :: Double,
    coordLon :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Coord where
  parseJSON (Object v) = Coord <$> v .:   "lat" <*> v .:   "lon"
  parseJSON _          = mzero


instance ToJSON Coord where
  toJSON     (Coord {..}) = object ["lat" .= coordLat, "lon" .= coordLon]
  toEncoding (Coord {..}) = pairs  ("lat" .= coordLat<>"lon" .= coordLon)


data City = City {
    cityCoord      :: Coord,
    cityCountry    :: Text,
    citySys        :: (Maybe (Sys:|:[(Maybe Value)])),
    cityName       :: Text,
    cityId         :: Double,
    cityPopulation :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON City where
  parseJSON (Object v) = City <$> v .:   "coord" <*> v .:   "country" <*> v .:?? "sys" <*> v .:   "name" <*> v .:   "id" <*> v .:   "population"
  parseJSON _          = mzero


instance ToJSON City where
  toJSON     (City {..}) = object ["coord" .= cityCoord, "country" .= cityCountry, "sys" .= citySys, "name" .= cityName, "id" .= cityId, "population" .= cityPopulation]
  toEncoding (City {..}) = pairs  ("coord" .= cityCoord<>"country" .= cityCountry<>"sys" .= citySys<>"name" .= cityName<>"id" .= cityId<>"population" .= cityPopulation)


data Forecast = Forecast {
    list    :: [ForecastElt],
    cod     :: Text,
    city    :: City,
    message :: Double,
    cnt     :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Forecast where
  parseJSON (Object v) = Forecast <$> v .:   "list" <*> v .:   "cod" <*> v .:   "city" <*> v .:   "message" <*> v .:   "cnt"
  parseJSON _          = mzero


instance ToJSON Forecast where
  toJSON     (Forecast {..}) = object ["list" .= list, "cod" .= cod, "city" .= city, "message" .= message, "cnt" .= cnt]
  toEncoding (Forecast {..}) = pairs  ("list" .= list<>"cod" .= cod<>"city" .= city<>"message" .= message<>"cnt" .= cnt)


parse :: BSL.ByteString -> Maybe Forecast
parse input = do
    mv <- decode input :: Maybe Forecast
    return mv

parse' :: FilePath -> IO Forecast
parse' filename = do
    input <- BSL.readFile filename
    case decode input of
      Nothing -> fatal $ case (decode input :: Maybe Value) of
                           Nothing -> "Invalid JSON file: "     ++ filename
                           Just v  -> "Mismatched JSON value from file: " ++ filename
      Just r  -> return (r :: Forecast)
      where
          fatal :: String -> IO a
          fatal msg = do
              hPutStrLn stderr msg
              exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse' f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
