{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Network.Owm.Weather
(
    Rain,
    Wind,
    Coord,
    Main,
    Sys,
    Clouds,
    WE,
    Weather,
    parse,
    fromCityId,
    fromCityName,
    fromCoords,
    fromZipCode
)
where

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
import           Network.HTTP
import qualified Network.Owm                     as Owm
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)


-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

data Rain = Rain {
    rain_3h :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Rain where
  parseJSON (Object v) = Rain <$> v .:   "3h"
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


data Coord = Coord {
    lat :: Double,
    lon :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Coord where
  parseJSON (Object v) = Coord <$> v .:   "lat" <*> v .:   "lon"
  parseJSON _          = mzero


instance ToJSON Coord where
  toJSON     (Coord {..}) = object ["lat" .= lat, "lon" .= lon]
  toEncoding (Coord {..}) = pairs  ("lat" .= lat<>"lon" .= lon)


data Main = Main {
    seaLevel  :: (Maybe (Double:|:[(Maybe Value)])),
    humidity  :: Int,
    tempMin   :: Double,
    temp      :: Double,
    tempMax   :: Double,
    pressure  :: Double,
    grndLevel :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON Main where
  parseJSON (Object v) = Main <$> v .:?? "sea_level" <*> v .:   "humidity" <*> v .:   "temp_min" <*> v .:   "temp" <*> v .:   "temp_max" <*> v .:   "pressure" <*> v .:?? "grnd_level"
  parseJSON _          = mzero


instance ToJSON Main where
  toJSON     (Main {..}) = object ["sea_level" .= seaLevel, "humidity" .= humidity, "temp_min" .= tempMin, "temp" .= temp, "temp_max" .= tempMax, "pressure" .= pressure, "grnd_level" .= grndLevel]
  toEncoding (Main {..}) = pairs  ("sea_level" .= seaLevel<>"humidity" .= humidity<>"temp_min" .= tempMin<>"temp" .= temp<>"temp_max" .= tempMax<>"pressure" .= pressure<>"grnd_level" .= grndLevel)


data Sys = Sys {
    sunset  :: Int,
    country :: Text,
    sysId   :: (Maybe (Double:|:[(Maybe Value)])),
    sunrise :: Int,
    sysType :: (Maybe (Int:|:[(Maybe Value)])),
    message :: (Maybe (Int:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON Sys where
  parseJSON (Object v) = Sys <$> v .:   "sunset" <*> v .:   "country" <*> v .:?? "id" <*> v .:   "sunrise" <*> v .:?? "type" <*> v .:?? "message"
  parseJSON _          = mzero


instance ToJSON Sys where
  toJSON     (Sys {..}) = object ["sunset" .= sunset, "country" .= country, "id" .= sysId, "sunrise" .= sunrise, "type" .= sysType, "message" .= message]
  toEncoding (Sys {..}) = pairs  ("sunset" .= sunset<>"country" .= country<>"id" .= sysId<>"sunrise" .= sunrise<>"type" .= sysType<>"message" .= message)


data Clouds = Clouds {
    clouds :: Int
  } deriving (Show,Eq,Generic)


instance FromJSON Clouds where
  parseJSON (Object v) = Clouds <$> v .:   "all"
  parseJSON _          = mzero


instance ToJSON Clouds where
  toJSON     (Clouds {..}) = object ["all" .= clouds]
  toEncoding (Clouds {..}) = pairs  ("all" .= clouds)


-- | WE stands for weather element
data WE = WE {
    weIcon        :: Text,
    weMain        :: Text,
    weId          :: Int,   -- see https://openweathermap.org/weather-conditions
    weDescription :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON WE where
  parseJSON (Object v) = WE <$> v .:   "icon" <*> v .:   "main" <*> v .:   "id" <*> v .:   "description"
  parseJSON _          = mzero


instance ToJSON WE where
  toJSON     (WE {..}) = object ["icon" .= weIcon, "main" .= weMain, "id" .= weId, "description" .= weDescription]
  toEncoding (WE {..}) = pairs  ("icon" .= weIcon<>"main" .= weMain<>"id" .= weId<>"description" .= weDescription)


data Weather = Weather {
    wRain   :: (Maybe (Rain:|:[(Maybe Value)])),
    wWind   :: Wind,
    wBase   :: (Maybe (Text:|:[(Maybe Value)])),
    wCoord  :: Coord,
    wCod    :: Int,
    wDt     :: Int,
    wMain   :: Main,
    wSys    :: Sys,
    wName   :: Text,
    wClouds :: Clouds,
    wId     :: Int,
    weather :: [WE]
  } deriving (Show,Eq,Generic)


instance FromJSON Weather where
  parseJSON (Object v) = Weather <$> v .:?? "rain" <*> v .:   "wind" <*> v .:?? "base" <*> v .:   "coord" <*> v .:   "cod" <*> v .:   "dt" <*> v .:   "main" <*> v .:   "sys" <*> v .:   "name" <*> v .:   "clouds" <*> v .:   "id" <*> v .:   "weather"
  parseJSON _          = mzero


instance ToJSON Weather where
  toJSON     (Weather {..}) = object ["rain" .= wRain, "wind" .= wWind, "base" .= wBase, "coord" .= wCoord, "cod" .= wCod, "dt" .= wDt, "main" .= wMain, "sys" .= wSys, "name" .= wName, "clouds" .= wClouds, "id" .= wId, "weather" .= weather]
  toEncoding (Weather {..}) = pairs  ("rain" .= wRain<>"wind" .= wWind<>"base" .= wBase<>"coord" .= wCoord<>"cod" .= wCod<>"dt" .= wDt<>"main" .= wMain<>"sys" .= wSys<>"name" .= wName<>"clouds" .= wClouds<>"id" .= wId<>"weather" .= weather)



parse :: BSL.ByteString -> Maybe Weather
parse input = do
    mv <- decode input :: Maybe Weather
    return mv

parse' :: FilePath -> IO Weather
parse' filename = do
    input <- BSL.readFile filename
    case decode input of
        Nothing -> fatal $ case (decode input :: Maybe Value) of
            Nothing -> "Invalid JSON file: "     ++ filename
            Just v  -> "Mismatched JSON value from file: " ++ filename
        Just r  -> return (r :: Weather)
    where
        fatal :: String -> IO a
        fatal msg = do
            hPutStrLn stderr msg
            exitFailure

get :: Owm.Key -> Owm.Units -> Owm.Lang -> String -> IO (Maybe Weather)
get key units lang url = do
    let url' = url ++ show units ++ show lang ++ "&APPID=" ++ key
    response <- (simpleHTTP . getRequest) url' >>= getResponseBody
    return $ parse $ BSL.pack response

fromCityId :: Owm.Key -> Owm.Units -> Owm.Lang -> Owm.CityId -> IO (Maybe Weather)
fromCityId key units lang id_ = get key units lang (Owm.owm_base_url ++ "/weather?id=" ++ show id_)

fromCityName :: Owm.Key -> Owm.Units -> Owm.Lang -> Owm.CityName -> Maybe Owm.CountryCode -> IO (Maybe Weather)
fromCityName key units lang name ccode = get key units lang (Owm.owm_base_url ++ "/weather?" ++ q) where
    q = case ccode of
        Just ccode' -> "q=" ++ name ++ "," ++ ccode'
        Nothing     -> "q=" ++ name

fromCoords :: Owm.Key -> Owm.Units -> Owm.Lang -> Owm.Coords -> IO (Maybe Weather)
fromCoords key units lang (lat, lon) = get key units lang (Owm.owm_base_url ++ "/weather?" ++ q) where
    q = "lat=" ++ show lat ++ "&lon=" ++ show lon

fromZipCode :: Owm.Key -> Owm.Units -> Owm.Lang -> Owm.ZipCode -> Maybe Owm.CountryCode -> IO (Maybe Weather)
fromZipCode key units lang zipcode ccode = get key units lang (Owm.owm_base_url ++ "/weather?" ++ q) where
    q = case ccode of
        Just ccode' -> "zip=" ++ show zipcode ++ "," ++ ccode'
        Nothing     -> "zip=" ++ show zipcode

main :: IO ()
main = do
  filenames <- getArgs
  -- forM_ filenames (\f -> parse' f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  forM_ filenames (\f -> parse' f >>= (\p -> print p))
  exitSuccess
