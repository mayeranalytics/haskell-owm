{-# LANGUAGE OverloadedStrings #-}

module Network.Owm.Weather (
    module Network.Owm.Internal.Weather,
    fromCityId, fromCityName, fromCoords, fromZipCode
) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson(decode)
import           Network.HTTP.Conduit (simpleHttp)
import           Network.Owm.Internal.Weather hiding(parse)
import qualified Network.Owm as Owm

get :: Owm.Key -> Owm.Units -> Owm.Lang -> String -> IO (Maybe Weather)
get key units lang url = do
    let url' = url ++ show units ++ show lang ++ "&APPID=" ++ key
    response <- simpleHttp url'
    return $ decode response

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
