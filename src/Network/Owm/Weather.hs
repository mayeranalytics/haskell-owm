{-# LANGUAGE OverloadedStrings #-}

module Network.Owm.Weather (
    module Network.Owm.Internal.Weather,
    fromCityId, fromCityName, fromCoords, fromZipCode
) where

import qualified Data.ByteString as BS
import           Data.Aeson(decode)
import qualified Network.Wreq (get)
import           Network.Wreq (responseBody, getWith, defaults, param)
import           Control.Lens ((.~), (^.), (&))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Maybe (maybe)
import           Network.Owm.Internal.Weather hiding(parse)
import           Network.Owm

get :: Key -> Units -> Lang -> String -> IO (Maybe Weather)
get key units lang url = do
    let params = [
            makeParam key,
            makeParam units,
            makeParam lang
            ]
        opts = foldl (&) defaults params
    r <- getWith opts url >>= (return . (^. responseBody))
    return $ decode r

fromCityId :: Key -> Units -> Lang -> CityId -> IO (Maybe Weather)
fromCityId key units lang id_ = get key units lang (owmBaseUrl25 ++ "/weather?id=" ++ show id_)

fromCityName :: Key -> Units -> Lang -> CityName -> Maybe CountryCode -> IO (Maybe Weather)
fromCityName key units lang name ccode = get key units lang (owmBaseUrl25 ++ "/weather?" ++ q) where
    q = "q=" ++ name ++ (maybe "" ("," ++) ccode)

fromCoords :: Key -> Units -> Lang -> Coords -> IO (Maybe Weather)
fromCoords key units lang (lat, lon) = get key units lang (owmBaseUrl25 ++ "/weather?" ++ q) where
    q = "lat=" ++ show lat ++ "&lon=" ++ show lon

fromZipCode :: Key -> Units -> Lang -> ZipCode -> Maybe CountryCode -> IO (Maybe Weather)
fromZipCode key units lang zipcode ccode = get key units lang (owmBaseUrl25 ++ "/weather?" ++ q) where
    q = case ccode of
        Just ccode' -> "zip=" ++ show zipcode ++ "," ++ ccode'
        Nothing     -> "zip=" ++ show zipcode
