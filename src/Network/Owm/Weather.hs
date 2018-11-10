{-# LANGUAGE OverloadedStrings #-}

module Network.Owm.Weather (
    module Network.Owm.Internal.Weather,
    fromCityId, fromCityName, fromCoords, fromZipCode
) where

import           Network.Owm.Internal.Weather (Weather)
import           Network.Owm

fromCityId :: Key -> Units -> Lang -> CityId -> IO (Maybe Weather)
fromCityId key units lang id_ = 
    getOwmWith [makeParam id_] key units lang (owmBaseUrl25 ++ "/weather?")

fromCityName :: Key -> Units -> Lang -> CityName -> IO (Maybe Weather)
fromCityName key units lang name = 
    getOwmWith [makeParam name] key units lang (owmBaseUrl25 ++ "/weather?") where

fromCoords :: Key -> Units -> Lang -> Coords -> IO (Maybe Weather)
fromCoords key units lang coords = 
    getOwmWith [makeParam coords] key units lang (owmBaseUrl25 ++ "/weather?")

fromZipCode :: Key -> Units -> Lang -> ZipCode -> IO (Maybe Weather)
fromZipCode key units lang zipcode = 
    getOwmWith [makeParam zipcode] key units lang (owmBaseUrl25 ++ "/weather?") where
