{-# LANGUAGE OverloadedStrings #-}

module Network.Owm.Forecast (
    module Network.Owm.Internal.Forecast,
    fromCityId, fromCityName, fromCoords, fromZipCode
) where

import           Network.Owm.Internal.Forecast (Forecast)
import           Network.Owm

fromCityId :: Key -> Units -> Lang -> CityId -> Count -> IO (Maybe Forecast)
fromCityId key units lang id_ count = 
    getOwmWith [makeParam id_, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/?")

fromCityName :: Key -> Units -> Lang -> CityName -> Count -> IO (Maybe Forecast)
fromCityName key units lang name count = 
    getOwmWith [makeParam name, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/?") where

fromCoords :: Key -> Units -> Lang -> Coords -> Count -> IO (Maybe Forecast)
fromCoords key units lang coords count = 
    getOwmWith [makeParam coords, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/?")

fromZipCode :: Key -> Units -> Lang -> ZipCode -> Count -> IO (Maybe Forecast)
fromZipCode key units lang zipcode count = 
    getOwmWith [makeParam zipcode, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/?") where
