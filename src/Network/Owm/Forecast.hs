{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Owm.Forecast where

import           Network.Owm.Internal.Forecast
import           Network.Owm
import           Control.Lens.TH

makeClassy_ ''Forecast
makeClassy_ ''Snow
makeClassy_ ''Wind
--makeClassy_ ''Main
makeClassy_ ''Sys
makeClassy_ ''Temp
makeClassy_ ''Clouds
makeClassy_ ''WeatherElt
makeClassy_ ''ListElt
makeClassy_ ''Coord
makeClassy_ ''City

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
