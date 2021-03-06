{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Owm.ForecastDaily  where

import           Network.Owm.Internal.ForecastDaily
import           Network.Owm
import           Control.Lens.TH

makeClassy_ ''ForecastDaily
makeClassy_ ''Temp
makeClassy_ ''WeatherElt
makeClassy_ ''ListElt
makeClassy_ ''Coord
makeClassy_ ''City

fromCityId :: Key -> Units -> Lang -> CityId -> Count -> IO (Maybe ForecastDaily)
fromCityId key units lang id_ count = 
    getOwmWith [makeParam id_, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/daily?")

fromCityName :: Key -> Units -> Lang -> CityName -> Count -> IO (Maybe ForecastDaily)
fromCityName key units lang name count = 
    getOwmWith [makeParam name, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/daily?") where

fromCoords :: Key -> Units -> Lang -> Coords -> Count -> IO (Maybe ForecastDaily)
fromCoords key units lang coords count = 
    getOwmWith [makeParam coords, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/daily?")

fromZipCode :: Key -> Units -> Lang -> ZipCode -> Count -> IO (Maybe ForecastDaily)
fromZipCode key units lang zipcode count = 
    getOwmWith [makeParam zipcode, makeParam count] key units lang (owmBaseUrl25 ++ "/forecast/daily?") where
