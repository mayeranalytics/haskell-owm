{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Owm.Weather where

import           Network.Owm.Internal.Weather
import           Network.Owm
import           Control.Lens.TH

makeClassy_ ''Weather
makeClassy_ ''Rain
makeClassy_ ''Wind
makeClassy_ ''Coord
--makeClassy_ ''Main
makeClassy_ ''Sys
makeClassy_ ''Clouds
makeClassy_ ''WeatherElt

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
