{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.String.Utils          (strip)
import           Network.HTTP
import qualified Network.Owm                as Owm
import           Network.Owm                (CityName(..), CityId(..), Coords(..), CountryCode,
                                             ZipCode(..), Count(..))
import qualified Network.Owm.Forecast       as Forecast
import qualified Network.Owm.ForecastDaily  as ForecastDaily
import qualified Network.Owm.Weather        as Weather

getKey :: FilePath -> IO Owm.Key
getKey f = (Owm.Key . strip) <$> readFile f

main :: IO()
main = do
    key <- getKey "owm.key"
    {-
    -- Weather
    print =<< Weather.fromCityName key Owm.Metric Owm.EN (CityName "Altendorf" "CH") 
    print =<< Weather.fromCityId key Owm.Metric Owm.EN (CityId 2172797)
    print =<< Weather.fromCoords key Owm.Metric Owm.EN (Owm.Coords 47.37643 8.54782)
    print =<< Weather.fromZipCode key Owm.Metric Owm.EN (ZipCode 8001 "CH")
    -}
    -- Forecast
    print =<< Forecast.fromCityName key Owm.Metric Owm.EN (CityName "Altendorf" "CH") DefaultCount
    print =<< Forecast.fromZipCode key Owm.Metric Owm.DE (ZipCode 8001 "CH") (Count 1)

    -- Forecast daily
    print =<< ForecastDaily.fromZipCode key Owm.Metric Owm.DE (ZipCode 8001 "CH") (Count 1)