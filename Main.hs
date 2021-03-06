{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.String.Utils          (strip)
import           Data.Maybe                 (fromJust)
import qualified Network.Owm                as Owm
import           Network.Owm                (CityName(..), CityId(..), Coords(..), CountryCode,
                                             ZipCode(..), Count(..), toUTCTime)
import qualified Network.Owm.Forecast       as Forecast
import qualified Network.Owm.ForecastDaily  as ForecastDaily
import qualified Network.Owm.Weather        as Weather
import           Control.Lens ((^.), (^..))
import           Control.Monad (forM_)

getKey :: FilePath -> IO Owm.Key
getKey f = (Owm.Key . strip) <$> readFile f

main :: IO()
main = do
    key <- getKey "owm.key"

    -- Weather
    print =<< Weather.fromCityName key Owm.Metric Owm.EN (CityName "Altendorf" "CH") 
    print =<< Weather.fromCityId key Owm.Metric Owm.EN (CityId 2172797)
    print =<< Weather.fromCoords key Owm.Metric Owm.EN (Owm.Coords 47.37643 8.54782)
    print =<< Weather.fromZipCode key Owm.Metric Owm.EN (ZipCode 8001 "CH")
    print ""

    -- Forecast
    print =<< Forecast.fromCityName key Owm.Metric Owm.EN (CityName "Altendorf" "CH") DefaultCount
    print =<< Forecast.fromZipCode key Owm.Metric Owm.DE (ZipCode 700000 "VN") (Count 1)
    print ""

    -- Forecast daily
    print =<< ForecastDaily.fromZipCode key Owm.Metric Owm.DE (ZipCode 60606 "US") (Count 1)
    print ""

    -- extract some data
    w <- fromJust <$> ForecastDaily.fromZipCode key Owm.Metric Owm.DE (ZipCode 8001 "CH") (Count 3)
    let cod = w ^. ForecastDaily._forecastDailyCod
        dts = toUTCTime <$> w ^. ForecastDaily._forecastDailyList ^.. traverse . ForecastDaily._listEltDt
        temps = w ^. ForecastDaily._forecastDailyList ^.. traverse . ForecastDaily._listEltTemp
    forM_ (zip dts temps) print