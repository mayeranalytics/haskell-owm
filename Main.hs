{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.String.Utils          (strip)
import           Network.HTTP
import qualified Network.Owm                as Owm
import qualified Network.Owm.Forecast       as Owm.Forecast
import qualified Network.Owm.Weather        as Owm.Weather



getKey :: IO String
getKey = strip <$> readFile "owm.key"

main :: IO()
main = do
    key <- getKey
    weather <- Owm.Weather.fromCityName key Owm.Metric Owm.EN "Altendorf" (Just "CH")
    print weather
