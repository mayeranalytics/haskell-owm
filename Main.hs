{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.String.Utils          (strip)
import           Network.HTTP
import qualified Network.Owm                as Owm
import qualified Network.Owm.Forecast       as Owm.Forecast
import qualified Network.Owm.Weather        as Owm.Weather

getKey :: FilePath -> IO Owm.Key
getKey f = (Owm.Key . strip) <$> readFile f

main :: IO()
main = do
    key <- getKey "owm.key"
    print =<< Owm.Weather.fromCityName key Owm.Metric Owm.EN "Altendorf" (Just "CH") 
    print =<< Owm.Weather.fromCityId key Owm.Metric Owm.EN 2172797
    print =<< Owm.Weather.fromCoords key Owm.Metric Owm.EN (12,47)
    print =<< Owm.Weather.fromZipCode key Owm.Metric Owm.EN 8001 (Just "CH") 
