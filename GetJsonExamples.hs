#!/usr/bin/env stack
-- stack --resolver lts-12.16 script
module Main where

import           Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BSL
import           Data.String.Utils (strip)
import           Control.Monad.State
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)

inc = get >>= \i->put (i+1) :: StateT Int IO ()
resetCounter = put 1 :: StateT Int IO ()
done = return () :: StateT Int IO ()
readKey = strip <$> (liftIO.readFile) "owm.key" :: StateT Int IO String
wget = liftIO . simpleHttp :: String -> StateT Int IO BSL.ByteString

save :: FilePath -> BSL.ByteString -> StateT Int IO ()
save f b = do
    i <- get
    let path = f ++ show i ++ ".json"
    liftIO $ BSL.writeFile path b
    liftIO $ putStrLn $ "Saved " ++ path
    inc

baseurl25 = "http://api.openweathermap.org/data/2.5/"
baseurl30 = "http://api.openweathermap.org/data/3.0/"

queries = [
    "q=Altendorf,CH&units=metric&lang=de",
    "q=London,gb&units=metric&lang=en",
    "q=Paris,gb&units=metric&lang=fr",
    "q=Moscow,gb&units=metric&lang=ru",
    "q=Mayer,us&units=metric&lang=en",
    "q=München,DE",
    "lat=35&lon=139&units=metric",
    "lat=90&lon=0&lang=no&units=metric",
    "lat=-90&lon=180&lang=es&units=metric",
    "zip=60606,us&units=imperial",
    "id=524901"
  ]

main :: IO ((), Int)
main = (flip runStateT) 1 $ do
    key <- readKey

    -- weather
    resetCounter
    forM_ queries $ \q -> save "json/Weather" =<< wget (baseurl25 ++ "weather?" ++ q ++ "&APPID=" ++ key)

    -- Forecast
    resetCounter
    forM_ queries $ \q -> save "json/Forecast" =<< wget (baseurl25 ++ "forecast?" ++ q ++ "&APPID=" ++ key)

    -- ForecastDaily
    resetCounter
    forM_ queries $ \q -> save "json/ForecastDaily" =<< wget (baseurl25 ++ "forecast/daily?cnt=5&" ++ q ++ "&APPID=" ++ key)

    {-
    -- History: Need paid subscription
    resetCounter
    forM_ queries $ \q -> save "json/History" =<< wget (baseurl25 ++ "history/city?type=hour&" ++ q ++ "&APPID=" ++ key)
    -}

    done

