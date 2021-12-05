# OpenWeatherMap #

## Overview

> **[OpenWeatherMap](https://openweathermap.org)** is an online service that provides weather data, including current weather data, forecasts, and historical data to the developers of web services and mobile applications. 
>
> For data sources, it utilizes meteorological broadcast services, raw data from [airport](https://en.wikipedia.org/wiki/Airport) [weather stations](https://en.wikipedia.org/wiki/Weather_station), raw data from radar stations, and raw data from other official weather stations. 
>
> [Wikipedia](https://en.wikipedia.org/wiki/OpenWeatherMap)

Coverage:

 - [Current weather](https://openweathermap.org/current)
 - [5 day / 3 hour forecast](https://openweathermap.org/forecast5)
 - [16 day / daily forecast](https://openweathermap.org/forecast16)

## Getting Started

In order to make requests you need to register at [https://openweathermap.org](https://openweathermap.org)  and obtain an API. Save the key in a file called `own.key` . Then run

```bash
make run	
# or: stack exec test-owm
```

## Usage

```haskell
import qualified Network.Owm                as Owm
import           Network.Owm                (CityName(..), CityId(..), Coords(..),                     												CountryCode, ZipCode(..), Count(..))
import qualified Network.Owm.Forecast       as Forecast
import qualified Network.Owm.ForecastDaily  as ForecastDaily
import qualified Network.Owm.Weather        as Weather

```

```haskell
key <- Owm.Key <$> readFile f
```

Current weather is obtained by calling one of the functions in `Network.Owm.Weather`,

```haskell
weather <- Weather.fromCityId key Owm.Metric Owm.EN (CityId 2172797)
print weather
```

All functions require the arguments `Units` and `Lang`. For example, `Units` is defined as

```haskell
data Units = Metric | Standard | Imperial	-- src/Network/Owm/Owm.hs
```

Similarly, `Lang` is one of the supported ISO codes like `Lang.EN`, `Lang.FR`, `Lang.ES`.

For `Weather` (current weather), `Forecast` and `ForecastDaily` there are four functions each. The four ways to get the weather are

```haskell
-- by name
Weather.fromCityName key Owm.Metric Owm.EN (CityName "Altendorf" "CH") 
-- by city id
Weather.fromCityId key Owm.Metric Owm.EN (CityId 2172797)
-- by geo coordinates (latitude/longitude)
Weather.fromCoords key Owm.Metric Owm.EN (Owm.Coords 47.37643 8.54782)
-- by zip code
Weather.fromZipCode key Owm.Metric Owm.EN (ZipCode 8001 "CH")
```

`CityName` and `ZipCode` allow an optional country code. To specify the default country (USA) code use

```haskell
ZipCode 60606 DefaultCountry
```

`Forecast` and `ForecastDaily` have similar four functions, with an extra parameter `Count`. For example, the 3 day forcast for the north pole, in Swedish, is obtained like this:

```haskell
w<-ForecastDaily.fromCoords key Owm.Metric Owm.SE (Owm.Coords 90.0 0.0) (Count 3)
```

```haskell
w' = fromJust w	-- import Data.Maybe (fromJust)
forecastDailyCod w'	-- check return code, should be 200
ForecastDaily.listEltTemp  <$> forecastDailyList w'
```

The result is something like

```haskell
[Temp {tempMax = -17.8, tempDay = -17.94, tempNight = -20.45, tempMin = -20.45, 
       tempMorn = -17.8, tempEve = -18.76},
 Temp {tempMax = -21.36, tempDay = -22.46, tempNight = -21.36, tempMin = -22.46, 
       tempMorn = -22.46, tempEve = -22.4},
 Temp {tempMax = -17.7, tempDay = -20.14, tempNight = -17.7, tempMin = -21.36, 
       tempMorn = -21.36, tempEve = -18.12}
]
```

The data accessors look a little nicer by using [lenses](https://hackage.haskell.org/package/lens):

```Haskell
import Control.Lens
w <- fromJust <$> ForecastDaily.fromCoords key Owm.Metric Owm.ES (Owm.Coords 90.0 180.0) (Count 6)	-- 6 12h intervals
cod = w ^. ForecastDaily._forecastDailyCod
dts = w ^. ForecastDaily._forecastDailyList ^.. traverse . ForecastDaily._listEltDt
temps = w ^. ForecastDaily._forecastDailyList ^.. traverse . ForecastDaily._listEltTemp
```

OWM represents timestamps as UNIX epoch `Double`s. Use  `toUTCTime :: Double -> UTCTime` to convert to a proper time stamp.

```haskell
print $ zip (toUTCTome <$> dts) temps
```

This gives a result like

```haskell
(2018-11-9 05:00:00 UTC,Temp {tempMax = -17.8, tempDay = -17.94, tempNight = -20.45, tempMin = -20.45, tempMorn = -17.8, tempEve = -18.76})
(2018-11-9 11:00:00 UTC,Temp {tempMax = -21.33, tempDay = -22.4, tempNight = -21.33, tempMin = -22.4, tempMorn = -22.4, tempEve = -22.35})
(2018-11-10 05:00:00 UTC,Temp {tempMax = -17.7, tempDay = -20.14, tempNight = -17.7, tempMin = -21.33, tempMorn = -21.33, tempEve = -18.12})
(2018-11-10 11:00:00 UTC,Temp {tempMax = -17.7, tempDay = -18.12, tempNight = -18.52, tempMin = -20.14, tempMorn = -20.14, tempEve = -17.7})
(2018-11-11 05:00:00 UTC,Temp {tempMax = -18.52, tempDay = -20.73, tempNight = -19.17, tempMin = -20.73, tempMorn = -18.52, tempEve = -20.68})
(2018-11-11 11:00:00 UTC,Temp {tempMax = -18.05, tempDay = -20.68, tempNight = -18.05, tempMin = -20.73, tempMorn = -20.73, tempEve = -19.17})
```

## About the API

The JSON generators/parsers are auto-generated with the help of [json-autotype](https://github.com/mgajda/json-autotype). This approach has advantages and disadvantages: No effort is spent on writing `ToJSON` and `FromJSON` instances, so it's easy to adapt and extend the library. The downside are not very intuitive naming and an abundance of name-clashes, resulting in lengthy (qualified) function names.

## `json-autotype`

[json-autotype](https://github.com/mgajda/json-autotype) does automatic type inference from JSON input.

```
# installation: 
stack install json-autotype runghc
```

JSON input is generated with `GetJsonExamples.hs`, it will write files into the folder `json`.

TODO: 
- Explain the setup
- Add some tests
