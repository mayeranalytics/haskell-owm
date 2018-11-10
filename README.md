# OpenWeatherMap #

## Overview

> **[OpenWeatherMap](https://openweathermap.org)** is an online service that provides weather data, including current weather data, forecasts, and historical data to the developers of web services and mobile applications. 
>
> For data sources, it utilizes meteorological broadcast services, raw data from [airport](https://en.wikipedia.org/wiki/Airport) [weather stations](https://en.wikipedia.org/wiki/Weather_station), raw data from radar stations, and raw data from other official weather stations. 
>
> [Wikipedia](https://en.wikipedia.org/wiki/OpenWeatherMap)

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

TODO: MORE

## About the API

## `json-autotype`

[json-autotype](https://github.com/mgajda/json-autotype) does automatic type inference from JSON input.

```
# installation: 
stack install json-autotype runghc
```

JSON input is generated with `GetJsonExamples.hs`, it will write files into the folder `json`.