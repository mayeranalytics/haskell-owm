.PHONY: prof clean json clean_json atom code run json-examples install

all:
	stack build

run:
	stack build && stack exec parse-owm

json: auto-src/Weather.hs auto-src/Forecast.hs auto-src/ForecastDaily.hs 

json-examples:
	stack runhaskell GetJsonExamples.hs

clean:
	stack clean

prof:
	stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

install:
	sed 's/module Weather/module Network.Owm.Internal.Weather/' auto-src/Weather.hs > src/Network/Owm/Internal/Weather.hs; \
	sed 's/module Forecast/module Network.Owm.Internal.Forecast/' auto-src/Forecast.hs > src/Network/Owm/Internal/Forecast.hs; \
	sed 's/module ForecastDaily/module Network.Owm.Internal.ForecastDaily/' auto-src/ForecastDaily.hs > src/Network/Owm/Internal/ForecastDaily.hs

auto-src/Weather.hs:
	cd auto-src/; json-autotype -tWeather ../json/Weather*.json -o Weather.hs; \
	stack runhaskell Weather.hs ../json/Weather*.json

auto-src/Forecast.hs:
	cd auto-src/; json-autotype -tForecast ../json/Forecast*.json -o Forecast.hs; \
	stack runhaskell Forecast.hs ../json/Forecast*.json

auto-src/Station.hs:
	cd auto-src/; json-autotype -tStation ../json/Station*.json -o Station.hs; \
	stack runhaskell Station.hs ../json/Station*.json

auto-src/ForecastDaily.hs:
	cd auto-src/; json-autotype -tForecastDaily ../json/ForecastDaily*.json -o ForecastDaily.hs; \
	stack runhaskell ForecastDaily.hs ../json/ForecastDaily*.json

city_list.txt:
	wget http://openweathermap.org/help/city_list.txt

clean_json:
	rm auto-src/*.hs

atom:
	stack exec atom .

code:
	stack build stylish-haskell hlint intero hoogle && \
	zsh -c -i "code ."
