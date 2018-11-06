.PHONY: prof clean json clean_json atom code run

all:
	stack build

run:
	stack build && stack exec parse-owm

json: auto-src/Weather.hs auto-src/Forecast.hs auto-src/ForecastDaily.hs auto-src/Station.hs

clean:
	stack clean

prof:
	stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

auto-src/Weather.hs:
	cd auto-src/; json-autotype ../json/Weather*.json -o Weather.hs; \
	stack runhaskell Weather.hs ../json/Weather*.json

auto-src/Forecast.hs:
	cd auto-src/; json-autotype ../json/Forecast*.json -o Forecast.hs; \
	stack runhaskell Forecast.hs ../json/Forecast*.json

auto-src/Station.hs:
	cd auto-src/; json-autotype ../json/Station*.json -o Station.hs; \
	stack runhaskell Station.hs ../json/Station*.json

auto-src/ForecastDaily.hs:
	cd auto-src/; json-autotype ../json/ForecastDaily*.json -o ForecastDaily.hs; \
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
