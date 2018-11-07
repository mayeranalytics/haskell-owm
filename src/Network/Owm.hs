module Network.Owm
(
    Key,
    CityName,
    CityId,
    Coords,
    Lat,
    Lon,
    ZipCode,
    CountryCode,
    Lang(..),
    Units(..),
    owmBaseUrl25
) where

import Prelude hiding (LT)

owmBaseUrl25 :: String
owmBaseUrl25 = "http://api.openweathermap.org/data/2.5/"

owmBaseUrl30 :: String
owmBaseUrl30 = "http://api.openweathermap.org/data/3.0/"

type Key = String
type CityName = String
type CityId = Int
type Coords = (Lat, Lon)
type Lat = Double
type Lon = Double
type ZipCode = Int
type CountryCode = String


-- |Owm lang setting. See https://openweathermap.org/current
-- | awk 'BEGIN{FS=","}{print toupper($2)}' languages.txt | tr "\n" "|"
data Lang = AR|BG|CA|CZ|DE|EL|EN|FA|FI|FR|GL|HR|HU|IT|JA|KR|LA|LT|MK|NL|PL|PT|RO|RU|SE|SK|SL|ES|TR|UA|VI|ZH_CN|ZH_TW

-- |awk 'BEGIN{FS=","}{print "show " toupper($2) " = \"" $2 "\"" }' languages.txt
instance Show Lang where
    show AR = "ar"
    show BG = "bg"
    show CA = "ca"
    show CZ = "cz"
    show DE = "de"
    show EL = "el"
    show EN = "en"
    show FA = "fa"
    show FI = "fi"
    show FR = "fr"
    show GL = "gl"
    show HR = "hr"
    show HU = "hu"
    show IT = "it"
    show JA = "ja"
    show KR = "kr"
    show LA = "la"
    show LT = "lt"
    show MK = "mk"
    show NL = "nl"
    show PL = "pl"
    show PT = "pt"
    show RO = "ro"
    show RU = "ru"
    show SE = "se"
    show SK = "sk"
    show SL = "sl"
    show ES = "es"
    show TR = "tr"
    show UA = "ua"
    show VI = "vi"
    show ZH_CN = "zh_cn"
    show ZH_TW = "zh_tw"

-- |Owm units setting. See https://openweathermap.org/current
data Units = Metric | Standard | Imperial

instance Show Units where
    show Metric   = "metric"
    show Standard = ""
    show Imperial = "imperial"
