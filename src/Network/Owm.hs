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
    owm_base_url
) where

owm_base_url :: String
owm_base_url = "http://api.openweathermap.org/data/2.5/"

type Key = String
type CityName = String
type CityId = Int
type Coords = (Lat, Lon)
type Lat = Double
type Lon = Double
type ZipCode = Int
type CountryCode = String


-- |Owm lang setting. See https://openweathermap.org/current
data Lang = EN|RU|IT|ES|SP|UK|UA|DE|PT|RO|PL|FI|NL|FR|BG|SV|SE|ZH_TW|ZH|ZH_CN|TR|HR|CA

instance Show Lang where
    show EN    = "&lang=en"
    show RU    = "&lang=ru"
    show IT    = "&lang=it"
    show ES    = "&lang=es"
    show SP    = "&lang=sp"
    show UK    = "&lang=uk"
    show UA    = "&lang=ua"
    show DE    = "&lang=de"
    show PT    = "&lang=pt"
    show RO    = "&lang=ro"
    show PL    = "&lang=pl"
    show FI    = "&lang=fi"
    show NL    = "&lang=nl"
    show FR    = "&lang=fr"
    show BG    = "&lang=bg"
    show SV    = "&lang=sv"
    show SE    = "&lang=se"
    show ZH_TW = "&lang=zh_tw"
    show ZH    = "&lang=zh"
    show ZH_CN = "&lang=zh_cn"
    show TR    = "&lang=tr"
    show HR    = "&lang=hr"
    show CA    = "&lang=ca"


-- |Owm units setting. See https://openweathermap.org/current
data Units = Metric | Standard | Imperial

instance Show Units where
    show Metric   = "&units=metric"
    show Standard = ""
    show Imperial = "&units=imperial"
