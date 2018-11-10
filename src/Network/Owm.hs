{-# LANGUAGE OverloadedStrings #-}

module Network.Owm
(
    CityName(..),
    CityId(..),
    Coords (..),
    Count (..),
    Key (..),
    Lat,
    Lon,
    ZipCode(..),
    CountryCode(..),
    Lang(..),
    Units(..),
    owmBaseUrl25,
    owmBaseUrl30,
    makeParam, MakeParam,
    getOwm, getOwmWith
) where

import           Prelude hiding (LT)
import           Network.Wreq (Options)
import           Network.Wreq (param, defaults, getWith, responseBody)
import           Control.Lens ((.~), (&), (^.))
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           Data.String (IsString(..))
import           Data.Aeson(FromJSON, decode)

owmBaseUrl25 :: String
owmBaseUrl25 = "http://api.openweathermap.org/data/2.5/"

owmBaseUrl30 :: String
owmBaseUrl30 = "http://api.openweathermap.org/data/3.0/"


class MakeParam a where
    makeParam ::  a -> Options -> Options

type Lat = Double
type Lon = Double

data CountryCode = CountryCode String | AnyCountry deriving (Show)

instance IsString CountryCode where
    fromString s = if s == "" then AnyCountry else CountryCode s


newtype Key = Key String deriving (Show)

instance MakeParam Key where
    makeParam (Key key) = param "APPID" .~ [T.pack key]


newtype CityId = CityId Int deriving (Show)

instance MakeParam CityId where
    makeParam (CityId i) = (param "id" .~ [T.pack $ show i])


data CityName = CityName String CountryCode deriving (Show)

instance MakeParam CityName where
    makeParam (CityName n AnyCountry) = param "q" .~ [T.pack n]
    makeParam (CityName n (CountryCode c)) = param "q" .~ [T.pack n']
        where n' = n ++ "," ++  c


data Coords = Coords Lat Lon deriving (Show)

instance MakeParam Coords where
    makeParam (Coords lat lon) = (param "lat" .~ [show' lat])
                                . (param "lon" .~ [show' lon])
                                where show' = T.pack . show
   

data Count = Count Int | DefaultCount deriving (Show)

instance MakeParam Count where
    makeParam DefaultCount = id
    makeParam (Count i) = (param "cnt" .~ [T.pack $ show i])
                                

data ZipCode = ZipCode Int CountryCode deriving (Show)

instance MakeParam ZipCode where
    makeParam (ZipCode z AnyCountry) = param "zip" .~ [T.pack $ show z]
    makeParam (ZipCode z (CountryCode c)) = param "q" .~ [T.pack z']
        where z' = (show z) ++ "," ++  c
    
        

-- |Owm lang setting. See https://openweathermap.org/current
-- | awk 'BEGIN{FS=","}{print toupper($2)}' languages.txt | tr "\n" "|"
data Lang = AR|BG|CA|CZ|DE|EL|EN|FA|FI|FR|GL|HR|HU|IT|JA|KR|LA|LT|MK|NL|PL|PT|RO|RU|SE|SK|SL|ES|TR|UA|VI|ZH_CN|ZH_TW
            deriving (Eq)

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

instance MakeParam Lang where
    makeParam EN = id
    makeParam l = param "lang" .~ [T.pack $ show l]

-- |Owm units setting. See https://openweathermap.org/current
data Units = Metric | Standard | Imperial deriving (Eq)

instance Show Units where
    show Metric   = "metric"
    show Standard = ""
    show Imperial = "imperial"

instance MakeParam Units where
    makeParam Standard = id
    makeParam Metric = param "units" .~ ["metric"]
    makeParam Imperial = param "units" .~ ["imperial"]

getOwmWith :: FromJSON a => [Options->Options] -> Key -> Units -> Lang -> String -> IO (Maybe a)
getOwmWith opts key units lang url = do
    let params = [
            makeParam key,
            makeParam units,
            makeParam lang
            ] ++ opts
        opts' = foldl (&) defaults params
    r <- getWith opts' url >>= (return . (^. responseBody))
    return $ decode r

getOwm :: FromJSON a => Key -> Units -> Lang -> String -> IO (Maybe a)
getOwm = getOwmWith []