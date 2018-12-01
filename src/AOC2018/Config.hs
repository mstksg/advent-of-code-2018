
module AOC2018.Config (
    Config(..), configFile, defConfPath
  , session
  ) where
 
import           Control.Exception
import           Control.Monad
import           GHC.Generics      (Generic)
import           System.IO.Error
import           Text.Printf
import qualified Data.Aeson        as A
import qualified Data.ByteString   as BS
import qualified Data.Yaml         as Y


-- | Configuration for auto-runner.
newtype Config = Cfg { _cfgSession :: Maybe String }
  deriving (Generic)

-- | Default math to find a configuration file.
defConfPath :: FilePath
defConfPath = "aoc-conf.yaml"

-- | Load a 'Config' from a given filepath.
configFile :: FilePath -> IO Config
configFile fp = do
    cfgInp <- tryJust (guard . isDoesNotExistError)
            $ BS.readFile fp
    case cfgInp of
      Left () -> do
        Y.encodeFile fp emptyCfg
        return emptyCfg
      Right b ->
        case Y.decodeEither' b of
          Left e -> do
            printf "Configuration file at %s could not be parsed:\n" fp
            print e
            return emptyCfg
          Right cfg -> return cfg
  where
    emptyCfg = Cfg Nothing

-- | Load a session token from the configuration file at a given filepath.
session :: FilePath -> IO (Maybe String)
session = fmap _cfgSession . configFile

configJSON :: A.Options
configJSON = A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4 }

instance A.ToJSON Config where
    toJSON     = A.genericToJSON configJSON
    toEncoding = A.genericToEncoding configJSON
instance A.FromJSON Config where
    parseJSON  = A.genericParseJSON configJSON
