data Month = January | February | March | April | May | June | July
    | August | September | October | November | December
-- special case of data declaration, called enumeration.
-- NONE of the constructors can take arguments to have an enumeration.
-- unexample:
data Colour = Black | Red | Green | Blue | Cyan
    | Yellow | Magenta | White | RGB Int Int Int
-- Named fields allow you to get data from a type Much easier:
data Configuration = Configuration
    { username      :: String
    , localHost     :: String
    , remoteHost    :: String
    , isGuest       :: Bool
    , isSuperuser   :: Bool
    , currentDir    :: String
    , homeDir       :: String
    , timeConnected :: Integer
    } deriving (Eq, Show)
-- eg. person isGuest
changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir = cfg { currentDir = newDir }
-- variables are immutable; it's returning a new one
getHostData (Configuration { localHost = lh, remoteHost = rh }) = (lh, rh)
cfgFoo = Configuration { username = "Foo" } -- evaluating unspecified fields = runtime error!
