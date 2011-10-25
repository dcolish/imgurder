import Network.Imgurder
import System (getArgs)
import System.IO
import System.Exit

key :: String
key = "a05aaffbb595c38c2cd2d54d4c57eb3f"

main :: IO ()
main = do
    [file] <- getArgs
    imgurUpload <- upload key file
    case imgurUpload of
      Right a -> print a
      Left a -> print a -- Uknown Error happened
