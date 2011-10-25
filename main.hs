import Network.Imgurder
import System (getArgs)
import System.IO
import System.Exit

key :: String
key = "a05aaffbb595c38c2cd2d54d4c57eb3f"

main :: IO ()
main = do
    [file] <- getArgs
    maybeImgurUpload <- upload key file
    case maybeImgurUpload of
        Nothing -> hPutStrLn stderr "Something has gone wrong, try again later!" >> exitFailure
        Just imgurUpload -> print imgurUpload
