--
--  Imgur Uploader
--  Dan Colish <dcolish@gmail.com>
--  Copyright (c) 2010
--  All rights reserved
--
--  Usage: runghc imgur.hs <filepath>
--  Requires Network.Curl, HsLua and HXT
--  Both can be installed using `cabal install curl hxt hslua`
--  Be sure to add an api key before using it and put it in ~/imgurder.lua
--  A sample configuration file would be:
--  `echo '<your api key>' > ~/.imgurder`
--

import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree.Class
import Network.Curl
import System (getArgs)
import System.Directory
import System.IO
import Text.XML.HXT.DOM.TypeDefs
import qualified Text.XML.HXT.DOM.XmlNode as N
import qualified Text.XML.HXT.Parser.XmlParsec as H
import qualified Text.XML.HXT.XPath.XPathEval as X


myCurlPost :: String -> String -> [HttpPost]
myCurlPost apikey myImage =
    [ HttpPost { postName = "image"
               , contentType = Nothing , content = ContentFile myImage
               , extraHeaders = []
               , showName = Nothing },
      HttpPost { postName = "key"
               , contentType = Nothing
               , content = ContentString apikey
               , extraHeaders = []
               , showName = Nothing }]


pathTags :: [String]
pathTags = [ "/rsp/image_hash"
           ,"/rsp/delete_hash"
           ,"/rsp/original_image"
           ,"/rsp/large_thumbnail"
           ,"/rsp/small_thumbnail"
           ,"/rsp/imgur_page"
           ,"/rsp/delete_page" ]


xpathQN:: String -> XmlTree -> String
xpathQN str = fromJust . N.getQualifiedName . getNode . head . X.getXPath str


xpathTxt:: String -> XmlTree -> String
xpathTxt str = fromJust . N.getText . head . getChildren . head . X.getXPath str


keyVal :: XmlTree -> String -> String
keyVal res str = unwords [(xpathQN str res), " = ", (xpathTxt str res)]


formattedResult :: XmlTree -> [String]
formattedResult res = map (keyVal res) pathTags


loadConf :: IO String
loadConf = do
    h <- getHomeDirectory
    key <- readFile (h ++ "/.imgurder")
    return key


main ::  IO ()
main = withCurlDo $ do
    [file] <- getArgs
    key <- loadConf
    initialize
    ref <- newIORef []
    curlMultiPost "http://imgur.com/api/upload.xml"
            [CurlWriteFunction (gatherOutput ref), CurlVerbose False]
            $ myCurlPost key file
    response <- fmap reverse $ readIORef ref
    putStrLn "== Imgur Upload Complete ==\n"
    putStrLn . unlines . formattedResult . result $ response
    where
        result = head . H.xread . concatMap (unwords.tail.lines)

