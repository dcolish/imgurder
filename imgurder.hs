--
--  Imgur Uploader
--  Dan Colish <dan@unencrypted.org>
--  Copyright (c) 2009
--  All rights reserved
--
--  Usage: runghc imgur.hs <filepath>
--  Requires Network.Curl and HXT
--  Both can be installed using `cabal install curl hxt`
--  Be sure to add an api key before using
--
{-
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright
          notice, this list of conditions and the following disclaimer in the
          documentation and/or other materials provided with the distribution.
        * Neither the name of the <organization> nor the
          names of its contributors may be used to endorse or promote products
          derived from this software without specific prior written permission.

     THIS SOFTWARE IS PROVIDED BY Daniel Colish ''AS IS'' AND ANY
     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
     DISCLAIMED. IN NO EVENT SHALL Daniel Colish BE LIABLE FOR ANY
     DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
     (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
      LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
     ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
     SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}


import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe
import Data.Tree.Class
import Network.Curl
import qualified Scripting.Lua as  Lua
import System (getArgs)
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
keyVal res str = (xpathQN str res) ++ " = " ++
                    (xpathTxt str res)

formattedResult :: XmlTree -> [String]
formattedResult res = map (keyVal res) pathTags

loadConf :: IO String
loadConf = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.loadfile l "/home/dcolish/.imgurder.lua"
    Lua.pcall l 0 0 0
    Lua.getglobal l "key"
    q <- Lua.gettop l
    key <- Lua.tostring l q
    Lua.close l
    return key

main ::  IO ()
main = withCurlDo $ do
    [file] <- getArgs
    key <- loadConf
    request <- initialize
    ref <- newIORef []
    run <- curlMultiPost "http://imgur.com/api/upload.xml"
            [CurlWriteFunction (gatherOutput ref), CurlVerbose False]
            $ myCurlPost key file
    response <- fmap reverse $ readIORef ref
    putStrLn $ "== Imgur Upload Complete ==\n"
    putStrLn $ unlines $ formattedResult $ result response
    where
        result = head . H.xread . concatMap (unwords.tail.lines)

