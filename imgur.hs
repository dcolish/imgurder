import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe
import Data.Tree.Class
import Network.Curl
import System (getArgs)
import System.IO
import Text.XML.HXT.DOM.TypeDefs
import qualified Text.XML.HXT.DOM.XmlNode as N
import qualified Text.XML.HXT.Parser.XmlParsec as H
import qualified Text.XML.HXT.XPath.XPathEval as X

apikey :: String
apikey = "793579906fc2a799847e12db2b01cdcd"

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


mytags = [ "/rsp/image_hash"
           ,"/rsp/delete_hash"
           ,"/rsp/original_image"
           ,"/rsp/large_thumbnail"
           ,"/rsp/small_thumbnail"
           ,"/rsp/imgur_page"
           ,"/rsp/delete_page" ]


xpathQN:: String -> XmlTree -> String
xpathQN str =
    fromJust . N.getQualifiedName . getNode . head . X.getXPath str

xpathTxt:: String -> XmlTree -> String
xpathTxt str =
    fromJust . N.getText.head.getChildren.head . X.getXPath str

keyVal :: XmlTree -> String -> String
keyVal res str = (xpathQN str res) ++ " = " ++
                    (xpathTxt str res)

fullResponse :: XmlTree -> [String]
fullResponse res = map (keyVal res) mytags


main = withCurlDo $ do
    [file] <- getArgs
    r <- initialize
    ref <- newIORef []
    foo <- curlMultiPost "http://imgur.com/api/upload.xml"
            [CurlWriteFunction (gatherOutput ref), CurlVerbose False]
            $ myCurlPost apikey file
    out <- fmap reverse $ readIORef ref
    let res = head $ H.xread $ concatMap (unwords.tail.lines) out
    putStrLn $ "== Imgur Upload Complete ==\n"
    putStrLn $ unlines $ fullResponse res

