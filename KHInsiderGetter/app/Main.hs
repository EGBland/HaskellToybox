{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

import Control.Lens ( (^.) )
import Control.Monad ( (>=>) )
import qualified Data.Bifunctor as BiF
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.UTF8 ( toString )
import Data.Foldable ( traverse_ )
import Data.Functor ( (<&>) )
import Data.List ( nub )
import Network.Wreq ( get, responseBody )
import System.Environment ( getArgs )
import Text.Printf ( printf )
import Text.Regex.PCRE ( (=~) )

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

trd3 :: (a,b,c) -> c
trd3 (_,_,z) = z


tuple2lift1 :: (Monad m) => a -> m b -> m (a,b)
tuple2lift1 a b = b >>= \x -> return (a,x)


matchStep :: String -> (String,String,String) -> (String,String,String)
matchStep rgx = (=~rgx) . trd3

matchAll :: String -> String -> [String]
matchAll rgx = map snd3 . takeWhile ((/="") . trd3) . iterate (matchStep rgx) . (=~rgx)

getMP3Links :: String -> IO [String]
getMP3Links link = do
    let rgx = "<a.*?>"
    r <- get link
    let body = toString $ r ^. responseBody :: String
    let match = matchAll rgx body
    let f :: String -> [[String]]
        f = (=~"href=['\"](.*?)['\"]")
    return . nub . filter (=~".*.mp3") . concatMap (map (!!1) . filter ((>1) . length) . f) $ match

getMP3 :: String -> IO (String,BSL.ByteString)
getMP3 link = tuple2lift1 (getBasename link) (get link <&> (^.responseBody))

getDLLink :: (Int,String) -> IO String
getDLLink (n,link) = printf "Getting link %d\t%s\n" n (getBasename link) >> head <$> getMP3Links link

getBasename :: String -> String
getBasename = head . head . (=~"[^/]*$")

main :: IO ()
main = do
    args <- getArgs
    let url = head args
    mp3Links <- map ("https://downloads.khinsider.com"++) <$> getMP3Links url
    traverse_ (getDLLink >=> getMP3 >=> (uncurry BSL.writeFile . BiF.first ("mp3/"++))) . zip [1::Int ..] $ mp3Links
