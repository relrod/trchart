{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Control.Monad (join)
import Data.Aeson.Lens
import Data.List (intersperse)
import Data.Scientific
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Network.Wreq
import System.Environment (getArgs)

getWPMs :: String -> IO [(Int, Double)]
getWPMs username = do
  wpms <- get $ "https://typeracerdata.appspot.com/games?playerId=tr:" ++ username ++ "&n=2000&offset=0"
  return $ wpms ^.. responseBody . values ^@.. reindexed (+1) (reversed . traversed <. key "wpm" . _Number . to toRealFloat)

main :: IO ()
main = do
  usernames <- getArgs
  plots <- mapM plot' usernames
  toFile def ((join . intersperse "_" $ usernames) ++ ".svg") $ do
    layout_title .= "Typeracer WPM History"
    sequence_ plots

plot' :: String -> IO (EC (Layout Int Double) ())
plot' username = do
  wpms <- getWPMs username
  return $ plot (line username [wpms])
