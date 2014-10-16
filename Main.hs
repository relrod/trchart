{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Scientific
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Network.Wreq
import System.Environment (getArgs)

getWPMs :: String -> IO [Value]
getWPMs username = do
  wpms <- get $ "https://typeracerdata.appspot.com/games?playerId=tr:" ++ username ++ "&n=2000&offset=0"
  return $ wpms ^.. responseBody . values

main :: IO ()
main = do
  [username] <- getArgs
  wpms <- getWPMs username
  let wpms' = wpms ^@.. reindexed (+1) (reversed . traversed <. key "wpm" . _Number . to toRealFloat)
  toFile def (username ++ ".svg") $ do
    layout_title .= "Typeracer WPM History"
    plot (line "WPM" [wpms' :: [(Int, Double)]])
