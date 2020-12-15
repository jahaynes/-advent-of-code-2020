{-# LANGUAGE LambdaCase #-}

import           Control.Monad  (foldM)
import           Data.Functor   ((<&>))
import qualified Data.Judy as J

main :: IO ()
main = do

     input <- do input <- readFile "./day15/input"
                 pure . (read :: String -> [Word]) $ '[':input++"]"

     part2 input

part2 :: [Word] -> IO ()
part2 input = do

     hist <- J.new

     sturn <- foldM (\t i -> do J.insert i t hist
                                pure $! t + 1) 1 (init input)

     print =<< foldM (\lst turn -> do

                         next' <- J.lookup lst hist <&> \case
                                      Just x  -> turn - x
                                      Nothing -> 0

                         J.insert lst turn hist

                         pure next') (last input) [sturn..30000000-1]
