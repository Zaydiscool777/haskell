import Control.Monad
-- This wouldn't work, since IO [String] isn't [IO String]:
-- fiveGetLines = replicate 5 getLine
-- however, there is: sequence :: (Monad m) => [m a] -> m [a]
fiveGetLines = sequence $ replicate 5 getLine -- but why not:
fiveGetLinesAlt = replicateM 5 getLine -- seq $ replicate
-- seq $ map is mapM
-- seq $ flip map is forM
-- sequence_, replicateM_, mapM_, & forM_ discard final values (>>= vs. >>)

