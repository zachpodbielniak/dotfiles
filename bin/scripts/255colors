#!/usr/bin/runhaskell

-- old bash version 
-- for i in {0..255}; do
--   printf "\e[38;5;%dm%d " ${i} ${i}
-- done
--

import Text.Printf

main :: IO () 
main = do 
    let nums = [0 .. 255] :: [Int]
    mapM_ (\x -> printf "\x1b[38;5;%dm%d\x1b[0m " x x) nums

