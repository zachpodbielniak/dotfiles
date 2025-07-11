#!/usr/bin/runhaskell

{-
dotfiles - Personal configuration files and scripts
Copyright (C) 2025  Zach Podbielniak

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}


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

