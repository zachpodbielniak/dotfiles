{-# LANGUAGE TemplateHaskell #-}
module Shell where
import Shh
$(loadEnv SearchPath)

gpia :: Command t => t
gpia = curl ["icanhazip.com"]

gpia4 :: Command t => t 
gpia4 = curl ["-4", "icanhazip.com"]

gpia6 :: Command t => t 
gpia6 = curl ["-6", "icanhazip.com"]

