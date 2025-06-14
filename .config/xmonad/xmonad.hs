-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe, fromMaybe)

import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.Loggers
import XMonad.Util.Run

import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import System.Environment
import System.Process
import Data.IntMap (update)
import Data.List

-- main = do
--      session <- getEnv "DESKTOP_SESSION"
--      xmonad  $ maybe desktopConfig desktop session
--
-- desktop "gnome" = gnomeConfig
-- desktop "kde" = kde4Config
-- desktop "xfce" = xfceConfig
-- desktop "xmonad-mate" = gnomeConfig
-- desktop _ = desktopConfig

terminalBin :: String
terminalBin = "kitty"

backgroundPicture :: String
backgroundPicture = "/Pictures/Wallpapers/heroscreen-16042022-ROCKET-@3x.png"


        


main :: IO ()
main = do
    -- key info
    homeDir <- getEnvVar "HOME" ""
    hostName <- getEnvVar "HOSTNAME" "none"
    -- xrandrCmd <- getXrandrCmd hostName

    -- set xrandr 
    _ <- spawn "xsetup"
    -- _ <- spawn xrandrCmd

    -- spawn compistor
    _ <- spawn $ "picom --config " 
        <> homeDir 
        <> "/.config/picom/"
        <> getPicomConf hostName

    -- spawn xmobar 
    _ <- spawn "xmobar"

    -- set background
    _ <- spawn $ "sleep 2; feh --bg-fill " 
        <> homeDir 
        <> backgroundPicture

    -- start xmonad
    xmonad 
        $ ewmhFullscreen 
        $ ewmh 
        $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey -- bound to M-b
        $ myConfig


-- configuration, specifically keybinds
myConfig = def 
    { modMask = mod4Mask -- rebind mod to super
    , layoutHook = myLayout -- use custom layout
    , focusedBorderColor = "#0663d6"
    }
    -- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
    -- to get keys:
    -- sudo showkey -k
    -- xmodmap -pke
    `additionalKeysP`
    [ ("M-<Return>", spawn terminalBin)
    , ("M-i", spawn "flatpak --user run io.gitlab.librewolf-community")
    , ("M-y", spawn $ terminalBin <> " yazi")
    , ("M-S-s", unGrab *> spawn "flatpak --user run org.flameshot.Flameshot gui")
    , ("M-d", spawn "rofi -show drun")
    , ("M-S-d", spawn "rofi -show run")
    , ("M-C-d", spawn "rofi -show window")
    , ("M-S-t", spawn "i3lock -c000000")
    , ("M-S-p", lockAndSuspend)
    , ("M-C-r", restartXmonad)
    , ("M-<XF86MonBrightnessUp>", spawn "brightnessctl set 10%+")
    , ("M-<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
    , ("M-<XF86KbdBrightnessUp>", spawn "brightnessctl set 10%+")
    , ("M-<XF86KbdBrightnessDown>", spawn "brightnessctl set 10%-")
    , ("M-<XF86Messenger>", spawn "brightnessctl set 10%+")
    , ("M-<XF86Search>", spawn "brightnessctl set 10%-")
    ]


-- layout settings
-- myLayout = smartSpacing 24 
-- myLayout = spacingRaw True (Border 0 4 4 4) True (Border 5 4 4 4) True
myLayout = 
    spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
    $ tiled ||| Mirror tiled ||| Full ||| threeCol
    where 
        threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
        tiled = Tall nmaster delta ratio 
        nmaster = 1 --default number of windows in the master pane 
        ratio = 1/2 -- default proportion of screen occupied by master pane 
        delta = 3/100 -- percent of screen to increment by when resizing panes


myXmobarPP :: PP 
myXmobarPP = def
    { ppSep = magenta " * " 
    , ppTitleSanitize = xmobarStrip 
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
    where 
        formatFocused = wrap (white "[") (white "]") . magenta . ppWindow 
        formatUnfocused = wrap (lowWhite "[") (white "]") . blue . ppWindow 

        -- | Windows should have "some" title, which should not exceed a sane length 
        ppWindow :: String -> String 
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        blue, lowWhite, magenta, red, white, yellow :: String -> String 
        blue = xmobarColor "#bd93f9" "" 
        lowWhite = xmobarColor "#bbbbbb" "" 
        magenta = xmobarColor "#ff79c6" "" 
        red = xmobarColor "#ff5555" "" 
        white = xmobarColor "#f8f8f2" ""
        yellow = xmobarColor "#f1fa8c" ""




--        _   _ _ _ _            __                  _   _                 
--  _   _| |_(_) (_) |_ _   _   / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
-- | | | | __| | | | __| | | | | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- | |_| | |_| | | | |_| |_| | |  _| |_| | | | | (__| |_| | (_) | | | \__ \
--  \__,_|\__|_|_|_|\__|\__, | |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
--                      |___/                                              


-- lock and suspend
lockAndSuspend :: X () 
lockAndSuspend =  do 
    spawn "i3lock -c000000"
    spawn "systemctl suspend"

-- restart xmonad with a recompile
restartXmonad :: X ()
restartXmonad = do 
    spawn "xmonad --recompile; killall -9 picom; killall -9 xmobar; xmonad --restart"
 

--  _          _                    __                  _   _                 
-- | |__   ___| |_ __   ___ _ __   / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
-- | '_ \ / _ \ | '_ \ / _ \ '__| | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- | | | |  __/ | |_) |  __/ |    |  _| |_| | | | | (__| |_| | (_) | | | \__ \
-- |_| |_|\___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
--              |_|                                                           


getEnvVar :: String -> String -> IO String
getEnvVar varName defaultValue = do 
    envValue <- lookupEnv varName 
    return $ fromMaybe defaultValue envValue
    -- return $ case result of 
    --     Just value -> value 
    --     Nothing -> defaultValue


--                   __ _          __ _ _                   _   _   _             
--   ___ ___  _ __  / _(_) __ _   / _(_) | ___    __ _  ___| |_| |_(_)_ __   __ _ 
--  / __/ _ \| '_ \| |_| |/ _` | | |_| | |/ _ \  / _` |/ _ \ __| __| | '_ \ / _` |
-- | (_| (_) | | | |  _| | (_| | |  _| | |  __/ | (_| |  __/ |_| |_| | | | | (_| |
--  \___\___/|_| |_|_| |_|\__, | |_| |_|_|\___|  \__, |\___|\__|\__|_|_| |_|\__, |
--                        |___/                  |___/                      |___/ 
--

getPicomConf :: String -> String 
getPicomConf hostName 
    | hostName == "lt-zach" = "picom-gpu.conf"
    | otherwise = "picom.conf"




--                           _                      _   _             
-- __  ___ __ __ _ _ __   __| |_ __   ___  ___  ___| |_(_) ___  _ __  
-- \ \/ / '__/ _` | '_ \ / _` | '__| / __|/ _ \/ __| __| |/ _ \| '_ \ 
--  >  <| | | (_| | | | | (_| | |    \__ \  __/ (__| |_| | (_) | | | |
-- /_/\_\_|  \__,_|_| |_|\__,_|_|    |___/\___|\___|\__|_|\___/|_| |_|
--                                                                    


extractMonitorCount :: [String] -> Int
extractMonitorCount xrandrOutput = 
    case xrandrOutput of 
        (firstLine:_) ->
            let wordsInLine = words firstLine 
            in if length wordsInLine >= 2 && head wordsInLine == "Monitors:"
                then read (wordsInLine !! 1)
                else 0 
        [] -> 0


getXrandrMonitorList :: IO [String]
getXrandrMonitorList = do
    resultRaw <- readProcess "/usr/bin/xrandr" ["--listmonitors"] ""
    return $ lines resultRaw


isDocked :: IO Bool
isDocked = do
    monitorList <- getXrandrMonitorList
    return $ extractMonitorCount monitorList > 1

    


getXrandrCmd :: String -> IO String 
getXrandrCmd "lt-zach" = xrandrLtZach
getXrandrCmd _ = pure ""


xrandrLtZach :: IO String 
xrandrLtZach = do
    dockStatus <- isDocked
    let result = if dockStatus then xrandrLtZachDocked else xrandrLtZachUnDocked
    return result


xrandrLtZachUnDocked :: String 
xrandrLtZachUnDocked = ""

xrandrLtZachDocked :: String 
-- xrandrLtZachDocked = unlines -- unlines adds \n
xrandrLtZachDocked = 
    intercalate " "
        [ "xrandr"
        , "--output eDP-1 --primary --mode 2256x1504 --pos 1461x2880 --rotate normal"
        , "--output DP-1 --off"
        , "--output DP-2 --off"
        , "--output DP-3 --off"
        , "--output DP-4 --off"
        , "--output DP-5 --off"
        , "--output DP-6 --off"
        , "--output DP-7 --off"
        , "--output DP-8 --off"
        , "--output DP-9 --off"
        , "--output DP-10 --off"
        , "--output DP-11 --mode 2560x2880 --pos 0x0 --rotate normal"
        , "--output DP-12 --mode 2560x2880 --pos 2560x0 --rotate normal"
        , "--output DP-13 --off"
        ]


