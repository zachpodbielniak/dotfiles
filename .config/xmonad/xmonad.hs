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

-- main = do
--      session <- getEnv "DESKTOP_SESSION"
--      xmonad  $ maybe desktopConfig desktop session
--
-- desktop "gnome" = gnomeConfig
-- desktop "kde" = kde4Config
-- desktop "xfce" = xfceConfig
-- desktop "xmonad-mate" = gnomeConfig
-- desktop _ = desktopConfig

backgroundPicture = "/Pictures/Wallpapers/heroscreen-16042022-ROCKET-@3x.png"


getEnvVar :: String -> String -> IO String
getEnvVar varName defaultValue = do 
    envValue <- lookupEnv varName 
    return $ fromMaybe defaultValue envValue
    -- return $ case result of 
    --     Just value -> value 
    --     Nothing -> defaultValue
        


main :: IO ()
main = do
    -- _ <- spawn "xmonad"
    homeDir <- getEnvVar "HOME" ""

    -- set background
    _ <- spawn $ "feh --bg-fill " 
        <> homeDir 
        <> backgroundPicture

    -- spawn compistor
    _ <- spawnPipe $ "picom --backend xrender --config " 
        <> homeDir 
        <> "/.config/picom/picom.conf"

    -- start xmonad
    xmonad 
        $ ewmhFullscreen 
        $ ewmh 
        $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey -- bound to M-b
        $ myConfig


focusedBorderColor = "#ffffff"

-- configuration, specifically keybinds
myConfig = def 
    { modMask = mod4Mask -- rebind mod to super
    , layoutHook = myLayout -- use custom layout
    }
    `additionalKeysP`
    [ ("M-<Return>", spawn "kitty")
    , ("M-i", spawn "flatpak --user run io.gitlab.librewolf-community")
    , ("M-C-s", unGrab *> spawn "scrot -s")
    , ("M-d", spawn "rofi -show drun")
    , ("M-S-d", spawn "rofi -show run")
    , ("M-w", spawn "rofi -show window")
    , ("M-S-t", spawn "i3lock -c000000")
    ]


-- layout settings
myLayout = smartSpacing 24 $ tiled ||| Mirror tiled ||| Full ||| threeCol
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
