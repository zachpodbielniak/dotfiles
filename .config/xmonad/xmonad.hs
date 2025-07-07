-- if using cabal for other projects might need to run the following:
-- cabal install --lib xmonad xmonad-contrib --package-env .
-- cabal install --lib xmonad xmonad-contrib X11 unix process containers mtl --overwrite-policy=always 


import System.Posix.Env (getEnv)
import Data.Maybe (maybe, fromMaybe)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import qualified XMonad.Layout.ToggleLayouts as TGL
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

import XMonad.Actions.FloatKeys
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS

import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.ShowWName

import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS

import System.Environment
import System.Process
import System.Posix.Process (forkProcess)
import Control.Concurrent (forkIO)
import Control.Monad (unless, forM_, when)
import Data.IntMap (update)
import Data.List
import Data.Char
import Data.Typeable
import qualified Data.Map as M

import XMonad.Hooks.ManageHelpers

--                      _              _              _   _   _                 
--   ___ ___  _ __  ___| |_ __ _ _ __ | |_   ___  ___| |_| |_(_)_ __   __ _ ___ 
--  / __/ _ \| '_ \/ __| __/ _` | '_ \| __| / __|/ _ \ __| __| | '_ \ / _` / __|
-- | (_| (_) | | | \__ \ || (_| | | | | |_  \__ \  __/ |_| |_| | | | | (_| \__ \
--  \___\___/|_| |_|___/\__\__,_|_| |_|\__| |___/\___|\__|\__|_|_| |_|\__, |___/
--                                                                    |___/     


myFont :: String 
myFont = "xft:Hack Nerd Font Mono:regular:size=12:antialias=true:hinting=true"

myModMask :: KeyMask 
myModMask = mod4Mask  -- super key

myTerminal :: String
myTerminal = "st "

myTerminalAlt :: String 
myTerminalAlt = "kitty "

myBrowser :: String 
myBrowser = "flatpak --user run io.gitlab.librewolf-community " 

myEditor :: String 
myEditor = "nvim "

openEditor :: String 
openEditor = myTerminal <> myEditor 

myBackgroundPicture :: String
myBackgroundPicture = "/Pictures/Wallpapers/heroscreen-16042022-ROCKET-@3x.png"


myNoOp :: String 
myNoOp = "sleep 0"


--            _                             _         
--   ___ __ _| |_ _ __  _ __  _   _  ___ ___(_)_ __  
--  / __/ _` | __| '_ \| '_ \| | | |/ __/ __| | '_ \ 
-- | (_| (_| | |_| |_) | |_) | |_| | (_| (__| | | | |
--  \___\__,_|\__| .__/| .__/ \__,_|\___\___|_|_| |_|
--              |_|   |_|                           

-- Catppuccin Mocha color palette
catRosewater = "#f5e0dc"
catFlamingo = "#f2cdcd"
catPink = "#f5c2e7"
catMauve = "#cba6f7"
catRed = "#f38ba8"
catMaroon = "#eba0ac"
catPeach = "#fab387"
catYellow = "#f9e2af"
catGreen = "#a6e3a1"
catTeal = "#94e2d5"
catSky = "#89dceb"
catSapphire = "#74c7ec"
catBlue = "#89b4fa"
catLavender = "#b4befe"
catText = "#cdd6f4"
catSubtext1 = "#bac2de"
catSubtext0 = "#a6adc8"
catOverlay2 = "#9399b2"
catOverlay1 = "#7f849c"
catOverlay0 = "#6c7086"
catSurface2 = "#585b70"
catSurface1 = "#45475a"
catSurface0 = "#313244"
catBase = "#1e1e2e"
catMantle = "#181825"
catCrust = "#11111b"


--  ___ ___  _ __  / _(_) __ _ _   _ _ __ __ _| |_(_) ___  _ __  ___ 
-- / __/ _ \| '_ \| |_| |/ _` | | | | '__/ _` | __| |/ _ \| '_ \/ __|
-- | (_| (_) | | | |  _| | (_| | |_| | | | (_| | |_| | (_) | | | \__ \
--  \___\___/|_| |_|_| |_|\__, |\__,_|_|  \__,_|\__|_|\___/|_| |_|___/
--                        |___/                                      

-- ShowWName configuration for workspace switch animations
myShowWNameConfig :: SWNConfig
myShowWNameConfig = def
    { swn_font = myFont
    , swn_bgcolor = catMantle
    , swn_color = catText
    , swn_fade = 0.8
    }

-- GridSelect configuration with Catppuccin colors
myGSConfig :: GSConfig Window
myGSConfig = def
    { gs_cellheight = 60
    , gs_cellwidth = 200
    , gs_cellpadding = 10
    , gs_font = myFont
    , gs_navigate = myGSNavigation
    , gs_colorizer = myGSColorizer
    }

-- GridSelect navigation with vim keys
myGSNavigation :: TwoD a (Maybe a)
myGSNavigation = defaultNavigation

-- GridSelect colorizer with Catppuccin colors
myGSColorizer :: Window -> Bool -> X (String, String)
myGSColorizer w active = do
    if active
        then return (catBase, catBlue)
        else return (catBase, catLavender)

-- GridSelect workspace configuration
myGSWorkspaceConfig :: GSConfig WorkspaceId
myGSWorkspaceConfig = def
    { gs_cellheight = 60
    , gs_cellwidth = 200
    , gs_cellpadding = 10
    , gs_font = myFont
    , gs_navigate = defaultNavigation
    }


--                                     _   _                 _    
--  _   _ _ __ __ _  ___ _ __   ___ _| |_| |__   ___   ___ | | __
-- | | | | '__/ _` |/ _ \ '_ \ / __| | __| '_ \ / _ \ / _ \| |/ /
-- | |_| | | | (_| |  __/ | | | (__| | |_| | | | (_) | (_) |   < 
--  \__,_|_|  \__, |\___|_| |_|\___|\__|\__|_|  \___/ \___/|_|\_\
--            |___/                                              

-- Custom urgency hook configuration
myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = def 
    { suppressWhen = Focused
    , remindWhen = Every 30
    }

-- Timer-based border pulsing for urgent windows
data BorderPulse = BorderPulse deriving (Read, Show, Typeable)
instance ExtensionClass BorderPulse where
    initialValue = BorderPulse

-- Function to pulse border color for urgent windows (simplified)
pulseUrgentBorder :: X ()
pulseUrgentBorder = return () -- Simplified for now, urgency hook handles border color


--   __           _        _                  _   _           
--  / _| __ _  __| | ___  (_)_ __   __ _  ___| |_(_)_   _____ 
-- | |_ / _` |/ _` |/ _ \ | | '_ \ / _` |/ __| __| \ \ / / _ \
-- |  _| (_| | (_| |  __/ | | | | | (_| | (__| |_| |\ V /  __/
-- |_|  \__,_|\__,_|\___| |_|_| |_|\__,_|\___|\__|_| \_/ \___|

-- Custom fade inactive log hook that uses simple fadeInactiveLogHook
-- but with a custom query to check window properties
myFadeInactiveLogHook :: X ()
myFadeInactiveLogHook = do
    -- First apply the standard fade
    fadeInactiveLogHook 0.85
    -- Then reset opacity for excluded windows
    withWindowSet $ \ws -> do
        let allWindows = W.allWindows ws
        forM_ allWindows $ \w -> do
            shouldExclude <- runQuery isExcludedFromFade w
            when shouldExclude $ setOpacity w 1.0

-- Query to check if a window should be excluded from fading
isExcludedFromFade :: Query Bool
isExcludedFromFade = do
    isLibrewolfToolkit <- (className =? "librewolf") <&&> (resource =? "Toolkit")
    -- isStTerm <- className =? "st-256color"  -- Uncomment to exclude st from fading
    return isLibrewolfToolkit  -- Add "|| isStTerm" if uncommenting above

 
--                                              _                 _        
--  _ __ ___   __ _ _ __   __ _  __ _  ___  | |__   ___   ___ | | _____ 
-- | '_ ` _ \ / _` | '_ \ / _` |/ _` |/ _ \ | '_ \ / _ \ / _ \| |/ / __|
-- | | | | | | (_| | | | | (_| | (_| |  __/ | | | | (_) | (_) |   <\__ \
-- |_| |_| |_|\__,_|_| |_|\__,_|\__, |\___| |_| |_|\___/ \___/|_|\_\___/
--                              |___/                                   

myManageHook :: ManageHook
myManageHook = composeAll
    [ isDialog            --> doFloat
    , isFullscreen        --> doFullFloat
    , className =? "Gimp" --> doFloat
    , className =? "Galculator" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , className =? "Xmessage" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "Pinentry" --> doFloat
    , className =? "Pinentry-gtk-2" --> doFloat
    , className =? "Gpick" --> doFloat
    , className =? "Lxappearance" --> doFloat
    , className =? "Nitrogen" --> doFloat
    , className =? "Arandr" --> doFloat
    , className =? "SimpleScreenRecorder" --> doFloat
    , className =? "Blueman-manager" --> doFloat
    , className =? "Gnome-calculator" --> doFloat
    , className =? "flameshot" --> doFloat
    , className =? "Flameshot" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "vlc" --> doFloat
    , className =? "VirtualBox Manager" --> doFloat
    , className =? "VirtualBox Machine" --> doFloat
    , className =? "Virt-manager" --> doFloat
    , className =? "qemu" --> doFloat
    , className =? "Qemu-system-x86_64" --> doFloat
    , className =? "File-roller" --> doFloat
    , className =? "Engrampa" --> doFloat
    , className =? "zoom" --> doFloat
    , className =? "zoom " --> doFloat
    , className =? "Zoom - Free Account" --> doFloat
    , className =? "Zoom Meeting" --> doFloat
    , className =? "Zoom Cloud Meetings" --> doFloat
    , className =? "Steam" --> doFloat
    , className =? "steam" --> doFloat
    , className =? "Wine" --> doFloat
    , className =? "Wine-Programs" --> doFloat
    , className =? "Lutris" --> doFloat
    , className =? "Heroic" --> doFloat
    , resource  =? "Dialog" --> doFloat
    , resource  =? "dialog" --> doFloat
    , title     =? "Mozilla Firefox" --> doFloat
    , title     =? "Picture-in-Picture" --> doFloat
    , title     =? "Library" --> doFloat  -- Firefox download window
    , title     =? "Save As" --> doFloat
    , title     =? "Open File" --> doFloat
    , title     =? "Choose Files" --> doFloat
    , title     =? "Upload" --> doFloat
    , title     =? "Download" --> doFloat
    , title     =? "Preferences" --> doFloat
    , title     =? "Settings" --> doFloat
    , title     =? "Properties" --> doFloat
    , title     =? "Options" --> doFloat
    , title     =? "About" --> doFloat
    , title     =? "Warning" --> doFloat
    , title     =? "Error" --> doFloat
    , title     =? "Information" --> doFloat
    , title     =? "Question" --> doFloat
    , title     =? "Confirm" --> doFloat
    , title     =? "Alert" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "bubble" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "task_dialog" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "Preferences" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "About" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "toolbox" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "splash" --> doFloat
    ]


--                  _       
--  _ __ ___   __ _(_)_ __  
-- | '_ ` _ \ / _` | | '_ \ 
-- | | | | | | (_| | | | | |
-- |_| |_| |_|\__,_|_|_| |_|
                         

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
    _ <- spawn $ "picom --vsync --config " 
        <> homeDir 
        <> "/.config/picom/"
        <> getPicomConf hostName

    -- spawn xmobar 
    _ <- spawn "xmobar"

    -- spawn trayer 
    _ <- spawn "trayer --monitor primary --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 8 --transparent true --tint 0x000000 --height 16"

    -- set background
    _ <- spawn $ "sleep 2; feh --bg-fill " 
        <> homeDir 
        <> myBackgroundPicture

    -- set Xorg settings asynchronously  
    _ <- spawn $ getXsettings hostName
    -- _ <- forkProcess $ setXsettings hostName

    -- start xmonad
    xmonad 
        $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = catRed } myUrgencyConfig
        $ ewmhFullscreen 
        $ ewmh 
        $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey -- bound to M-b
        $ myConfig


-- configuration, specifically keybinds
myConfig = def 
    { modMask = mod4Mask -- rebind mod to super
    , layoutHook = showWName' myShowWNameConfig myLayout -- use custom layout with workspace names
    , manageHook = myManageHook <+> manageHook def
    , logHook = myFadeInactiveLogHook -- custom fade inactive with exclusions
    , focusedBorderColor = catBlue
    , normalBorderColor = catSurface0
    , borderWidth = 2
    , terminal = myTerminal
    , mouseBindings = myMouseBindings
    }
    -- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
    -- to get keys:
    -- sudo showkey -k
    -- xmodmap -pke
    `additionalKeysP`
    [ ("M-<Return>", spawn myTerminal)
    , ("M-S-<Return>", spawn myTerminalAlt)
    , ("M-i", spawn "flatpak --user run io.gitlab.librewolf-community")
    , ("M-y", spawn $ myTerminal <> " yazi")
    , ("M-S-s", unGrab *> spawn "flatpak --user run org.flameshot.Flameshot gui")
    , ("M-d", spawn "rofi -show drun")
    , ("M-S-d", spawn "rofi -show run")
    , ("M-C-d", spawn "rofi -show window")
    , ("M-S-t", spawn "i3lock -c000000")
    , ("M-S-p", lockAndSuspend)
    , ("M-C-r", restartXmonad)
    , ("M-f", sendMessage $ TGL.Toggle "Full")
    , ("M-S-f", withFocused toggleFloat)
    , ("M-<XF86MonBrightnessUp>", spawn "brightnessctl set 10%+")
    , ("M-<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
    , ("M-<XF86KbdBrightnessUp>", spawn "brightnessctl set 10%+")
    , ("M-<XF86KbdBrightnessDown>", spawn "brightnessctl set 10%-")
    , ("M-<XF86Messenger>", spawn "brightnessctl set 10%+")
    , ("M-<XF86Search>", spawn "brightnessctl set 10%-")
    , ("C-M1-1", spawn $ toggleHassCmd "lamp-01")
    , ("C-M1-2", spawn $ toggleHassCmd "lamp-02")
    , ("C-M1-3", spawn $ toggleHassCmd "lamp-03")
    , ("C-M1-4", spawn $ toggleHassCmd "lamp-04")
    , ("C-M1-5", spawn $ toggleHassCmd "can-left")
    , ("C-M1-6", spawn $ toggleHassCmd "can-right")
    , ("C-M1-7", spawn $ toggleHassCmd "storage-room")
    , ("C-M1-8", spawn $ toggleHassCmd "server-room")
    , ("C-M1-9", spawn $ toggleHassCmd "fan-01")
    , ("C-M1-0", spawn $ toggleHassCmd "fan-02")

    -- Floating window stuff
    -- Move with Mod+Ctrl+<hjkl>
    , ("M-C-h", withFocused $ keysMoveWindow (-20,0))
    , ("M-C-l", withFocused $ keysMoveWindow (20,0))
    , ("M-C-k", withFocused $ keysMoveWindow (0,-20))
    , ("M-C-j", withFocused $ keysMoveWindow (0,20))
    -- Resize with Mod+Alt+<hjkl>
    , ("M-M1-h", withFocused $ keysResizeWindow (-20,0) (1,0))
    , ("M-M1-l", withFocused $ keysResizeWindow (20,0) (1,0))
    , ("M-M1-k", withFocused $ keysResizeWindow (0,-20) (0,1))
    , ("M-M1-j", withFocused $ keysResizeWindow (0,20) (0,1))
    
    -- GridSelect
    , ("M-g", goToSelected myGSConfig)
    , ("M-S-g", bringSelected myGSConfig)
    , ("M-C-g", gridselectWorkspace myGSWorkspaceConfig W.shift)
    
    -- FloatSnap (smooth window snapping)
    , ("M-<Left>", withFocused $ snapMove L Nothing)
    , ("M-<Right>", withFocused $ snapMove R Nothing)
    , ("M-<Up>", withFocused $ snapMove U Nothing)
    , ("M-<Down>", withFocused $ snapMove D Nothing)
    , ("M-S-<Left>", withFocused $ snapShrink R Nothing)
    , ("M-S-<Right>", withFocused $ snapGrow R Nothing)
    , ("M-S-<Up>", withFocused $ snapShrink D Nothing)
    , ("M-S-<Down>", withFocused $ snapGrow D Nothing)
    
    -- Dynamic workspace ordering
    , ("M-C-<Right>", nextWS)
    , ("M-C-<Left>", prevWS)
    , ("M-S-C-<Right>", shiftToNext >> nextWS)
    , ("M-S-C-<Left>", shiftToPrev >> prevWS)
    ]   


-- layout settings
-- myLayout = smartSpacing 24 
-- myLayout = spacingRaw True (Border 0 4 4 4) True (Border 5 4 4 4) True
myLayout = 
    TGL.toggleLayouts Full
        $ spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
        $ tiled ||| Mirror tiled ||| Full ||| threeCol
    where 
        threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
        tiled = Tall nmaster delta ratio 
        nmaster = 1 --default number of windows in the master pane 
        ratio = 1/2 -- default proportion of screen occupied by master pane 
        delta = 3/100 -- percent of screen to increment by when resizing panes


myXmobarPP :: PP 
myXmobarPP = def
    { ppSep = mauve " | " 
    , ppTitleSanitize = xmobarStrip 
    , ppCurrent = wrap " " "" . xmobarBorder "Top" catBlue 2
    , ppHidden = text . wrap " " ""
    , ppHiddenNoWindows = overlay0 . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
    where 
        formatFocused = wrap (text "[") (text "]") . pink . ppWindow 
        formatUnfocused = wrap (subtext1 "[") (subtext1 "]") . lavender . ppWindow 

        -- | Windows should have "some" title, which should not exceed a sane length 
        ppWindow :: String -> String 
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        -- Catppuccin Mocha color functions for XMobar
        rosewater, flamingo, pink, mauve, red, maroon, peach, yellow :: String -> String
        green, teal, sky, sapphire, blue, lavender :: String -> String
        text, subtext1, subtext0, overlay2, overlay1, overlay0 :: String -> String
        surface2, surface1, surface0, base, mantle, crust :: String -> String
        
        rosewater = xmobarColor catRosewater ""
        flamingo = xmobarColor catFlamingo ""
        pink = xmobarColor catPink ""
        mauve = xmobarColor catMauve ""
        red = xmobarColor catRed ""
        maroon = xmobarColor catMaroon ""
        peach = xmobarColor catPeach ""
        yellow = xmobarColor catYellow ""
        green = xmobarColor catGreen ""
        teal = xmobarColor catTeal ""
        sky = xmobarColor catSky ""
        sapphire = xmobarColor catSapphire ""
        blue = xmobarColor catBlue ""
        lavender = xmobarColor catLavender ""
        text = xmobarColor catText ""
        subtext1 = xmobarColor catSubtext1 ""
        subtext0 = xmobarColor catSubtext0 ""
        overlay2 = xmobarColor catOverlay2 ""
        overlay1 = xmobarColor catOverlay1 ""
        overlay0 = xmobarColor catOverlay0 ""
        surface2 = xmobarColor catSurface2 ""
        surface1 = xmobarColor catSurface1 ""
        surface0 = xmobarColor catSurface0 ""
        base = xmobarColor catBase ""
        mantle = xmobarColor catMantle ""
        crust = xmobarColor catCrust ""


-- mouse bindings configuration
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((controlMask, button2), \w -> focus w >> mouseMoveWindow w)
    , ((controlMask, button3), \w -> focus w >> Flex.mouseResizeWindow w)
    ]



--           _           _                 _          _                     
-- __      _(_)_ __   __| | _____      __ | |__   ___| |_ __   ___ _ __ ___ 
-- \ \ /\ / / | '_ \ / _` |/ _ \ \ /\ / / | '_ \ / _ \ | '_ \ / _ \ '__/ __|
--  \ V  V /| | | | | (_| | (_) \ V  V /  | | | |  __/ | |_) |  __/ |  \__ \
--   \_/\_/ |_|_| |_|\__,_|\___/ \_/\_/   |_| |_|\___|_| .__/ \___|_|  |___/
--                                                     |_|                  

-- Helper function to toggle floating of a window
toggleFloat :: Window -> X ()
toggleFloat w = do 
    floats <- gets $ W.floating . windowset 
    if M.member w floats 
        then withFocused $ windows . W.sink 
        else float w



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



--                   __ _          __                  _   _                 
--   ___ ___  _ __  / _(_) __ _   / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
--  / __/ _ \| '_ \| |_| |/ _` | | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- | (_| (_) | | | |  _| | (_| | |  _| |_| | | | | (__| |_| | (_) | | | \__ \
--  \___\___/|_| |_|_| |_|\__, | |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
--                        |___/                                              


-- outside of XMonad context so we can't use spawn
-- setXsettings :: String -> IO ()
-- setXsettings hostName = do 
--     _ <- readProcess "/usr/bin/bash" [] "echo 'setXsettings running' >  /tmp/xmonad-debug.log"
--     _ <- readProcess "/usr/bin/bash" [] "echo 'about to print hostname' >> /tmp/xmonad-debug.log"
--     _ <- system $ "echo \"" <> hostName <> "\" >> /tmp/xmonad-debug.log"
--     -- command <- getXsettings hostName 
--     let command = getXsettings hostName
--     _ <- system "echo 'got command to run' >> /tmp/xmonad-debug.log"
--     _ <- system $ "echo '" <> command <> "' >> /tmp/xmonad-debug.log"
--     _ <- system command
--     return ()


getXsettings :: String -> String
getXsettings hostName = 
    -- specificSettings <- getXsettingsSpecificHost hostName
    intercalate "; " 
        [ "xset s off -dpms"
        , "xset r rate 200 50"
        , ""
        ] -- <> specificSettings

-- unused
-- getXsettingsSpecificHost :: String -> IO String
-- getXsettingsSpecificHost "lt-zach" = do
--     touchPadPropertyCmd <- getXpropertyTouchpadCmd "lt-zach"
--     return $ intercalate "; " 
--         [ "sleep 5"
--         , touchPadPropertyCmd
--         , ""
--         ]
-- getXsettingsSpecificHost _ = pure ""


-- https://askubuntu.com/questions/1000016/how-to-reverse-trackpoint-direction
-- unused, see xsetup script
-- getXpropertyTouchpadCmd :: String -> IO String 
-- getXpropertyTouchpadCmd "lt-zach" = do 
--     result <- readProcess 
--         "/usr/bin/bash" 
--         []
--         "sleep 5; xinput list | grep -i Touchpad | awk '{print $6}' | awk -F= '{print $2}'" 
--     let cleanResult = filter (not . null) $ lines $ trim result 
--     if null cleanResult
--         then return myNoOp
--         else return $ "xinput set-prop " <> (head cleanResult) <> " 295 1"
--     where 
--         trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


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



--            _                        _             _ _     
--   _____  _| |_ ___ _ __ _ __   __ _| |   ___ __ _| | |___ 
--  / _ \ \/ / __/ _ \ '__| '_ \ / _` | |  / __/ _` | | / __|
-- |  __/>  <| ||  __/ |  | | | | (_| | | | (_| (_| | | \__ \
--  \___/_/\_\\__\___|_|  |_| |_|\__,_|_|  \___\__,_|_|_|___/



-- home assistant calls
toggleHassSwitchCmd :: String -> String
toggleHassSwitchCmd switchId =
    "bash -c 'source $HOME/.bashrc && hass-cli service call switch.toggle --arguments entity_id=switch." <> switchId <> "'"

toggleHassLightCmd :: String -> String
toggleHassLightCmd lightId =
    "bash -c 'source $HOME/.bashrc && hass-cli service call light.toggle --arguments entity_id=light." <> lightId <> "'"

toggleHassCmd :: String -> String
toggleHassCmd "lamp-01" = toggleHassSwitchCmd "b4_b0_24_0e_09_83"
toggleHassCmd "lamp-02" = toggleHassSwitchCmd "b4_b0_24_0e_0d_c5"
toggleHassCmd "lamp-03" = toggleHassSwitchCmd "54_af_97_f4_25_28"
toggleHassCmd "lamp-04" = toggleHassSwitchCmd "28_87_ba_6a_2a_ef"
toggleHassCmd "can-left" = toggleHassLightCmd "1c_61_b4_64_ea_42"
toggleHassCmd "can-right" = toggleHassLightCmd "30_de_4b_77_19_54"
toggleHassCmd "storage-room" = toggleHassSwitchCmd "6c_5a_b0_9b_38_ae"
toggleHassCmd "server-room" = toggleHassSwitchCmd "d8_47_32_9e_e9_15"
toggleHassCmd "fan-01" = toggleHassSwitchCmd "b4_b0_24_29_80_df"
toggleHassCmd "fan-02" = toggleHassSwitchCmd "b4_b0_24_29_c2_2c"
-- end home assistant calls


