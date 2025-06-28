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

import System.Environment
import System.Process
import System.Posix.Process (forkProcess)
import Control.Concurrent (forkIO)
import Data.IntMap (update)
import Data.List
import Data.Char
import qualified Data.Map as M


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
    _ <- spawn $ "picom --config " 
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
        $ ewmhFullscreen 
        $ ewmh 
        $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey -- bound to M-b
        $ myConfig


-- configuration, specifically keybinds
myConfig = def 
    { modMask = mod4Mask -- rebind mod to super
    , layoutHook = myLayout -- use custom layout
    , focusedBorderColor = "#0663d6"
    , terminal = myTerminal
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
    -- | hostName == "lt-zach" = "picom-gpu.conf"
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


