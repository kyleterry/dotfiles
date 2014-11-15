import XMonad
import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.SimpleFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import Control.Monad
import System.IO
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "urxvtc"
myModMask = mod4Mask
myWorkspaces = ["main", "web", "code", "build", "docs", "games"] ++ map show [7..9]
myManagehook = composeAll . concat $
    [
        [ className =? b  --> viewShift "web" | b <- myClassWebShifts ],
        [ className =? i  --> doFloat | i <- myClassFloats ++ myClassGames ],
        [ className =? g  --> viewShift "games" | g <- myClassGames ],
        -- [ className =? ig --> doIgnore | ig <- myClassGames ],
        [ isDialog        --> doCenterFloat ],
        [ isFullscreen    --> doFullFloat ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts = ["Firefox", "Iceweasel"]
        myClassFloats    = ["mplayer", "Gimp", "VLC"]
        myClassGames     = ["Steam"]
myLogHook h = dynamicLogWithPP $ xmobarPP
                                    { ppTitle = xmobarColor "green" "" . shorten 50
                                    , ppOutput = hPutStrLn h
                                    , ppCurrent = xmobarColor myCurrentWSColor ""
                                    , ppVisible = xmobarColor myVisibleWSColor ""
                                    , ppUrgent = xmobarColor myUrgentWSColor ""
                                        . wrap myUrgentWSLeft myUrgentWSRight
                                    }

-- Layouts
defaultLayouts = tiled ||| Mirror tiled ||| simpleFloat ||| Full ||| simpleTabbed
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

myLayoutHook = avoidStruts
    $ onWorkspace "games" gamesLayout
    $ defaultLayouts
    where
        gamesLayout = noBorders $ Full

-- Workspace colors
myCurrentWSColor = "#F0412E" -- color of active workspace
-- myCurrentWSColor = "#652DC1" -- color of active workspace
myVisibleWSColor = "#7B7B7B" -- color of inactive workspace
myUrgentWSColor = "#cc0000" -- color of workspace with 'urgent' window
myUrgentWSLeft = "{" -- wrap urgent workspace with these
myUrgentWSRight = "}"

myFocusedBorderColor = "#652DC1"
myNormalBorderColor = "#372D2C"

startupWorkspace = "code"

--myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
--myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
keysToAdd x =
    [ ((modMask x .|. controlMask, xK_l), spawn "slock")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2000+")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2000-")
    , ((0, xF86XK_AudioNext), spawn "ncmpcpp next")
    , ((0, xF86XK_AudioPrev), spawn "ncmpcpp prev")
    , ((0, xF86XK_AudioPlay), spawn "ncmpcpp toggle")
    , ((0, xF86XK_AudioStop), spawn "ncmpcpp stop")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10%")
    , ((0, xK_Print), spawn "scrot '%Y-%m-%d_$wx$h.png'")
    ]
myKeys x = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))
main = do
    xmproc <- spawnPipe "/home/kyle/.cabal/bin/xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal    = myTerminal,
        workspaces  = myWorkspaces,
        manageHook  = manageDocks <+> myManagehook <+> manageHook defaultConfig,
        focusedBorderColor = myFocusedBorderColor,
        normalBorderColor = myNormalBorderColor,
        modMask     = myModMask,
        logHook     = myLogHook xmproc,
        layoutHook  = myLayoutHook,
        keys        = myKeys,
        borderWidth = 1
    }
