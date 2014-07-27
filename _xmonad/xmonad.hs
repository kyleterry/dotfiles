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
import qualified XMonad.StackSet as W

myTerminal = "urxvt"
myModMask = mod4Mask
myWorkspaces = ["main", "web", "code", "games", "docs"] ++ map show [6..9]
myManagehook = composeAll . concat $
    [
        [ className =? b  --> viewShift "web" | b <- myClassWebShifts ],
        [ className =? i  --> doFloat | i <- myClassFloats ++ myClassGames ],
        [ className =? g  --> viewShift "games" | g <- myClassGames ],
        [ className =? ig --> doIgnore | ig <- myClassGames ],
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

gamesLayout = noBorders $ Full

myLayoutHook = avoidStruts
             $ onWorkspace "games" gamesLayout
             $ defaultLayouts

myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor = "#cc0000" -- color of workspace with 'urgent' window
myUrgentWSLeft = "{" -- wrap urgent workspace with these
myUrgentWSRight = "}"

startupWorkspace = "code"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal    = myTerminal,
        workspaces  = myWorkspaces,
        manageHook  = manageDocks <+> myManagehook <+> manageHook defaultConfig,
        modMask     = myModMask,
        logHook     = myLogHook xmproc,
        layoutHook  = myLayoutHook,
        borderWidth = 1
    }
