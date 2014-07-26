import XMonad
import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import Control.Monad
import System.IO
import qualified XMonad.StackSet as W

myTerminal = "urxvt"
myModMask = mod4Mask
myWorkspaces = ["main", "web", "code"] ++ map show [4..9]
myManagehook = composeAll . concat $
    [
        [ className =? b --> viewShift "web" | b <- myClassWebShifts ],
        [ className =? i --> doFloat | i <- myClassFloats ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts = ["Firefox", "Iceweasel"]
        myClassFloats    = ["mplayer", "Gimp", "VLC"]
myLogHook h = dynamicLogWithPP $ xmobarPP
                                    { ppTitle = xmobarColor "green" "" . shorten 50
                                    , ppOutput = hPutStrLn h
                                    , ppCurrent = xmobarColor myCurrentWSColor ""
                                    , ppVisible = xmobarColor myVisibleWSColor ""
                                    , ppUrgent = xmobarColor myUrgentWSColor ""
                                        . wrap myUrgentWSLeft myUrgentWSRight
                                    }
myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft = "[" -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft = "(" -- wrap inactive workspace with these
myVisibleWSRight = ")"
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
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
        borderWidth = 1
    }
