import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Prompt
import System.IO

import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

import XMonad.Util.NamedScratchpad

import Data.Ratio ((%))
import Data.List (isInfixOf)

import qualified XMonad.StackSet as W

--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"
  xmonad $ def
      { terminal = "urxvt"
      , borderWidth = myBorderWidth
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , clickJustFocuses = True
      , focusFollowsMouse = True
      , manageHook = myManageHook
      , layoutHook = myLayoutHook
      , workspaces = myWorkspaces
      , modMask = mod4Mask -- Mod is Super Key
      , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
      , logHook = dynamicLogWithPP xmobarPP
                      { ppOutput = hPutStrLn xmproc
                      , ppTitle = xmobarColor "green" "" . shorten 50
                      }
      }
      `additionalKeys`
          [ ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 5%+")
          , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 5%-")
          , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
          -- , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
          , ((0, 0x1008FFB2), spawn "amixer set Capture toggle")
          -- ]
          -- ++
          -- scratchpads
          ,((mod4Mask .|. controlMask, xK_k), namedScratchpadAction myScratchpads "keepassxc")
          ,((mod4Mask, xK_z), namedScratchpadAction myScratchpads "zeal")
          ]

-- Borders
myBorderWidth :: Dimension
myBorderWidth = 1
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#306EFF"

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2:work", "3:doc", "4:IM", "5:vm", "6:mov", "7:gs", "8:music", "9:gimp"] ++ ["NSP"]

-- Scratchpads
isKeepass = className =? "keepassxc"
isZeal = className =? "Zeal"

myScratchpads =
    [ NS "keepassxc" "keepassxc" isKeepass manageKeepass
    , NS "zeal" "zeal" isZeal manageZeal
    ]
  where
    manageKeepass = customFloating $ W.RationalRect 0.6 0.6 0.3 0.3
    manageZeal = nonFloating

-- Layouts
myLayoutHook =
    onWorkspace "4:IM" imLayout $
    -- onWorkspace "5:vm" full $
    onWorkspace "6:mov" full $
    onWorkspace "7:gs" full $
    standardLayouts
  where
    standardLayouts =
        avoidStruts $
        (tiled ||| reflectTiled ||| Mirror tiled ||| Grid ||| Full)

    tiled = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled = (reflectHoriz tiled)
    full = noBorders Full

    imLayout = avoidStruts $
               smartBorders $
               withIM ratio pidginRoster $
               reflectHoriz $
               withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid)
      where
        chatLayout = Grid
        ratio = (1%9)
        skypeRatio = (1%2)
        pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
        skypeRoster = And (ClassName "Skype") (Role "")

-- Hooks
myManageHook :: ManageHook
myManageHook =
    manageDocks
    <+> manageSpecific
    <+> namedScratchpadManageHook myScratchpads
    where
        manageSpecific = composeAll
            -- [ isFullscreen --> doFullFloat
            [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
            -- , isKeepass --> doCenterFloat
            , className =? "Gimp" --> doShift "9:gimp"
            , className =? "Pidgin" --> doShift "4:IM"
            , className =? "Skype" --> doShift "4:IM"
            , className =? "Steam" --> doShift "7:gs"
            , className =? "spotify" --> doShift "8:music"
            , className =? "xfreerdp" --> doShift "5:vm"
            , className =? "vlc" --> doShift "6:mov"
            , className =? "Chromium-browser" --> doShift "6:mov"
            ]

