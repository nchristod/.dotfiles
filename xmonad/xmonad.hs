import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar /home/drugsha/.xmonad/.xmobarrc"
  xmonad $ def
      { manageHook = manageDocks <+> manageHook def
      , layoutHook = avoidStruts  $  layoutHook def
      , modMask = mod4Mask -- Mod is Super Key
      , logHook = dynamicLogWithPP xmobarPP
                      { ppOutput = hPutStrLn xmproc
                      , ppTitle = xmobarColor "green" "" . shorten 50
                      }
      }
