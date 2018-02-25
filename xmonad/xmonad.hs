import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmonad $ def {
         manageHook = manageDocks <+> manageHook def,
         layoutHook = avoidStruts  $  layoutHook def,
         modMask = mod4Mask -- Mod is Super Key
         }
