import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ desktopConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , terminal = "kitty"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys` myAdditionalKeys

myAdditionalKeys = [
  ((0, xF86XK_AudioLowerVolume), spawn "/home/andrew/dotfiles/scripts/volume.sh down %5"),
  ((0, xF86XK_AudioRaiseVolume), spawn "/home/andrew/dotfiles/scripts/volume.sh up %5"),
  ((0, xF86XK_AudioMute), spawn "/home/andrew/dotfiles/scripts/volume.sh mute"),
  ((0, xF86XK_AudioPrev), spawn "playerctl prev"),
  ((0, xF86XK_AudioNext), spawn "playerctl next"),
  ((0, xF86XK_AudioPlay), spawn "playerctl play"),
  ((0, xF86XK_AudioStop), spawn "playerctl stop")
  ]
