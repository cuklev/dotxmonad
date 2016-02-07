import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PerWindowKbdLayout
import System.IO

import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FlexibleResize
import qualified XMonad.Actions.FloatSnap as FS
import XMonad.Actions.DeManage
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CycleWS

import XMonad.Layout.NoBorders
import XMonad.Layout.MultiColumns
import XMonad.Layout.TrackFloating
import XMonad.Layout.LimitWindows
import XMonad.Layout.Maximize

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)

import XMonad.Prompt
import XMonad.Prompt.Shell

-- floatnext
-- position store float
-- hooks.place

main = xmonad defaultConfig
    { modMask = mod4Mask
    , terminal = "gnome-terminal"
    , keys = myKeys
    , mouseBindings = myMouse
    , normalBorderColor = "#dddddd"
    , focusedBorderColor = "#0000ff"
    , workspaces = words "1 2 3 4 5 6 7 8 9 0"
    , manageHook = manageDocks <+> composeOne
        [ isFullscreen -?> doFullFloat
        , title =? "ettercap" -?> doFloat
        , isDialog -?> doCenterFloat
--        , className =? "Chromium-browser" -?> doShift "2"
        , className =? "Firefox" -?> doShift "2"
--        , className =? "Taffybar-linux-x86_64" -?> doIgnore
--        , className =? "Skype" -?> doShift "3"
--        , className =? "utox" -?> doShift "3"
        , className =? "qTox" -?> doShift "3"
        , className =? "Transmission-gtk" -?> doShift "8"
--        , className =? "Conky" -?> doIgnore
--        , className =? "Wine" -?> doFloat
        ]
    , handleEventHook = fullscreenEventHook <+> perWindowKbdLayout <+> docksEventHook
    , layoutHook = avoidStruts $ lessBorders Screen layout
    , logHook = do
        updatePointer (0.5, 0.5) (0.7, 0.7)
        dynamicLogString xmobarPP
            { ppTitle = xmobarColor "cyan" "" . shorten 100
            , ppLayout = xmobarColor "green" ""
            , ppSep = xmobarColor "gray" "" " | "
            , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
            , ppUrgent = xmobarColor "orange" "" . wrap "*" "*"
            }
            >>= xmonadPropLog
    }

layout = maximize (tall ||| Mirror tall) ||| trackFloating Full ||| maximize mulcol -- ||| Mirror mulcol ||| trackFloating Full ||| limitSelect 1 2 mulcol
    where
        tall = Tall 1 0.01 0.5
        mulcol = multiCol [1, 3] 4 0.01 0.5

runCommand = shellPrompt defaultXPConfig
    { bgColor = "black"
    , fgColor = "white"
    , bgHLight = "white"
    , fgHLight = "black"
    , borderColor = "cyan"
    , height = 20
    , font = "xft:DejaVu:pixelsize=12"
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = mm}) = M.fromList $
    [ ((mm, xK_q), kill)
    , ((mm .|. shiftMask, xK_q), kill1)
    , ((mm .|. controlMask, xK_q), kill1)

    , ((mm, xK_Return), spawn $ XMonad.terminal conf)
    , ((mm, xK_r), runCommand)
    , ((mm, xK_backslash), withFocused (sendMessage . maximizeRestore))

    , ((mm, xK_space), sendMessage NextLayout)
    , ((mm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((mm, xK_f), sendMessage ToggleStruts)

    , ((mm, xK_z), spawn "transset --actual --dec .05")
    , ((mm .|. shiftMask, xK_z), spawn "transset --actual 0")
    , ((mm, xK_x), spawn "transset --actual --inc .05")
    , ((mm .|. shiftMask, xK_x), spawn "transset --actual 1")
    , ((mm, xK_s), spawn "pavucontrol")
    , ((mm, xK_c), spawn "gpaste ui")

    , ((mm, xK_F1), spawn "xscreensaver-command -lock")
--    , ((mm .|. shiftMask, xK_F1), spawn "xautolock -toggle")
--    , ((mm, xK_F2), withFocused demanage)
    , ((mm, xK_F3), spawn "xmonad --restart")
    , ((mm, xK_F4), spawn "xmonad --recompile && xmonad --restart")
    , ((mm, xK_F5), refresh)
    , ((mm, xK_F8), spawn "sleep 0.1; xset dpms force off")
    , ((mm, xK_F9), spawn "xscreensaver-command -lock; systemctl suspend -i")
--    , ((mm, xK_F10), spawn "systemctl hibernate -i")
--    , ((mm, xK_F11), spawn "systemctl reboot -i")
--    , ((mm, xK_F12), spawn "systemctl poweroff -i")

    , ((mm, xK_Tab), windows W.focusUp)
    , ((mm .|. shiftMask, xK_Tab), windows W.focusDown)
    , ((mm, xK_k), windows W.focusUp)
    , ((mm, xK_j), windows W.focusDown)
    , ((mm, xK_m), windows W.focusMaster)
    , ((mm .|. shiftMask, xK_k), windows W.swapUp)
    , ((mm .|. shiftMask, xK_j), windows W.swapDown)
    , ((mm .|. shiftMask, xK_m), windows W.swapMaster)

--    , ((mm, xK_space), windows W.shiftMaster)
    , ((mm, xK_h), sendMessage Shrink)
    , ((mm, xK_l), sendMessage Expand)
    , ((mm, xK_comma), sendMessage $ IncMasterN 1)
    , ((mm, xK_period), sendMessage $ IncMasterN (-1))
    , ((mm, xK_i), increaseLimit)
    , ((mm, xK_o), decreaseLimit)
    , ((mm, xK_t), withFocused $ windows . W.sink)

    , ((controlMask .|. mod1Mask, xK_w), spawn "xdotool keydown Super")
    , ((mm, xK_v), spawn "xdotool keyup Super")

    , ((0, 0x1008ffa9), spawn "synclient TouchpadOff=$(synclient | grep -c 'TouchpadOff.*0')")
    , ((0, 0x1008ff11), spawn "amixer set Master 5%-")
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")
    , ((0, 0x1008ff13), spawn "amixer set Master 5%+")
    , ((0, 0x1008ff17), spawn "mpc next")
    , ((0, 0x1008ff16), spawn "mpc prev")
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff15), spawn "mpc stop")
    , ((0, 0x1008ff02), spawn "xbacklight -inc 10")
    , ((0, 0x1008ff03), spawn "xbacklight -dec 10")

    , ((mm, xK_Down), spawn "pactl set-sink-volume 0 -10%")
    , ((mm, xK_Up), spawn "pactl set-sink-volume 0 +10%")

    , ((controlMask, xK_KP_Left), spawn "xdotool mousemove_relative -- -10 0")
    , ((controlMask, xK_KP_Right), spawn "xdotool mousemove_relative 10 0")
    , ((controlMask, xK_KP_Up), spawn "xdotool mousemove_relative -- 0 -10")
    , ((controlMask, xK_KP_Begin), spawn "xdotool mousemove_relative 0 10")
    , ((controlMask, xK_KP_End), spawn "xdotool click 1")
    , ((controlMask, xK_KP_Down), spawn "xdotool click 2")
    , ((controlMask, xK_KP_Page_Down), spawn "xdotool click 3")

    , ((mm, xK_w), prevWS)
    , ((mm, xK_e), nextWS)
    ]
    ++
    [ ((m .|. mm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, controlMask)]
    ]
    ++
    [ ((m .|. mm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Page_Down, xK_Page_Up] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse (XConfig {XMonad.modMask = mm}) = M.fromList
    [ ((mm, button1), (\w -> focus w >> mouseMoveWindow w >> FS.snapMagicMove (Just 20) (Just 20) w))
    , ((mm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> FS.snapMagicResize [FS.L, FS.R, FS.U, FS.D] (Just 20) (Just 20) w))
    , ((mm, button3), (\w -> focus w >> mouseResizeEdgeWindow 0.5 w))
    , ((mm .|. shiftMask, button3), (\w -> focus w >> Sqr.mouseResizeWindow w True))
    ]
