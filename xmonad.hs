import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PerWindowKbdLayout

import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FlexibleResize
import qualified XMonad.Actions.FloatSnap as FS
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS

import XMonad.Layout.NoBorders
import XMonad.Layout.MultiColumns
import XMonad.Layout.TrackFloating

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace

-- floatnext
-- position store float
-- hooks.place

main = xmonad $ ewmh defaultConfig
    { modMask = mod4Mask
    , terminal = "roxterm"
    , keys = keyBindings
    , mouseBindings = mouseKeyBindings
    , normalBorderColor = "#dddddd"
    , focusedBorderColor = "#0000ff"
    , workspaces = words "1 2 3 4 5 6 7 8 9 0"
    , manageHook = composer <+> manageDocks
    , handleEventHook = perWindowKbdLayout <+> fullscreenEventHook <+> docksEventHook
    , layoutHook = layout
    , logHook = myLogHook
    }

layout = avoidStruts $ lessBorders Screen $ tall ||| Mirror tall ||| trackFloating Full ||| mulcol
    where
        tall = Tall 1 0.01 0.5
        mulcol = multiCol [1, 3] 4 0.01 0.5

myLogHook = do
    updatePointer (0.5, 0.5) (0.7, 0.7)
    dynamicLogString xmobarPP
        { ppTitle = xmobarColor "cyan" "" . shorten 100
        , ppLayout = xmobarColor "green" ""
        , ppSep = xmobarColor "gray" "" " | "
        , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
        , ppUrgent = xmobarColor "orange" "" . wrap "*" "*"
        }
        >>= xmonadPropLog

composer = composeOne
    [ isFullscreen -?> doFullFloat
    , title =? "ettercap" -?> doFloat
    , isDialog -?> doCenterFloat
--    , className =? "Chromium-browser" -?> doShift "2"
    , className =? "Firefox" -?> doShift "2"
--    , className =? "Taffybar-linux-x86_64" -?> doIgnore
--    , className =? "Skype" -?> doShift "3"
    , className =? "utox" -?> doShift "3"
    , className =? "Pidgin" -?> doShift "3"
--    , className =? "qTox" -?> doShift "3"
    , className =? "Transmission-gtk" -?> doShift "8"
--    , className =? "Conky" -?> doIgnore
--    , className =? "Wine" -?> doFloat
    ]


promptConfig = defaultXPConfig
    { bgColor = "black"
    , fgColor = "white"
    , bgHLight = "white"
    , fgHLight = "black"
    , borderColor = "cyan"
    , height = 24
    , font = "xft:Monospace:pixelsize=14:antialias=true"
    , position = Top
    }


keyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_q), kill)
    , ((modm .|. shiftMask, xK_q), kill1)
    , ((modm .|. controlMask, xK_q), kill1)

    , ((modm, xK_bracketleft), addWorkspacePrompt promptConfig)
    , ((modm .|. shiftMask, xK_bracketleft), renameWorkspace promptConfig)
    , ((modm, xK_bracketright), withWorkspace promptConfig (windows . W.shift))
    , ((modm, xK_BackSpace), removeEmptyWorkspace)

    , ((modm, xK_grave), toggleWS)

    , ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_p), shellPrompt promptConfig)

    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_f), sendMessage ToggleStruts)

    , ((modm, xK_s), spawn "pavucontrol")
    , ((modm, xK_c), spawn "gpaste-client ui")

    , ((modm, xK_F1), spawn "xscreensaver-command -lock")
--    , ((modm .|. shiftMask, xK_F1), spawn "xautolock -toggle")
--    , ((modm, xK_F2), withFocused demanage)
    , ((modm, xK_F2), spawn "killall -q xmobar; exec xmobar ~/.xmonad/xmobar")
    , ((modm, xK_F3), spawn "xmonad --restart")
    , ((modm, xK_F4), spawn "xmonad --recompile && xmonad --restart")
    , ((modm, xK_F5), refresh)
    , ((modm, xK_F8), spawn "sleep 0.1; xset dpms force off")
    , ((modm, xK_F9), spawn "xscreensaver-command -lock; systemctl suspend -i")
--    , ((modm, xK_F10), spawn "systemctl hibernate -i")
--    , ((modm, xK_F11), spawn "systemctl reboot -i")
--    , ((modm, xK_F12), spawn "systemctl poweroff -i")

    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab), windows W.focusUp)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_m), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster)

--    , ((modm, xK_space), windows W.shiftMaster)
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    , ((modm, xK_comma), sendMessage $ IncMasterN 1)
    , ((modm, xK_period), sendMessage $ IncMasterN (-1))
    , ((modm, xK_t), withFocused $ windows . W.sink)

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

    , ((modm, xK_Down), spawn "pactl set-sink-volume 0 -10%")
    , ((modm, xK_Up), spawn "pactl set-sink-volume 0 +10%")

    , ((modm, xK_Page_Down), spawn "transset --actual --dec .05")
    , ((modm .|. shiftMask, xK_Page_Down), spawn "transset --actual 0")
    , ((modm, xK_Page_Up), spawn "transset --actual --inc .05")
    , ((modm .|. shiftMask, xK_Page_Up), spawn "transset --actual 1")

    , ((controlMask, xK_KP_Left), spawn "xdotool mousemove_relative -- -10 0")
    , ((controlMask, xK_KP_Right), spawn "xdotool mousemove_relative 10 0")
    , ((controlMask, xK_KP_Up), spawn "xdotool mousemove_relative -- 0 -10")
    , ((controlMask, xK_KP_Begin), spawn "xdotool mousemove_relative 0 10")
    , ((controlMask, xK_KP_End), spawn "xdotool click 1")
    , ((controlMask, xK_KP_Down), spawn "xdotool click 2")
    , ((controlMask, xK_KP_Page_Down), spawn "xdotool click 3")
    ]
    ++
    [ ((modm .|. m, key), windows $ f i)
        | (i, key) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
        , (m, f) <- workspaceModifiers
    ]
    ++
    [ ((modm .|. m, xK_o), workspacePrompt promptConfig { autoComplete = Just 10 } (windows . f))
        | (m, f) <- workspaceModifiers
    ]
    ++
    [ ((modm .|. m, key), screenWorkspace screen >>= flip whenJust (windows . f))
        | (key, screen) <- zip [xK_w, xK_e, xK_r] [0..]
        , (m, f) <- screenModifiers
    ]
    where
        workspaceModifiers = [ (0, W.greedyView)
                             , (shiftMask, W.shift)
                             , (controlMask, copy)
                             ]

        screenModifiers = [ (0, W.view)
                          , (shiftMask, W.shift)
                          ]

mouseKeyBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseKeyBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), floatMove)
    , ((modm .|. shiftMask, button1), floatSnapResize)
    , ((modm, button3), floatResize)
    , ((modm .|. shiftMask, button3), floatResizeKeepRatio)
    ]
    where
        floatMove w = do
            focus w
            mouseMoveWindow w
            FS.snapMagicMove (Just 20) (Just 20) w

        floatSnapResize w = do
            focus w
            mouseMoveWindow w
            FS.snapMagicResize [FS.L, FS.R, FS.U, FS.D] (Just 20) (Just 20) w

        floatResize w = do
            focus w
            mouseResizeEdgeWindow 0.5 w

        floatResizeKeepRatio w = do
            focus w
            Sqr.mouseResizeWindow w True
