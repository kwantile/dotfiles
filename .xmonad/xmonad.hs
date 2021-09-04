{-# LANGUAGE PatternSynonyms #-}
  -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (Direction1D(Next, Prev), moveTo, shiftTo, WSType(WSIs), nextScreen, prevScreen)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (killAll)

    -- Data
import Data.Maybe (fromJust)
import Data.Monoid (Endo)
import Data.Maybe (isJust)
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops (ewmh)  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.SetWMName

    -- Layouts
import XMonad.Layout.ResizableTile (pattern ResizableTall, MirrorResize(MirrorShrink, MirrorExpand))
import XMonad.Layout.ThreeColumns (pattern ThreeColMid)

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, Toggle(..), single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Layout.Renamed (renamed, pattern Replace)
import XMonad.Layout.ShowWName (showWName', SWNConfig, swn_font, swn_fade, swn_bgcolor, swn_color)
import XMonad.Layout.Simplest (pattern Simplest)
import XMonad.Layout.Spacing (pattern Border, Spacing(..), spacingRaw, decWindowSpacing, incWindowSpacing, decScreenSpacing, incScreenSpacing)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg)
import XMonad.Layout.WindowNavigation (windowNavigation)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

   -- Utilities
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import Control.Monad (ap, liftM2)
import XMonad.Config.Desktop

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows/cmd key

myTerminal :: String
myTerminal = "termite"

myBrowser :: String
myBrowser = "firefox"

myBorderWidth :: Dimension
myBorderWidth = 2           -- Border width for windows

myNormColor :: String
myNormColor = "#4C566A" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#A3BE8C" -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom &"
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    spawnOnce "~/.fehbg &"  -- set last saved feh wallpaper
    setWMName "LG3D" -- This is apparently required for Java GUI apps. God help us all. Leaving here for future me just in case

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Layouts I like to use
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
threeCol = renamed [Replace "three col mid"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 4
           $ limitWindows 7
           $ ThreeColMid 1 (3/100) (1/2)

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Fira:bold:size=30"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#4c566a"
    , swn_color             = "#eceff4"
    }

-- The layout hook
myLayoutHook = avoidStruts $ windowArrange $ T.toggleLayouts tall
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =  withBorder myBorderWidth tall
                              ||| withBorder myBorderWidth threeCol


myWorkspaces = [ " dev ", " web ", " doc ", " chat " ]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , className =? "atom"            --> doShift ( myWorkspaces !! 0 )
     , resource =? "firefox"          --> doShift ( myWorkspaces !! 1 )
     , resource =? "telegram-desktop" --> doShift ( myWorkspaces !! 3 )
     , resource =? "skypeforlinux"    --> doShift ( myWorkspaces !! 3 )
     , (className =? "firefox" <&&> title =? "Picture-in-Picture") --> doFloat
     , isFullscreen                   -->  doFullFloat
     , isDialog                       --> doFloat
     , resource =? "desktop_window"   --> doIgnore
     ]

-- Key Binding Helpers
type KeyBinding = ((KeyMask, KeySym), X ())

keyBinding :: KeyMask -> Maybe KeyMask -> KeySym -> X () -> KeyBinding
keyBinding modifier Nothing keySym command = ((modifier, keySym), command)
keyBinding modifier (Just other) keySym command = ((modifier .|. other, keySym), command)

modNothing :: KeySym -> X () -> KeyBinding
modNothing = keyBinding myModMask Nothing

modJust :: KeyMask -> KeySym -> X () -> KeyBinding
modJust = keyBinding myModMask . Just

altNothing :: KeySym -> X () -> KeyBinding
altNothing = keyBinding altMask Nothing

altJust :: KeyMask -> KeySym -> X () -> KeyBinding
altJust = keyBinding altMask . Just

controlNothing :: KeySym -> X () -> KeyBinding
controlNothing = keyBinding controlMask Nothing

controlJust :: KeyMask -> KeySym -> X () -> KeyBinding
controlJust = keyBinding controlMask . Just

myWorkspaceKeySyms :: [(WorkspaceId, KeySym)]
myWorkspaceKeySyms = zip myWorkspaces [xK_1 ..]

-- When applied with a KeySym and WorkspaceId this shifts the
-- currently focused window to the provided workspace.
shiftWindowKeyBinding :: KeySym -> WorkspaceId -> KeyBinding
shiftWindowKeyBinding keySym id = modJust controlMask keySym $ windows $ W.shift id

-- Since myWorkspaceKeySyms has to be in the order of (WorkspaceId, KeySym)
-- and not (KeySym, WorkspaceId), we flip the args of shiftWindowKeyBinding,
-- then we uncurry it so we can just apply the tuple directly. Finally we apply
-- that to map so it can operate on [(WorkspaceId, KeySym)].
shiftWindowKeyBindings :: [(WorkspaceId, KeySym)] -> [KeyBinding]
shiftWindowKeyBindings = map $ uncurry $ flip shiftWindowKeyBinding

shiftWindowKeyBindings' :: [KeyBinding]
shiftWindowKeyBindings' =
    shiftWindowKeyBindings myWorkspaceKeySyms

myKeys = [
            controlJust   shiftMask    xK_a        $ windows copyToAll, -- copy window to all workspaces
            controlJust   shiftMask    xK_z        $ killAllOtherCopies, -- kill copies of window on other workspaces
            -- Language switcher
            modNothing                 xK_space    $ spawn "$HOME/.xmonad/scripts/lang_switch.sh",
            -- Xmonad recompile with restart
            modJust       shiftMask    xK_r        $ spawn "xmonad --recompile && xmonad --restart",
            -- Task manager
            controlJust   shiftMask    xK_Escape   $ spawn $ myTerminal ++ " 'bpytop manager' -e bpytop",
            -- Kill commands
            modNothing                 xK_q          kill,
            modNothing                 xK_Escape   $ spawn "xkill",
            -- DMenu
            modJust       shiftMask    xK_d        $ spawn "dmenu_run -fn 'NotoMonoRegular:bold:pixelsize=14'",
            -- File manager
            modJust       shiftMask    xK_Return   $ spawn "pcmanfm",
            -- Sound control
            modNothing                 xK_v        $ spawn "pavucontrol",
            modJust       altMask      xK_m        $ spawn "xfce4-settings-manager",
            -- Logout
            modNothing                 xK_x        $ spawn "arcolinux-logout",
            -- My terminal
            modNothing                 xK_Return   $ spawn myTerminal,
            -- SCREENSHOTS
            ((0,                       xK_Print),    spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES) && kdialog --passivepopup \"Снимок готов.\" 2'"),
            controlNothing             xK_Print    $ spawn "xfce4-screenshooter",
            -- My Browser
            modNothing                 xK_b        $ spawn myBrowser,
            -- Workspaces
            -- modNothing xK_period nextScreen, -- Switch focus to next monitor
            -- modNothing xK_comma prevScreen, -- Switch focus to previous monitor
            controlJust   altMask     xK_p         $ decWindowSpacing 4, -- Decrease window gaps by 4
            controlJust   altMask     xK_k         $ incWindowSpacing 4, -- Increase window gaps by 4
            modNothing                xK_m         $ windows W.focusMaster, -- Shift focus to master
            altNothing                xK_Tab       $ windows W.focusDown, -- Toggle focus down (also clockwise in tall)
            altJust       shiftMask   xK_Tab       $ windows W.focusUp, -- Toggle focus up (also counter-clockwise in tall)
            modJust       shiftMask   xK_m         $ windows W.swapMaster,
            modJust       shiftMask   xK_Left      $ windows W.swapDown,
            modJust       shiftMask   xK_Right     $ windows W.swapUp,
            modNothing                xK_BackSpace   promote, -- Makes focused window master
            controlJust   myModMask   xK_Tab       $ sendMessage NextLayout, -- Toggles next layout
            controlJust   shiftMask   xK_j         $ sendMessage Shrink,
            controlJust   shiftMask   xK_k         $ sendMessage Expand,
            modNothing                xK_f         $ sendMessage $ Toggle NBFULL,
            ((controlMask .|. altMask .|. shiftMask, xK_j), sendMessage MirrorShrink),
            ((controlMask .|. altMask .|. shiftMask, xK_k), sendMessage MirrorExpand)
	      --  Reset the layouts on the current workspace to default.
  	    --  modJust shiftMask xK_space $ setLayout $ XMonad.layoutHook defaultConfig
          ]
          ++
          --MULTIMEDIA KEYS
          [
            -- Mute volume
            ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle"),
            -- Decrease volume
            ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-"),
            -- Increase volume
            ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+"),
            -- Increase brightness
            ((0, xF86XK_MonBrightnessUp),  spawn $ "light -A 5"),
            -- Decrease brightness
            ((0, xF86XK_MonBrightnessDown), spawn $ "light -U 5"),
            -- Touchpad off/on ~/.config/scripts/touchpad-toggle.sh
            ((controlMask .|. myModMask .|. altMask, xK_t), spawn "$HOME/.xmonad/scripts/touchpad-toggle.sh")
          ]
          ++ shiftWindowKeyBindings'

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc0 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc0"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def {
    	  manageHook           = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        -- , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ xmobarPP
              { ppOutput = \x -> hPutStrLn xmproc0 x
              , ppCurrent = xmobarColor "#A3BE8C" ""                    -- Current workspace
              , ppVisible = xmobarColor "#A3BE8C" "" . clickable        -- Visible but not current workspace
              , ppHidden = xmobarColor "#5E81AC" "" .clickable          -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#D08770" "" .clickable -- Hidden workspaces (no windows)
              , ppTitle = xmobarColor "#D8DEE9" "" . shorten 60         -- Title of active window
              , ppSep =  "<fc=#4C566A> <fn=1>|</fn> </fc>"              -- Separator character
              , ppUrgent = xmobarColor "#BF616A" "" . wrap "!" "!"      -- Urgent workspace
              , ppOrder  = \(ws:l:t:_) -> [ws, l, t]                    -- order of things in xmobar
              }
        } `additionalKeys` myKeys
