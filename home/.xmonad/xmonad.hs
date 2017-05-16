import XMonad
import XMonad.Operations
import XMonad.Actions.GridSelect
import XMonad.Layout.Spacing
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Config.Azerty
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWS

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama

import System.IO

import Text.Shaun
import Text.Shaun.Sweeper

import Data.Maybe
import qualified Data.Map as M

import Control.Exception

home :: String -> String
home = ("/home/neliell/" ++)

main = do
  spawn "feh --bg-scale '/home/neliell/wall.png'"
  spawn "xcompmgr -c &"

  sw <- liftIO (screenWidth 0)

  let status_w = show $ sw / 3
  let rest_w   = show $ sw - (sw / 3)

  dzenTop <- spawnPipe $
    "dzen2 -ta l -bg black -fn inconsolata-11 -x 0 -y 0 -h 20 -w "
    ++ status_w

  spawn $
    "conky | dzen2 -ta r -bg black -fn inconsolata-11 -x "
    ++ status_w
    ++ " -y 0 -h 20 -w "
    ++ rest_w

  xmonad $ azertyConfig
    {
      terminal = "urxvt +sb",
      focusFollowsMouse = False,
      borderWidth = 2,
      normalBorderColor = "#000000",
      focusedBorderColor = "#ffffff",
      workspaces = map show [1..9],
      handleEventHook = handleEventHook azertyConfig
                    <+> fullscreenEventHook,
      manageHook = manageDocks
               <+> composeAll [ isDialog --> doCenterFloat
                              , isFullscreen --> doFullFloat],
      layoutHook = avoidStruts $
                   smartBorders $
                   reflectHoriz $
                   spacing 4 $
                   layoutHook azertyConfig,
      logHook = dynamicLogWithPP $
                def { ppOutput = hPutStrLn dzenTop },
      modMask = mod4Mask
    } `additionalKeys`
      [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -q -D pulse sset Master 2%-")
      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q -D pulse sset Master 2%+")
      , ((0, xF86XK_AudioMute), spawn "amixer -q -D pulse sset Master toggle")
      , ((0, xF86XK_MonBrightnessUp), spawn "sudo /home/neliell/bin/brightness inc")
      , ((0, xF86XK_MonBrightnessDown), spawn "sudo /home/neliell/bin/brightness dec")
      , ((mod4Mask, xK_g), spawnSelected def ["firefox", "krita", "vlc"])
      , ((mod4Mask .|. shiftMask, xK_g), goToSelected def)
      , ((mod4Mask .|. shiftMask, xK_m), runAppGrid)
      , ((mod4Mask, xK_b), sendMessage ToggleStruts)
      ]

screenWidth :: Int -> IO Double
screenWidth s = do
    dsp <- openDisplay ""
    mss <- xineramaQueryScreens dsp
    return $ case mss of
        Nothing -> 0
        Just [] -> 0
        Just ss -> if s >= 0 && s < length ss -- prevent bad index
            then fromIntegral . xsi_width $ ss !! s else 0
{-
dzenWidth :: String -> String ->  Int
dzenWidth font text = fmap read $ readProcess "dzen2" [font, text] ""
-}

defaultIO :: a -> IOException -> IO a
defaultIO a e = return a

runAppGrid :: X ()
runAppGrid = do
  menu <- fmap  read 
               (liftIO (catch (readFile "/home/neliell/.xmonad/app_menu.sn")
                              (defaultIO "")))

  case menu of
    SObject (("apps", l):_) -> menuGrid (menuSweeper l) []
    _ -> return ()

mgNav = makeXEventhandler $
          shadowWithKeymap navKeyMap
                           navDefaultHandler
  where
    navKeyMap = M.fromList
      [ ((0, xK_BackSpace), cancel)
      , ((0, xK_Return), select)
      , ((0, xK_Escape), return (Just (Command "")))
      , ((0, xK_Up   ), move ( 0, -1) >> mgNav)
      , ((0, xK_Down ), move ( 0,  1) >> mgNav)
      , ((0, xK_Left ), move (-1,  0) >> mgNav)
      , ((0, xK_Right), move ( 1,  0) >> mgNav) ]

    navDefaultHandler = const mgNav
      
mgConf = def
  { gs_colorizer = \com sel -> return $ case com of
                      Command _ -> if sel
                                   then ("#006868", "#55aaaa")
                                   else ("#55aaaa", "#006868")
                      SubMenu _ -> if sel
                                   then ("#ad7300", "#ffd580")
                                   else ("#ffd580", "#ad7300")
  , gs_font = "inconsolata-12"
  , gs_navigate = mgNav
  }

menuGrid coms pred = do
  sel <- gridselect mgConf coms

  case sel of
    Just (Command c) -> spawn c
    Just (SubMenu sub) -> refresh >> menuGrid sub (coms:pred)
    Nothing -> case pred of
                 (p:l) -> refresh >> menuGrid p l
                 [] -> return ()

data MenuItem = Command String | SubMenu [(String, MenuItem)]

menuSweeper :: ShaunValue -> [(String, MenuItem)]
menuSweeper apps = case apps of
    SList l -> catMaybes $ map getCommand l
    _       -> []

  where
    getCommand (SList ((SString name):(SString command):_))
      = Just (name, Command command)
    getCommand (SList ((SString name):(SList coms):_))
      = Just (name, SubMenu (menuSweeper (SList coms)))
    getCommand _ = Nothing
