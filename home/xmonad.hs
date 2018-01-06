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
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama

import System.FilePath.Posix
import System.IO

import Text.Shaun
import Text.Shaun.Types

import Data.Maybe
import qualified Data.Map as M

import Control.Exception
import Control.Monad.Catch (throwM, MonadThrow(..))

home :: String
home = "/home/shumush/"

wallpaper :: String
wallpaper = "ergo_proxy.jpg"

main = do
  spawn $ "feh --bg-scale '"++(home </> wallpaper)++"'"
  spawn "xcompmgr -c &"

  sw <- liftIO (screenWidth 0)

  let status_w = show $ sw / 3
  let rest_w   = show $ sw - (sw / 3)

  dzenTop <- spawnPipe $
    "dzen2 -dock -ta l -bg black -fn inconsolata-11 -x 0 -y 0 -h 20 -w "
    ++ status_w

  spawn $
    "conky | dzen2 -dock -ta r -bg black -fn inconsolata-11 -x "
    ++ status_w
    ++ " -y 0 -h 20 -w "
    ++ rest_w

  xmonad $ docks $ withUrgencyHook dzenUrgencyHook $ azertyConfig
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
      ([ ((0, xF86XK_AudioLowerVolume), spawn "amixer -q -D pulse sset Master 2%- && killall -SIGUSR1 conky")
      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q -D pulse sset Master 2%+ && killall -SIGUSR1 conky")
      , ((0, xF86XK_AudioMute), spawn "amixer -q -D pulse sset Master toggle && killall -SIGUSR1 conky")
      , ((0, xF86XK_MonBrightnessUp), spawn "sudo xbacklight -ctrl radeon_bl0 +10")
      , ((0, xF86XK_MonBrightnessDown), spawn "sudo xbacklight -ctrl radeon_bl0 -10")
      , ((mod4Mask .|. shiftMask, xK_g), goToSelected def)
      , ((mod4Mask .|. shiftMask, xK_m), runAppGrid)
      , ((mod4Mask, xK_b), sendMessage ToggleStruts)
      , ((mod4Mask, xK_p), shellPrompt def)
      , ((0, xK_Print), spawn "/home/shumush/sbin/screenshot screen screenshot")
      --, ((mod4Mask, xK_o), xmonadPromptC [("prout", return ())] def)
      ] ++ [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]])

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
  menu <- fmap read 
               (liftIO (catch (readFile $ home </> ".xmonad/app_menu.sn")
                              (defaultIO "")))

  case menu of
    SObject (("apps", (SList l)):_) -> menuGrid (mapMaybe readCommand l) []
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

menuGrid :: [(String, MenuItem)] -> [[(String, MenuItem)]] -> X ()
menuGrid coms pred = do
  sel <- gridselect mgConf coms

  case sel of
    Just (Command c) -> spawn c
    Just (SubMenu sub) -> refresh >> menuGrid sub (coms:pred)
    Nothing -> case pred of
                 (p:l) -> refresh >> menuGrid p l
                 [] -> return ()

data MenuItem = Command String | SubMenu [(String, MenuItem)]

instance Shaun MenuItem where
  toShaun (Command c) = toShaun c
  toShaun (SubMenu l) = SList []

  fromShaun (SString com) = return $ Command com
  fromShaun (SList l) = fmap SubMenu $ mapM readCommand l
  fromShaun _ = throwM $ Custom "fromShaun : cannot create MenuItem"

readCommand :: (MonadThrow m) => ShaunValue -> m (String, MenuItem)
readCommand (SList (name:item:_)) = do
  name' <- fromShaun name
  item' <- fromShaun item
  return $ (name', item')
readCommand _ = throwM $ Custom "readCommand"
