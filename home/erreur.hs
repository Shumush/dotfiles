import Data.IORef
import Control.Monad
import System.IO.Unsafe




main = forever $ do
  modifyIORef lel (1:)

