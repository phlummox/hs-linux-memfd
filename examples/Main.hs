
module Main where

import System.Linux.MemFd
import Foreign.C
import Control.Monad (void)

foreign import ccall unsafe "system"
  c_system :: CString -> IO CInt

system :: String -> IO CInt
system cmd =
  withCString cmd $ \cStr ->
    c_system cStr

main = do
  fd <- memFdCreate "myfile" []
  let fname = "/proc/self/fd/" ++ show fd
  writeFile fname "How now brown cow"
  putStrLn "file conts:"
  void $ system $ "cat " ++ fname
  putStrLn ""



