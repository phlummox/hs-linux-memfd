{- |

Module      :  System.Linux.MemFd
License     :  PublicDomain
Maintainer  :  phlummox2@gmail.com
Portability :  non-portable (requires Linux)

Create anonymous, memory-backed files with the Linux
@memfd_create@ syscall.

-}

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

module System.Linux.MemFd 
  (
  -- * memFdCreate
    memFdCreate
  , MemFdCreateFlag(..)
  -- * Low-level access
  --
  -- | Access to the C-level functions and constants.
  , c_memfd_create
  , c_MFD_CLOEXEC
  , c_MFD_ALLOW_SEALING
  )
where

import Foreign.C 
import System.Posix.Types
import Data.Bits        ( (.|.), Bits(..) )
import Data.List        ( foldl' )

-- | Wrapper around
-- @int memfd_create(const char *name, unsigned int flags)@
foreign import ccall unsafe "memfd_create"
  c_memfd_create :: CString -> CUInt -> IO Fd


-- | Correspond to the unsigned int flags from @memfd.h@.
data MemFdCreateFlag =
    CloseOnExec   -- ^ MFD_CLOEXEC: close file descriptor if any @exec@ family functions are successfully called
  | AllowSealing  -- ^ MFD_ALLOW_SEALING: allow file descriptor to be sealed using @fcntl@
  deriving Eq

-- CAPI calling convention lets us import even macros

-- | MFD_CLOEXEC 
foreign import capi "memfd.h value MFD_CLOEXEC" c_MFD_CLOEXEC :: CUInt

-- | MFD_ALLOW_SEALING 
foreign import capi "memfd.h value MFD_ALLOW_SEALING" c_MFD_ALLOW_SEALING :: CUInt

memFdCreateFlagToInt :: MemFdCreateFlag -> CUInt
memFdCreateFlagToInt op = case op of
  CloseOnExec       -> c_MFD_CLOEXEC
  AllowSealing      -> c_MFD_ALLOW_SEALING 

packMemFdCreateFlags :: [MemFdCreateFlag] -> CUInt 
packMemFdCreateFlags flags =
  foldl' (\acc flag -> memFdCreateFlagToInt flag .|. acc) 0 flags

-- | @memFdCreate name flags@ creates
-- an anonymous in-memory file and return a
-- file descriptor referring to it.
--
-- @name@ is used
-- as a filename for debugging purposes, and will be displayed
-- as the target of the corresponding symbolic link in the directory
-- @\/proc\/self\/fd\/@. The displayed name is always prefixed with 
-- the string "@memfd:@".
-- Names do not affect the behavior
-- of the file descriptor, and multiple files can therefore
-- have the same name without any side effects.
--
-- The file behaves like a regular file, and so can be
-- modified, truncated, memory-mapped, and so on. However, unlike a 
-- regular file, it lives in RAM and has a volatile backing storage. Once
-- all
-- OS references to the file are dropped, it is automatically released.
--
-- A list of flags may be passed in @flags@.
--
-- If the 'CloseOnExec' flag is passed, then the descriptor
-- will be automatically and atomically closed
-- when any of the @exec@ family functions succeed.
--
-- If the 'AllowSealing' flag is passed, then the file can be
-- /sealed/ using the @fcntl@ functions
-- (see https://hackage.haskell.org/package/unix-fcntl
-- for bindings to fcntl.)
--
-- As a convenience, 'memFdSeal' is provided, which
-- is a simplified interface to the @fcntl@ function.
--
-- A path to the file is available via the @/proc@ fileystem,
-- at @\/proc\/self\/fd\//myfd/@ (where "/myfd/" is the value of the file
-- descriptor -- this file can be opened etc. like any other
-- file using typical Haskell IO functions.
--
-- Furthermore, as long as the 'CloseOnExec' flag
-- is not passed, the file descriptor (and associated
-- "@/proc@" path) will remain available to @fork@ed and
-- @exec@ed child processes -- see the \"Examples\" directory
-- for sample usage.
--
-- Can also be used for "zero-trust" IPC -- see
-- https://github.com/a-darwish/memfd-examples
--
-- Example:
--
-- >>> import System.Posix.IO (fdWrite)
-- >>> fd <- memFdCreate "myfile" []
-- >>> _ <- fdWrite fd "The quality of mercy is not strained"
-- >>> let fname = "/proc/self/fd/" ++ show fd
-- >>> readFile fname >>= print
-- "The quality of mercy is not strained"
memFdCreate :: String -> [MemFdCreateFlag] -> IO Fd
memFdCreate name flags = 
  throwErrnoIfMinus1 "memFdCreate" $
    withCString name $ \cStr ->
      c_memfd_create cStr $ packMemFdCreateFlags flags

-- vim: syntax=haskell :
