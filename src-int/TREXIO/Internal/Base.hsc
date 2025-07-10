module TREXIO.Internal.Base where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.ConstPtr

#include <trexio.h>

type ExitCodeC = #type trexio_exit_code

-- | TREXIO Exit Codes. Can be thrown as Haskell exceptions in IO.
data ExitCode
  = Failure
  | Success
  | InvalidArg1
  | InvalidArg2
  | InvalidArg3
  | InvalidArg4
  | InvalidArg5
  | End
  | ReadOnly
  | Errno
  | InvalidID
  | AllocationFailed
  | HasNot
  | InvalidNum
  | AttrAlreadyExists
  | DSetAlreadyExists
  | OpenError
  | LockError
  | UnlockError
  | FileError
  | GroupReadError
  | GroupWriteError
  | ElemReadError
  | ElemWriteError
  | UnsafeArrayDim
  | AttrMissing
  | DSetMissing
  | BackEndMissing
  | InvalidArg6
  | InvalidArg7
  | InvalidArg8
  | InvalidStrLen
  | IntSizeOverflow
  | SafeMode
  | InvalidElectronNum
  | InvalidDeterminantNum
  | InvalidState
  | VersionParsingIssue
  | PhaseChange
  deriving (Show, Eq, Ord, Generic)

instance Enum ExitCode where
  fromEnum Failure = #const TREXIO_FAILURE
  fromEnum Success = #const TREXIO_SUCCESS
  fromEnum InvalidArg1 = #const TREXIO_INVALID_ARG_1
  fromEnum InvalidArg2 = #const TREXIO_INVALID_ARG_2
  fromEnum InvalidArg3 = #const TREXIO_INVALID_ARG_3
  fromEnum InvalidArg4 = #const TREXIO_INVALID_ARG_4
  fromEnum InvalidArg5 = #const TREXIO_INVALID_ARG_5
  fromEnum End = #const TREXIO_END
  fromEnum ReadOnly = #const TREXIO_READONLY
  fromEnum Errno = #const TREXIO_ERRNO
  fromEnum InvalidID = #const TREXIO_INVALID_ID
  fromEnum AllocationFailed = #const TREXIO_ALLOCATION_FAILED
  fromEnum HasNot = #const TREXIO_HAS_NOT
  fromEnum InvalidNum = #const TREXIO_INVALID_NUM
  fromEnum AttrAlreadyExists = #const TREXIO_ATTR_ALREADY_EXISTS
  fromEnum DSetAlreadyExists = #const TREXIO_DSET_ALREADY_EXISTS
  fromEnum OpenError = #const TREXIO_OPEN_ERROR
  fromEnum LockError = #const TREXIO_LOCK_ERROR
  fromEnum UnlockError = #const TREXIO_UNLOCK_ERROR
  fromEnum FileError = #const TREXIO_FILE_ERROR
  fromEnum GroupReadError = #const TREXIO_GROUP_READ_ERROR
  fromEnum GroupWriteError = #const TREXIO_GROUP_WRITE_ERROR
  fromEnum ElemReadError = #const TREXIO_ELEM_READ_ERROR
  fromEnum ElemWriteError = #const TREXIO_ELEM_WRITE_ERROR
  fromEnum UnsafeArrayDim = #const TREXIO_UNSAFE_ARRAY_DIM
  fromEnum AttrMissing = #const TREXIO_ATTR_MISSING
  fromEnum DSetMissing = #const TREXIO_DSET_MISSING
  fromEnum BackEndMissing = #const TREXIO_BACK_END_MISSING
  fromEnum InvalidArg6 = #const TREXIO_INVALID_ARG_6
  fromEnum InvalidArg7 = #const TREXIO_INVALID_ARG_7
  fromEnum InvalidArg8 = #const TREXIO_INVALID_ARG_8
  fromEnum InvalidStrLen = #const TREXIO_INVALID_STR_LEN
  fromEnum IntSizeOverflow = #const TREXIO_INT_SIZE_OVERFLOW
  fromEnum SafeMode = #const TREXIO_SAFE_MODE
  fromEnum InvalidElectronNum = #const TREXIO_INVALID_ELECTRON_NUM
  fromEnum InvalidDeterminantNum = #const TREXIO_INVALID_DETERMINANT_NUM
  fromEnum InvalidState = #const TREXIO_INVALID_STATE
  fromEnum VersionParsingIssue = #const TREXIO_VERSION_PARSING_ISSUE
  fromEnum PhaseChange = #const TREXIO_PHASE_CHANGE

  toEnum (#const TREXIO_FAILURE) = Failure
  toEnum (#const TREXIO_SUCCESS) = Success
  toEnum (#const TREXIO_INVALID_ARG_1) = InvalidArg1
  toEnum (#const TREXIO_INVALID_ARG_2) = InvalidArg2
  toEnum (#const TREXIO_INVALID_ARG_3) = InvalidArg3
  toEnum (#const TREXIO_INVALID_ARG_4) = InvalidArg4
  toEnum (#const TREXIO_INVALID_ARG_5) = InvalidArg5
  toEnum (#const TREXIO_END) = End
  toEnum (#const TREXIO_READONLY) = ReadOnly
  toEnum (#const TREXIO_ERRNO) = Errno
  toEnum (#const TREXIO_INVALID_ID) = InvalidID
  toEnum (#const TREXIO_ALLOCATION_FAILED) = AllocationFailed
  toEnum (#const TREXIO_HAS_NOT) = HasNot
  toEnum (#const TREXIO_INVALID_NUM) = InvalidNum
  toEnum (#const TREXIO_ATTR_ALREADY_EXISTS) = AttrAlreadyExists
  toEnum (#const TREXIO_DSET_ALREADY_EXISTS) = DSetAlreadyExists
  toEnum (#const TREXIO_OPEN_ERROR) = OpenError
  toEnum (#const TREXIO_LOCK_ERROR) = LockError
  toEnum (#const TREXIO_UNLOCK_ERROR) = UnlockError
  toEnum (#const TREXIO_FILE_ERROR) = FileError
  toEnum (#const TREXIO_GROUP_READ_ERROR) = GroupReadError
  toEnum (#const TREXIO_GROUP_WRITE_ERROR) = GroupWriteError
  toEnum (#const TREXIO_ELEM_READ_ERROR) = ElemReadError
  toEnum (#const TREXIO_ELEM_WRITE_ERROR) = ElemWriteError
  toEnum (#const TREXIO_UNSAFE_ARRAY_DIM) = UnsafeArrayDim
  toEnum (#const TREXIO_ATTR_MISSING) = AttrMissing
  toEnum (#const TREXIO_DSET_MISSING) = DSetMissing
  toEnum (#const TREXIO_BACK_END_MISSING) = BackEndMissing
  toEnum (#const TREXIO_INVALID_ARG_6) = InvalidArg6
  toEnum (#const TREXIO_INVALID_ARG_7) = InvalidArg7
  toEnum (#const TREXIO_INVALID_ARG_8) = InvalidArg8
  toEnum (#const TREXIO_INVALID_STR_LEN) = InvalidStrLen
  toEnum (#const TREXIO_INT_SIZE_OVERFLOW) = IntSizeOverflow
  toEnum (#const TREXIO_SAFE_MODE) = SafeMode
  toEnum (#const TREXIO_INVALID_ELECTRON_NUM) = InvalidElectronNum
  toEnum (#const TREXIO_INVALID_DETERMINANT_NUM) = InvalidDeterminantNum
  toEnum (#const TREXIO_INVALID_STATE) = InvalidState
  toEnum (#const TREXIO_VERSION_PARSING_ISSUE) = VersionParsingIssue
  toEnum (#const TREXIO_PHASE_CHANGE) = PhaseChange
  toEnum _ = error "toEnum(ExitCode): invalid argument"

instance Exception ExitCode where
  displayException = stringOfError 

exitCodeH :: ExitCodeC -> ExitCode
exitCodeH = toEnum . fromIntegral

-- | Helper to make a C function returning an exit code to an easier to handle
-- exception
checkEC :: (MonadIO m, MonadThrow m) => IO ExitCodeC -> m ()
checkEC f = do
  ec <- exitCodeH <$> liftIO f
  if ec == Success
    then return ()
    else throwIO ec

foreign import capi "trexio.h trexio_string_of_error" stringOfError_ :: (#type trexio_exit_code) -> IO (ConstPtr CChar)
stringOfError :: ExitCode -> String
stringOfError exitCode = unsafePerformIO $ do
  stringOfError_ exitCodeInt >>= peekCString . unConstPtr
  where
    exitCodeInt = fromIntegral . fromEnum $ exitCode

--------------------------------------------------------------------------------

-- | TREXIO version: Major, Minor, Patch
version :: (Int, Int, Int)
version =
  ( #const TREXIO_VERSION_MAJOR
  , #const TREXIO_VERSION_MINOR
  , #const TREXIO_VERSION_PATCH
  )

--------------------------------------------------------------------------------

data Backend
  = Hdf5
  | Text
  | Invalid
  | Auto
  deriving (Eq, Ord, Show)

instance Enum Backend where
  fromEnum Hdf5 = #const TREXIO_HDF5
  fromEnum Text = #const TREXIO_TEXT
  fromEnum Invalid = #const TREXIO_INVALID_BACK_END
  fromEnum Auto = #const TREXIO_AUTO

  toEnum (#const TREXIO_HDF5) = Hdf5
  toEnum (#const TREXIO_TEXT) = Text
  toEnum (#const TREXIO_INVALID_BACK_END) = Invalid
  toEnum (#const TREXIO_AUTO) = Auto
  toEnum _ = error "toEnum(Backend): invalid argument"

backendC :: Backend -> (#type back_end_t)
backendC = fromIntegral . fromEnum 

foreign import capi "trexio.h trexio_has_backend" hasBackend_ :: (#type back_end_t) -> CBool
hasBackend :: Backend -> Bool
hasBackend backend = toBool . hasBackend_ . fromIntegral . fromEnum $ backend

--------------------------------------------------------------------------------

-- | TREXIO file handle
newtype Trexio = Trexio (Ptr Trexio)

-- | File access mode for 'Trexio'.
data FileMode
  = FileRead
  | FileWrite
  | FileUnsafe -- ^ Allows to delete and override data
  deriving (Eq, Show, Ord, Generic)

modeC :: FileMode -> CChar
modeC FileRead = castCharToCChar 'r'
modeC FileWrite = castCharToCChar 'w'
modeC FileUnsafe = castCharToCChar 'u'

foreign import capi "trexio.h trexio_open" open_ ::
  CString ->
  CChar ->
  (#type back_end_t) ->
  Ptr (#type trexio_exit_code) ->
  IO Trexio
open :: (MonadIO m, MonadThrow m) => FilePath -> FileMode -> Backend -> m Trexio
open filepath mode backend = liftIO . withCString filepath $ \filepathC ->
  alloca $ \ecPtr -> do
    trexio <- open_ filepathC (modeC mode) (backendC backend) ecPtr
    ec <- exitCodeH <$> peek ecPtr
    if ec == Success
      then return trexio
      else throwM ec

foreign import capi "trexio.h trexio_close" close_ :: Trexio -> IO ExitCodeC
close :: (MonadIO m, MonadThrow m) => Trexio -> m ()
close trexio = checkEC $ close_ trexio

foreign import capi "trexio.h trexio_mark_safety" markSafety_ :: Trexio -> Int32 -> IO ExitCodeC

-- | After a file has been opened unsafely, it can be marked as safe by this function 
-- forcefully. You are responsible for internal incosistencies that may arise from this.
markSafety :: (MonadIO m, MonadThrow m) => Trexio -> m ()
markSafety trexio = checkEC $ markSafety_ trexio 0

--------------------------------------------------------------------------------

-- | Bit field type for storing determinant information.
newtype BitFieldT = BitFieldT Int64
  deriving (Eq, Ord, Show)
  deriving Storable via (Int64)