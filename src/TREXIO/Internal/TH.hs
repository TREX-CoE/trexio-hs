{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TREXIO.Internal.TH where

import Control.Exception.Safe
import Control.Monad
import Data.Aeson hiding (Success, withArray)
import Data.Bit.ThreadSafe (Bit)
import Data.Bit.ThreadSafe qualified as BV
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.ByteString.Unsafe qualified as BS
import Data.Char
import Data.Coerce
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Massiv.Array as Massiv hiding (Dim, dropWhile, forM, forM_, mapM, product, replicate, takeWhile, throwM, toList, zip)
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.Manifest.Vector qualified as Massiv
import Data.Massiv.Array.Unsafe (unsafeWithPtr)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Foreign hiding (peekArray, void, withArray)
import Foreign.C.ConstPtr
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO
import System.IO.Temp
import System.Process.Typed
import TREXIO.CooArray
import TREXIO.Internal.Base
import TREXIO.Internal.Marshaller
import Text.Casing
import Text.Read (readMaybe)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------

{- | Attempts to obtain the JSON specification from the trexio.h header. This is
a little bit arcane process:

1. Write a temporary file @trexio.c@ that merely includes the header @#include <trexio.h>@
2. Run the C preprocessor on it using @gcc -E trexio.c@. Comments will include
   the included header paths
3. Parse the output to find the header paths
4. From the extracted header path, get the JSON specification
-}
getJsonSpec :: (MonadIO m, MonadMask m) => m TrexioScheme
getJsonSpec = withSystemTempFile "trexio.c" $ \tmpPath tmpHandle -> do
  -- Write the temporary file
  liftIO $ do
    T.hPutStrLn tmpHandle "#include <trexio.h>"
    hFlush tmpHandle

  -- Run the C preprocessor
  (stdo, _) <- readProcess_ . shell $ "gcc -E " <> tmpPath

  -- Filter for trexio.h header paths
  let trexioLines =
        filter ("/trexio.h" `BL.isSuffixOf`)
          . fmap (BLC.filter (/= '"') . BLC.dropWhileEnd (/= '"') . BLC.dropWhile (/= '"'))
          . filter ("#" `BL.isPrefixOf`)
          . BLC.lines
          $ stdo
  trexioPath <- case trexioLines of
    t : _ -> liftIO . BS.toFilePath . BS.toStrict $ t
    _ -> throwString "Could not find trexio.h header path"

  -- Get the JSON specification from the header
  trexioHeader <- liftIO $ BL.readFile trexioPath
  let jsonString =
        BLC.unlines
          . L.tail
          . takeWhile (/= "*/")
          . dropWhile (/= "/* JSON configuration")
          . BLC.lines
          $ trexioHeader
  case eitherDecode jsonString of
    Right trexio -> return trexio
    Left err -> throwString $ "Could not parse JSON specification: " <> err

--------------------------------------------------------------------------------

{- | The overall data structure TREXIO uses to represent a wave function as a
JSON specification. A TREXIO scheme consists of multiple data groups and each
data group has multiple fields. A field may require knowledge of other fields.
-}
newtype TrexioScheme = TrexioScheme (Map GroupName Group)
  deriving (Generic, Show, Eq, Ord, Lift)
  deriving (ToJSON, FromJSON) via Map GroupName Group

{- | The name of a data group, e.g. @ao@ for atomic orbitals, @basis@ for basis
functions, etc.
-}
newtype GroupName = GroupName Text
  deriving (Generic, Show, Eq, Ord, Lift)
  deriving (ToJSONKey, FromJSONKey) via Text

{- | A data group is a record like data structure with named fields of different
types. Each field may or may not be set, thus the 'Maybe' type.
-}
newtype Group = Group (Map DataName Typ)
  deriving (Generic, Show, Eq, Ord, Lift)
  deriving (ToJSON, FromJSON) via Map DataName Typ

{- | The name of a data field, as specified by the TREXIO scheme. There is no
guarantee that the name is a valid Haskell identifier. To ensure that, use the
'sanId' function.
-}
newtype DataName = DataName Text
  deriving (Generic, Show, Eq, Ord, Lift)
  deriving (ToJSONKey, FromJSONKey) via Text

instance ToJSON DataName where
  toJSON (DataName name) = String name

instance FromJSON DataName where
  parseJSON (String name) = return . DataName $ name
  parseJSON _ = fail "parseJSON(DataName): could not parse"

{- | The TREXIO type of a data field including sparsity, buffering, dimensionality
etc.
-}
data Typ
  = -- | A 32 integer but meant to represent the size in a given dimension. The
    -- Bool indicates if field can also be written
    Dim Bool Length
  | -- | A 32 bit integer
    Int Length
  | -- | A double precision float. The Bool indicates whether this field is
    -- buffered
    Float Bool Length
  | -- | A string with a given length
    Str Length
  | -- | An index type
    Idx Length
  | -- | Sparse array of floats
    SparseFloat Length
  | -- | A bit field
    BitField Length
  deriving (Generic, Show, Eq, Ord, Lift)

instance ToJSON Typ where
  toJSON (Dim False len) = Array ["dim", toJSON len]
  toJSON (Dim True len) = Array ["dim readonly", toJSON len]
  toJSON (Int len) = Array ["int", toJSON len]
  toJSON (Float False len) = Array ["float", toJSON len]
  toJSON (Float True len) = Array ["float buffered", toJSON len]
  toJSON (Str len) = Array ["str", toJSON len]
  toJSON (Idx len) = Array ["index", toJSON len]
  toJSON (SparseFloat len) = Array ["float sparse", toJSON len]
  toJSON (BitField len) = Array ["int special", toJSON len]

instance FromJSON Typ where
  parseJSON (Array ["dim", len]) = Dim True <$> parseJSON len
  parseJSON (Array ["dim readonly", len]) = Dim False <$> parseJSON len
  parseJSON (Array ["int", len]) = Int <$> parseJSON len
  parseJSON (Array ["float", len]) = Float False <$> parseJSON len
  parseJSON (Array ["float buffered", len]) = Float True <$> parseJSON len
  parseJSON (Array ["str", len]) = Str <$> parseJSON len
  parseJSON (Array ["index", len]) = Idx <$> parseJSON len
  parseJSON (Array ["float sparse", len]) = SparseFloat <$> parseJSON len
  parseJSON (Array ["int special", len]) = BitField <$> parseJSON len
  parseJSON _ = fail "parseJSON(Typ): could not parse"

{- | TREXIO data fields are annotated with a length specification. This
specification is a list of sizes along the dimensions of an $n$D array.
An empty length specification refers to a scalar. A dimension may have a
constant size or refer to another field that stores its size, see 'DimLength'.
-}
newtype Length = Length [DimLength] deriving (Generic, Show, Eq, Ord, Lift)

instance ToJSON Length where
  toJSON (Length dim) = Array . V.fromList . fmap toJSON $ dim

instance FromJSON Length where
  parseJSON (Array arr) =
    Length . V.toList
      <$> traverse (parseJSON @DimLength) arr
  parseJSON _ = fail "parseJSON(Length): could not parse"

{- | The size along a dimension of a field. It can be a constant or refer to
a field that stores a scalar describing a length.
-}
data DimLength
  = Const Int
  | Field GroupName DataName
  deriving (Generic, Show, Eq, Ord, Lift)

instance ToJSON DimLength where
  toJSON (Const int) = String . tshow $ int
  toJSON (Field (GroupName groupName) (DataName dataName)) = String $ groupName <> "." <> dataName

instance FromJSON DimLength where
  parseJSON (String s) = case readMaybe . T.unpack $ s of
    Just i -> return . Const $ i
    Nothing -> case T.splitOn "." s of
      [groupName, dataName] -> return $ Field (GroupName groupName) (DataName dataName)
      _ -> fail "parseJSON(DimLength): could not parse"
  parseJSON _ = fail "parseJSON(DimLength): could not parse"

--------------------------------------------------------------------------------
-- Helper functions

{- | Sanitise an identifier, e.g. a field name or function name. I.e. we ensure
it starts with a valid lower case letter or symbol.
-}
sanId :: String -> String
sanId "" = error "sanId: empty string"
sanId ind@(c : cs)
  | isUpperCase c = sanId $ toLower c : cs
  | isDigit c = sanId $ '_' : c : cs
  | ind == "type" = "type'"
  | ind == "class" = "class'"
  | otherwise = c : cs

--------------------------------------------------------------------------------
-- Template Haskell binding generator

-- | The standard operations on data fields.
data FieldOps
  = -- | Check if a field is set
    Has
  | -- | Read data from a field
    Read
  | -- | Write data to a field
    Write
  deriving (Generic, Eq, Show, Ord)

opsFnName :: FieldOps -> String
opsFnName Has = "has"
opsFnName Read = "read"
opsFnName Write = "write"

-- | Associate a TREXIO 'Typ' with a Haskell 'Type'.
typToType :: (Quote m) => Typ -> m Type
typToType (Dim _ (Length [])) = [t|Int|]
typToType (Dim _ (Length [_])) = [t|Vector S Int|]
typToType (Int (Length [])) = [t|Int|]
typToType (Int (Length [_])) = [t|Vector S Int|]
typToType (Float False (Length [])) = [t|Double|]
typToType (Float False (Length [_])) = [t|Vector S Double|]
typToType (Float False (Length [_, _])) = [t|Matrix S Double|]
typToType (Float False (Length [_, _, _])) = [t|Massiv.Array S Ix3 Double|]
typToType (Float False (Length [_, _, _, _])) = [t|Massiv.Array S Ix4 Double|]
typToType (Float True (Length [_])) = [t|Vector S Double|]
typToType (Str (Length [])) = [t|Text|]
typToType (Str (Length [_])) = [t|Vector B Text|]
typToType (Idx (Length [])) = [t|Int|]
typToType (Idx (Length [_])) = [t|Vector S Int|]
typToType (SparseFloat (Length [_, _])) = [t|CooArray U Ix2 Double|]
typToType (SparseFloat (Length [_, _, _])) = [t|CooArray U Ix3 Double|]
typToType (SparseFloat (Length [_, _, _, _])) = [t|CooArray U Ix4 Double|]
typToType (SparseFloat (Length [_, _, _, _, _, _])) = [t|CooArray U (IxN 6) Double|]
typToType (SparseFloat (Length [_, _, _, _, _, _, _, _])) = [t|CooArray U (IxN 8) Double|]
typToType (BitField (Length [_])) = [t|BV.Vector Word8|]
typToType t = error $ "Can not associate " <> show t <> " with a Type"

-- | Associate a 'FieldOps' and a TREXIO field 'Typ' with a Haskell function type.
mkCFnSig :: FieldOps -> Typ -> Q Type
mkCFnSig Has _ = [t|Trexio -> IO ExitCodeC|]
mkCFnSig Read (Dim _ _) = [t|Trexio -> Ptr Int32 -> IO ExitCodeC|]
mkCFnSig Read (Int _) = [t|Trexio -> Ptr Int32 -> IO ExitCodeC|]
mkCFnSig Read (Float False _) = [t|Trexio -> Ptr CDouble -> IO ExitCodeC|]
mkCFnSig Read (Float True _) = [t|Trexio -> Int64 -> Ptr Int64 -> Ptr CDouble -> IO ExitCodeC|]
mkCFnSig Read (Str (Length [])) = [t|Trexio -> Ptr CChar -> Int32 -> IO ExitCodeC|]
mkCFnSig Read (Str (Length [_])) = [t|Trexio -> Ptr (Ptr CChar) -> Int32 -> IO ExitCodeC|]
mkCFnSig Read (Idx _) = [t|Trexio -> Ptr Int32 -> IO ExitCodeC|]
mkCFnSig Read (SparseFloat _) = [t|Trexio -> Int64 -> Ptr Int64 -> Ptr Int32 -> Ptr CDouble -> IO ExitCodeC|]
mkCFnSig Read (BitField _) = [t|Trexio -> Int64 -> Ptr Int64 -> Ptr Int64 -> IO ExitCodeC|]
mkCFnSig Write (Dim _ (Length [])) = [t|Trexio -> Int32 -> IO ExitCodeC|]
mkCFnSig Write (Dim _ (Length [_])) = [t|Trexio -> Ptr Int32 -> IO ExitCodeC|]
mkCFnSig Write (Int (Length [])) = [t|Trexio -> Int32 -> IO ExitCodeC|]
mkCFnSig Write (Int (Length [_])) = [t|Trexio -> Ptr Int32 -> IO ExitCodeC|]
mkCFnSig Write (Float False (Length [])) = [t|Trexio -> CDouble -> IO ExitCodeC|]
mkCFnSig Write (Float False (Length _)) = [t|Trexio -> Ptr CDouble -> IO ExitCodeC|]
mkCFnSig Write (Float True (Length _)) = [t|Trexio -> Int64 -> Int64 -> Ptr CDouble -> IO ExitCodeC|]
mkCFnSig Write (Str (Length [])) = [t|Trexio -> ConstPtr CChar -> Int32 -> IO ExitCodeC|]
mkCFnSig Write (Str (Length [_])) = [t|Trexio -> ConstPtr (ConstPtr CChar) -> Int32 -> IO ExitCodeC|]
mkCFnSig Write (Idx (Length [])) = [t|Trexio -> Int32 -> IO ExitCodeC|]
mkCFnSig Write (Idx (Length [_])) = [t|Trexio -> Ptr Int32 -> IO ExitCodeC|]
mkCFnSig Write (SparseFloat _) = [t|Trexio -> Int64 -> Int64 -> Ptr Int32 -> Ptr CDouble -> IO ExitCodeC|]
mkCFnSig Write (BitField _) = [t|Trexio -> Int64 -> Int64 -> Ptr Int64 -> IO ExitCodeC|]
mkCFnSig op t = error $ "Can not associate " <> show op <> " and " <> show t <> " with a Type"

{- | Associate a 'FieldOps' and a field 'Typ' with the type of a Haskell function.
The Haskell function is already abstracted and expected to perform other queries
such as vector sizes as necessary.
-}
mkHsFnSig :: FieldOps -> Typ -> Q Type
mkHsFnSig Has _ = [t|forall m. (MonadIO m) => Trexio -> m Bool|]
mkHsFnSig Read (Dim _ (Length [])) = [t|forall m. (MonadIO m) => Trexio -> m Int|]
mkHsFnSig Read (Dim _ (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Vector S Int)|]
mkHsFnSig Read (Int (Length [])) = [t|forall m. (MonadIO m) => Trexio -> m Int|]
mkHsFnSig Read (Int (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Vector S Int)|]
mkHsFnSig Read (Float False (Length [])) = [t|forall m. (MonadIO m) => Trexio -> m Double|]
mkHsFnSig Read (Float False (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Vector S Double)|]
mkHsFnSig Read (Float False (Length [_, _])) = [t|forall m. (MonadIO m) => Trexio -> m (Matrix S Double)|]
mkHsFnSig Read (Float False (Length [_, _, _])) = [t|forall m. (MonadIO m) => Trexio -> m (Massiv.Array S Ix3 Double)|]
mkHsFnSig Read (Float False (Length [_, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> m (Massiv.Array S Ix4 Double)|]
mkHsFnSig Read (Float True (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Vector S Double)|]
mkHsFnSig Read (Str (Length [])) = [t|forall m. (MonadIO m) => Trexio -> m Text|]
mkHsFnSig Read (Str (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Vector B Text)|]
mkHsFnSig Read (Idx (Length [])) = [t|forall m. (MonadIO m) => Trexio -> m Int|]
mkHsFnSig Read (Idx (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Vector S Int)|]
mkHsFnSig Read (SparseFloat (Length [_, _])) = [t|forall m. (MonadIO m) => Trexio -> m (CooArray U Ix2 Double)|]
mkHsFnSig Read (SparseFloat (Length [_, _, _])) = [t|forall m. (MonadIO m) => Trexio -> m (CooArray U Ix3 Double)|]
mkHsFnSig Read (SparseFloat (Length [_, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> m (CooArray U Ix4 Double)|]
mkHsFnSig Read (SparseFloat (Length [_, _, _, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> m (CooArray U (IxN 6) Double)|]
mkHsFnSig Read (SparseFloat (Length [_, _, _, _, _, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> m (CooArray U (IxN 8) Double)|]
mkHsFnSig Read (BitField (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> m (Matrix U (Bit, Bit))|]
mkHsFnSig Write (Dim _ (Length [])) = [t|forall m. (MonadIO m) => Trexio -> Int -> m ()|]
mkHsFnSig Write (Dim _ (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Vector S Int -> m ()|]
mkHsFnSig Write (Int (Length [])) = [t|forall m. (MonadIO m) => Trexio -> Int -> m ()|]
mkHsFnSig Write (Int (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Vector S Int -> m ()|]
mkHsFnSig Write (Float False (Length [])) = [t|forall m. (MonadIO m) => Trexio -> Double -> m ()|]
mkHsFnSig Write (Float False (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Vector S Double -> m ()|]
mkHsFnSig Write (Float False (Length [_, _])) = [t|forall m. (MonadIO m) => Trexio -> Matrix S Double -> m ()|]
mkHsFnSig Write (Float False (Length [_, _, _])) = [t|forall m. (MonadIO m) => Trexio -> Massiv.Array S Ix3 Double -> m ()|]
mkHsFnSig Write (Float False (Length [_, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> Massiv.Array S Ix4 Double -> m ()|]
mkHsFnSig Write (Float True (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Vector S Double -> m ()|]
mkHsFnSig Write (Str (Length [])) = [t|forall m. (MonadIO m) => Trexio -> Text -> m ()|]
mkHsFnSig Write (Str (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Vector B Text -> m ()|]
mkHsFnSig Write (Idx (Length [])) = [t|forall m. (MonadIO m) => Trexio -> Int -> m ()|]
mkHsFnSig Write (Idx (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Vector S Int -> m ()|]
mkHsFnSig Write (SparseFloat (Length [_, _])) = [t|forall m. (MonadIO m) => Trexio -> CooArray U Ix2 Double -> m ()|]
mkHsFnSig Write (SparseFloat (Length [_, _, _])) = [t|forall m. (MonadIO m) => Trexio -> CooArray U Ix3 Double -> m ()|]
mkHsFnSig Write (SparseFloat (Length [_, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> CooArray U Ix4 Double -> m ()|]
mkHsFnSig Write (SparseFloat (Length [_, _, _, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> CooArray U (IxN 6) Double -> m ()|]
mkHsFnSig Write (SparseFloat (Length [_, _, _, _, _, _, _, _])) = [t|forall m. (MonadIO m) => Trexio -> CooArray U (IxN 8) Double -> m ()|]
mkHsFnSig Write (BitField (Length [_])) = [t|forall m. (MonadIO m) => Trexio -> Matrix U (Bit, Bit) -> m ()|]
mkHsFnSig op t = error $ "Can not associate " <> show op <> " and " <> show t <> " with a Type"

-- | Generate a Haskell function name for a given operation, group and field of that group.
mkHsFnName :: FieldOps -> GroupName -> DataName -> String
mkHsFnName op (GroupName groupName) (DataName dataName) =
  sanId . camel $ opsFnName op <> "_" <> T.unpack groupName <> "_" <> T.unpack dataName

-- | Generate a C function name for a given operation, group and field of that group.
mkCFnName :: FieldOps -> GroupName -> DataName -> String
mkCFnName op (GroupName groupName) (DataName dataName) =
  "trexio_" <> opsFnName op <> "_" <> (T.unpack . T.toLower $ groupName) <> "_" <> (T.unpack . T.toLower $ dataName)

-- | Convert a field to a type
fieldToType :: (Quote m) => DataName -> Typ -> m VarBangType
fieldToType (DataName dataName) typ = do
  let fieldName = mkName . sanId . camel . T.unpack $ dataName
  fieldType <- typToType typ
  maybeFieldType <- [t|Maybe $(return fieldType)|]
  return (fieldName, Bang NoSourceUnpackedness NoSourceStrictness, maybeFieldType)

stdDerivs :: [DerivClause]
stdDerivs = [DerivClause Nothing [ConT ''Generic, ConT ''Show, ConT ''Ord, ConT ''Eq]]

-- | Create a record from a given data group
mkRecord :: GroupName -> Group -> Q Dec
mkRecord (GroupName groupName) (Group fields) = do
  groupNameTD <- newName . pascal . T.unpack $ groupName
  groupNameTC <- newName . pascal . T.unpack $ groupName
  fieldsT <- traverse (uncurry fieldToType) . Map.toList $ fields
  return $ DataD [] groupNameTD [] Nothing [RecC groupNameTC fieldsT] stdDerivs

-- | Create the TREXIO scheme type with subrecords for each data group.
mkTrexioScheme :: TrexioScheme -> Q Dec
mkTrexioScheme (TrexioScheme groups) = do
  dataName <- newName "TREXIO"
  constructorName <- newName "TREXIO"
  fieldsT <- forM (Map.toList groups) $ \(GroupName groupName, _) -> do
    groupFieldName <- newName . camel . T.unpack $ groupName
    groupFieldType <- [t|$(conT . mkName . pascal . T.unpack $ groupName)|]
    return (groupFieldName, Bang NoSourceUnpackedness NoSourceStrictness, groupFieldType)
  return $ DataD [] dataName [] Nothing [RecC constructorName fieldsT] stdDerivs

-- | Create all C function bindings for a given group
mkCBindings :: GroupName -> Group -> Q [Dec]
mkCBindings groupName (Group fields) = do
  -- Group bindings for delete
  groupDelBind <- mkCDeleteFn groupName
  fieldBinds <- fmap (catMaybes . concat) . forM (Map.toList fields) $ \(fieldName, fieldTyp) -> do
    -- Standard bindings
    stdBindings <- forM [Has, Read, Write] $ \op -> do
      let cFnName = mkCFnName op groupName fieldName
      cFnNameT <- newName cFnName
      cFnSig <- mkCFnSig op fieldTyp

      -- Dim fields, that are read only, do not have a write function
      if fieldTyp == Dim False (Length []) && op == Write
        then return Nothing
        else return . Just . ForeignD $ ImportF CApi Unsafe ("trexio.h " <> cFnName) cFnNameT cFnSig

    -- "size" bindings: bitfields, sparse arrays and buffered arrays have an
    -- additional function "_size", that tells how many COO elements there are.
    let cSizeFnString = mkCSizeFnName groupName fieldName
    cSizeFnName <- newName cSizeFnString
    cFnSig <- [t|Trexio -> Ptr Int64 -> IO Int32|]
    let imprt =
          ForeignD $
            ImportF CApi Unsafe ("trexio.h " <> cSizeFnString) cSizeFnName cFnSig
    let sizeBinding = case fieldTyp of
          SparseFloat _ -> Just imprt
          Float True _ -> Just imprt
          _ -> Nothing

    -- Return all bindings
    return $ stdBindings <> [sizeBinding]
  return $ groupDelBind : fieldBinds

-- Make a Has function for a given field
mkHsHasFn :: GroupName -> DataName -> Typ -> Q [Dec]
mkHsHasFn groupName dataName fieldTyp = do
  let hsFnName = mkHsFnName Has groupName dataName
      cFnName = mkCFnName Has groupName dataName
  hsFnSig <- mkHsFnSig Has fieldTyp
  hsExp <-
    [e|
      \trexio -> liftIO $ do
        cRes <- $(varE . mkName $ cFnName) trexio
        if exitCodeH cRes == Success
          then return True
          else return False
      |]
  return
    [ SigD (mkName hsFnName) hsFnSig
    , FunD (mkName hsFnName) [Clause [] (NormalB hsExp) []]
    ]

{- | Generate an expression to obtain the size of an array field along a given
dimension.
-}
mkSizeFn :: DimLength -> Q Exp
mkSizeFn (Const i) = [e|\_ -> return i|]
mkSizeFn (Field groupName dataName) = do
  let cFnName = mkCFnName Read groupName dataName
  [e|
    ( \trexio -> alloca $ \(dimPtr :: Ptr Int32) -> do
        ec <- exitCodeH <$> $(varE . mkName $ cFnName) trexio dimPtr
        case ec of
          Success -> fromIntegral <$> peek dimPtr
          _ -> throwM ec
    )
    |]

isIntField :: Typ -> Bool
isIntField (Dim _ _) = True
isIntField (Int _) = True
isIntField (Idx _) = True
isIntField _ = False

isWritableIntField :: Typ -> Bool
isWritableIntField (Dim True _) = True
isWritableIntField (Int _) = True
isWritableIntField (Idx _) = True
isWritableIntField _ = False

isProtectedIntField :: Typ -> Bool
isProtectedIntField (Dim False _) = True
isProtectedIntField _ = False

isFloatField :: Typ -> Bool
isFloatField (Float False _) = True
isFloatField _ = False

isBufferedFloat :: Typ -> Bool
isBufferedFloat (Float True _) = True
isBufferedFloat _ = False

isSparseFloat :: Typ -> Bool
isSparseFloat (SparseFloat _) = True
isSparseFloat _ = False

isStringField :: Typ -> Bool
isStringField (Str _) = True
isStringField _ = False

isBitField :: Typ -> Bool
isBitField (BitField _) = True
isBitField _ = False

{- | Sparse fields have an associated @_size@ function, that returns the number
of COO elements.
-}
mkCSizeFnName :: GroupName -> DataName -> String
mkCSizeFnName (GroupName groupName) (DataName dataName) =
  T.unpack $
    "trexio_read_" <> groupName <> "_" <> dataName <> "_size"

{- | Create abstracted read functions, that automatically obtain sizes for arrays
as required from other fields. 'CooArray's are read in a single, big chunk and
need to fit in memory.
-}
mkReadFns :: GroupName -> DataName -> Typ -> Q Exp
mkReadFns groupName dataName fieldType = case dims of
  []
    | isIntField fieldType ->
        [e|
          \trexio -> liftIO . alloca $ \buf -> do
            ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio buf
            case ec of
              Success -> fromIntegral <$> peek buf
              _ -> throwM ec
          |]
    | isFloatField fieldType ->
        [e|
          \trexio -> liftIO . alloca $ \buf -> do
            ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio buf
            case ec of
              Success -> peek (castPtr buf)
              _ -> throwM ec
          |]
    | isStringField fieldType ->
        [e|
          \trexio -> liftIO . allocaBytes 256 $ \strPtr -> do
            ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio strPtr 256
            case ec of
              Success -> T.pack <$> peekCString strPtr
              _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 0D data: " <> show fieldType
  [d1]
    | isIntField fieldType ->
        [e|
          \trexio -> liftIO $ do
            sz1 <- $(mkSizeFn d1) trexio
            allocaArray sz1 $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio buf
              case ec of
                Success -> peekIntArray (Sz1 sz1) buf
                _ -> throwM ec
          |]
    | isFloatField fieldType ->
        [e|
          \trexio -> liftIO $ do
            sz1 <- $(mkSizeFn d1) trexio
            allocaArray sz1 $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio buf
              case ec of
                Success -> peekArray (Sz1 sz1) (castPtr buf)
                _ -> throwM ec
          |]
    | isStringField fieldType ->
        [e|
          \trexio -> liftIO $ do
            nStrings <- $(mkSizeFn d1) trexio
            let maxStrLen = 256

            allocaArray nStrings $ \(superPtr :: Ptr (Ptr CChar)) ->
              -- Allocate the buffers for the strings
              bracket
                (replicateM nStrings $ callocArray0 maxStrLen)
                (traverse free)
                $ \(strPtrs :: [Ptr CChar]) -> do
                  -- Write the individual buffers to the super buffer
                  forM_ (zip [0 ..] strPtrs) $ \(i, strPtr) ->
                    pokeElemOff superPtr i strPtr

                  -- Call the C function
                  ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio superPtr (fromIntegral maxStrLen)
                  case ec of
                    Success -> Massiv.fromList Seq . fmap T.pack <$> traverse peekCString strPtrs
                    _ -> throwM ec
          |]
    | isBitField fieldType ->
        [e|
          \trexio -> liftIO $ do
            moNum <- $(mkSizeFn $ Field (GroupName "mo") (DataName "num")) trexio
            nDets <- $(mkSizeFn d1) trexio
            nInt64PerDet <- intsPerDet trexio

            -- Allocate a buffer
            allocaArray (nDets * nInt64PerDet * 2) $ \detBuf -> do
              let readDets :: IO (Matrix U (Bit, Bit))
                  readDets = do
                    -- Read each determinant individually
                    dets <- forM [0 .. nDets - 1] $ \i -> do
                      let upPtr = detBuf `plusPtr` (i * nInt64PerDet * 2 * sizeOf (undefined :: Int64))
                          downPtr = upPtr `plusPtr` (nInt64PerDet * sizeOf (undefined :: Int64))
                          nBytes = nInt64PerDet * sizeOf (undefined :: Int64)

                      upBS <- BS.unsafePackCStringLen (castPtr upPtr, nBytes)
                      downBS <- BS.unsafePackCStringLen (castPtr downPtr, nBytes)

                      let toDet =
                            compute @U
                              . Massiv.take moNum
                              . (Massiv.fromVector' Par (Sz $ nBytes * 8) :: BV.Vector Bit -> Vector U Bit)
                              . BV.cloneFromByteString
                          upDet = toDet upBS
                          downDet = toDet downBS

                      return $ Massiv.zip upDet downDet

                    compute <$> stackOuterSlicesM dets

              -- Call the C function and populate the buffer
              with (fromIntegral nDets) $ \bufSz -> do
                ec <-
                  exitCodeH
                    <$> $(varE . mkName $ mkCFnName Read groupName dataName)
                      trexio
                      0
                      bufSz
                      detBuf
                case ec of
                  Success -> readDets
                  End -> readDets
                  _ -> throwM ec
          |]
    | isBufferedFloat fieldType ->
        [e|
          \trexio -> liftIO $ do
            sz1 <- $(mkSizeFn d1) trexio
            with (fromIntegral sz1) $ \bufSz ->
              allocaArray sz1 $ \buf -> do
                ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio 0 bufSz buf
                case ec of
                  Success -> peekArray (Sz1 sz1) (castPtr buf)
                  End -> peekArray (Sz1 sz1) (castPtr buf)
                  _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 1D data: " <> show fieldType
  [d1, d2]
    | isFloatField fieldType ->
        [e|
          \trexio -> liftIO $ do
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio
            allocaArray (sz1 * sz2) $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio buf
              case ec of
                Success -> peekArray (Sz2 sz1 sz2) (castPtr buf)
                _ -> throwM ec
          |]
    | isSparseFloat fieldType ->
        [e|
          \trexio -> liftIO $ do
            -- Size of the array
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio

            -- Number of COO elements in the sparse array
            nCoo <- alloca $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCSizeFnName groupName dataName) trexio buf
              case ec of
                Success -> fromIntegral <$> peek buf
                _ -> throwM ec

            -- Read the COO array in a single chunk
            with (fromIntegral nCoo) $ \bufSz ->
              allocaArray (nCoo * 2) $ \ixBuf ->
                allocaArray nCoo $ \valBuf -> do
                  ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio 0 bufSz ixBuf valBuf
                  case ec of
                    Success -> do
                      ixs <- peek2DCoords (Sz1 nCoo) ixBuf
                      vals <- peekArray (Sz1 nCoo) . castPtr $ valBuf
                      mkCooArray (Sz2 sz1 sz2) ixs . compute @U $ vals
                    _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 2D data: " <> show fieldType
  [d1, d2, d3]
    | isFloatField fieldType ->
        [e|
          \trexio -> liftIO $ do
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio
            sz3 <- $(mkSizeFn d3) trexio
            allocaArray (sz1 * sz2 * sz3) $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio buf
              case ec of
                Success -> peekArray (Sz3 sz1 sz2 sz3) (castPtr buf)
                _ -> throwM ec
          |]
    | isSparseFloat fieldType ->
        [e|
          \trexio -> liftIO $ do
            -- Size of the array
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio
            sz3 <- $(mkSizeFn d3) trexio

            -- Number of COO elements in the sparse array
            nCoo <- alloca $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCSizeFnName groupName dataName) trexio buf
              case ec of
                Success -> fromIntegral <$> peek buf
                _ -> throwM ec

            -- Read the COO array in a single chunk
            with (fromIntegral nCoo) $ \bufSz ->
              allocaArray (nCoo * 3) $ \ixBuf ->
                allocaArray nCoo $ \valBuf -> do
                  ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio 0 bufSz ixBuf valBuf
                  case ec of
                    Success -> do
                      ixs <- peek3DCoords (Sz1 nCoo) ixBuf
                      vals <- peekArray (Sz1 nCoo) . castPtr $ valBuf
                      mkCooArray (Sz3 sz1 sz2 sz3) ixs . compute @U $ vals
                    _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 3D data: " <> show fieldType
  [d1, d2, d3, d4]
    | isSparseFloat fieldType ->
        [e|
          \trexio -> liftIO $ do
            -- Size of the array
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio
            sz3 <- $(mkSizeFn d3) trexio
            sz4 <- $(mkSizeFn d4) trexio

            -- Number of COO elements in the sparse array
            nCoo <- alloca $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCSizeFnName groupName dataName) trexio buf
              case ec of
                Success -> fromIntegral <$> peek buf
                _ -> throwM ec

            -- Read the COO array in a single chunk
            with (fromIntegral nCoo) $ \bufSz ->
              allocaArray (nCoo * 4) $ \ixBuf ->
                allocaArray nCoo $ \valBuf -> do
                  ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio 0 bufSz ixBuf valBuf
                  case ec of
                    Success -> do
                      ixs <- peek4DCoords (Sz1 nCoo) ixBuf
                      vals <- peekArray (Sz1 nCoo) . castPtr $ valBuf
                      mkCooArray (Sz4 sz1 sz2 sz3 sz4) ixs . compute @U $ vals
                    _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 4D data: " <> show fieldType
  [d1, d2, d3, d4, d5, d6]
    | isSparseFloat fieldType ->
        [e|
          \trexio -> liftIO $ do
            -- Size of the array
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio
            sz3 <- $(mkSizeFn d3) trexio
            sz4 <- $(mkSizeFn d4) trexio
            sz5 <- $(mkSizeFn d5) trexio
            sz6 <- $(mkSizeFn d6) trexio

            -- Number of COO elements in the sparse array
            nCoo <- alloca $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCSizeFnName groupName dataName) trexio buf
              case ec of
                Success -> fromIntegral <$> peek buf
                _ -> throwM ec

            -- Read the COO array in a single chunk
            with (fromIntegral nCoo) $ \bufSz ->
              allocaArray (nCoo * 6) $ \ixBuf ->
                allocaArray nCoo $ \valBuf -> do
                  ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio 0 bufSz ixBuf valBuf
                  case ec of
                    Success -> do
                      ixs <- peek6DCoords (Sz1 nCoo) ixBuf
                      vals <- peekArray (Sz1 nCoo) . castPtr $ valBuf
                      mkCooArray (Sz $ sz1 :> sz2 :> sz3 :> sz4 :> sz5 :. sz6) ixs . compute @U $ vals
                    _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 6D data: " <> show fieldType
  [d1, d2, d3, d4, d5, d6, d7, d8]
    | isSparseFloat fieldType ->
        [e|
          \trexio -> liftIO $ do
            -- Size of the array
            sz1 <- $(mkSizeFn d1) trexio
            sz2 <- $(mkSizeFn d2) trexio
            sz3 <- $(mkSizeFn d3) trexio
            sz4 <- $(mkSizeFn d4) trexio
            sz5 <- $(mkSizeFn d5) trexio
            sz6 <- $(mkSizeFn d6) trexio
            sz7 <- $(mkSizeFn d7) trexio
            sz8 <- $(mkSizeFn d8) trexio

            -- Number of COO elements in the sparse array
            nCoo <- alloca $ \buf -> do
              ec <- exitCodeH <$> $(varE . mkName $ mkCSizeFnName groupName dataName) trexio buf
              case ec of
                Success -> fromIntegral <$> peek buf
                _ -> throwM ec

            -- Read the COO array in a single chunk
            with (fromIntegral nCoo) $ \bufSz ->
              allocaArray (nCoo * 8) $ \ixBuf ->
                allocaArray nCoo $ \valBuf -> do
                  ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Read groupName dataName) trexio 0 bufSz ixBuf valBuf
                  case ec of
                    Success -> do
                      ixs <- peek8DCoords (Sz1 nCoo) ixBuf
                      vals <- peekArray (Sz1 nCoo) . castPtr $ valBuf
                      mkCooArray (Sz $ sz1 :> sz2 :> sz3 :> sz4 :> sz5 :> sz6 :> sz7 :. sz8) ixs . compute @U $ vals
                    _ -> throwM ec
          |]
    | otherwise -> error $ "mkReadFns: unsupported field type for 8D data: " <> show fieldType
  dl -> error $ "mkReadFns: unsupported number of dimensions: " <> show dl
 where
  dims = getCrossRefs fieldType

-- | Get the Length specifications of a field
getCrossRefs :: Typ -> [DimLength]
getCrossRefs (Dim _ (Length lspec)) = lspec
getCrossRefs (Int (Length lspec)) = lspec
getCrossRefs (Float _ (Length lspec)) = lspec
getCrossRefs (Str (Length lspec)) = lspec
getCrossRefs (Idx (Length lspec)) = lspec
getCrossRefs (SparseFloat (Length lspec)) = lspec
getCrossRefs (BitField (Length lspec)) = lspec

{- | Make a Read function for a given field. This generator takes care to query
referenced 'Dim' fields to obtain the correct size of the result. If any
of this 'Dim' fields is not set, the function will fail.
-}
mkHsReadFn :: GroupName -> DataName -> Typ -> Q [Dec]
mkHsReadFn groupName dataName fieldTyp = do
  -- Generate the function name for Haskell
  let hsFnName = mkHsFnName Read groupName dataName

  -- Generate the Haskell function
  hsFnSig <- mkHsFnSig Read fieldTyp
  hsExp <- mkReadFns groupName dataName fieldTyp
  return
    [ SigD (mkName hsFnName) hsFnSig
    , FunD (mkName hsFnName) [Clause [] (NormalB hsExp) []]
    ]

-- | Make a writer function for a given 'DimLength'.
mkWriteSzFn :: TrexioScheme -> DimLength -> Q Exp
mkWriteSzFn _ (Const i) = [e|\_ _ -> return i|]
mkWriteSzFn (TrexioScheme scheme) dimLength@(Field groupName dataName)
  | isReadOnly = [e|\_ _ -> return ()|]
  | otherwise = do
      let cFnName = mkCFnName Write groupName dataName
      [e|
        \trexio sz -> liftIO $ do
          ec <- exitCodeH <$> $(varE . mkName $ cFnName) trexio (fromIntegral sz)
          case ec of
            Success -> return ()
            ReadOnly -> return ()
            -- If the attribute already exists, read it and check if it is the
            -- same value we want to write
            AttrAlreadyExists -> do
              currentSz <- $(mkSizeFn dimLength) trexio
              if currentSz == sz
                then return ()
                else throwM AttrAlreadyExists
            _ -> throwM ec
        |]
 where
  Group grp = scheme Map.! groupName
  fieldTyp = grp Map.! dataName
  isReadOnly = case fieldTyp of
    Dim False _ -> True
    _ -> False

-- | Make a writer function for a given field
mkWriteFns :: TrexioScheme -> GroupName -> DataName -> Typ -> Q Exp
mkWriteFns scheme groupName dataName fieldType = case dims of
  []
    | isWritableIntField fieldType ->
        [e|
          \trexio int -> liftIO $ do
            ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Write groupName dataName) trexio (fromIntegral int)
            case ec of
              Success -> return ()
              _ -> throwM ec
          |]
    | isFloatField fieldType ->
        [e|
          \trexio float -> liftIO $ do
            ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Write groupName dataName) trexio (coerce float)
            case ec of
              Success -> return ()
              _ -> throwM ec
          |]
    | isStringField fieldType ->
        [e|
          \trexio str -> liftIO . withCStringLen (T.unpack str) $ \(strPtr, len) -> do
            ec <- exitCodeH <$> $(varE . mkName $ mkCFnName Write groupName dataName) trexio (ConstPtr strPtr) (fromIntegral len)
            case ec of
              Success -> return ()
              _ -> throwM ec
          |]
    | isProtectedIntField fieldType -> [e|\_ _ -> return ()|]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 0D data: " <> show fieldType
  [d1]
    | isIntField fieldType ->
        [e|
          \trexio arr -> liftIO . unsafeWithPtr (compute . Massiv.map fromIntegral $ arr) $ \arrPtr -> do
            let Sz1 sz1 = size arr
            $(mkWriteSzFn scheme d1) trexio sz1
            checkEC $ $(varE . mkName $ mkCFnName Write groupName dataName) trexio arrPtr
          |]
    | isFloatField fieldType ->
        [e|
          \trexio arr -> liftIO . unsafeWithPtr arr $ \arrPtr -> do
            let Sz1 sz1 = size arr
            $(mkWriteSzFn scheme d1) trexio sz1
            checkEC $ $(varE . mkName $ mkCFnName Write groupName dataName) trexio (castPtr arrPtr)
          |]
    | isStringField fieldType ->
        [e|
          \trexio arr -> liftIO $ do
            let Sz1 nStrings = size arr
                maxStrLen = 255
            $(mkWriteSzFn scheme d1) trexio nStrings
            ptrArr <- compute <$> mapM (fmap ConstPtr . newCString . T.unpack) arr
            unsafeWithPtr ptrArr $ \arrPtr ->
              checkEC $
                $(varE . mkName $ mkCFnName Write groupName dataName)
                  trexio
                  (ConstPtr arrPtr)
                  maxStrLen
          |]
    | isBitField fieldType ->
        [e|
          \trexio dets -> liftIO $ do
            nInt64PerDet <- intsPerDet trexio
            let Sz2 nDets _nMos = size dets
            $(mkWriteSzFn scheme d1) trexio nDets

            allocaArray (nDets * nInt64PerDet * 2) $ \detBuf -> do
              -- Write each determinant to the buffer
              forM_ [0 .. nDets - 1] $ \i -> do
                let det = dets !> i
                    detToByteString bv accFn =
                      BV.cloneToByteString
                        . Massiv.toVector
                        . compute @U
                        . Massiv.map accFn
                        $ bv
                    up = detToByteString det fst
                    down = detToByteString det snd
                    nBytes = BS.length up
                    upPtr = detBuf `plusPtr` (i * nInt64PerDet * 2 * sizeOf (undefined :: Int64))
                    downPtr = upPtr `plusPtr` (nInt64PerDet * sizeOf (undefined :: Int64))

                -- Up spin
                BS.unsafeUseAsCString up $ \charPtr -> do
                  copyBytes (castPtr upPtr) charPtr nBytes

                -- Down spin
                BS.unsafeUseAsCString down $ \charPtr -> do
                  copyBytes (castPtr downPtr) charPtr nBytes

              -- Call the C funciton with the buffer
              checkEC $
                $(varE . mkName $ mkCFnName Write groupName dataName)
                  trexio
                  0
                  (fromIntegral nDets)
                  detBuf
          |]
    | isBufferedFloat fieldType ->
        [e|
          \trexio vec -> liftIO $ do
            let Sz1 sz1 = size vec
            $(mkWriteSzFn scheme d1) trexio sz1
            unsafeWithPtr vec $ \arrPtr ->
              checkEC $ $(varE . mkName $ mkCFnName Write groupName dataName) trexio 0 (fromIntegral sz1) (castPtr arrPtr)
          |]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 1D data: " <> show fieldType
  [d1, d2]
    | isFloatField fieldType ->
        [e|
          \trexio arr -> liftIO . unsafeWithPtr arr $ \arrPtr -> do
            let Sz2 sz1 sz2 = size arr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            checkEC $ $(varE . mkName $ mkCFnName Write groupName dataName) trexio (castPtr arrPtr)
          |]
    | isSparseFloat fieldType ->
        [e|
          \trexio cooArr -> liftIO $ do
            let Sz2 sz1 sz2 = cooSize cooArr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            let cooVals = convert . values $ cooArr :: Vector S Double
                cooIxs = castCoords2D . coords $ cooArr :: Matrix S Int32
                Sz1 nCoo = size cooVals
            unsafeWithPtr cooVals $ \valPtr ->
              unsafeWithPtr cooIxs $ \ixPtr -> do
                checkEC $
                  $(varE . mkName $ mkCFnName Write groupName dataName)
                    trexio
                    0
                    (fromIntegral nCoo :: Int64)
                    ixPtr
                    (castPtr valPtr)
          |]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 2D data: " <> show fieldType
  [d1, d2, d3]
    | isFloatField fieldType ->
        [e|
          \trexio arr -> liftIO . unsafeWithPtr arr $ \arrPtr -> do
            let Sz3 sz1 sz2 sz3 = size arr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            $(mkWriteSzFn scheme d3) trexio sz3
            checkEC $ $(varE . mkName $ mkCFnName Write groupName dataName) trexio (castPtr arrPtr)
          |]
    | isSparseFloat fieldType ->
        [e|
          \trexio cooArr -> liftIO $ do
            let Sz3 sz1 sz2 sz3 = cooSize cooArr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            $(mkWriteSzFn scheme d3) trexio sz3
            let cooVals = convert . values $ cooArr
                cooIxs = castCoords3D . coords $ cooArr
                Sz1 nCoo = size cooVals
            unsafeWithPtr cooVals $ \valPtr ->
              unsafeWithPtr cooIxs $ \ixPtr ->
                checkEC $
                  $(varE . mkName $ mkCFnName Write groupName dataName)
                    trexio
                    0
                    (fromIntegral nCoo)
                    ixPtr
                    (castPtr valPtr)
          |]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 3D data: " <> show fieldType
  [d1, d2, d3, d4]
    | isSparseFloat fieldType ->
        [e|
          \trexio cooArr -> liftIO $ do
            let Sz4 sz1 sz2 sz3 sz4 = cooSize cooArr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            $(mkWriteSzFn scheme d3) trexio sz3
            $(mkWriteSzFn scheme d4) trexio sz4
            let cooVals = convert . values $ cooArr
                cooIxs = castCoords4D . coords $ cooArr
                Sz1 nCoo = size cooVals
            unsafeWithPtr cooVals $ \valPtr ->
              unsafeWithPtr cooIxs $ \ixPtr ->
                checkEC $
                  $(varE . mkName $ mkCFnName Write groupName dataName)
                    trexio
                    0
                    (fromIntegral nCoo)
                    ixPtr
                    (castPtr valPtr)
          |]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 4D data: " <> show fieldType
  [d1, d2, d3, d4, d5, d6]
    | isSparseFloat fieldType ->
        [e|
          \trexio cooArr -> liftIO $ do
            let Sz (sz1 :> sz2 :> sz3 :> sz4 :> sz5 :. sz6) = cooSize cooArr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            $(mkWriteSzFn scheme d3) trexio sz3
            $(mkWriteSzFn scheme d4) trexio sz4
            $(mkWriteSzFn scheme d5) trexio sz5
            $(mkWriteSzFn scheme d6) trexio sz6
            let cooVals = convert . values $ cooArr
                cooIxs = castCoords6D . coords $ cooArr
                Sz1 nCoo = size cooVals
            unsafeWithPtr cooVals $ \valPtr ->
              unsafeWithPtr cooIxs $ \ixPtr ->
                checkEC $
                  $(varE . mkName $ mkCFnName Write groupName dataName)
                    trexio
                    0
                    (fromIntegral nCoo)
                    ixPtr
                    (castPtr valPtr)
          |]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 6D data: " <> show fieldType
  [d1, d2, d3, d4, d5, d6, d7, d8]
    | isSparseFloat fieldType ->
        [e|
          \trexio cooArr -> liftIO $ do
            let Sz (sz1 :> sz2 :> sz3 :> sz4 :> sz5 :> sz6 :> sz7 :. sz8) = cooSize cooArr
            $(mkWriteSzFn scheme d1) trexio sz1
            $(mkWriteSzFn scheme d2) trexio sz2
            $(mkWriteSzFn scheme d3) trexio sz3
            $(mkWriteSzFn scheme d4) trexio sz4
            $(mkWriteSzFn scheme d5) trexio sz5
            $(mkWriteSzFn scheme d6) trexio sz6
            $(mkWriteSzFn scheme d7) trexio sz7
            $(mkWriteSzFn scheme d8) trexio sz8
            let cooVals = convert . values $ cooArr
                cooIxs = castCoords8D . coords $ cooArr
                Sz1 nCoo = size cooVals
            unsafeWithPtr cooVals $ \valPtr ->
              unsafeWithPtr cooIxs $ \ixPtr ->
                checkEC $
                  $(varE . mkName $ mkCFnName Write groupName dataName)
                    trexio
                    0
                    (fromIntegral nCoo)
                    ixPtr
                    (castPtr valPtr)
          |]
    | otherwise -> error $ "mkWriteFns: unsupported field type for 8D data: " <> show fieldType
  dl -> error $ "mkWriteFns: unsupported number of dimensions: " <> show dl
 where
  dims = getCrossRefs fieldType

mkHsWriteFn :: TrexioScheme -> GroupName -> DataName -> Typ -> Q [Dec]
mkHsWriteFn scheme groupName dataName fieldTyp = do
  -- Generate the function names in C and Haskell
  let hsFnName = mkHsFnName Write groupName dataName

  -- Generate the Haskell function
  hsFnSig <- mkHsFnSig Write fieldTyp
  hsExp <- mkWriteFns scheme groupName dataName fieldTyp
  return
    [ SigD (mkName hsFnName) hsFnSig
    , FunD (mkName hsFnName) [Clause [] (NormalB hsExp) []]
    ]

mkCDeleteName :: GroupName -> String
mkCDeleteName (GroupName groupName) = "trexio_delete_" <> T.unpack groupName

mkHsDeleteName :: GroupName -> String
mkHsDeleteName (GroupName groupName) = sanId . camel $ "delete_" <> T.unpack groupName

mkCDeleteFn :: GroupName -> Q Dec
mkCDeleteFn groupName = do
  let cFnName = mkCDeleteName groupName
  cTyp <- [t|Trexio -> IO ExitCodeC|]
  return . ForeignD $ ImportF CApi Unsafe ("trexio.h " <> cFnName) (mkName cFnName) cTyp

mkHsDeleteFn :: GroupName -> Q [Dec]
mkHsDeleteFn groupName = do
  let cFnName = mkName . mkCDeleteName $ groupName
      hsFnName = mkName . mkHsDeleteName $ groupName
  hsTyp <- [t|forall m. (MonadIO m) => Trexio -> m ()|]
  hsFn <- [e|\trexio -> liftIO . checkEC $ $(varE cFnName) trexio|]
  return
    [ SigD hsFnName hsTyp
    , FunD hsFnName [Clause [] (NormalB hsFn) []]
    ]
