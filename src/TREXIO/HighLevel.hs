{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: TREXIO.HighLevel
Description: Abstracted Haskell bindings to the TREXIO library
Copyright: Phillip Seeber 2024
License: BSD-3-Clause
Maintainer: phillip.seeber@uni-jena.de
Stability: experimental
Portability: POSIX
-}
module TREXIO.HighLevel where

import Control.Monad
import Data.Aeson
import Data.Map qualified as Map
import Data.Massiv.Array as Massiv hiding (Dim, forM)
import Foreign.C.ConstPtr
import Foreign.C.Types
import Language.Haskell.TH
import TREXIO.Internal.Base
import TREXIO.Internal.TH
import TREXIO.LowLevel
import TREXIO.LowLevel.Scheme

$( do
    let trexio@(TrexioScheme trexioScheme) = scheme
        groups = Map.toList trexioScheme

    -- Generate abstracted Haskell wrappers around all C functions
    fmap concat . forM groups $ \(groupName, Group group) -> do
        deleteBind <- mkHsDeleteFn groupName
        fieldBinds <- fmap concat . forM (Map.toList group) $ \(dataName, fieldType) -> do
            hasBind <- mkHsHasFn groupName dataName fieldType
            readBind <- mkHsReadFn groupName dataName fieldType
            writeBind <- mkHsWriteFn trexio groupName dataName fieldType
            return $ hasBind <> readBind <> writeBind
        return $ deleteBind <> fieldBinds
 )
