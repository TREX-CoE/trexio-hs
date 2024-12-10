{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: TREXIO.HighLevel.Records
Description: Records representing the TREXIO scheme
Copyright: Phillip Seeber 2024
License: BSD-3-Clause
Maintainer: phillip.seeber@uni-jena.de
Stability: experimental
Portability: POSIX
-}
module TREXIO.HighLevel.Records where

import Data.Map qualified as Map
import Data.Massiv.Array as Massiv hiding (Dim, forM)
import TREXIO.Internal.TH
import TREXIO.LowLevel.Scheme

$( do
    let trexio@(TrexioScheme trexioScheme) = scheme
        groups = Map.toList trexioScheme

    -- Generate records for all data groups
    dataGroupRecords <- traverse (uncurry mkRecord) groups

    -- Generate the TREXIO super record with fields for all data groups
    trexioSuperRecord <- mkTrexioScheme trexio

    return $ trexioSuperRecord : dataGroupRecords
 )