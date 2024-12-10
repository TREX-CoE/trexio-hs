{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: TREXIO.LowLevel.Scheme
Description: The TREXIO scheme
Copyright: Phillip Seeber 2024
License: BSD-3-Clause
Maintainer: phillip.seeber@uni-jena.de
Stability: experimental
Portability: POSIX
-}
module TREXIO.LowLevel.Scheme where

import Data.Aeson
import Language.Haskell.TH
import TREXIO.Internal.TH
import Language.Haskell.TH.Syntax (lift)

scheme :: TrexioScheme
scheme = $(do
    Just trexio <- runIO $ decodeFileStrict @TrexioScheme "./data/trexio.json"
    lift trexio
  )