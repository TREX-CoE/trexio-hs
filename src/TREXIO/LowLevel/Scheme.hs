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

import Language.Haskell.TH
import TREXIO.Internal.TH
import Language.Haskell.TH.Syntax (lift)

-- | The JSON specification of the code generator, that constructs the C-API and
-- that this package binds to.
scheme :: TrexioScheme
scheme = $(do
    trexio <- runIO getJsonSpec
    lift trexio
  )