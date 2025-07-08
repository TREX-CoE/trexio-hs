import Control.Exception.Safe
import Control.Monad
import Data.Bit.ThreadSafe (Bit)
import Data.Massiv.Array as Massiv hiding (Size, elem, forM, forM_, mapM, mapM_, take, zip, zipWith)
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Hedgehog (Gen, MonadGen, Size, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory
import System.IO.Temp
import TREXIO
import TREXIO.CooArray
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "TREXIO"
        [ testGroup
            "0D"
            [ testGroup "Integers" . appFn $
                [ ("nucleus.num", genDim, deleteNucleus, hasNucleusNum, readNucleusNum, writeNucleusNum)
                , ("grid.max_ang_num", genPosInt, deleteGrid, hasGridMaxAngNum, readGridMaxAngNum, writeGridMaxAngNum)
                , ("state.id", genIndex, deleteState, hasStateId, readStateId, writeStateId)
                ]
            , testGroup "Floats" . appFn $
                [ ("nucleus.repulsion", genPosFloat, deleteNucleus, hasNucleusRepulsion, readNucleusRepulsion, writeNucleusRepulsion)
                ]
            , testGroup "Strings" . appFn $
                [ ("metadata.description", genIdentifier, deleteMetadata, hasMetadataDescription, readMetadataDescription, writeMetadataDescription)
                ]
            ]
        , testGroup
            "1D"
            [ testGroup "Integers" . appFn $
                [ ("ecp.ang_num", genVector genInt, deleteEcp, hasEcpAngMom, readEcpAngMom, writeEcpAngMom)
                ]
            , testGroup "Floats" . appFn $
                [ ("basis.shell_factor", genVector genFloat, deleteBasis, hasBasisShellFactor, readBasisShellFactor, writeBasisShellFactor)
                ]
            , testGroup "Strings" . appFn $
                [ ("metadata.author", genVector genIdentifier, deleteMetadata, hasMetadataAuthor, readMetadataAuthor, writeMetadataAuthor)
                ]
            , testCase "Determinant IO" . withSystemTempFile "trexio.dat" $ \fp _ ->
                withTrexio fp FileWrite Hdf5 $ \trexio -> do
                    writeMoNum trexio 3

                    has1 <- hasDeterminantList trexio
                    has1 @?= False

                    ingoreExcp [AttrMissing] (readDeterminantList trexio)

                    let detList =
                            Massiv.fromLists' @U @Ix2 @(Bit, Bit)
                                Seq
                                [ [(1, 1), (1, 0), (0, 0)]
                                , [(1, 0), (1, 1), (0, 0)]
                                ]
                        detCoeffs = Massiv.fromList @S @Double Seq [1.0, 2.0]

                    writeElectronUpNum trexio 2
                    writeElectronDnNum trexio 1

                    writeDeterminantList trexio detList

                    has2 <- hasDeterminantList trexio
                    has2 @?= True

                    detList' <- readDeterminantList trexio
                    detList' @?= detList

                    writeDeterminantCoefficient trexio detCoeffs

                    detCoeffs' <- readDeterminantCoefficient trexio
                    detCoeffs' @?= detCoeffs
            ]
        , testGroup
            "2D"
            [ testGroup "Dense" . appFn $
                [ ("ao_1e_int.overlap", genMatrix SameAs1 genFloat, deleteAo1eInt, hasAo1eIntOverlap, readAo1eIntOverlap, writeAo1eIntOverlap)
                ]
            , testGroup "Sparse" . appFn $
                [ ("amplitude.single", genSparseArr2 SameAs1 genFloat, deleteAmplitude, hasAmplitudeSingle, readAmplitudeSingle, writeAmplitudeSingle)
                ]
            ]
        , testGroup
            "3D"
            [ testGroup "Sparse" . appFn $
                [ ("mo_2e_int.eri_lr_cholesky", genSparseArr3 SameAs1 SameAs1 genFloat, deleteMo2eInt, hasMo2eIntEriLrCholesky, readMo2eIntEriLrCholesky, writeMo2eIntEriLrCholesky)
                ]
            ]
        , testGroup "4D" . appFn $
            [ ("mo_2e_int.eri", genSparseArr4 SameAs1 SameAs1 SameAs1 genFloat, deleteMo2eInt, hasMo2eIntEri, readMo2eIntEri, writeMo2eIntEri)
            ]
        , testGroup "6D" . appFn $
            [ ("amplitude.triple", genSparseArr6 SameAs1 SameAs1 SameAs1 SameAs1 SameAs1 genFloat, deleteAmplitude, hasAmplitudeTriple, readAmplitudeTriple, writeAmplitudeTriple)
            ]
        , testGroup "8D" . appFn $
            [ ("amplitude.quadruple", genSparseArr8 SameAs1 SameAs1 SameAs1 SameAs1 SameAs1 SameAs1 SameAs1 genFloat, deleteAmplitude, hasAmplitudeQuadruple, readAmplitudeQuadruple, writeAmplitudeQuadruple)
            ]
        ]
  where
    appFn :: (Eq a, Show a) => [(TestName, Gen a, Trexio -> IO (), Trexio -> IO Bool, Trexio -> IO a, Trexio -> a -> IO ())] -> [TestTree]
    appFn = fmap (\(name, val, delFn, hasFn, readFn, writeFn) -> testField name val delFn hasFn readFn writeFn)

ingoreExcp :: (MonadCatch m) => [ExitCode] -> m a -> m ()
ingoreExcp excps action = catch (void action) $ \e ->
    if e `elem` excps
        then return ()
        else throw e

testField ::
    (Eq a, Show a) =>
    -- | Name of the test
    TestName ->
    -- | Value to write
    Gen a ->
    -- | The group delete function
    (Trexio -> IO ()) ->
    -- | The "Has" function
    (Trexio -> IO Bool) ->
    -- | The "Read" function
    (Trexio -> IO a) ->
    -- | The "Write" function
    (Trexio -> a -> IO ()) ->
    TestTree
testField name gen delFn hasFn readFn writeFn = testProperty name . property $ do
    val <- forAll gen
    fp <- liftIO $ emptySystemTempFile "trexio.h5"

    trexio <- open fp FileUnsafe Hdf5

    -- Mark operations as unsafe
    safetyFlagU <- readMetadataUnsafe trexio
    safetyFlagU === 1
    markSafety trexio
    safetyFlagS <- readMetadataUnsafe trexio
    safetyFlagS === 0

    -- Nothing should be there yet
    has1 <- liftIO $ hasFn trexio
    has1 === False

    -- Reading should return a missing attribute exception
    ingoreExcp [AttrMissing] (liftIO $ readFn trexio)

    -- Write the value to the file
    liftIO $ writeFn trexio val

    -- Writing again should return an attribute already exists exception
    -- ingoreExcp [AttrAlreadyExists, DSetAlreadyExists] (write trexio val)

    -- Check if it is there
    has2 <- liftIO $ hasFn trexio
    has2 === True

    -- Read it back
    val' <- liftIO $ readFn trexio
    val' === val

    -- Delete the entire group
    liftIO $ delFn trexio

    -- Nothing should be there anymore
    has3 <- liftIO $ hasFn trexio
    has3 === False

    close trexio
    liftIO $ removeFile fp

data DimDep
    = Independent
    | SameAs1
    | SameAs2
    deriving (Show, Eq, Ord)

-- | Generate a random integer
genInt :: Gen Int
genInt = Gen.integral (Range.linearFrom 0 (-1_000_000) 1_000_000)

genPosInt :: Gen Int
genPosInt = Gen.integral (Range.linearFrom 0 0 100)

-- | Generate a @dim@ value, which is a positive integer
genDim :: Gen Int
genDim = Gen.integral (Range.linear 1 100)

-- | Generate a random index, which is a non-negative integer
genIndex :: Gen Int
genIndex = Gen.integral (Range.linear 0 1000)

genFloat :: Gen Double
genFloat = Gen.realFloat (Range.linearFrac (-1_000_000) 1_000_000)

genPosFloat :: Gen Double
genPosFloat = Gen.realFloat (Range.linearFrac 0 1_000_000)

-- | Generate a identifier, that is a single word without spaces or stuff
genIdentifier :: Gen Text
genIdentifier = Gen.text (Range.linear 1 10) Gen.alphaNum

-- | Generate a Massiv vector from elements from another generator
genVector :: (Manifest r a) => Gen a -> Gen (Vector r a)
genVector gen = do
    dim <- genDim
    Massiv.fromList Seq <$> Gen.list (Range.singleton dim) gen

-- | Generate a Massiv matrix from elements from another generator
genMatrix :: (Manifest r a, Ord a) => DimDep -> Gen a -> Gen (Matrix r a)
genMatrix dimDep2 gen = do
    rows <- genDim
    cols <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return rows
        SameAs2 -> error "genMatrix: SameAs2 not supported for Dim2"
    Massiv.fromLists' Seq <$> Gen.list (Range.singleton rows) (Gen.list (Range.singleton cols) gen)

genArr3 :: (Manifest r a, Ord a) => DimDep -> DimDep -> Gen a -> Gen (Array r Ix3 a)
genArr3 dimDep2 dimDep3 gen = do
    d1 <- genDim
    d2 <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> error "genArr3: SameAs2 not supported for Dim2"
    d3 <- case dimDep3 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    vals <- Gen.list (Range.singleton $ d1 * d2 * d3) gen
    pure . Massiv.resize' (Sz3 d1 d2 d3) $ Massiv.fromList Par vals

genSparseArr2 ::
    forall r a.
    (Manifest r a, Ord a, Manifest r Ix2, Stream r Ix1 Ix2) =>
    DimDep ->
    Gen a ->
    Gen (CooArray r Ix2 a)
genSparseArr2 dimDep2 gen = scaleSparse $ do
    d1 <- genDim
    d2 <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> error "genSparseArr2: SameAs2 not supported for Dim2"
    let sz = Sz2 d1 d2
    cooVals <-
        catMaybes . Set.toAscList
            <$> Gen.set
                (Range.singleton $ d1 * d2)
                ( do
                    c1 <- Gen.int $ Range.linear 0 (d1 - 1)
                    c2 <- Gen.int $ Range.linear 0 (d2 - 1)
                    v <- gen
                    Gen.maybe . pure $ (c1 :. c2, v)
                )
    if null cooVals
        then Gen.discard
        else pure . fromJust $ mkCooArrayF sz cooVals

genSparseArr3 ::
    forall r a.
    (Manifest r a, Manifest r Ix3, Ord a, Stream r Ix1 Ix3) =>
    DimDep ->
    DimDep ->
    Gen a ->
    Gen (CooArray r Ix3 a)
genSparseArr3 dimDep2 dimDep3 gen = scaleSparse $ do
    d1 <- genDim
    d2 <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> error "genSparseArr3: SameAs2 not supported for Dim2"
    d3 <- case dimDep3 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    let sz = Sz3 d1 d2 d3
    cooVals <-
        catMaybes . Set.toAscList
            <$> Gen.set
                (Range.singleton $ d1 * d2 * d3)
                ( do
                    c1 <- Gen.int $ Range.linear 0 (d1 - 1)
                    c2 <- Gen.int $ Range.linear 0 (d2 - 1)
                    c3 <- Gen.int $ Range.linear 0 (d3 - 1)
                    v <- gen
                    Gen.maybe . pure $ (c1 :> c2 :. c3, v)
                )
    if null cooVals
        then Gen.discard
        else pure . fromJust $ mkCooArrayF sz cooVals

genSparseArr4 ::
    forall r a.
    (Manifest r a, Ord a, Manifest r Ix4, Stream r Ix1 Ix4) =>
    DimDep ->
    DimDep ->
    DimDep ->
    Gen a ->
    Gen (CooArray r Ix4 a)
genSparseArr4 dimDep2 dimDep3 dimDep4 gen = scaleSparse $ do
    d1 <- genDim
    d2 <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> error "genSparseArr4: SameAs2 not implemented"
    d3 <- case dimDep3 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d4 <- case dimDep4 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    let sz = Sz4 d1 d2 d3 d4
    cooVals <-
        catMaybes . Set.toAscList
            <$> Gen.set
                (Range.singleton $ d1 * d2 * d3)
                ( do
                    c1 <- Gen.int $ Range.linear 0 (d1 - 1)
                    c2 <- Gen.int $ Range.linear 0 (d2 - 1)
                    c3 <- Gen.int $ Range.linear 0 (d3 - 1)
                    c4 <- Gen.int $ Range.linear 0 (d4 - 1)
                    v <- gen
                    Gen.maybe . pure $ (c1 :> c2 :> c3 :. c4, v)
                )
    if null cooVals
        then Gen.discard
        else pure . fromJust $ mkCooArrayF sz cooVals

genSparseArr6 ::
    forall r a.
    (Manifest r a, Ord a, Manifest r (IxN 6), Stream r Ix1 (IxN 6)) =>
    DimDep ->
    DimDep ->
    DimDep ->
    DimDep ->
    DimDep ->
    Gen a ->
    Gen (CooArray r (IxN 6) a)
genSparseArr6 dimDep2 dimDep3 dimDep4 dimDep5 dimDep6 gen = scaleSparse $ do
    d1 <- genDim
    d2 <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> error "genSparseArr4: SameAs2 not implemented"
    d3 <- case dimDep3 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d4 <- case dimDep4 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d5 <- case dimDep5 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d6 <- case dimDep6 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    let sz = Sz $ d1 :> d2 :> d3 :> d4 :> d5 :. d6
    cooVals <-
        catMaybes . Set.toAscList
            <$> Gen.set
                (Range.singleton $ d1 * d2 * d3)
                ( do
                    c1 <- Gen.int $ Range.linear 0 (d1 - 1)
                    c2 <- Gen.int $ Range.linear 0 (d2 - 1)
                    c3 <- Gen.int $ Range.linear 0 (d3 - 1)
                    c4 <- Gen.int $ Range.linear 0 (d4 - 1)
                    c5 <- Gen.int $ Range.linear 0 (d5 - 1)
                    c6 <- Gen.int $ Range.linear 0 (d6 - 1)
                    v <- gen
                    Gen.maybe . pure $ (c1 :> c2 :> c3 :> c4 :> c5 :. c6, v)
                )
    if null cooVals
        then Gen.discard
        else pure . fromJust $ mkCooArrayF sz cooVals

genSparseArr8 ::
    forall r a.
    (Manifest r a, Ord a, Manifest r (IxN 8), Stream r Ix1 (IxN 8)) =>
    DimDep ->
    DimDep ->
    DimDep ->
    DimDep ->
    DimDep ->
    DimDep ->
    DimDep ->
    Gen a ->
    Gen (CooArray r (IxN 8) a)
genSparseArr8 dimDep2 dimDep3 dimDep4 dimDep5 dimDep6 dimDep7 dimDep8 gen = scaleSparse $ do
    d1 <- genDim
    d2 <- case dimDep2 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> error "genSparseArr4: SameAs2 not implemented"
    d3 <- case dimDep3 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d4 <- case dimDep4 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d5 <- case dimDep5 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d6 <- case dimDep6 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d7 <- case dimDep7 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    d8 <- case dimDep8 of
        Independent -> genDim
        SameAs1 -> return d1
        SameAs2 -> return d2
    let sz = Sz $ d1 :> d2 :> d3 :> d4 :> d5 :> d6 :> d7 :. d8
    cooVals <-
        catMaybes . Set.toAscList
            <$> Gen.set
                (Range.singleton $ d1 * d2 * d3)
                ( do
                    c1 <- Gen.int $ Range.linear 0 (d1 - 1)
                    c2 <- Gen.int $ Range.linear 0 (d2 - 1)
                    c3 <- Gen.int $ Range.linear 0 (d3 - 1)
                    c4 <- Gen.int $ Range.linear 0 (d4 - 1)
                    c5 <- Gen.int $ Range.linear 0 (d5 - 1)
                    c6 <- Gen.int $ Range.linear 0 (d6 - 1)
                    c7 <- Gen.int $ Range.linear 0 (d7 - 1)
                    c8 <- Gen.int $ Range.linear 0 (d8 - 1)
                    v <- gen
                    Gen.maybe . pure $ (c1 :> c2 :> c3 :> c4 :> c5 :> c6 :> c7 :. c8, v)
                )
    if null cooVals
        then Gen.discard
        else pure . fromJust $ mkCooArrayF sz cooVals

scaleSparse :: (MonadGen m) => m a -> m a
scaleSparse = Gen.scale sz2zs
  where
    sz2zs :: Size -> Size
    sz2zs x = round $ fromIntegral x * (0.25 :: Double)
