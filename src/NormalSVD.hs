{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields, TupleSections #-}
module NormalSVD (normalizeSVD) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sortOn, sort, nub)
import Data.List.Extra (groupSort)
import Data.Hashable
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Char (isDigit)
import System.FilePath
import System.Directory
import Control.Monad
import ParseSVD
import FixupSVD
import ClockControl
import PrettySVD
import PrettyVector
import Utils

instance Hashable Peripheral where
    hashWithSalt h Peripheral{..} = hashWithSalt h
        ( sortOn addressOffset registers
        , derivedFrom
        )

instance Hashable Interrupt where
    hashWithSalt h Interrupt{..} = hashWithSalt h
        ( name
        , value
        )

instance Hashable Register where
    hashWithSalt h Register{..} = hashWithSalt h
        ( name
        , addressOffset
        , resetValue
        , sortOn bitOffset fields
        )

instance Hashable Field where
    hashWithSalt h Field{..} = hashWithSalt h
        ( name
        , bitOffset
        , bitWidth
        )

data Normalization
    = Representative
    { svdName       :: !Text
    , name          :: !Text
    , baseAddress   :: !Int
    , groupName     :: !Text
    , digest        :: !Int
    , text          :: !FilePath
    }
    | Duplicate
    { svdName       :: !Text
    , name          :: !Text
    , baseAddress   :: !Int
    , digest        :: !Int
    }
    | Derived
    { svdName       :: !Text
    , name          :: !Text
    , baseAddress   :: !Int
    , derivedFrom   :: !Text
    } deriving (Show)

normalizeSVD
    :: FilePath
    -> FilePath
    -> Text
    -> [(Text, [Text])]     -- [(peripheral, (alternate-function])]
    -> [(Text, FilePath)]
    -> IO ()
normalizeSVD tmp dir family pfs xs = do
    mapM_ print pfs
    putStrLn $ T.unpack family <> " in " <> tmp
    (ys, rs, is) <- fmap unzip3 $ forM xs $ \(_, fn) -> do
        putStrLn $ "parsing " <> fn
        svd@SVD{..} <- parseSVD fn
        ys <- forM peripherals
            $ processPeripheral tmp family name
            . fixupPeripheral family
        let rs = mapMaybe (rccFlags name) peripherals
        return $! (ys, rs, interrupts)
    ys <- return $ concat ys
    let ds = groupSort $ map (remap $ digests ys) ys
        gs = groupSort
            [ (groupName, (r, fromMaybe [] $ lookup digest ds))
            | r@Representative{..} <- ys
            ]
    peripheralHeader dir family (concatMap snd ds)
    genPeripheralDefs dir family $ concatMap snd gs
    mapM_ (uncurry $ genHeader dir family pfs) gs
    comboHeader dir family $ map fst gs
    interruptHeader dir family $ concat is
    vectorHeader dir family $ concat is
    controlHeader dir family $ concat rs

genHeader
    :: FilePath
    -> Text
    -> [(Text, [Text])]
    -> Text
    -> [(Normalization, [(Text, Text, Int)])]
    -> IO ()
genHeader dir family pfs group rs = do
    hs <- forM (sortOn f rs) $ \(Representative{..}, _) -> T.readFile text
    let header = dir </> lower group <.> "h"
    putStrLn $ "writing " <> header
    writeText header $ preamble ++ hs
        ++ concatMap (uncurry genTraits) rs
        ++ [ "" ]
        ++ map genUsing names
        ++ genInstanceTraits pfs names
        ++ [ "" ]
    where f :: (Normalization, [(Text, Text, Int)]) -> Text
          f (Representative{..}, _) = name
          f _ = error "expected representative"
          preamble = "#pragma once" : banner
            [ family <> " " <> group <> " peripherals"
            ]
          names = nub $ sort [ name | (_, name, _) <- concatMap snd rs ]

genPeripheralDefs
    :: FilePath
    -> Text
    -> [(Normalization, [(Text, Text, Int)])]
    -> IO ()
genPeripheralDefs dir family rs = do
    let header = dir </> "peripheral" <.> "cpp"
    putStrLn $ "writing " <> header
    writeText header
        $ "#pragma once"
        : banner [ family <> " peripherals" ]
        ++ concatMap (uncurry genTraitsDefs) rs
        ++ [ "" ]

peripheralHeader
    :: FilePath
    -> Text
    -> [(Text, Text, Int)]
    -> IO ()
peripheralHeader dir family peripherals = do
    let header = dir </> "peripheral" <.> "h"
    putStrLn $ "writing " <> header
    writeText header
        $ "#pragma once"
        : banner [ family <> " peripherals" ]
        ++ enum "peripheral_enum_t" perips
        ++ [ ""
           , "template<mcu_svd_t SVD, peripheral_enum_t PERIPHERAL>"
           , "struct peripheral_t"
           , "{"
           , "    static_assert"
           , "        ( always_false_i<SVD>::value"
           , "        , \"peripheral not available on MCU!\""
           , "        );"
           , "};"
           , ""
           ]
    where perips = nub $ sort [ p | (_, p, _) <- peripherals ]

comboHeader
    :: FilePath
    -> Text
    -> [Text]
    -> IO ()
comboHeader dir family groups = do
    let header = dir </> "all" <.> "h"
    putStrLn $ "writing " <> header
    writeText header
        $ "#pragma once"
        : banner [ family <> " peripherals" ]
        ++ [ "" ]
        ++ map f groups
        ++ [ "" ]
        where f x = "#include \"" <> T.toLower x <> ".h\""

interruptHeader
    :: FilePath
    -> Text
    -> [Interrupt]
    -> IO ()
interruptHeader dir family xs = do
    let header = dir </> "interrupt" <.> "h"
    putStrLn $ "writing " <> header
    writeText header
        $ "#pragma once"
        : banner [ family <> " interrupts" ]
        ++ prettyInterrupt xs

vectorHeader
    :: FilePath
    -> Text
    -> [Interrupt]
    -> IO ()
vectorHeader dir family xs = do
    let header = dir </> "vector" <.> "h"
    putStrLn $ "writing " <> header
    writeText header
        $ "#pragma once"
        : banner [ family <> " vectors" ]
        ++ prettyVector xs

controlHeader
    :: FilePath
    -> Text
    -> [(Text, [(Text, Text)])]
    -> IO ()
controlHeader dir family xs = do
    let header = dir </> "control" <.> "h"
    putStrLn $ "writing " <> header
    writeText header
        $ "#pragma once"
        : banner [ family <> " peripheral clock control" ]
        ++ [ ""
           , "template<typename PERIPHERAL>"
           , "struct clock_control_t {};"
           ]
        ++ concatMap (uncurry prettyRCC) xs

genTraits
    :: Normalization
    -> [(Text, Text, Int)]
    -> [Text]
genTraits Representative{..}
    = concatMap (genTrait . ((svdName, name),))
genTraits _
    = error "exprected representative"

genTrait
    :: ((Text, Text), (Text, Text, Int))
    -> [Text]
genTrait ((repSvd, repName), (svd, name, addr)) =
    [ ""
    , "template<>"
    , "struct peripheral_t<" <> svd <> ", " <> name <> ">"
    , "{"
    , "    typedef " <> T.toLower repSvd
                     <> "_"
                     <> T.toLower repName
                     <> "_t T;"
    , "    static T& V;"
    , "};"
    ]

genTraitsDefs
    :: Normalization
    -> [(Text, Text, Int)]
    -> [Text]
genTraitsDefs Representative{..}
    = concatMap (genTraitDef . ((svdName, name),))
genTraitsDefs _
    = error "exprected representative"

genTraitDef
    :: ((Text, Text), (Text, Text, Int))
    -> [Text]
genTraitDef ((repSvd, repName), (svd, name, addr)) =
    [ ""
    , "typename " <> s <> "::T& " <> s <> "::V ="
    , "    "
    <> "*reinterpret_cast<typename " <> s <> "::T*>"
    <> "(" <> hex addr <> ");"
    ]
    where s = "peripheral_t<" <> svd <> ", " <> name <> ">"

genUsing :: Text -> Text
genUsing name
    = "using "
    <> T.toLower name
    <> "_t"
    <> " = peripheral_t<mcu_svd, "
    <> name
    <> ">;"

genInstanceTraits :: [(Text, [Text])] -> [Text] -> [Text]
genInstanceTraits pfs names = concatMap (uncurry g) xs
    where xs = groupSort [ p | p@(_, n) <- map f names, n /= "" ]
          f s | Just x <- T.stripPrefix "I2C" s = ("I2C", x)
              | otherwise = T.break isDigit s
          g :: Text -> [Text] -> [Text]
          g prefix insts =
            [ ""
            , "template<int INST> struct "
            <> lprefix <> "_traits {};"
            ] ++ concatMap h insts
            where h inst =
                    [ ""
                    , "template<> struct "
                    <> lprefix <> "_traits<" <> inst <> ">"
                    , "{"
                    , "    using " <> lprefix <> " = " <> lprefix
                    <> inst <> "_t;"
                    ] ++
                    map (altfun inst) (fromMaybe [] $ lookup (prefix <> inst) pfs) ++
                    [ "};"
                    ]
                  lprefix = T.toLower prefix
                  altfun inst fun = T.concat
                    [ "    "
                    , "static constexpr alternate_function_t "
                    , fun
                    , " = "
                    , prefix <> inst
                    , "_"
                    , fun
                    , ";"
                    ]

processPeripheral
    :: FilePath
    -> Text
    -> Text
    -> Peripheral
    -> IO Normalization
processPeripheral _ _ svdName Peripheral{derivedFrom=Just s,..} =
    return $! Derived{derivedFrom=s,..}
processPeripheral tmp family svdName p@Peripheral{..} = do
    let h = hash p
        fn = tmp </> T.unpack (hex (abs h)) <.> "h"
    already <- doesFileExist fn
    if already then do
        putStrLn $ T.unpack name <> " normalized"
        return $! Duplicate{digest=h,..}
    else do
        putStr $ T.unpack name <> fn <> "..."
        writeText fn $ prettyPeripheral svdName p
        putStrLn $ "done"
        return $! Representative{digest=h,text=fn,..}

digests :: [Normalization] -> [((Text, Text), Int)]
digests = mapMaybe f
    where f Representative{..} = Just ((svdName, name), digest)
          f Duplicate{..} = Just ((svdName, name), digest)
          f _ = Nothing

remap
    :: [((Text, Text), Int)]
    -> Normalization
    -> (Int, (Text, Text, Int))
remap _ Representative{..} = (digest, (svdName, name, baseAddress))
remap _ Duplicate{..} = (digest, (svdName, name, baseAddress))
remap ss Derived{..} = (digest, (svdName, name, baseAddress))
    where digest = fromMaybe (error $ "failed to derive " <> qname)
                 $ lookup (svdName, derivedFrom) ss
          qname = T.unpack $ svdName <> "." <> derivedFrom

lower :: Text -> String
lower = T.unpack . T.toLower

