{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields, TupleSections #-}
module NormalSVD (normalizeSVD) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (partition, sortOn, sort, nub)
import Data.List.Extra (groupSort)
import Data.Hashable
import Data.Maybe (fromMaybe, mapMaybe)
import System.FilePath
import System.Directory
import Control.Monad
import Numeric (showHex)
import ParseSVD
import FixupSVD
import PrettySVD

type Text = T.Text

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
    , groupName     :: !(Maybe Text)
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

normalizeSVD :: FilePath -> FilePath -> Text -> [(Text, FilePath)] -> IO ()
normalizeSVD tmp dir family xs = do
    putStrLn $ T.unpack family <> " in " <> tmp
    ys <- fmap concat $ forM xs $ \(x, fn) -> do
        putStrLn $ "parsing " <> fn
        SVD{..} <- parseSVD <$> T.readFile fn
        mapM (processPeripheral tmp family name) peripherals
    let ds = groupSort $ map (remap $ digests ys) ys
        gs = groupSort
            [ (groupName, (r, fromMaybe [] $ lookup digest ds))
            | r@Representative{..} <- ys
            ]
    dir <- return $ dir </> lower family
    createDirectoryIfMissing False dir
    familyHeader dir family (concatMap snd ds)
    mapM_ (uncurry $ genHeader dir family) gs
    comboHeader dir family $ map fst gs

genHeader
    :: FilePath
    -> Text
    -> Maybe Text
    -> [(Normalization, [(Text, Text, Int)])]
    -> IO ()
genHeader dir family group rs = do
    hs <- forM (sortOn f rs) $ \(Representative{..}, _) -> T.readFile text
    let header = dir </> maybe "other" lower group <.> "h"
    putStrLn $ "writing " <> header
    T.writeFile header $ T.unlines $ preamble ++ hs
        ++ concatMap (uncurry genTraits) rs
        ++ map genUsing (nub $ sort [ name | (_, name, _) <- concatMap snd rs ])
        ++ [ "" ]
    where f :: (Normalization, [(Text, Text, Int)]) -> Text
          f (Representative{..}, _) = name
          preamble = "#pragma once" : banner
            [ family <> " " <> fromMaybe "other" group <> " peripherals"
            ]

familyHeader
    :: FilePath
    -> Text
    -> [(Text, Text, Int)]
    -> IO ()
familyHeader dir family peripherals = do
    let header = dir </> "family" <.> "h"
    putStrLn $ "writing " <> header
    T.writeFile header $ T.unlines
        $ "#pragma once"
        : banner [ family <> " members" ]
        ++ enum "family_member_t" svds
        ++ enum "peripheral_enum_t" perips
        ++ [ "" ]
    where svds = nub $ sort [ svd | (svd, _, _) <- peripherals ]
          perips = nub $ sort [ p | (_, p, _) <- peripherals ]

comboHeader
    :: FilePath
    -> Text
    -> [Maybe Text]
    -> IO ()
comboHeader dir family groups = do
    let header = dir </> "peripheral" <.> "h"
    putStrLn $ "writing " <> header
    T.writeFile header $ T.unlines
        $ "#pragma once"
        : banner [ family <> " peripherals" ]
        ++ [ "" ]
        ++ map f groups
        ++ [ "" ]
        where f x = "#include \"" <> maybe "other" T.toLower x <> ".h\""

genTraits :: Normalization -> [(Text, Text, Int)] -> [Text]
genTraits Representative{..} = concatMap (genTrait . ((svdName, name),))

genTrait :: ((Text, Text), (Text, Text, Int)) -> [Text]
genTrait ((repSvd, repName), (svd, name, addr)) =
    [ "template<>"
    , "struct peripheral_t<" <> svd <> ", " <> name <> ">"
    , "{"
    , "    typedef " <> T.toLower repSvd
                     <> "_"
                     <> T.toLower repName
                     <> "_t type;"
    , "    static constexpr uint32_t base_address = " <> hex addr <> ";"
    , "};"
    , ""
    ]

genUsing :: Text -> Text
genUsing name
    = "using "
    <> T.toLower name
    <> "_t"
    <> " = peripheral_t<family_member, "
    <> name
    <> ">;"

processPeripheral
    :: FilePath
    -> Text
    -> Text
    -> Peripheral
    -> IO Normalization
processPeripheral tmp family svdName p@Peripheral{derivedFrom=Just s,..} =
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
        T.writeFile fn
            $ T.unlines
            . prettyPeripheral
            . qualify svdName
            $ fixupPeripheral family p
        putStrLn $ "done"
        return $! Representative{digest=h,text=fn,..}

digests :: [Normalization] -> [((Text, Text), Int)]
digests = mapMaybe f
    where f Representative{..} = Just ((svdName, name), digest)
          f Duplicate{..} = Just ((svdName, name), digest)
          f _ = Nothing

remap :: [((Text, Text), Int)] -> Normalization -> (Int, (Text, Text, Int))
remap _ Representative{..} = (digest, (svdName, name, baseAddress))
remap _ Duplicate{..} = (digest, (svdName, name, baseAddress))
remap ss Derived{..} = (digest, (svdName, name, baseAddress))
    where digest = fromMaybe (error $ "failed to derive " <> qname)
                 $ lookup (svdName, derivedFrom) ss
          qname = T.unpack $ svdName <> "." <> derivedFrom

qualify :: Text -> Peripheral -> Peripheral
qualify svdName p@Peripheral{..} = p { name = svdName <> "_" <> name }

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

lower :: Text -> String
lower = T.unpack . T.toLower

enum :: Text -> [Text] -> [Text]
enum name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x
      | (s, x) <- zip ("    { " : repeat "    , ") xs
      ]
    , [ "    };" ]
    ]

