{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FrontEnd
    ( Family(..)
    , Mcu(..)
    , PeriphType(..)
    , PeriphRef(..)
    , PeriphInst(..)
    , Register(..)
    , Reserve(..)
    , Field(..)
    , Interrupt(..)
    , CCMap
    , MCU(..)
    , Pin(..)
    , Signal(..)
    , IpGPIO(..)
    , parseFamily
    , ipGPIOName
    ) where

import System.FilePath
import Data.List (find, sort, nub)
import Data.List.Extra (groupSort)
import Data.Text as T (pack, unpack, isPrefixOf, break)
import Families hiding (Peripheral)
import FrontEnd.ParseSVD hiding (Peripheral)
import FrontEnd.ParseMCU
import FrontEnd.ParseIpGPIO
import FrontEnd.Normalize
import FrontEnd.Fixup
import FrontEnd.ClockControl
import Utils

data Family = Family
    { family        :: Text
    , mcus          :: [Mcu]
    , svds          :: [Text]
    , peripherals   :: [(Text, ([PeriphType Reserve], [Peripheral]))]
    , interrupts    :: [Interrupt]
    , specs         :: [MCU]
    , ipGPIOs       :: [IpGPIO]
    } deriving (Show)

data Peripheral = Peripheral
    { name      :: !Text
    , instNo    :: !Int
    , altFuns   :: ![Void]
    } deriving (Show, Eq, Ord)

parseFamily
    :: FilePath             -- ^ STM32CubeIDE install path
    -> FilePath             -- ^ STM32CubeMX database directory
    -> Text                 -- ^ family name
    -> [SubFamily]          -- ^ sub-families from catalogue
    -> IO Family            -- ^ raw family data
parseFamily svdDir dbDir family subFamilies = do
    let mcus = concatMap snd subFamilies
    svds <- familySVDs family <$> cacheLines svdFiles svdDir
    svd <- resolveCC . fixup . normalize family
       <$> mapM parseSVD (map snd svds)
    specs <- mapM (parseMCU $ map fst svds) $ mcuFiles dbDir subFamilies
    ipGPIOs <- map (fixupIpGPIO $ svdNames svd)
           <$> mapM parseIpGPIO (ipGPIOFiles dbDir specs)
    let peripherals = processPeripherals svd
        interrupts = (\NormalSVD{..} -> interrupts) svd
    return Family{svds=svdNames svd,..}

familySVDs :: Text -> [FilePath] -> [(Text, FilePath)]
familySVDs family = sort . filter pred . map f
    where f s = (pack $ dropExtension $ takeFileName s, s)
          pred (x, _)
            | fam == "STM32L4+" = isL4plus x
            | fam == "STM32G4" = any (`isPrefixOf` x) [ fam, "STM32GBK1" ]
            | otherwise = fam `isPrefixOf` x && not (isL4plus x)
            where fam = family

isL4plus :: Text -> Bool
isL4plus x = any (`isPrefixOf`x) $ map ("STM32L4"<>) [ "P", "Q", "R", "S" ]

svdFiles :: FilePath -> IO [FilePath]
svdFiles = traverseDir (\_ -> True) accept []
    where accept :: [FilePath] -> FilePath -> IO [FilePath]
          accept xs fp
            | takeExtension fp == ".svd" = return $ fp : xs
            | otherwise = return xs

mcuFiles :: FilePath -> [SubFamily] -> [FilePath]
mcuFiles dir = map f . mcuNames
    where f x = dir </> unpack x <.> "xml"

ipGPIOFiles :: FilePath -> [MCU] -> [FilePath]
ipGPIOFiles dir mcus = map f $ nub $ sort
    [ version | MCU{..} <- mcus , IP{name="GPIO",..} <- ips ]
    where f x = dir </> "IP" </> "GPIO-" <> unpack x <> "_Modes" <.> "xml"

ipGPIOName :: MCU -> Text
ipGPIOName MCU{..}
    | Just IP{..} <- find (\IP{..} -> name == "GPIO") ips
    = fst $ T.break (=='_') version
    | otherwise = error $ "failed to detepmine IpGPIO for " <> unpack refName

svdNames :: NormalSVD a b -> [Text]
svdNames NormalSVD{..} = nub $ sort
    [ svd | PeriphType{..} <- periphTypes, PeriphRef{..} <- [ typeRef ] ]

fixupIpGPIO :: [Text] -> IpGPIO -> IpGPIO
fixupIpGPIO svds x@IpGPIO{..}
    | name' `elem` svds = x { name = name' <> "_" } -- to avoid collissions
    | otherwise = x { name = name' }
    where name' = fst $ T.break (=='_') version

processPeripherals
    :: NormalSVD a Reserve
    -> [(Text, ([PeriphType Reserve], [Peripheral]))]
processPeripherals NormalSVD{..} = map f $ groupSort xs
    where xs = [ (groupName, p) | p@PeriphType{..} <- periphTypes ]
          f (groupName, ps) = (groupName, (ps, map g xs))
              where xs = nub $ sort
                        [ name
                        | PeriphType{..} <- ps
                        , PeriphInst{..} <- periphInsts
                        , PeriphRef{..} <- [ instRef ]
                        ]
          g name = let (_, instNo) = nameNum name
                       altFuns = []
                    in Peripheral{..}
{-
periphMap :: NormalSVD a b -> Map.Map Text [Periph]
periphMap NormalSVD{..}
    = Map.fromList $ groupSort $ nub $ sort $ concatMap f periphTypes
    where f :: PeriphType b -> [(Text, Periph)]
          f PeriphType{..} =
            [ let altFuns = []
                  instNo = undefined
               in (groupName, Periph{..})
            | PeriphInst{..} <- periphInsts
            , PeriphRef{..} <- [ instRef ]
            ]
          exclude = [ "ADC12_COMMON"
                    , "ADC345_COMMON"
                    ]
-}
