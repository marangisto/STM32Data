{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, TupleSections #-}
module FrontEnd
    ( Family(..)
    , Mcu(..)
    , Peripheral(..)
    , PeriphType(..)
    , PeriphRef(..)
    , PeriphInst(..)
    , Register(..)
    , Reserve(..)
    , Field(..)
    , Interrupt(..)
    , ClockControl(..)
    , CCMap
    , MCU(..)
    , Pin(..)
    , GPIO(..)
    , Signal(..)
    , processFamily
    , peripheralNames
    , peripheralInsts
    , ipGPIOName
    , configNames
    , signalNames
    , altfunNames
    ) where

import Prelude as P
import System.FilePath
import Data.Char (ord)
import Data.Bits (shift)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find, sort, nub)
import Data.List.Extra (groupSort, sortOn, zipWithFrom)
import Data.Text as T (pack, unpack, break)
import Data.Text as T (head, tail, length, snoc)
import Data.Text as T (isPrefixOf, isSuffixOf)
import Data.Text as T (stripPrefix, stripSuffix)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Families hiding (Peripheral)
import FrontEnd.ParseSVD hiding (Peripheral)
import FrontEnd.ParseMCU hiding (Signal)
import FrontEnd.ParseIpGPIO
import FrontEnd.Normalize hiding (peripheralNames)
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
    , gpio          :: GPIO
    } deriving (Show)

data Peripheral = Peripheral
    { name      :: !Text
    , instNo    :: !(Maybe Int)
    , altFuns   :: ![Text]
    , control   :: !(Maybe ClockControl)
    } deriving (Show)

data GPIO = GPIO
    { ports     :: ![(Text, Int)]
    , pins      :: ![(Text, Int)]
    , signals   :: ![Signal]
    } deriving (Show)

data Signal = Signal
    { pin       :: !Text
    , signal    :: !Text
    , altfun    :: !Text
    , configs   :: ![Text]
    , partial   :: !Bool
    } deriving (Show)

processFamily
    :: FilePath             -- ^ STM32CubeIDE install path
    -> FilePath             -- ^ STM32CubeMX database directory
    -> Text                 -- ^ family name
    -> [SubFamily]          -- ^ sub-families from catalogue
    -> IO Family            -- ^ raw family data
processFamily svdDir dbDir family subFamilies = do
    let mcus = concatMap snd subFamilies
    svds <- familySVDs family <$> cacheLines svdFiles svdDir
    svd <- resolveCC . fixup . normalize family
       <$> mapM parseSVD (map snd svds)
    specs <- mapM (parseMCU $ map fst svds) $ mcuFiles dbDir subFamilies
    ipGPIOs <- map (fixupAF . fixupIpGPIO (svdNames svd))
           <$> mapM parseIpGPIO (ipGPIOFiles dbDir specs)
    let peripherals = processPeripherals svd $ altFunMap ipGPIOs
        interrupts = (\NormalSVD{..} -> interrupts) svd
        gpio = processGPIO specs ipGPIOs
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

fixupAF :: IpGPIO -> IpGPIO
fixupAF x@IpGPIO{..} = x { gpioPins = map f gpioPins }
    where f :: GPIOPin -> GPIOPin
          f y@GPIOPin{..} = y { pinSignals = mapMaybe g pinSignals }
          g :: PinSignal -> Maybe PinSignal
          g z@PinSignal{..} = (\af -> z { gpioAF = af }) <$> h gpioAF
          h s | Just rest <- stripPrefix "GPIO_" s
              = Just $ fst $ T.break (=='_') rest
              | otherwise = Nothing   -- FIXME: all the F1 remap crap comes here

processPeripherals
    :: NormalSVD CCMap Reserve
    -> Map.Map Text [Text]
    -> [(Text, ([PeriphType Reserve], [Peripheral]))]
processPeripherals NormalSVD{..} afMap = map f $ groupSort xs
    where xs = [ (groupName, p) | p@PeriphType{..} <- periphTypes ]
          f (groupName, ps) = (groupName, (ps, xs))
              where xs = map g $ nub $ sort
                        [ name
                        | PeriphType{..} <- ps
                        , PeriphInst{..} <- periphInsts
                        , PeriphRef{..} <- [ instRef ]
                        ]
          g name = let instNo = instanceNo name
                       altFuns = fromMaybe [] $ Map.lookup name afMap
                       control = Map.lookup name clockControl
                    in Peripheral{..}

instanceNo :: Text -> Maybe Int
instanceNo name
    | Just s <- stripSuffix "_COMMON" name
    = Just $ snd $ nameNum s
    | Just s <- stripPrefix "GPIO" name
    , T.length s == 1
    = Just $ ord (T.head s) - ord 'A'
    | otherwise = Just $ snd $ nameNum name

altFunMap :: [IpGPIO] -> Map.Map Text [Text]
altFunMap = Map.fromList . groupSort . nub . sort . concatMap f
    where f :: IpGPIO -> [(Text, Text)]
          f IpGPIO{..} = concatMap g gpioPins
          g GPIOPin{..} = mapMaybe h pinSignals
          h PinSignal{..}
              | sig == "" = Nothing
              | otherwise = Just (dev, T.tail sig)
              where (dev, sig) = T.break (=='_') name

peripheralNames :: Family -> [Text]
peripheralNames Family{..} = 
    [ name 
    | (_, (_, ps)) <- peripherals
    , Peripheral{..} <- ps
    ]

peripheralInsts :: Family -> [PeriphInst]
peripheralInsts Family{..} = 
    [ pi
    | (_, (pts, _)) <- peripherals
    , PeriphType{..} <- pts
    , pi <- periphInsts
    ]

processGPIO :: [MCU] -> [IpGPIO] -> GPIO
processGPIO mcus ipGPIOs = GPIO{..}
    where ports = nub $ sort [ (port, portNo) | IOPin{..} <- xs ]
          pins = [ (pin, portNo * 16 + pinNo) | IOPin{..} <- xs ]
          signals = toSignals ipGPIOs
          xs = ioPins mcus

data IOPin = IOPin
    { portNo    :: !Int     -- order on this first
    , pinNo     :: !Int     -- and second on this
    , port      :: !Text
    , pin       :: !Text
    } deriving (Show, Eq, Ord)

ioPins :: [MCU] -> [IOPin]
ioPins mcus = nub $ sort
    [ portPin
    | MCU{..} <- mcus
    , Pin{..} <- pins
    , Just portPin <- [ toIOPin name ]
    ]

toIOPin :: Text -> Maybe IOPin
toIOPin pin
    | ('P':p:xs) <- unpack pin, Just pinNo <- readMaybe xs
    = Just IOPin{port=T.snoc "P" p, portNo=ord p - ord 'A',..}
    | otherwise = Nothing

toSignals :: [IpGPIO] -> [Signal]
toSignals xs =
    [ let partial = P.length configs /= n in Signal{..}
    | ((pin, signal, altfun), configs) <- groupSort $ concatMap f xs
    ]
    where f IpGPIO{..} = concatMap (g name) gpioPins
          g conf GPIOPin{..} = map (h conf name) pinSignals
          h conf pin PinSignal{..} = ((pin, name, gpioAF), conf)
          n = P.length xs

configNames :: [Signal] -> [(Text, Int)]
configNames = zipWithFrom f 0 . nub . sort . concatMap configs
    where f i x = (x, shift 1 i)

altfunNames :: [Signal] -> [(Text, Int)]
altfunNames = sortOn snd . map splitAF . nub . sort . map altfun

signalNames :: [Signal] -> [Text]
signalNames = nub . sort . map signal

splitAF :: Text -> (Text, Int)
splitAF s = let (_, i) = nameNum s in (s, i)

