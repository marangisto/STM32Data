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
    , Bank(..)
    , AnalogFun(..)
    , Analog(..)
    , singleMCU
    , processFamily
    , peripheralNames
    , peripheralInsts
    , ipGPIOName
    , signalNames
    , altfunNames
    ) where

import Prelude as P
import System.FilePath
import Data.Char (ord)
import Data.Bits (shift)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find, sort, nub)
import Data.List.Extra (groupSort, sortOn, firstJust, nubSort)
import Data.Text as T (pack, unpack, break)
import Data.Text as T (head, tail, length, snoc)
import Data.Text as T (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Text as T (stripPrefix, stripSuffix {-, unlines-})
--import Data.Text.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import FrontEnd.Families hiding (Peripheral)
import FrontEnd.ParseSVD hiding (Peripheral)
import FrontEnd.ParseMCU
import FrontEnd.ParseIpGPIO
import FrontEnd.Normalize hiding (peripheralNames)
import FrontEnd.Fixup
import FrontEnd.ClockControl
import Utils

data Family = Family
    { family        :: !Text
    , mcus          :: ![Mcu]
    , svds          :: ![(Text, Int)]
    , peripherals   :: ![(Text, ([PeriphType Reserve], [Peripheral]))]
    , interrupts    :: ![Maybe Interrupt] -- pad for nothings
    , specs         :: ![MCU]
    , gpio          :: !GPIO
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
    , configs   :: ![(Text, Int)]
    , signals   :: ![Signal]
    , analogs   :: ![Analog]
    } deriving (Show)

data Signal = Signal
    { pin       :: !Text
    , signal    :: !Text
    , altfun    :: ![(Text, Text)]  -- (af, config)
    } deriving (Show)

data Bank = BankA | BankB deriving (Eq, Ord, Show)

data AnalogFun = InP | InN | Out | ExtI | Dig deriving (Eq, Ord, Show)

data Analog = Analog
    { peripheral    :: !Text
    , pin           :: !Text
    , function      :: !AnalogFun
    , chanBank      :: ![((Maybe Int, Bank), [Text])] -- ((ch, bk), [svd])
    } deriving (Eq, Ord, Show)

singleMCU
    :: FilePath             -- ^ STM32CubeIDE install path
    -> FilePath             -- ^ STM32CubeMX database directory
    -> Bool                 -- ^ recache files
    -> Text                 -- ^ mcu name
    -> [SubFamily]          -- ^ sub-families from catalogue
    -> Text                 -- ^ family name
    -> IO MCU               -- ^ single MCU descriptor
singleMCU svdDir dbDir recache family subFamilies name' = do
    let mcus = concatMap snd subFamilies
        mcu = find (\Mcu{..} -> refName == name' || rpn == name') mcus
        name = maybe (error "no such mcu") (\Mcu{..} -> name) mcu
        file = dbDir </> unpack name <.> "xml"
    svds <- familySVDs family <$> cacheLines recache svdFiles svdDir
    parseMCU (map fst svds) file

processFamily
    :: FilePath             -- ^ STM32CubeIDE install path
    -> FilePath             -- ^ STM32CubeMX database directory
    -> Bool                 -- ^ recache files
    -> Text                 -- ^ family name
    -> [SubFamily]          -- ^ sub-families from catalogue
    -> IO Family            -- ^ raw family data
processFamily svdDir dbDir recache family subFamilies = do
    let mcus = concatMap snd subFamilies
    svds <- familySVDs family <$> cacheLines recache svdFiles svdDir
    svd <- resolveCC . fixup . normalize family
       <$> mapM parseSVD (map snd svds)
    specs <- mapM (parseMCU $ map fst svds) $ mcuFiles dbDir subFamilies
    ipGPIOs <- map (fixupAF . fixupIpGPIO (svdNames svd))
           <$> mapM parseIpGPIO (ipGPIOFiles dbDir specs)
    let peripherals = processPeripherals svd $ altFunMap ipGPIOs
        interrupts = (\NormalSVD{..} -> padInterrupts interrupts) svd
        gpio = processGPIO specs ipGPIOs
        ss = [ (s, shift 1 i) | (i, s) <- zip [0..] $ svdNames svd ]
{-
    writeText ("c:/tmp/" <> T.unpack family <> ".txt")
        $ fromStrict $ T.unlines
        $ map (T.pack . show) $ analogs gpio
-}
    return Family {svds=ss, ..}

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

ipGPIOName :: [Text] -> MCU -> Text
ipGPIOName svds MCU{..}
    | Just IP{..} <- find (\IP{..} -> name == "GPIO") ips
    = let s = fst $ T.break (=='_') version
       in if s `elem` svds then s <> "_" else s -- to avoid collissions
    | otherwise = error $ "failed to find IpGPIO for " <> unpack refName

svdNames :: NormalSVD a b -> [Text]
svdNames NormalSVD{..} = nub $ sort $ concatMap f periphTypes
    where f PeriphType{..} = h typeRef : map g periphInsts
          g PeriphInst{..} = h instRef
          h PeriphRef{..} = svd

fixupIpGPIO :: [Text] -> IpGPIO -> IpGPIO
fixupIpGPIO svds x@IpGPIO{..}
    | name' `elem` svds = x { name = name' <> "_" } -- to avoid collissions
    | otherwise = x { name = name' }
    where name' = fst $ T.break (=='_') version

fixupAF :: IpGPIO -> IpGPIO
fixupAF x@IpGPIO{..} = x { gpioPins = mapMaybe f gpioPins }
    where f :: GPIOPin -> Maybe GPIOPin
          f y@GPIOPin{..}
              | "_C" `isSuffixOf` name = Nothing -- dual pad pin
              | otherwise = Just $ y { pinSignals = mapMaybe g pinSignals }
          g :: PinSignal -> Maybe PinSignal
          g z@PinSignal{..}
              | "Stingray" `isInfixOf` name = Nothing
              | otherwise = Just $ z
                  { name = cleanName name
                  , gpioAF = h gpioAF
                  }
          h s | Just rest <- stripPrefix "GPIO_" s
              = fst $ T.break (=='_') rest
              | "_REMAP0" `isSuffixOf` s = "REMAP"
              | Just rest <- stripPrefix "__HAL_AFIO_" s = rest
              | otherwise = error $ "unexpected gpioAF: " <> unpack s

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
    | Just s <- stripPrefix "GPIO" name
            <|> stripPrefix "HRTIM_TIM" name
    , T.length s == 1
    = Just $ ord (T.head s) - ord 'A'
    -- FIXME: introduce constants for these
    | name == "ADC_COMMON" = Just 123
    | name == "ADC12_COMMON" = Just 12
    | name == "ADC345_COMMON" = Just 345
    | name == "ADC1_2" = Just 12
    | name == "ADC3_4" = Just 34
    | name == "HRTIM_MASTER" = Just 10
    | name == "HRTIM_COMMON" = Just 11
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
peripheralNames Family{..} = nub $ sortOn nameNum
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
          analogs = toAnalogs mcus
          configs = [ (name, shift 1 i)
                    | (i, IpGPIO{..}) <- zip [0..] ipGPIOs
                    ]
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
    , Just portPin <- [ toIOPin $ cleanPin name ]
    ]

toIOPin :: Text -> Maybe IOPin
toIOPin pin
    | ('P':p:xs) <- unpack pin, Just pinNo <- readMaybe xs
    = Just IOPin{port=T.snoc "P" p, portNo=ord p - ord 'A',..}
    | otherwise = Nothing

toSignals :: [IpGPIO] -> [Signal]
toSignals xs =
    [ Signal{..}
    | ((pin, signal), altfun) <- groupSort $ concatMap f xs
    ]
    where f IpGPIO{..} = concatMap (g name) gpioPins
          g conf GPIOPin{..} = map (h conf $ cleanPin name) pinSignals
          h conf pin PinSignal{..} = ((pin, name), (gpioAF, conf))

toAnalogs :: [MCU] -> [Analog]
toAnalogs mcus = sortOn (\Analog{..} -> (peripheral, nameNum pin, function))
    [ let chanBank = map (second nubSort) cbs in Analog {..}
    | ((peripheral, pin, function), cbs) <- xs
    , not ("_C" `isSuffixOf` pin) -- dual pad pin
    ]
    where xs :: [((Text, Text, AnalogFun), [((Maybe Int, Bank), [Text])])]
          xs = map (second groupSort) $ groupSort
                  [ let (fun, chan, bank) = parseAnalogFun sig
                     in ((peripheral, pin, fun), ((chan, bank), svd))
                  | MCU{..} <- mcus
                  , (pin, ys) <- map f pins
                  , (peripheral, sig) <- ys
                  -- FIXME: skipping peripherals missing from SVD!
                  , not ("STM32H7" `isPrefixOf` svd && peripheral == "DAC2")
                  , not ("STM32L4P" `isPrefixOf` svd && peripheral == "ADC2")
                  , not ("STM32L4Q" `isPrefixOf` svd && peripheral == "ADC2")
                  , not ("STM32L4R" `isPrefixOf` svd && peripheral == "ADC2")
                  , not ("STM32L4S" `isPrefixOf` svd && peripheral == "ADC2")
                  , not ("STM32L5" `isPrefixOf` svd && peripheral == "ADC2")
                  ]
          f :: Pin -> (Text, [(Text, Text)]) -- pin peripheral sig
          f Pin{..} = (cleanPin name, map g $ filter pred xs)
              where xs = [ name | Sig{..} <- signals ]
                    pred s = any (`isPrefixOf` s)
                      [ "ADC", "DAC", "OPAMP", "COMP" ]
                    g = first h . second T.tail . T.break (=='_')
                    h name | name `elem` [ "ADC", "DAC" ] = name <> "1"
                           | otherwise = name

parseAnalogFun :: Text -> (AnalogFun, Maybe Int, Bank)
parseAnalogFun s
    | s == "DIG" = (Dig, Nothing, BankA)
    | s == "VINM_SEC" = (InN, Nothing, BankB)
    | s == "VINP_SEC" = (InP, Nothing, BankB)
    | Just x <- strip [ "INN", "INM", "VINM" ] s
    = (InN, number x, BankA)
    | Just x <- strip [ "INP", "VINP", "IN" ] s
    = case stripSuffix "b" x of
        Just y -> (InP, number y, BankB)
        Nothing -> (InP, number x, BankA)
    | Just x <- strip [ "OUT", "VOUT" ] s
    = (Out, number x, BankA)
    | Just x <- stripPrefix "EXTI" s
    = (ExtI, number x, BankA)
    | otherwise = error $ "unrecognized analog function: " <> unpack s
    where number :: Text -> Maybe Int
          number "" = Nothing
          number s = Just $ read $ unpack s
          strip :: [Text] -> Text -> Maybe Text
          strip ps s = firstJust (`stripPrefix` s) ps

cleanPin :: Text -> Text
cleanPin = fst . T.break (`elem` ['-', ' ', '/']) -- FIXME: see PA10 on G0!

altfunNames :: [Signal] -> [(Text, Int)]
altfunNames xs
    | "REMAP" `isPrefixOf` y = flip zip [0..] ys
    | otherwise = sortOn snd $ map splitAF ys
    where ys@(y:_) = nub . sort $ concatMap (map fst . altfun) xs

signalNames :: [Signal] -> [Text]
signalNames = nub . sort . map signal

splitAF :: Text -> (Text, Int)
splitAF s = let (_, i) = nameNum s in (s, i)

padInterrupts :: [Interrupt] -> [Maybe Interrupt]
padInterrupts xs = map (flip Map.lookup imap) [lo..hi]
    where lo = minimum $ map value xs
          hi = maximum $ map value xs
          imap = Map.fromList $ map (\i@Interrupt{..} -> (value, i)) xs

