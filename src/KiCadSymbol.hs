{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, DuplicateRecordFields #-}
module KiCadSymbol (kiCadSymbol) where

import FrontEnd.ParseMCU
import Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson hiding (Options)
import Data.List.Extra (groupSortOn)
import Data.List.Split
import Text.Mustache

data Metrics = Metrics
    { grid  :: Int
    , xtra  :: Int
    , n     :: Int
    , m     :: Int
    , h2    :: Int
    , w2    :: Int
    } deriving (Show)

metrics :: Int -> Int -> [a] -> Metrics
metrics grid xtra xs = Metrics{..}
    where n = length xs
          m = n `div` 4     -- FIXME: what if not modular?
          h2 = (m + 1 + 2 * xtra) * grid `div` 2
          w2 = h2

data Orient = Horizontal | Vertical

instance Show Orient where
    show Horizontal = "H"
    show Vertical   = "V"

data Align = ALeft | ARight | Centre | Bottom | Top

instance Show Align where
    show ALeft  = "L"
    show ARight = "R"
    show Centre = "C"
    show Bottom = "B"
    show Top    = "T"

data POrient = Up | Down | PRight | PLeft

instance Show POrient where
    show Up     = "U"
    show Down   = "D"
    show PRight = "R"
    show PLeft  = "L"

data EType = Input | Output | BiDi | TriState | Passive
           | Unspecified | PowerInput | PowerOutput
           | OpenCollector | OpenEmitter | NotConnected

instance Show EType where
    show Input         = "I"
    show Output        = "O"
    show BiDi          = "B"
    show TriState      = "T"
    show Passive       = "P"
    show Unspecified   = "U"
    show PowerInput    = "W"
    show PowerOutput   = "w"
    show OpenCollector = "C"
    show OpenEmitter   = "E"
    show NotConnected  = "N"

data Shape = Line | Inverted | Clock | InvertedClock
           | InputLow | ClockLow | OutputLow
           | FallingEdgeClock | NonLogic

instance Show Shape where
    show Line             = ""
    show Inverted         = "I"
    show Clock            = "C"
    show InvertedClock    = "CI"
    show InputLow         = "L"
    show ClockLow         = "CL"
    show OutputLow        = "V"
    show FallingEdgeClock = "F"
    show NonLogic         = "X"

data Def = Def
    { name              :: Text
    , reference         :: Text
    , textOffset        :: Int
    , drawPinNumber     :: Bool
    , drawPinName       :: Bool
    , unitCount         :: Int
    , unitsLocked       :: Bool
    , optionFlag        :: Bool
    } deriving (Show)

mkDef :: Metrics -> Text -> Def
mkDef _ name = Def{..}
    where reference = "U"
          textOffset = 20
          drawPinNumber = True
          drawPinName = True
          unitCount = 1
          unitsLocked = False
          optionFlag = False

defInfo :: Def -> Value
defInfo Def{..} = object
    [ "name"            .= name
    , "reference"       .= reference
    , "textOffset"      .= textOffset
    , "drawPinNumber"   .= (if drawPinNumber then "Y" else "N" :: Text)
    , "drawPinName"     .= (if drawPinName then "Y" else "N" :: Text)
    , "unitCount"       .= unitCount
    , "unitsLocked"     .= (if unitsLocked then "L" else "F" :: Text)
    , "optionFlag"      .= (if optionFlag then "N" else "P" :: Text)
    ]

data Field = Field
    { fieldNo   :: Int
    , text      :: Text
    , positionX :: Int
    , positionY :: Int
    , dimension :: Int
    , orient    :: Orient
    , visible   :: Bool
    , hAlign    :: Align
    , vAlign    :: Align
    , italic    :: Bool
    , bold      :: Bool
    } deriving (Show)

field :: Metrics -> Int -> Text -> Field
field Metrics{..} fieldNo text = Field{..}
    where positionX = 0
          positionY = fieldNo * grid
          dimension = 50
          orient    = Horizontal
          visible   = fieldNo `elem` [ 0, 1 ]
          hAlign    = Centre
          vAlign    = Centre
          italic    = False
          bold      = False

fieldInfo :: Field -> Value
fieldInfo Field{..} = object
    [ "fieldNo"     .= fieldNo
    , "text"        .= text
    , "positionX"   .= positionX
    , "positionY"   .= positionY
    , "dimension"   .= dimension
    , "orient"      .= show orient
    , "visible"     .= (if visible then "V" else "I" :: Text)
    , "hAlign"       .= show hAlign
    , "vAlign"       .= show vAlign
    , "italic"      .= (if italic then "I" else "N" :: Text)
    , "bold"        .= (if italic then "B" else "N" :: Text)
    ]

data Graphic
    = Rectangle
    { startX    :: Int
    , startY    :: Int
    , endX      :: Int
    , endY      :: Int
    , unit      :: Int
    , convert   :: Int
    , thickness :: Int
    , filled    :: Bool
    } deriving (Show)

rectangle :: Int -> Int -> Int -> Int -> Graphic
rectangle startX startY endX endY = Rectangle{..}
    where unit = 0
          convert = 1
          thickness = 10
          filled = False

graphicInfo :: Graphic -> Value
graphicInfo Rectangle{..} = object [ "rectangle" .= object
    [ "startX"      .= startX
    , "startY"      .= startY
    , "endX"        .= endX
    , "endY"        .= endY
    , "unit"        .= unit
    , "convert"     .= convert
    , "thickness"   .= thickness
    , "filled"      .= (if filled then "f" else "N" :: Text)
    ] ]

mkGraphics :: Metrics -> [Graphic]
mkGraphics Metrics{..} =
    [ (rectangle (-w2) (-h2) w2 h2) { filled = True }
    ]

data PIN = PIN
    { orient    :: POrient
    , name      :: Text
    , number    :: Text
    , posX      :: Int
    , posY      :: Int
    , len       :: Int
    , snum      :: Int
    , snom      :: Int
    , unit      :: Int
    , convert   :: Int
    , etype     :: EType
    , shape     :: Shape
    } deriving (Show)

toPIN :: Metrics -> Pin -> PIN
toPIN Metrics{..} Pin{name=name',..} = PIN{..}
    where orient = PLeft
          name = cleanPin name'
          number = either id (T.pack . show) position
          posX = 0
          posY = 0
          len = grid
          snum = grid `div` 2
          snom = grid `div` 2
          unit = 1
          convert = 1
          etype = fromType type_
          shape = Line
          fromType "Power" = PowerInput
          fromType "I/O" = BiDi
          fromType "MonoIO" = BiDi
          fromType _ = Unspecified

pinInfo :: PIN -> Value
pinInfo PIN{..} = object
    [ "orient"  .= show orient
    , "name"    .= name
    , "number"  .= number
    , "posX"    .= posX
    , "posY"    .= posY
    , "len"     .= len
    , "snum"    .= snum
    , "snom"    .= snom
    , "unit"    .= unit
    , "convert" .= convert
    , "etype"   .= show etype
    , "shape"   .= show shape
    ]

layoutPins :: Metrics -> [Pin] -> [PIN]
layoutPins ms@Metrics{..} xs = concat
    [ zipWith fl [1..] ls
    , zipWith fb [1..] bs
    , zipWith fr [1..] rs
    , zipWith ft [1..] ts
    ]
    where [ ls, bs, rs, ts ] = chunksOf m xs
          fl i x = (toPIN ms x)
              { posX = -(w2 + grid)
              , posY = h2 - (i + xtra) * grid
              , orient = PRight
              }
          fb i x = (toPIN ms x)
              { posX = (i + xtra) * grid - w2
              , posY = -(h2 + grid)
              , orient = Up
              }
          fr i x = (toPIN ms x)
              { posX = w2 + grid
              , posY = (i + xtra) * grid  - h2
              , orient = PLeft
              }
          ft i x = (toPIN ms x)
              { posX = w2 - (i + xtra) * grid
              , posY = h2 + grid
              , orient = Down
              }

kiCadSymbol :: Text -> MCU -> IO ()
kiCadSymbol nm MCU{..} = do
    let template = $(TH.compileMustacheFile $ "src/kicad-symbol.lib")
        -- fn = root </> "stm32" </> "stm32.h"
        -- values = object [ "families" .= (markEnds $ map nameInfo xs) ]
    putStr $ T.unpack $ TL.toStrict $ renderMustache template $ object
        [ "def"      .= defInfo (mkDef m nm)
        , "fields"   .= map fieldInfo fields
        , "graphics" .= map graphicInfo (mkGraphics m)
        , "pins"     .= map pinInfo (layoutPins m ps)
        ]
    -- createDirectoryIfMissing True $ takeDirectory fn
    -- writeText fn $ renderMustache template values
    where ps = fixUp pins
          m = metrics 100 2 ps
          fields = [ field m 0 "U"
                   , field m 1 nm
                   , field m 2 package
                   , field m 3 ""
                   ]

pinSym :: Pin -> Text
pinSym Pin{..} = T.unwords
    [ "X"
    , cleanPin name
    , either id (T.pack . show) position
    ]

cleanPin :: Text -> Text
cleanPin s = T.intercalate "-" $ x : filter p xs
    where (x:xs) = T.words $ T.map f s
          f c | c `elem` [ '(', ')', '[', ']', '-' ] = ' '
              | otherwise = c
          p s | any (`T.isPrefixOf` s) [ "NRST", "OSC" ] = True
              | otherwise = False

fixUp :: [Pin] -> [Pin]
fixUp = map head . groupSortOn position

