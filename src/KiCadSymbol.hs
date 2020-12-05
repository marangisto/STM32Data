{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module KiCadSymbol (kiCadSymbol) where

import FrontEnd.ParseMCU
import Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Mustache.Compile.TH as TH
import Text.Mustache
import Data.Aeson hiding (Options)

data Orient = Horizontal | Vertical deriving (Show, Eq)
data Align = ALeft | ARight | Centre | Bottom | Top deriving (Show)

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

mkDef :: Text -> Def
mkDef name = Def{..}
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
    , align     :: Align
    , italic    :: Bool
    , bold      :: Bool
    } deriving (Show)

field :: Int -> Text -> Field
field fieldNo text = Field{..}
    where positionX = 0
          positionY = 0
          dimension = 50
          orient    = Horizontal
          visible   = fieldNo `elem` [ 0, 1 ]
          align     = ALeft
          italic    = False
          bold      = False

fieldInfo :: Field -> Value
fieldInfo Field{..} = object
    [ "fieldNo"     .= fieldNo
    , "text"        .= text
    , "positionX"   .= positionX
    , "positionY"   .= positionY
    , "dimension"   .= dimension
    , "orient"      .= (if orient == Horizontal then "H" else "V" :: Text)
    , "visible"     .= (if visible then "V" else "I" :: Text)
    , "align"       .= case align of
                         ALeft  -> "L" :: Text
                         ARight -> "R"
                         Centre -> "C"
                         Bottom -> "B"
                         Top    -> "T"
    , "italic"      .= (if italic then "I" else "N" :: Text)
    , "bold"        .= (if italic then "B" else "N" :: Text)
    ]

kiCadSymbol :: Text -> MCU -> IO ()
kiCadSymbol nm MCU{..} = do
    let -- fn = root </> "stm32" </> "stm32.h"
        template = $(TH.compileMustacheFile $ "src/kicad-symbol.lib")
        -- values = object [ "families" .= (markEnds $ map nameInfo xs) ]
        values = object
            [ "def" .= defInfo def
            , "fields" .= map fieldInfo fields
            ]
    -- createDirectoryIfMissing True $ takeDirectory fn
    -- writeText fn $ renderMustache template values
    putStr $ T.unpack $ TL.toStrict $ renderMustache template values
    where def = mkDef nm
          fields = [ field 0 "U"
                   , field 1 nm
                   , field 2 package
                   , field 3 ""
                   ]

pinSym :: Pin -> Text
pinSym Pin{..} = T.unwords
    [ "X"
    , cleanPin name
    , either id (T.pack . show) position
    ]

cleanPin :: Text -> Text
cleanPin = T.filter (/=' ')

