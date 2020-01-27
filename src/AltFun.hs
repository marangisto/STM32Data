{-# LANGUAGE RecordWildCards #-}
module AltFun (pretty) where

import System.FilePath
import Family as F
import PinMode
import PinSpec as M

pretty :: FilePath -> FilePath -> F.MCU -> IO ()
pretty dbDir outputDir mcu = do
    putStrLn $ name mcu
    -- pmm <- pinModeMap <$> readFile "c:/tmp/null" -- modeFile
    mcu@M.MCU{..} <- parseMCU <$> readFile (dbDir </> name mcu <.> "xml")
    print $ gpioConfig
    afmap <- altFunMap <$> readFile (dbDir </> "IP" </> "GPIO-" <> gpioConfig <> "_Modes" <.> "xml")
    mcu@M.MCU{..} <- return $ mcu { pins = map (resolveFunctions afmap) pins }
    print mcu
