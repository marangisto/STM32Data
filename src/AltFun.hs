module AltFun (pretty) where

import System.FilePath
import Family
import PinMode
import PinSpec

pretty :: FilePath -> FilePath -> MCU -> IO ()
pretty dbDir outputDir mcu = do
    putStrLn $ name mcu
    pmm <- pinModeMap <$> readFile "c:/tmp/null" -- modeFile
    pins <- pinSpecs pmm <$> readFile (dbDir </> name mcu <.> "xml")
    print $ pins
