module Printer.Console
    ( green
    , red
    , reset
    )
    where

import Data.Word (Word8)

import qualified System.Console.ANSI as Console
import qualified System.Console.ANSI.Types as Color


red :: String -> String
red =
    setFontColor (Color.xterm6LevelRGB 5 0 0)


green :: String -> String
green =
    setFontColor (Color.xterm6LevelRGB 0 5 0)


reset :: String
reset =
    Console.setSGRCode [ Color.Reset ]


setFontColor :: Word8 -> String -> String
setFontColor color str =
    Console.setSGRCode [ Color.SetPaletteColor Color.Foreground color ]
        ++ str
        ++ reset
