module Main where

import           Parsing.Parser                 ( whileParser )
import           Text.Megaparsec                ( parse )
import           Code.CodeGeneration            ( cs )
import           AM.AbstractMachine             ( steps
                                                , mkStartConf
                                                , finalConf
                                                )
import qualified Data.Map                      as M
import           Control.Arrow                  ( (>>>) )

main :: IO ()
main = getContents
    >>= (parse whileParser ""
        >>> either print (runProg >>> print))
    where runProg = cs >>> mkStartConf >>> finalConf
