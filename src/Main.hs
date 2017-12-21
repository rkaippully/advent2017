module Main where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, ReadM, ParserInfo, option, long, short, help
                           , eitherReader, execParser, info, helper, (<**>), progDesc)
import Text.Read (readMaybe)
import Advent.Types (Day(AllDays), Part(AllParts))
import Advent (execProblems)

options :: Parser (Day, Part)
options = (,)
        <$> option dayReader
            (long "day"
            <> short 'd'
            <> help "The day number (1-25) of the problem or `ALL`")
        <*> option partReader
            (long "part"
            <> short 'p'
            <> help "The part number (1-2) of the problem or `ALL`")
  where
    intReader :: (Enum a) => a -> (Int -> Bool) -> ReadM a
    intReader allVal validator = eitherReader $ \s ->
                                     case (s, readMaybe s) of
                                       ("ALL", _)                -> Right allVal
                                       (_, Just n) | validator n -> Right (toEnum n)
                                       (_, _)                    -> Left $ "Invalid value: " <> s

    dayReader :: ReadM Day
    dayReader = intReader AllDays (\n -> n >= 1 && n <= 25)

    partReader :: ReadM Part
    partReader = intReader AllParts (\n -> n >= 1 && n <= 2)

main :: IO ()
main = execParser opts >>= execProblems
  where
    opts :: ParserInfo (Day, Part)
    opts = info (options <**> helper)
           (progDesc "Advent of Code 2017 solutions")
