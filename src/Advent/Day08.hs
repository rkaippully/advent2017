module Advent.Day08 (day08part1, day08part2) where

import           Advent.Types (Problem(Problem))
import           Advent.Util (toByteString)
import           Control.Monad.State (State, execState, when, get, modify)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import           Data.HashMap.Lazy as HM (HashMap, empty, lookup, insert)
import           Data.Maybe (fromMaybe, fromJust)
import           Text.Parsec (Parsec, runParser, many1, skipMany1, string)
import           Text.Parsec.Char (space, satisfy)


type RegName = BS.ByteString
data Action = Inc | Dec deriving (Show)
data Opr = OprGT | OprLT | OprEQ | OprGE | OprLE | OprNE deriving (Show)
data Condition = Condition RegName Opr Int deriving (Show)

data Instruction = Instruction {
  insnRegName :: RegName
  , insnAction     :: Action
  , insnAmount     :: Int
  , insnCondition  :: Condition
  } deriving (Show)

parseLine :: BS.ByteString -> Instruction
parseLine str =
  case runParser lineParser () "" str of
    Left e  -> error $ show e
    Right v -> v
  where
    lineParser :: Parsec BS.ByteString () Instruction
    lineParser = do
      regName <- word
      skipSpaces
      act <- action
      skipSpaces
      amt <- read <$> word
      skipSpaces
      _ <- string "if"
      skipSpaces
      cond <- condition
      return Instruction {
        insnRegName = BS.pack regName
        , insnAction = act
        , insnAmount = amt
        , insnCondition = cond}

    word :: Parsec BS.ByteString () String
    word = many1 $ satisfy (not . isSpace)

    skipSpaces :: Parsec BS.ByteString () ()
    skipSpaces = skipMany1 space

    action :: Parsec BS.ByteString () Action
    action = do
      s <- word
      case s of
        "inc" -> return Inc
        "dec" -> return Dec
        _     -> fail $ "Invalid action: " ++ s

    condition :: Parsec BS.ByteString () Condition
    condition = do
      regName <- word
      skipSpaces
      opr <- operator
      skipSpaces
      v <- word
      return $ Condition (BS.pack regName) opr (read v)

    operator :: Parsec BS.ByteString () Opr
    operator = do
      s <- word
      case s of
        "<"  -> return OprLT
        "<=" -> return OprLE
        ">"  -> return OprGT
        ">=" -> return OprGE
        "==" -> return OprEQ
        "!=" -> return OprNE
        _    -> fail $ "Invalid operator: " ++ s

data CPUState = CPUState {
  registers  :: HashMap RegName Int
  , maxValue :: Maybe Int }
  deriving (Show)

getRegister :: RegName -> State CPUState Int
getRegister reg = (fromMaybe 0 . HM.lookup reg . registers) <$> get

modifyRegister :: RegName -> (Int -> Int) -> State CPUState ()
modifyRegister reg f = do
  v <- f <$> getRegister reg
  modify (\s -> CPUState {registers = HM.insert reg v (registers s)
                         , maxValue = case maxValue s of
                                        Nothing -> Just v
                                        Just x  -> Just $ max x v})

evalCondition :: Condition -> State CPUState Bool
evalCondition (Condition reg opr amt) = do
  v <- getRegister reg
  let f = case opr of
            OprLT -> (<)
            OprLE -> (<=)
            OprGT -> (>)
            OprGE -> (>=)
            OprEQ -> (==)
            OprNE -> (/=)
  return $ v `f` amt

doAction :: Instruction -> State CPUState ()
doAction insn = do
  let reg = insnRegName insn
      amt = insnAmount insn
      f = case insnAction insn of
            Inc -> (+ amt)
            Dec -> \v -> v - amt
  modifyRegister reg f

runInstruction :: Instruction -> State CPUState ()
runInstruction insn = do
  c <- evalCondition (insnCondition insn)
  when c $ doAction insn

day08part1 :: Problem
day08part1 = Problem "day08part1" $ \s ->
  let
    insns = (runInstruction . parseLine) <$> BS.lines s
    state = execState (sequence insns) CPUState {registers = HM.empty, maxValue = Nothing}
  in
    toByteString $ maximum $ registers state

day08part2 :: Problem
day08part2 = Problem "day08part2" $ \s ->
  let
    insns = (runInstruction . parseLine) <$> BS.lines s
    state = execState (sequence insns) CPUState {registers = HM.empty, maxValue = Nothing}
  in
    toByteString $ fromJust $ maxValue state
