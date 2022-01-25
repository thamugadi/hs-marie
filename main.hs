import Data.List
import Data.Bits
import Data.Word
import Data.Char
import Data.Maybe
import Text.Read
import Text.Printf

data Instruction = Load Word16 | Store Word16 | Add Word16 | Sub Word16 | Input | Output | Halt | Skipcond Word16 | Jump Word16 deriving (Eq, Show, Read)
data CPU = CPU Word16 Word16 Word16
data Memory = Memory [Word16]
data Machine = Machine CPU Memory

decode :: Word16 -> Maybe Instruction
encode :: Instruction -> Word16
decode instr =
        case op of
          1 -> pure $ Load addr
          2 -> pure $ Store addr
          3 -> pure $ Add addr
          4 -> pure $ Sub addr
          5 -> pure Input
          6 -> pure Output
          7 -> pure Halt
          8 -> pure $ Skipcond sk
          9 -> pure $ Jump addr
          otherwise -> Nothing
        where 
                op   = shiftR instr 12
                addr = andb instr 0x0fff
                sk   = andb instr 3
encode instr =
        case instr of
          Load addr  -> 0x1000 + addr
          Store addr -> 0x2000 + addr
          Add addr   -> 0x3000 + addr
          Sub addr   -> 0x4000 + addr
          Input      -> 0x5000
          Output     -> 0x6000
          Halt       -> 0x7000
          Skipcond sk-> 0x8000 + mod sk 3
          Jump addr  -> 0x9000 + addr

accessMemory :: Machine -> Word16 -> Word16
modifyMemory :: Machine -> Word16 -> Word16 -> Machine
accessMemory (Machine cpu (Memory mem)) addr = maybe 0xffff id $ listToMaybe $ drop pos mem where
        pos = w16i addr
modifyMemory (Machine cpu (Memory mem)) addr new 
     | pos < length mem && pos > 0 = Machine cpu $ Memory $ take pos mem ++ [new] ++ drop (pos+1) mem
     | otherwise = Machine cpu $ Memory mem where
          pos = w16i addr

ir :: Machine -> Word16
ac :: Machine -> Word16
pc :: Machine -> Word16
setIR :: Machine -> Word16 -> Machine
setAC :: Machine -> Word16 -> Machine
setPC :: Machine -> Word16 -> Machine
ir (Machine (CPU i a p) mem) = i
ac (Machine (CPU i a p) mem) = a
pc (Machine (CPU i a p) mem) = p
setIR (Machine (CPU i a p) mem) newIR = Machine (CPU newIR a p) mem
setAC (Machine (CPU i a p) mem) newAC = Machine (CPU i newAC p) mem
setPC (Machine (CPU i a p) mem) newPC = Machine (CPU i a newPC) mem

cycle_ :: Machine -> IO()
cycle_ machine = do
        let newMachineFetch  = setIR machine $ accessMemory machine $ pc machine
        let newMachine       = setPC newMachineFetch $ pc newMachineFetch + 1
        case maybe Halt id $ decode $ ir newMachine of
             Load x     -> cycle_ $ load x newMachine
             Store x    -> cycle_ $ store x newMachine
             Add x      -> cycle_ $ add x newMachine
             Sub x      -> cycle_ $ sub x newMachine
             Input      -> cycle_ =<< input newMachine
             Output     -> cycle_ =<< output newMachine
             Jump x     -> cycle_ $ jump x newMachine
             Skipcond x -> cycle_ $ skipcond x newMachine 
             Halt       -> halt

load :: Word16 -> Machine -> Machine
store :: Word16 -> Machine -> Machine
add :: Word16 -> Machine -> Machine
sub :: Word16 -> Machine -> Machine
input :: Machine -> IO Machine
output :: Machine -> IO Machine
jump :: Word16 -> Machine -> Machine
skipcond :: Word16 -> Machine -> Machine
halt :: IO()
load x machine = setAC machine $ accessMemory machine x
store x machine = modifyMemory machine x $ ac machine
add x machine = setAC machine $ ac machine + accessMemory machine x
sub x machine = setAC machine $ ac machine - accessMemory machine x
input machine = do
        ch <- getChar
        let byte = fromIntegral . ord $ ch :: Word16
        return $ setAC machine byte
output machine = do 
        printf "0x%04x\n" $ ac machine
        return machine
jump x machine = setPC machine x
skipcond 0 machine = if (ac machine <  0)  then setPC machine $ pc machine + 1 else machine
skipcond 1 machine = if (ac machine == 0)  then setPC machine $ pc machine + 1 else machine
skipcond 2 machine = if (ac machine >  0)  then setPC machine $ pc machine + 1 else machine
halt = putStrLn "Halting."


list2data :: [String] -> [Word16]
encodeList :: [String] -> [Word16]
encodeStr :: String -> [Word16]
list2data [] = []
list2data (x:xs) = [maybe 0xffff id (readMaybe x :: Maybe Word16)] ++ list2data xs
encodeList [] = []
encodeList (".data":xs) = list2data xs
encodeList (x:xs) = [maybe 0xffff encode (readMaybe x :: Maybe Instruction)] ++ encodeList xs
encodeStr str = encodeList $ lines str

andb :: Word16 -> Word16 -> Word16
orb :: Word16 -> Word16 -> Word16
w16i :: Word16 -> Int
andb a b = (.&.) a b
orb  a b = (.|.) a b
w16i n = fromIntegral n :: Int

startMachine :: Int -> [Word16] -> IO()
startMachine memsize rom = cycle_ initialMachine where
        emptyMachine   = Machine (CPU 0 0 0) $ Memory $ rom ++ replicate memsize 0
        initialMachine = setIR emptyMachine $ accessMemory emptyMachine $ pc emptyMachine

main :: IO()
main = do
        putStrLn "open file: "
        c <- readFile =<< getLine
        startMachine 1000 $ encodeStr c
