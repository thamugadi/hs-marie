import Data.List
import Data.Bits
import Data.Word
import Data.Char
import Data.Maybe
import Text.Read
import Text.Printf
import Numeric

data Instruction = Load Word16 | Store Word16 | Add Word16 | Sub Word16 |
                   Input | Output | Halt | Skipcond Word16 | Jump Word16
                   deriving (Eq, Show, Read)
data Reg = Reg Word16 Word16 Word16
data Memory = Memory [Word16]
data MachineData = MachineData Reg Memory

instance Show Memory where
        show (Memory mem)="MEMORY: " ++ (concat $ map (\x -> "0x" ++ showHex x " ") mem)
instance Show Reg where
        show (Reg ir ac pc)="REGISTERS: "++"IR=0x"++(showHex ir "")++(show $ maybe Halt id $ decode ir)++" AC=0x"++(showHex ac "")++" PC=0x"++(showHex pc "")
instance Show MachineData where
        show (MachineData reg mem) = show reg++"\n"++show mem

decode :: Word16 -> Maybe Instruction
encode :: Instruction -> Word16
decode instr =
        case op of
          1 -> pure $ Load addr
          2 -> pure $ Store addr
          3 -> pure $ Add addr
          4 -> pure $ Sub addr
          5 -> pure $ Input
          6 -> pure $ Output
          7 -> pure $ Halt
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

accessMemory :: MachineData -> Word16 -> Word16
modifyMemory :: MachineData -> Word16 -> Word16 -> MachineData
accessMemory (MachineData reg (Memory mem)) addr = maybe 0xffff id $ listToMaybe $ drop pos mem where
        pos = w16i addr
modifyMemory (MachineData reg (Memory mem)) addr new 
     | pos < length mem && pos > 0 = MachineData reg $ Memory $ take pos mem ++ [new] ++ drop (pos+1) mem
     | otherwise = MachineData reg $ Memory mem where
          pos = w16i addr

ir :: MachineData -> Word16
ac :: MachineData -> Word16
pc :: MachineData -> Word16
setIR :: MachineData -> Word16 -> MachineData
setAC :: MachineData -> Word16 -> MachineData
setPC :: MachineData -> Word16 -> MachineData
ir (MachineData (Reg i a p) mem) = i
ac (MachineData (Reg i a p) mem) = a
pc (MachineData (Reg i a p) mem) = p
setIR (MachineData (Reg i a p) mem) newIR = MachineData (Reg newIR a p) mem
setAC (MachineData (Reg i a p) mem) newAC = MachineData (Reg i newAC p) mem
setPC (MachineData (Reg i a p) mem) newPC = MachineData (Reg i a newPC) mem

interface :: Instruction -> MachineData -> IO MachineData
interface instr machine = do
        print machine
        case instr of 
             Input ->  do
                        ch <- getChar
                        pure $ setAC machine (fromIntegral . ord $ ch :: Word16)
             Output -> do
                        printf "0x%04x\n" $ ac machine
                        pure machine
             otherwise -> pure machine

halt :: MachineData -> IO()
halt machine = do
        print machine
        putStrLn "Halting."

fetch :: MachineData -> MachineData
fetch machine = setPC newMachineFetch $ pc newMachineFetch + 1 where
        newMachineFetch = setIR machine $ accessMemory machine $ pc machine

execute :: Instruction -> MachineData -> MachineData
execute instr machine =
        case instr of
             Load x     -> load x machine
             Store x    -> store x machine
             Add x      -> add x machine
             Sub x      -> sub x machine
             Input      -> machine
             Output     -> machine
             Jump x     -> jump x machine
             Skipcond x -> skipcond x machine

cycle_ :: MachineData -> IO()
cycle_ machine = do
        if instr == Halt then halt newMachine else
            cycle_ =<< (interface instr $ execute instr $ newMachine) where
                newMachine = fetch machine
                instr = maybe Halt id $ decode $ ir $ newMachine

load :: Word16 -> MachineData -> MachineData
store :: Word16 -> MachineData -> MachineData
add :: Word16 -> MachineData -> MachineData
sub :: Word16 -> MachineData -> MachineData
jump :: Word16 -> MachineData -> MachineData
skipcond :: Word16 -> MachineData -> MachineData
load x machine = setAC machine $ accessMemory machine x
store x machine = modifyMemory machine x $ ac machine
add x machine = setAC machine $ ac machine + accessMemory machine x
sub x machine = setAC machine $ ac machine - accessMemory machine x
jump x machine = setPC machine x
skipcond 0 machine = if (ac machine <  0)  then setPC machine $ pc machine + 1 else machine
skipcond 1 machine = if (ac machine == 0)  then setPC machine $ pc machine + 1 else machine
skipcond 2 machine = if (ac machine >  0)  then setPC machine $ pc machine + 1 else machine


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
startMachine memsize rom = cycle_ $ MachineData (Reg 0 0 0) $ Memory $ rom ++ replicate memsize 0

main :: IO()
main = do
        putStrLn "open file: "
        c <- readFile =<< getLine
        startMachine 10 $ encodeStr c
