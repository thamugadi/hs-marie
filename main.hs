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

decode :: Word16 -> Maybe Instruction
decode instr =
        case op of
          1 -> Just $ Load addr
          2 -> Just $ Store addr
          3 -> Just $ Add addr
          4 -> Just $ Sub addr
          5 -> Just Input
          6 -> Just Output
          7 -> Just Halt
          8 -> Just $ Skipcond sk
          9 -> Just $ Jump addr
          otherwise -> Nothing
        where 
                op   = shiftR instr 12
                addr = andb instr 0x0fff
                sk   = andb instr 3

encode :: Instruction -> Word16
encode instr =
        case instr of
          Load addr  -> 0x1000 + addr
          (Store addr) -> 0x2000 + addr
          (Add addr)   -> 0x3000 + addr
          (Sub addr)   -> 0x4000 + addr
          Input        -> 0x5000
          Output       -> 0x6000
          Halt         -> 0x7000
          (Skipcond sk)-> 0x8000 + (mod sk 3)
          (Jump addr)  -> 0x9000 + addr

accessMemory :: Memory -> Word16 -> Word16
accessMemory (Memory mem) addr = maybe 0xffff id $ listToMaybe $ drop pos mem where
        pos = w16i addr

modifyMemory :: Memory -> Word16 -> Word16 -> Memory
modifyMemory (Memory mem) addr new
        | w16i addr < length mem && w16i addr >= 0 = Memory $ take (pos-1) mem ++ [new] ++ drop pos mem
        | otherwise = Memory mem where
                pos = w16i addr

startMarie :: Int -> [Word16] -> IO()
startMarie memsize romlist = executeCycle firstInstr initialCPU initialmem where
        annexmem   = take memsize $ repeat 0
        initialmem = Memory $ romlist ++ annexmem
        emptyCPU   = CPU 0 0 0
        initialCPU = fetchCycle emptyCPU initialmem
        firstInstr = decodeCycle initialCPU

illegalInstruction :: IO()
illegalInstruction = putStrLn "!!BAD INSTRUCTION!!"

fetchCycle :: CPU -> Memory -> CPU
fetchCycle (CPU ir ac pc) mem = CPU (accessMemory mem pc) ac (pc+1) 

decodeCycle :: CPU -> Maybe Instruction
decodeCycle (CPU ir ac pc) = decode ir

executeCycle :: Maybe Instruction -> CPU -> Memory -> IO()
executeCycle Nothing cpu memory = illegalInstruction
executeCycle (Just instr) cpu memory = do
        case instr of
                Load x     -> newCycleNewCPU load x cpu memory
                Store x    -> newCycleNewMemory store x cpu memory 
                Add x      -> newCycleNewCPU add x cpu memory
                Sub x      -> newCycleNewCPU sub x cpu memory
                Input      -> newCycleNewIOCPU input cpu memory
                Output     -> newCycleAfterOutput output cpu memory
                Halt       -> halt
                Jump     x -> newCycleNewCPU jump x cpu memory
                Skipcond x -> newCycleNewCPU skipcond x cpu memory

newCycleNewCPU :: (Word16 -> CPU -> Memory -> CPU) -> Word16 -> CPU -> Memory -> IO()
newCycleNewCPU instr x cpu memory =
        executeCycle nextInstruction newCPU memory where
                cpuInstr        = instr x cpu memory
                newCPU          = fetchCycle cpuInstr memory
                nextInstruction = decodeCycle newCPU 
newCycleNewMemory :: (Word16 -> CPU -> Memory -> Memory) -> Word16 -> CPU -> Memory -> IO()
newCycleNewMemory instr x cpu memory =
        executeCycle nextInstruction newCPU newMemory where
                newMemory       = instr x cpu memory
                newCPU          = fetchCycle cpu newMemory
                nextInstruction = decodeCycle newCPU

newCycleNewIOCPU :: (CPU -> IO CPU) -> CPU -> Memory -> IO()
newCycleNewIOCPU instr cpu memory = do
        cpuInstr <- instr cpu
        let newCPU          = fetchCycle cpuInstr memory
        let nextInstruction = decodeCycle newCPU
        executeCycle nextInstruction newCPU memory

newCycleAfterOutput :: (CPU -> IO()) -> CPU -> Memory -> IO()
newCycleAfterOutput instr cpu memory = do
        instr cpu
        executeCycle (decodeCycle $ fetchCycle cpu memory) (fetchCycle cpu memory) memory

load :: Word16 -> CPU -> Memory -> CPU
load x (CPU ir ac pc) memory = CPU ir (accessMemory memory x) pc
store :: Word16 -> CPU -> Memory -> Memory
store x (CPU ir ac pc) memory = modifyMemory memory x ac
add :: Word16 -> CPU -> Memory -> CPU
add x (CPU ir ac pc) memory = CPU ir (ac+(accessMemory memory x)) pc
sub :: Word16 -> CPU -> Memory -> CPU
sub x (CPU ir ac pc) memory = CPU ir (ac-(accessMemory memory x)) pc  
input :: CPU -> IO CPU
input (CPU ir ac pc) = do
        ch <- getChar
        let byte = fromIntegral . ord $ ch :: Word16
        return $ CPU ir byte pc
output :: CPU -> IO()
output (CPU ir ac pc) = printf "0x%02x\n" ac 
jump :: Word16 -> CPU -> Memory -> CPU
jump x (CPU ir ac pc) memory = CPU ir ac x

nop :: CPU -> Memory -> CPU
nop cpu memory = cpu

skipcond :: Word16 -> CPU -> Memory -> CPU
skipcond 0 (CPU ir ac pc) memory = if (ac < 0)  then CPU ir ac (pc+1) else CPU ir ac pc
skipcond 1 (CPU ir ac pc) memory = if (ac == 0) then CPU ir ac (pc+1) else CPU ir ac pc
skipcond 2 (CPU ir ac pc) memory = if (ac > 0)  then CPU ir ac (pc+1) else CPU ir ac pc

halt :: IO()
halt = putStrLn "Halting."

andb a b = (.&.) a b
orb  a b = (.|.) a b

w16i :: Word16 -> Int
w16i n = fromIntegral n :: Int

list2data :: [String] -> [Word16]
list2data [] = []
list2data (x:xs) = [maybe 0xffff id (readMaybe x :: Maybe Word16)] ++ list2data xs

encodeList :: [String] -> [Word16]
encodeList [] = []
encodeList (".data":xs) = list2data xs
encodeList (x:xs) = [maybe 0xffff encode (readMaybe x :: Maybe Instruction)] ++ encodeList xs
encodeStr :: String -> [Word16]
encodeStr str = encodeList $ lines str

main :: IO()
main = do
        putStrLn "open file: "
        name <- getLine
        c <- readFile name
        startMarie 1000 $ encodeStr c
