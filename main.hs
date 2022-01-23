import Data.List as DL
import Data.Bits as DB
import Data.Word as DW
import Data.Char as DC
import Text.Read as TR
data Instruction = Load Word16 | Store Word16 | Add Word16 | Sub Word16 | Input | Output | Halt | Skipcond Word16 | Jump Word16 deriving (Eq, Show, Read)
data CPU = CPU Word16 Word16 Word16 Word16 Word16
data Memory = Memory [Word16]

decode :: Word16 -> Maybe Instruction
decode instr =
        case op of
          1 -> Just (Load addr)
          2 -> Just (Store addr)
          3 -> Just (Add addr)
          4 -> Just (Sub addr)
          5 -> Just Input
          6 -> Just Output
          7 -> Just Halt
          8 -> Just (Skipcond sk)
          9 -> Just (Jump addr)
          15 -> Just Halt
          otherwise -> Nothing
        where 
                op   = shiftR (andb instr 0xf000) 12
                addr = andb instr 0x0fff
                sk   = andb instr 3

encode :: Maybe Instruction -> Word16
encode instr =
        case instr of
          Just (Load  addr)  ->  0x1000 + addr
          Just (Store addr)  ->  0x2000 + addr
          Just (Add   addr)  ->  0x3000 + addr
          Just (Sub   addr)  ->  0x4000 + addr
          Just  Input        ->  0x5000
          Just  Output       ->  0x6000
          Just  Halt         ->  0x7000
          Just (Skipcond sk) ->  0x8000 + (mod sk 3)
          Just (Jump  addr)  ->  0x9000 + addr
          Nothing ->             0xf000
accessMemory :: Memory -> Word16 -> Word16
accessMemory (Memory mem) addr = if (w16i addr < length mem) then (mem!!(w16i addr))
                                                        else fromIntegral 0 :: Word16

modifyMemory :: Memory -> Word16 -> Word16 -> Memory
modifyMemory (Memory mem) addr new =
        if (w16i addr < length mem) then (Memory ((take ((w16i addr)-1) mem) ++ [new] ++ (drop ((w16i addr)) mem)) )
                             else (Memory mem)


startMarie :: Int -> [Word16] -> IO()
startMarie memsize romlist = executeCycle firstInstr initialCPU initialmem where
        initialmem = Memory (romlist ++ (take memsize (repeat 0)))
        emptyCPU = CPU 0 0 0 0 0
        initialCPU = fetchCycle emptyCPU initialmem
        firstInstr = decodeCycle initialCPU

fetchCycle :: CPU -> Memory -> CPU
fetchCycle (CPU mar mbr ir ac pc) mem = CPU pc (accessMemory mem pc) (accessMemory mem pc) ac (pc+1)

decodeCycle :: CPU -> Maybe Instruction
decodeCycle (CPU mar mbr ir ac pc) = decode ir

executeCycle :: Maybe Instruction -> CPU -> Memory -> IO()
executeCycle Nothing _ _ = putStrLn "!!BAD INSTRUCTION!!"
executeCycle (Just instr) cpu memory = do
        case instr of
                Load x -> newCycleNewCPU load x cpu memory
                Store x -> newCycleNewMemory store x cpu memory 
                Add x -> newCycleNewCPU add x cpu memory
                Sub x -> newCycleNewCPU sub x cpu memory
                Input -> newCycleNewIOCPU input cpu memory
                Output -> newCycleAfterOutput output cpu memory
                Halt -> putStrLn "Halting."
                Jump x -> newCycleNewCPU jump x cpu memory
                Skipcond x -> newCycleNewCPU skipcond x cpu memory

newCycleNewCPU :: (Word16 -> CPU -> Memory -> CPU) -> Word16 -> CPU -> Memory -> IO()
newCycleNewCPU instr x cpu memory =
        executeCycle (decodeCycle newCPU) (fetchCycle newCPU memory) memory where
                newCPU = instr x cpu memory

newCycleNewMemory :: (Word16 -> CPU -> Memory -> Memory) -> Word16 -> CPU -> Memory -> IO()
newCycleNewMemory instr x cpu memory =
        executeCycle (decodeCycle cpu) (fetchCycle cpu newMemory) newMemory where
                newMemory = instr x cpu memory

newCycleNewIOCPU :: (CPU -> IO CPU) -> CPU -> Memory -> IO()
newCycleNewIOCPU instr cpu memory = do
        newCPU <- instr cpu
        executeCycle (decodeCycle newCPU) (fetchCycle newCPU memory) memory

newCycleAfterOutput :: (CPU -> IO()) -> CPU -> Memory -> IO()
newCycleAfterOutput instr cpu memory = do
        instr cpu
        executeCycle (decodeCycle cpu) (fetchCycle cpu memory) memory

load :: Word16 -> CPU -> Memory -> CPU
load x (CPU mar mbr ir ac pc) memory = CPU mar mbr ir (accessMemory memory x) pc
store :: Word16 -> CPU -> Memory -> Memory
store x (CPU mar mbr ir ac pc) memory = modifyMemory memory x ac
add :: Word16 -> CPU -> Memory -> CPU
add x (CPU mar mbr ir ac pc) memory = CPU mar mbr ir (ac+(accessMemory memory x)) pc
sub :: Word16 -> CPU -> Memory -> CPU
sub x (CPU mar mbr ir ac pc) memory = CPU mar mbr ir (ac-(accessMemory memory x)) pc  
input :: CPU -> IO CPU
input (CPU mar mbr ir ac pc) = do
        ch <- getChar
        let byte = fromIntegral . ord $ ch :: Word16
        return (CPU mar mbr ir byte pc)
output :: CPU -> IO()
output (CPU mar mbr ir ac pc) = print ac
jump :: Word16 -> CPU -> Memory -> CPU
jump x (CPU mar mbr ir ac pc) memory = CPU mar mbr ir ac x

nop :: CPU -> Memory -> CPU
nop cpu memory = cpu

skipcond :: Word16 -> CPU -> Memory -> CPU
skipcond 0 (CPU mar mbr ir ac pc) memory = 
        if (ac < 0) then (CPU mar mbr ir ac (pc+1)) else nop (CPU mar mbr ir ac pc) memory
skipcond 1 (CPU mar mbr ir ac pc) memory =
        if (ac == 0) then (CPU mar mbr ir ac (pc+1)) else nop (CPU mar mbr ir ac pc) memory
skipcond 2 (CPU mar mbr ir ac pc) memory =
        if (ac > 0) then (CPU mar mbr ir ac (pc+1)) else nop (CPU mar mbr ir ac pc) memory

andb a b = (.&.) a b
orb  a b = (.|.) a b

w16i :: Word16 -> Int
w16i n = fromIntegral n :: Int

list2data :: [String] -> [Word16]
list2data [] = []
list2data (x:xs) = (read x :: Word16) : list2data xs

encodeList :: [String] -> [Word16]
encodeList [] = []
encodeList (".data":xs) = list2data xs 
encodeList (x:xs) = (encode (readMaybe x :: Maybe Instruction)) : encodeList xs
encodeStr :: String -> [Word16]
encodeStr str = encodeList (lines str)

main :: IO()
main = do
        putStrLn "open file: "
        name <- getLine
        c <- readFile name
        startMarie 1000 $ encodeStr c
