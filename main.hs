import Data.List as DL
import Data.Bits as DB
import Data.Word as DW
import Data.Char as DC
data Instruction = Load Word16 | Store Word16 | Add Word16 | Sub Word16 | Input | Output | Halt | Skipcond Word16 | Jump Word16 deriving (Eq, Show, Read)

decode :: Word16 -> Instruction
decode instr =
        case op of
          1 -> Load addr
          2 -> Store addr
          3 -> Add addr
          4 -> Sub addr
          5 -> Input
          6 -> Output
          7 -> Halt
          8 -> Skipcond sk
          9 -> Jump addr
        where 
                op   = shiftR (andb instr 0xf000) 12
                addr = andb instr 0x0fff
                sk   = andb instr 3
encode :: Instruction -> Word16
encode instr =
        case instr of
          Load  addr ->  0x1000 + addr
          Store addr ->  0x2000 + addr
          Add   addr ->  0x3000 + addr
          Sub   addr ->  0x4000 + addr
          Input       -> 0x5000
          Output      -> 0x6000
          Halt        -> 0x7000
          Skipcond sk -> 0x8000 + sk
          Jump  addr  -> 0x9000 + addr

marie :: [Word16] -> Word16 -> Word16 -> Word16 -> IO()
marie memory ir ac pc =
        case decode ir of
          Load x -> marie memory (memory!!addrpc) (memory!!addrx) (pc+1) where
                  addrx  = w16i x
                  addrpc = w16i pc
          Store x -> marie newmemory (memory!!addrpc) ac (pc+1) where
                  addrx  = w16i x
                  addrpc = w16i pc
                  newmemory = take (addrx-1) memory ++ [ac] ++ drop (addrx+1) memory
          Add x -> marie memory (memory!!addrpc) (ac+memory!!addrx) (pc+1) where
                  addrx = w16i x
                  addrpc = w16i pc
          Sub x -> marie memory (memory!!addrpc) (ac-memory!!addrx) (pc+1) where
                  addrx = w16i x
                  addrpc = w16i pc
          Input -> do
                  hee <- getChar
                  let byte = fromIntegral (ord hee) :: Word16
                  marie memory (memory!!addrpc) byte (pc+1) where 
                          addrpc = w16i pc
          Output -> do
                  print ac
                  marie memory (memory!!addrpc) ac (pc+1) where
                          addrpc = w16i pc
          Halt -> do
                  putStrLn "Halting."
                  pure ()
          Skipcond 0 -> if (ac < 0) then marie memory (memory!!(w16i pc + 1)) ac (pc+2)
                             else marie memory (memory!!(w16i pc)) ac (pc+1)
          Skipcond 1 -> if (ac == 0) then marie memory (memory!!(w16i pc + 1)) ac (pc+2)
                             else marie memory (memory!!(w16i pc)) ac (pc+1)
          Skipcond 2 -> if (ac > 0) then marie memory (memory!!(w16i pc + 1)) ac (pc+2)
                             else marie memory (memory!!(w16i pc)) ac (pc+1)
          Skipcond 3 -> putStrLn "An error has occured."
          Jump x -> marie memory (memory!!addrpc) ac (x+1) where
                  addrpc = w16i x
          otherwise -> putStrLn "!!BAD OPERATION!!"

startMarie :: Int -> [Word16] -> IO()
startMarie memsize romlist = marie mem (mem!!0) 0 1 where
        mem = romlist ++ (take memsize (repeat 0))

andb a b = (.&.) a b
orb  a b = (.|.) a b

w16i :: Word16 -> Int
w16i n = fromIntegral n :: Int

list2data :: [String] -> [Word16]
list2data (x:xs) = (read x :: Word16) : list2data xs

encodeList :: [String] -> [Word16]
encodeList (".data":xs) = list2data xs 
encodeList (x:xs) = encode ((read x :: Instruction)) : encodeList xs

encodeStr :: String -> [Word16]
encodeStr str = encodeList $ lines str

main :: IO()
main = do
        putStrLn "open file: "
        name <- getLine
        c <- readFile name

        startMarie 1000 $ encodeStr c
