import Distribution.Simple.Utils (xargs)
import Data.List (delete, sortOn)


data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Value = IntValue Integer | BoolVal String 
    deriving Show
type Stack = [Value]
instance Eq Value where
    IntValue x == IntValue y = x == y
    BoolVal _ == BoolVal _ = True
    _ == _ = False

type StateValue = (String, Value)
type State = [StateValue]

-- criar uma stack vazia
createEmptyStack :: Stack
createEmptyStack = []


-- funções para converter a
stackValue2Str :: Value -> String
stackValue2Str x
    | BoolVal "tt" <- x = "True," 
    | BoolVal "ff" <- x = "False," 
    | IntValue i <- x   = show i ++ "," 
    | otherwise         = "Unknown value" 

lastStackValue2Str :: Value -> String
lastStackValue2Str x 
    | BoolVal "tt" <- x = "True" 
    | BoolVal "ff" <- x = "False" 
    | IntValue i <- x   = show i
    | otherwise         = "Unknown value"

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = lastStackValue2Str x
stack2Str (x:xs) = stackValue2Str x ++ stack2Str xs

-- função para criar um estado vazio da maquina
createEmptyState :: State
createEmptyState = []

lastStateValue2Str :: StateValue -> String
lastStateValue2Str (varName, varVal)
    | BoolVal "tt" <- varVal = varName ++ "=True"
    | BoolVal "ff" <- varVal = varName ++ "=False"
    | IntValue i <- varVal = varName ++ "=" ++ show i 
    | otherwise = "Unknown Value"

stateValue2Str :: StateValue -> String
stateValue2Str (varName, varVal)
    | BoolVal "tt" <- varVal = varName ++ "=True,"
    | BoolVal "ff" <- varVal = varName ++ "=False,"
    | IntValue i <- varVal = varName ++ "=" ++ show i ++ ","
    | otherwise = "Unknown Value"


state2StrSorted :: State -> String
state2StrSorted [] = ""
state2StrSorted [x] = lastStateValue2Str x
state2StrSorted (x:xs) = stateValue2Str x ++ state2StrSorted xs

state2Str :: State -> String
state2Str state = state2StrSorted (sortOn fst state)

executeInstruction :: Inst -> Stack -> State -> (Stack, State)
executeInstruction instruction stack state = 
    case instruction of
        Noop -> (stack, state)
        Push n -> (IntValue n : stack, state)
        Tru -> (BoolVal "tt" : stack, state)
        Fals -> (BoolVal "ff" : stack, state)
        Add -> case stack of
            (IntValue x : IntValue y : rest) -> (IntValue (x + y) : rest, state)
            _ -> error "Run-time error"
        Mult -> case stack of
            (IntValue x : IntValue y : rest) -> (IntValue (x * y) : rest, state)
            _ -> error "Run-time error"
        Sub -> case stack of
            (IntValue x : IntValue y : rest) -> (IntValue (x - y) : rest, state)
            _ -> error "Run-time error"
        Le -> case stack of
            (IntValue x : IntValue y : rest) | x <= y -> (BoolVal "tt" : rest, state)
            (IntValue x : IntValue y : rest) -> (BoolVal "ff" : rest, state)
            _ -> error "Run-time error"
        Equ -> case stack of 
            (BoolVal x : BoolVal y : rest) | x == "tt" && y == "tt" -> (BoolVal "tt" : rest, state)
            (BoolVal x : BoolVal y : rest) -> (BoolVal "ff" : rest, state)
            (IntValue x : IntValue y : rest) | x /= y -> (BoolVal "ff" : rest, state)
            (IntValue x : IntValue y : rest) -> (BoolVal "tt" : rest, state)
            _ -> error "Run-time error"
        Store n -> case stack of
            (x:xs) -> let state' = filter ((/=n) . fst) state
                      in (xs, (n, x) : state')
            _ -> error "Run-time error"
        Fetch n -> case lookup n state of
            Just x -> (x:stack, state)
            Nothing -> error "Run-time error"
        Neg -> case stack of
            (BoolVal x : rest) | x == "tt" -> (BoolVal "ff" : rest, state)
            (BoolVal x : rest) | x == "ff" -> (BoolVal "tt" : rest, state)
        And -> case stack of 
            (BoolVal x : BoolVal y : rest) |  x == "tt" && y == "tt" -> (BoolVal "tt" : rest, state)
            (BoolVal x : BoolVal y : rest) -> (BoolVal "ff" : rest, state)
            _ -> error "Run-time error"
        Branch x y -> case stack of 
            (BoolVal n : rest) | n == "tt" -> let (_, newStack, newState) = run (x, rest, state)
                                              in (newStack, newState)
            (BoolVal n : rest) -> let (_, newStack, newState) = run (y, rest, state)
                                   in (newStack, newState)
            _ -> error "Run-time error"
        Loop c1 c2 ->
            let
                c2Instructions = c2 ++ [Loop c1 c2]
                (_, newStack, newState) = run (c1 ++ [Branch c2Instructions [Noop]], stack, state)
            in
                (newStack, newState)





run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (instruction:rest, stack, state) =
    let (newStack, newState) = executeInstruction instruction stack state
    in run (rest, newStack, newState)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
main :: IO()
main = do 
  print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
  print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  print $ testAssembler [Tru, Branch [Push 1, Push 4] [Push 2], Push 3] == ("3,4,1","")
  print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
--  print $ testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
--  print $ testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
