import Distribution.Simple.Utils (xargs)
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Value = IntValue Integer | BoolVal String 
    deriving Show
type Stack = [Value]

type StateValue = (String, Value)
type State = [StateValue]

-- criar uma stack vazia
createEmptyStack :: Stack
createEmptyStack = []


-- funções para converter a
stackValue2Str :: Value -> String
stackValue2Str x
    | BoolVal "tt" <- x = "True, " 
    | BoolVal "ff" <- x = "False, " 
    | IntValue i <- x   = show i ++ ", " 
    | otherwise         = "Unknown value" 

lastStackValue2Str :: Value -> String
lastStackValue2Str x 
    | BoolVal "tt" <- x = "True." 
    | BoolVal "ff" <- x = "False." 
    | IntValue i <- x   = show i ++ "." 
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
    | BoolVal "tt" <- varVal = varName ++ "=True."
    | BoolVal "ff" <- varVal = varName ++ "=False."
    | IntValue i <- varVal = varName ++ "=" ++ show i ++ "."
    | otherwise = "Unknown Value"

stateValue2Str :: StateValue -> String
stateValue2Str (varName, varVal)
    | BoolVal "tt" <- varVal = varName ++ "=True,"
    | BoolVal "ff" <- varVal = varName ++ "=False,"
    | IntValue i <- varVal = varName ++ "=" ++ show i ++ ","
    | otherwise = "Unknown Value"


state2Str :: State -> String
state2Str [] = ""
state2Str [x] = lastStateValue2Str x
state2Str (x:xs) = stateValue2Str x ++ state2Str xs

executeInstruction :: Inst -> Stack -> State -> (Stack, State)
executeInstruction instruction stack state = 
    case instruction of
        Push n -> (IntValue n : stack, state)
        Tru -> (BoolVal "tt" : stack, state)
        Fals -> (BoolVal "ff" : stack, state)
        Add -> case stack of
            (IntValue x : IntValue y : rest) -> (IntValue (x + y) : rest, state)
            _ -> error "Add: Error on interpreting this instruction"
        Mult -> case stack of
            (IntValue x : IntValue y : rest) -> (IntValue (x * y) : rest, state)
            _ -> error "Mult: Error on interpreting this instruction"
        Sub -> case stack of
            (IntValue x : IntValue y : rest) -> (IntValue (x - y) : rest, state)
            _ -> error "Sub: Error on interpreting this instruction"
        Le -> case stack of
            (IntValue x : IntValue y : rest) | x /= y -> (BoolVal "ff" : rest, state)
            (IntValue x : IntValue y : rest) -> (BoolVal "tt" : rest, state)
            _ -> error "Le: Error on interpreting this instruction"
        Equ -> case stack of 
            (BoolVal x : BoolVal y : rest) | x == "tt" && y == "tt" -> (BoolVal "tt" : rest, state)
            (BoolVal x : BoolVal y : rest) -> (BoolVal "ff" : rest, state)
            (IntValue x : IntValue y : rest) | x /= y -> (BoolVal "ff" : rest, state)
            (IntValue x : IntValue y : rest) -> (BoolVal "tt" : rest, state)
            _ -> error "Equ: Error on interpreting this instruction"



run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (instruction:rest, stack, state) =
    let (newStack, newState) = executeInstruction instruction stack state
    in run (rest, newStack, newState)

-- code1 :: Code
-- code1 = [Push 10,Push 4, Push 3,Sub, Mult]

-- code2 :: Code
-- code2 = [Push 10, Push 20, Le]

-- code3 :: Code
-- code3 = []

-- code4 :: Code
-- code4 = [Push 30, Push 40, Push 50, Tru, Fals, Equ]

-- main :: IO ()
-- main = do
--   let (_, stack1, _) = run (code1, [], [])
--   let (_, stack2, _) = run (code2, [], [])
--   let (_, stack3, _) = run (code3, [], [])
--   let (_, stack4, _) = run (code4, [], [])
--   putStrLn $ "Stack 1: " ++ stack2Str stack1
--   putStrLn $ "Stack 2: " ++ stack2Str stack2
--   putStrLn $ "Stack 3: " ++ stack2Str stack3
--   putStrLn $ "Stack 4: " ++ stack2Str stack4


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
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
