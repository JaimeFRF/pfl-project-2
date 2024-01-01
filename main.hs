import Distribution.Simple.Utils (xargs)
import Data.List (delete, sortOn)
import Debug.Trace
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum, isLower)


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
        Branch x y -> 
            case stack of
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

-- run
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (instruction:rest, stack, state) =
    let (newStack, newState) = executeInstruction instruction stack state
    in run (rest, newStack, newState)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Part 2

data Aexp = Const Integer | Var String | AddExp Aexp Aexp | SubExp Aexp Aexp | MultExp Aexp Aexp | ASkip deriving (Show)
data Bexp = BVar String | BConst Integer | BoolConst Bool | AndExp Bexp Bexp | Or Bexp Bexp | Not Bexp | Eq Bexp Bexp | BTrue | BFalse| LessOrEqual Bexp Bexp | EqAexp Aexp Aexp deriving (Show)
data Stm = Assign String Aexp | Seq [Stm] | If [Bexp] [Stm] [Stm] | While [Bexp] [Stm] | Skip deriving (Show)
type Program = [Stm]

data Token = PlusTok | MinusTok | TimesTok | DivTok | OpenTok | CloseTok | IntTok Integer | VarTok String | AssignTok | WhileTok | DoTok |
            TrueTok | FalseTok | AndTok | OrTok | NotTok | EqTok | LtTok | IfTok | ThenTok | IntEqTok | BoolEqTok|  ElseTok | SemicolonTok deriving (Show)

-- compA 
compA :: Aexp -> Code
compA (Const n) = [Push n]
compA (ASkip) = [Noop]
compA (Var x) = [Fetch x]
compA (AddExp n1 n2) = compA n1 ++ compA n2 ++ [Add]
compA (SubExp n1 n2) = compA n2 ++ compA n1 ++ [Sub]
compA (MultExp n1 n2) = compA n1 ++ compA n2 ++ [Mult]

-- compB 
compB :: [Bexp] -> Code
compB [] = []
compB (x:xs) = 
    case x of
        BoolConst b -> (if b then [Tru] else [Fals]) ++ compB xs
        BConst n -> [Push n] ++ compB xs
        BVar n -> [Fetch n] ++ compB xs
        BTrue -> [Tru] ++ compB xs
        BFalse -> [Fals] ++ compB xs
        AndExp x1 x2 -> compB [x1] ++ compB [x2] ++ [And] ++ compB xs
        Or x1 x2 -> compB [x1] ++ compB [x2] ++ [Branch [Tru] [Fals]] ++ compB xs
        Not x -> compB [x] ++ [Neg] ++ compB xs
        Eq x1 x2 -> compB [x1] ++ compB [x2] ++ [Equ] ++ compB xs
        LessOrEqual x1 x2 -> compB [x1] ++ compB [x2] ++ [Le] ++ compB xs
        EqAexp x1 x2 -> compA x1 ++ compA x2 ++ [Equ] ++ compB xs

-- compile 
compile :: Program -> Code
compile [] = []
compile ((Assign x a):xs) = compA a ++ [Store x] ++ compile xs
compile ((Seq stms):xs) = compile stms ++ compile xs
compile ((If x stm1 stm2):xs) = compB x ++ [Branch (compile stm1) (compile stm2)] ++ compile xs
compile ((While x stm):xs) = [Loop (compB x) (compile stm)] ++ compile xs
compile ((Skip):xs) = compile xs

-- lexer
lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('/' : restStr) = DivTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer (':':'=':rest) = AssignTok : lexer rest 
lexer (';' : restStr) = SemicolonTok : lexer restStr
lexer ('T': 'r':'u':'e' : restStr) = TrueTok : lexer restStr
lexer ('F': 'a':'l':'s':'e' : restStr) = FalseTok : lexer restStr
lexer ('n':'o':'t' : restStr) = NotTok : lexer restStr
lexer ('i':'f':restStr) = IfTok : lexer restStr
lexer ('t':'h':'e':'n':restStr) = ThenTok : lexer restStr
lexer ('e':'l':'s':'e':restStr) = ElseTok : lexer restStr
lexer ('<':'=':restStr) = LtTok : lexer restStr
lexer ('=':'=':restStr) = IntEqTok : lexer restStr
lexer ('=':restStr) = BoolEqTok : lexer restStr
lexer ('a':'n':'d':restStr) = AndTok : lexer restStr
lexer ('w':'h':'i':'l':'e':restStr) = WhileTok : lexer restStr
lexer ('d':'o':restStr) = DoTok : lexer restStr
lexer (c:cs)
  | isSpace c = lexer cs
  | isDigit c = let (number, rest) = span isDigit (c:cs) in IntTok (read number) : lexer rest
  | isLower c = let (var, rest) = span isAlphaNum (c:cs) in VarTok var : lexer rest
  | otherwise = error $ "Unexpected character: " ++ [c]

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = 
    case tokens of
        (IntTok n : restTokens) -> Just (Const n, restTokens)
        (VarTok v : restTokens) -> Just (Var v, restTokens)
        _ -> Nothing

parseAexpOrPar :: [Token] -> Maybe (Aexp, [Token])
parseAexpOrPar (OpenTok : restTokens1) = 
    case parseAddOrSubMultOrAexpOrPar restTokens1 of 
        Just (expr, (CloseTok : restTokens2)) ->
                Just (expr, restTokens2)
        Just _ -> Nothing
        Nothing -> Nothing
parseAexpOrPar tokens = parseAexp tokens

parseMultOrAexpOrPar :: [Token] -> Maybe (Aexp, [Token])
parseMultOrAexpOrPar tokens = 
    case parseAexpOrPar tokens of 
        Just (expr1, (TimesTok : restTokens1)) ->
            case parseMultOrAexpOrPar restTokens1 of
                Just (expr2, restTokens2) -> Just (MultExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

parseAddOrSubMultOrAexpOrPar :: [Token] -> Maybe (Aexp, [Token])
parseAddOrSubMultOrAexpOrPar tokens = 
  case parseMultOrAexpOrPar tokens of
    Just (expr1, (PlusTok : restTokens1)) ->
      case parseAddOrSubMultOrAexpOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AddExp expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (MinusTok : restTokens1)) ->
      case parseAddOrSubMultOrAexpOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (SubExp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseInteger :: [Token] -> Maybe (Bexp, [Token])
parseInteger (OpenTok : restTokens1) =
    case parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt restTokens1 of
        Just (expr, (CloseTok : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing
        Nothing -> Nothing

parseInteger (IntTok n : restTokens) = 
    Just (BConst n, restTokens)

parseInteger (TrueTok : restTokens) =  
    Just (BTrue, restTokens)

parseInteger (FalseTok : restTokens) =  
    Just (BFalse, restTokens)

parseInteger (VarTok n : restTokens) = 
    Just (BVar n, restTokens)

parseInteger tokens = 
    Nothing

parseInequalityOrInt :: [Token] -> Maybe (Bexp, [Token])
parseInequalityOrInt tokens = 
    case  parseInteger tokens of 
        Just (expr1, (LtTok : restTokens1)) ->
            case parseInteger restTokens1 of
                Just (expr2, restTokens2) -> Just (LessOrEqual expr2 expr1, restTokens2)
                Nothing -> Nothing
        result -> result

parseEqualityOrInequalityOrInt :: [Token] -> Maybe (Bexp, [Token])
parseEqualityOrInequalityOrInt tokens = 
    case parseAddOrSubMultOrAexpOrPar tokens of
        Just (expr1, (IntEqTok : restTokens1)) ->
            case parseAddOrSubMultOrAexpOrPar restTokens1 of
                Just (expr2, restTokens2) -> Just (EqAexp expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseInequalityOrInt tokens

parseNegationOrEqualityOrInequalityOrInt :: [Token] -> Maybe (Bexp, [Token])
parseNegationOrEqualityOrInequalityOrInt tokens = 
    case  tokens of
        (NotTok : restTokens1) ->
            case parseNegationOrEqualityOrInequalityOrInt restTokens1 of
                Just (expr, restTokens2) -> Just (Not expr, restTokens2)
                Nothing -> Nothing
        _ -> parseEqualityOrInequalityOrInt tokens

parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt :: [Token] -> Maybe (Bexp, [Token])
parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt tokens = 
    case parseNegationOrEqualityOrInequalityOrInt tokens of
        Just (expr1, BoolEqTok : restTokens1) ->
            case parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt restTokens1 of
                Just (expr2, restTokens2) -> Just (Eq expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result
 

parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt :: [Token] -> Maybe (Bexp, [Token])
parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt tokens = 
    case parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt tokens of
        Just (expr1, (AndTok : restTokens1)) ->
            case parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt restTokens1 of
                Just (expr2, restTokens2) -> Just (AndExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result


parseStmSeq :: [Token] -> Maybe ([Stm], [Token])
parseStmSeq tokens = loop tokens []
  where
    loop [] acc = Just (reverse acc, [])
    loop (ElseTok : rest) acc = Just (reverse acc, ElseTok : rest)
    loop (OpenTok : rest) acc = 
        case parseStm rest of
          Just (stm, restTokens) ->
            case  restTokens of
                (SemicolonTok : restTokens') -> loop restTokens' (stm : acc)
                (CloseTok : restTokens') -> loop restTokens' (stm : acc)
                _ -> Just (reverse (stm : acc), restTokens)
    loop tokens acc =
      case parseStm tokens of
        Just (stm, restTokens) ->
          case restTokens of
            (SemicolonTok : restTokens') -> loop restTokens' (stm : acc)
            _ -> Just (reverse (stm : acc), restTokens)
        
parseElseBlock :: [Token] -> Maybe ([Stm], [Token])
parseElseBlock tokens = 
    case tokens of
        (OpenTok : restTokens) -> loopOpen restTokens []
        _ -> loopSemicolon tokens []
  where
    loopOpen [] acc = Nothing
    loopOpen (SemicolonTok : CloseTok : SemicolonTok : restTokens) acc = Just (reverse acc, restTokens)
    loopOpen tokens acc =
      case parseStm tokens of
        Just (stm,  restTokens) -> loopOpen restTokens (stm : acc)
        _ -> Nothing

    loopSemicolon [] acc = Nothing
    loopSemicolon (SemicolonTok : restTokens) acc = Just (reverse acc, restTokens)
    loopSemicolon tokens acc =
      case parseStm tokens of
        Just (stm,  restTokens) -> loopSemicolon restTokens (stm : acc)
        _ -> Nothing


parseStmSeqUntilDo :: [Token] -> Maybe ([Bexp], [Token])
parseStmSeqUntilDo tokens = loop tokens []
  where
    loop [] acc = Nothing
    loop (DoTok : rest) acc = Just (reverse acc, DoTok : rest)
    loop tokens acc =
      case parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt tokens of
        Just (stm, restTokens) ->
          case restTokens of
            (SemicolonTok : restTokens') -> loop restTokens' (stm : acc)
            _ -> Just (reverse (stm : acc), restTokens)
        Nothing -> Nothing


parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens = 
    case tokens of
        (IfTok : restTokens1) ->
            case  parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt restTokens1 of
                Just (cond, ThenTok : restTokens2) ->
                    case parseStmSeq restTokens2 of
                        Just (stmSeqThen, ElseTok : restTokens3) ->
                            case parseElseBlock restTokens3 of
                                Just (stmSeqElse, restTokens4) -> Just (If [cond] stmSeqThen stmSeqElse, restTokens4)
                                Nothing ->  
                                    case parseStm restTokens3 of
                                        Just (stmElse, restTokens4) -> Just (If [cond] stmSeqThen [stmElse], restTokens4)
                                        Nothing -> error "Error parsing Else branch"
                        Nothing -> error "Error parsing Then branch"
                Nothing -> error "Error parsing condition of If Statement"       
        (WhileTok : restTokens1) ->
            case parseStmSeqUntilDo restTokens1 of
                Just (cond, DoTok : restTokens2) ->
                    case  parseElseBlock restTokens2 of
                        Just (stmSeq, restTokens3) -> Just (While cond stmSeq, restTokens3)
                        Nothing -> error "Error parsing Do block"
                Nothing -> error "Error parsing condition of While Statement"
        (VarTok v : AssignTok : restTokens1) ->
            case parseAddOrSubMultOrAexpOrPar restTokens1 of
                Just (expr, restTokens2) -> Just (Assign v expr, restTokens2)
                Nothing -> Nothing
        (OpenTok : VarTok v : AssignTok : restTokens1) ->
            case parseAddOrSubMultOrAexpOrPar restTokens1 of
                Just (expr, restTokens2) -> Just (Assign v expr, restTokens2)
                Nothing -> Nothing
        (SemicolonTok : restTokens) -> 
             Just (Skip, restTokens)
        (CloseTok : restTokens) ->
            Just (Skip, restTokens)
        _ -> Nothing

parse :: String -> Program
parse str = parseProgram (lexer str)


parseProgram :: [Token] -> Program
parseProgram tokens = 
  case tokens of
    [] -> []
    _ ->
      case parseStm tokens of
        Just (stm, restTokens) -> stm : parseProgram restTokens
        _ -> []

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Function to execute a program from an input string and print the stack and state
executeProgram :: String -> IO ()
executeProgram programCode = do
    let program = parse programCode
    let code = compile program

    let (_, finalStack, finalState) = run (code, createEmptyStack, createEmptyState)

    putStrLn "Final Stack:"
    putStrLn (stack2Str finalStack)
    putStrLn "Final State:"
    putStrLn (state2Str finalState)

runPart1Tests :: IO ()
runPart1Tests = do
    -- Part 1 tests
    putStrLn "--------------------------- PART 1 TESTS ------------------------"
    print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
    print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    -- If you test:
    -- testAssembler [Push 1,Push 2,And]
    -- You should get an exception with the string: "Run-time error"
    -- If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- You should get an exception with the string: "Run-time error"
    putStrLn "-----------------------------------------------------------------"
    
runPart2Tests :: IO ()
runPart2Tests = do
    -- Part 2 tests 
    putStrLn "--------------------------- PART 2 TESTS ------------------------"
    print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    print $ testParser "x := 0 - 2;" == ("","x=-2")
    print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
    print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
    print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 + 1 == 2 = 1 + 2 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
    print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
    print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    print $ testParser "x := 0 - 2;" == ("","x=-2")
    print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
    print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
    print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
    print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
    print $ testParser "x := 42; if x <= 43 then (x := 1; x:= x+ 1;); else (x := 33; x := x+1;);" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then (if x <= 42 then (x := 1; x:= x + 1;); else x := 33;); else x := x+1;" == ("", "x=2")
    putStrLn "-----------------------------------------------------------------"

-- Examples:
main :: IO ()
main = do
    putStrLn "\n"
    putStrLn "Enter 1 to run Part 1 tests, 2 to run Part 2 tests, or 3 to execute your own program code:"
    userInput <- getLine
    case userInput of
        "1" -> runPart1Tests
        "2" -> runPart2Tests
        "3" -> do
            putStrLn "Enter the program code:"
            programCode <- getLine
            executeProgram programCode
        _ -> putStrLn "Invalid input. Please enter 1, 2, or 3."
