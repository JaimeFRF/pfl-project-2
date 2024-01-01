# PFL - 2nd Project

### Group T06_G04 Members and Contributions
- Carlos Daniel Lopes Rebelo - 50 %
- Jaime Francisco Rodrigues Fonseca - 50% 

## Installation and Execution

To load the project you can simply compile the `main.hs` file using `ghci` compiler by doing `:l main.hs` in the project directory. By executing the project itself the group of tests provided in the project template will be ran. It is also implemented an I/O system that allows the user to give an expression and receive as output the result (the stack and state). The input should be provided with only the expression itself and nothing more.


## First Part 

For the initial part of the project, we were tasked with developing an assembler. This assembler is responsible for handling machine instructions and managing the machine's state and stack accordingly

The first decision we had to make was how to define the data types for our `Stack` and `State`. For the `Stack`, we decided on the following:

```bash
data Value = IntValue Integer | BoolVal String 
    deriving Show
type Stack = [Value]
instance Eq Value where
    IntValue x == IntValue y = x == y
    BoolVal _ == BoolVal _ = True
    _ == _ = False
```

The stack can contain either integer constants or the string constants `tt` and `ff`, which represent the boolean values `True` and `False`, respectively. Therefore, our stack needs to accommodate these types of values. To achieve this, we created a new data type called `Value` to represent each element in the stack. Consequently, our stack is a list of elements of type `Value`.

We created an instance declaration for the Eq typeclass for the custom data type Value. This provides an interface for testing equality and inequality, which is used in the fetch instruction, for example.

For the state, we have:

```bash
type StateValue = (String, Value)
type State = [StateValue]
```

We interpreted the state as a set of associations between a variable (represented as a string) and its corresponding value. The value is represented in the stack, as previously explained, with the custom data type Value. Therefore, the state is a list of StateValues, each represented as a tuple to define the relationship between the variable and its value.

To display the `Stack` and `State`, we have defined the functions `stack2Str` and `state2str` respectively. These functions print the values of each type, converting the `tt` and `ff` constants to their respective boolean values `True` and `False`, in accordance with the rules defined in the project guide.

The function `run`, which is already defined in the project template, is used to execute the set of instructions. This function recursively calls the `executeInstruction` function that we created. `executeInstruction` executes each instruction individually and updates the stack and state structures accordingly, until there are no more instructions to be executed. As you can see:

```bash
executeInstruction :: Inst -> Stack -> State -> (Stack, State)
```

This function takes an instruction, the current stack, and state, and returns the updated stack and state according to the executed instruction.
Some implementation details of the instructions that should be highlighted include, for example, the Store instruction:

```bash
 Store n -> case stack of
            (x:xs) -> let state' = filter ((/=n) . fst) state
                      in (xs, (n, x) : state')
            _ -> error "Run-time error"
```

The `Store` instruction should not add a new declaration if a variable's value changes. For instance, if we have x = 2 in our state and then change the value of x to 3, the state should be x = 3 and not x = 3 ; x = 2. To achieve this behavior, we first filter the `StateValues` in our State list using function composition. We get the first value of the tuple using `fst` and specify that it should be different from n (what we passed to the Store instruction). Then we add to the state the tuple with n and the value in the topmost position of the stack, as defined in the project guide.

In the `Fetch` instruction, we use lookup with the instance declaration we previously explained in the stack data implementation details. The `Loop` instruction is defined according to the transformation provided in the project guide.

## Second part

### Data types definition

Task 2 requires implementing a machine simulator and a compiler. The simulator should handle arithmetic, boolean operations, and control flows in a low-level machine. The compiler should translate a small imperative language into machine instructions, incorporating elements like arithmetic and boolean expressions, assignments, conditional statements, and loops. Both parts involve defining types and functions.

In order to do that, we started by defining data types for arithmetic expressions (Aexp), boolean expressions (Bexp), and statements (Stm).

```haskell
data Aexp = Const Integer | Var String | AddExp Aexp Aexp | SubExp Aexp Aexp | MultExp Aexp Aexp | ASkip deriving (Show)
data Bexp = BVar String | BConst Integer | BoolConst Bool | AndExp Bexp Bexp | Or Bexp Bexp | Not Bexp | Eq Bexp Bexp | BTrue | BFalse| LessOrEqual Bexp Bexp | EqAexp Aexp Aexp deriving (Show)
data Stm = Assign String Aexp | Seq [Stm] | If [Bexp] [Stm] [Stm] | While [Bexp] [Stm] | Skip deriving (Show)
type Program = [Stm]
```

These data types represent the structure of the imperative language to be compiled. 

### Arithmetic Expressions Compilation

We then implemented the compA function to compile arithmetic expressions into machine code, using operations like addition (AddExp), subtraction (SubExp), and multiplication (MultExp).
The same happened for boolean expressions, we developed an compB function.

```haskell
-- compA 
compA :: Aexp -> Code
compA (Const n) = [Push n]
compA (ASkip) = [Noop]
compA (Var x) = [Fetch x]
compA (AddExp n1 n2) = compA n1 ++ compA n2 ++ [Add]
compA (SubExp n1 n2) = compA n2 ++ compA n1 ++ [Sub]
compA (MultExp n1 n2) = compA n1 ++ compA n2 ++ [Mult]
```

#### Boolean Expressions Compilation

```haskell
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
```

The compB function compiles boolean expressions. This is crucial for conditional execution in the language. 

### Token Definition

Also, for helping our machine understand what symbols are used upon input we created Tokens, as shown in the slides. This will be used for converting the String input into Tokens (tokenizing), allowing our machine to understand what was given and converting, finally, to program code.

```haskell
data Token = PlusTok | MinusTok | TimesTok | DivTok | OpenTok | CloseTok | IntTok Integer | VarTok String | AssignTok | WhileTok | DoTok |
            TrueTok | FalseTok | AndTok | OrTok | NotTok | EqTok | LtTok | IfTok | ThenTok | IntEqTok | BoolEqTok|  ElseTok | SemicolonTok deriving (Show)
```

### Parsing

In order to translate the tokens into code we need to parse it.

Firstly, the parse function serves as the entry point for parsing the program. It takes a string, tokenizes it using lexer. This allows a given string and passes the tokens to parseProgram.
Secondly, parseProgram iterates through the tokens to parse each statement using parseStm. It constructs a list of statements (Program) representing the entire program.
parseStm is a critical function that interprets different types of statements (like assignments, if-then-else structures, while loops). It discerns the statement type based on the leading token and delegates to specific parsing functions for each statement type:

- If Statements: For If tokens, it calls a parser that handles conditional expressions and branches for 'then' and 'else' parts.

- While Loops: For While tokens, it uses parsers like parseStmSeqUntilDo to handle loop conditions and body.

- Assignments: For variable assignments, it employs arithmetic expression parsers like parseAddOrSubMultOrAexpOrPar.

The parseAddOrSubMultOrAexpOrPar function is part of a recursive descent parser for arithmetic expressions. It maintains operator precedence by the order in which it calls other parsing functions.

- parseMultOrAexpOrPar: This function handles multiplication expressions. If it encounters a multiplication operator, it recursively calls itself to parse the right-hand operand. This gives multiplication higher precedence than addition and subtraction, as it will consume as many tokens as it can before an addition or subtraction gets a chance to run. If it does not find a multiplication operator, it will call parseAexpOrPar.

- parseAexpOrPar: This function handles atomic expressions and parenthesized expressions. Atomic expressions are the simplest expressions, like integer constants and variables. Parenthesized expressions are handled by calling parseAddOrSubMultOrAexpOrPar to parse the expression inside the parentheses, allowing it to handle nested expressions of any depth.

- parseAexp: This is the base parser that handles integer constants and variables. It's the simplest parser and doesn't call any other parsers.

In parseAddOrSubMultOrAexpOrPar, if it encounters an addition or subtraction operator, it recursively calls itself to parse the right-hand operand. This gives addition and subtraction lower precedence than multiplication, as an addition or subtraction will only run after all possible multiplications have been parsed.

#

For boolean expressions, we also defined a recursive descent parsers for boolean expressions. The precedence of the operators is maintained by the order in which the parsers are called. 

- parseInequalityOrInt is the base parser that handles integer inequalities. If it encounters a LtTok (less than operator), it parses the integer on both sides and returns a LessOrEqual expression.

- parseEqualityOrInequalityOrInt extends parseInequalityOrInt by also handling integer equalities. If it encounters an IntEqTok (integer equality operator), it parses the arithmetic expressions on both sides and returns an EqAexp expression. If it doesn't find an IntEqTok, it falls back to parseInequalityOrInt.

- parseNegationOrEqualityOrInequalityOrInt extends parseEqualityOrInequalityOrInt by also handling negations. If it encounters a NotTok (negation operator), it recursively calls itself to parse the negated expression and returns a Not expression. If it doesn't find a NotTok, it falls back to parseEqualityOrInequalityOrInt.

- parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt extends parseNegationOrEqualityOrInequalityOrInt by also handling boolean equalities. If it encounters a BoolEqTok (boolean equality operator), it recursively calls itself to parse the expressions on both sides and returns an Eq expression. If it doesn't find a BoolEqTok, it falls back to parseNegationOrEqualityOrInequalityOrInt.

- parseConjunctionOrBooleanEqualityOrNegationOrEqualityOrInequalityOrInt extends parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt by also handling conjunctions. If it encounters an AndTok (and operator), it recursively calls itself to parse the expressions on both sides and returns an AndExp expression. If it doesn't find an AndTok, it falls back to parseBooleanEqualityOrNegationOrEqualityOrInequalityOrInt.

This structure ensures that the operators are parsed with the correct precedence: conjunction before boolean equality, boolean equality before negation, negation before integer equality, and integer equality before integer inequality.

#

### Compilation

After parsing, we must compile the sequence of instructions that using function `compile` that converts a sequence of Statements into Code:
```haskell
compile :: Program -> Code
compile [] = []
compile ((Assign x a):xs) = compA a ++ [Store x] ++ compile xs
compile ((Seq stms):xs) = compile stms ++ compile xs
compile ((If x stm1 stm2):xs) = compB x ++ [Branch (compile stm1) (compile stm2)] ++ compile xs
compile ((While x stm):xs) = [Loop (compB x) (compile stm)] ++ compile xs
compile ((Skip):xs) = compile xs
```

### After compilation && Running tests/input 

Having completed the compilation, the function run (1st part) is called and the statements are executed once at a time.

After loading and running `main` the user is given 3 options:
- Enter 1 to run Part 1 tests
- Enter 2 to run Part 2 tests
- Enter 3 to run input string










