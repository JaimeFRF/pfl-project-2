# PFL - 2nd Project

## Contributions   
- Carlos Daniel Lopes Rebelo - 50 %
- Jaime Francisco Rodrigues Fonseca - 50% 

### Installation and Execution

To load the project you can simply compile the `main.hs` file using `ghci` compiler by doing `:l main.hs` in the project directory. By executing the project itself the group of tests provided in the project template will be ran. It is also implemented an I/O system that allows the user to give an expression and receive as output the result (the stack and state). The input should be provided with only the expression itself and nothing more.


### First Part 

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