# Overview
This compiler was completed as part of the coursework for EECE 6083: Compiler Theory and Practice at the
University of Cincinnati in the spring semester of 2024.
- The project description can be found [here](./project.pdf)
- The compiled language spec can be found [here](./projectLanguage.pdf)

# Building and Running
To build this project you must have Rust installed. You can find instuctions for installation [here](https://www.rust-lang.org/tools/install).

## Build
To build the project, navigate to the root of the repo and run:
```
cargo build
```
This will build the target executable "cli", located at ./target/debug/cli. To run the executable, pass
in a single command line argument specifying a source file to compile. Example programs can be found in the testPgms
directory. Here is an example:
```
./target/debug/cli testPgms/correct/iterativeFib.src
```
This will produce an executable a.out. Run it with the following command:
```
./a.out
```

## Build and run simultaneously
Alternatively you can build the compiler and have it compile a program in a single command with cargo run:
```
cargo run -- testPgms/correct/iterativeFib.src
```

## Running tests
To run the unit test suite, run:
```
cargo test
```

## Debug output
To compile with additional debugging output add the debug command before the file to be compiled. This
debug output is ad hoc, and was only added for my own use to quickly debug issues. It's not very
pretty.
```
./target/debug/cli debug testPgms/correct/iterativeFib.src
```
# Structure
## cli
The cli directory contains the source files to build the command line interface executable.
[main.rs](./cli/src/main.rs) contains all the code to define the cli. It parses input arguments,
reads the target source file, creates a scanner and parser to parse the file, then performs semantic checking,
and code generation passes on the ast resulting from the parse. It also aggregates any errors from the compilation
passes and formats and displays them to the user. Depending on the severity of errors, and during which compilation
phases the errors occur, the command line program may halt executution. If the compilation is successful, the
command line program will produce a temporary .ll file containin the LLVM IR of the compiled source file. It will
then run clang directly to link the .ll file with the runtime static library and create an executable.
## compiler
The compiler directory contains the implementations for all of the compilation passes. This includes the scanner,
the LL(1) parser, the semantic checker (aka typechecker), and the LLVM IR code generator. These APIs are bundled into
the compiler library, which is used by the cli executable.
### scanner
The implementation for the scanner can be found in [scanner.rs](./compiler/src/scanner.rs) and the accompanying
token definitions can be found in [token.rs](./compiler/src/token.rs). The scanner is implemented similar to
what was discussed in class and in the book: based on a DFA but not rigorously defined. A scanner is created with
the new function, and scanning of the input character stream is done by repeatedly calling the scan method. The scan method
returns the next token found, terminating with an EOF token. Some interesting points to note:
- We took advantage of the capabilities provided by Rust enums and stored any additional information associated with a token
on the token itself. Identifier tokens contain the identifier string, IntLiteral and FloatLiteral tokens contain a numeric value,
and String tokens contain the string literal
- Numeric literals are checked and split into either an IntLiteral token or a FloatLiteral token by the scanner.
- We used a hash map to keep track of reserved words in the language, mapping from the keyword string to the corresponding
keyword token. Before returning an Identifier token from scan we would first check to see if the identifier existed in the
reserved words map, and if it did return the corresponding keyword token instead.
- The scanner does not have any knowledge of errors or warnings. Instead if the scanner encounters an unknown character it
returns an Invalid token with the associated character stream stored on the token itself. The parser can then make a more
informed decision about error handling.
- The unit test suite for the scanner is also included in [scanner.rs](./compiler/src/scanner.rs).
### llparser
The implementation of the LL(1) parser can be found in [llparser.rs](./compiler/src/llparser.rs). The parser takes
tokens produced by the scanner, defined in [token.rs](./compiler/src/token.rs), and either produces a valid abstract
syntax tree (ast) or a terminal error. The ast nodes are defined in [ast.rs](./compiler/src/ast.rs). The error type,
which includes both errors and warnings is defined in [error.rs](./compiler/src/error.rs). The parser is created with
the new function, and the parse occurs with call to the parse function. The parse function performs a recursive descent
parse, with a private function corresponding to each non-terminal. The parse function either returns a valid ast, or it
returns a terminal error. This is accomplished using the Rust Result type. In either case the parse may produce an error
log which can be accessed via the get_errors function. If the parse was successful, a valid ast will be producted, but
there may still be errors logged corresponding to warnings and resync errors and the API user must check these errors.
If only warnings are logged the user could continue with full code generation, but if errors are logged these correspond
to resync errors where the parser attempted to guess how to continue. Although a valid ast was produced, the semantics of
the tree may be incorrect and code generation should not be performed. In the case of a terminal error during the parse, the
parser encountered an error and was unable to proceed, therefore no ast is produced. A call to get_errors will produce the list
of any warnings and resync errors that occured before the terminal error, as well as the description of the terminal error itself.
Some interesting points to note:
- The grammar had to be modified to be LL(1). This included eliminating left-recursion in certain productions. The modfied productions
are defined in [grammar.txt](./grammar.txt).
- One resync on error point was implemented in the parser. This was in the case of an invalid identifier. In the language spec
an identifier can never be followed by an identifier, because of this we can look for chains of Identifier and/or Invalid tokens, and
bundle them all into a single identifier by concatenating the identifier and invalid char strings of each individual token. While
this error handling is not full-proof - hence why we still error - this allows us to continue the parse, and it catches common typo
errors like invalid chars or whitespace in an identifier. It also allow us to capture and report a more specific syntax error, which
is that identifiers can contain underscores, but cannot start with underscores. This was implemented by modifying the grammar for
identifiers. This change is defined in [grammar.txt](./grammar.txt).
    - Personal notes on error handling, as well as additional candidates for future resync points are included in [errors.txt](./errors.txt).
- The unit test suite for the parser is also included in [llparser.rs](./compiler/src/llparser.rs).
### abstract syntax tree (ast) and symbol table
The ast nodes produced by the parse are defined in [ast.rs](./compiler/src/ast.rs). Analysis and transformation passes can be written
for the ast. Defined in the ast file is the AstVisitor trait. Any Rust type that implements the AstVisitor trait can walk the ast
and perform some operation. The walk is initiated by calling the accept method on the ast, and passing an instance of a visitor to the
method. The visitor is reposonsible for implementing the logic of the traversal, whether pre-order, post-order, in-order, or some
alternative. Right now, all visitors are able to mutate the ast at will. Maybe in the future we can distinguish between mutable visitors
and read-only visitors.
The ast is not explicitly linked to any symbol table or other external data structure. Instead the ast includes nodes for declarations, both
variable declarations and procedure declarations. These declarations are listed in the tree structure in the same order that they are encountered
in the source file. If a visitor needs to keep track of declarations with the scoping rules as defined in the language spec, a generic symbol table
implementation has been provided in [symtable.rs](./compiler/src/symtable.rs). The SymTable type allows a visitor to associate a string (identifier)
with some associated data. The associated data type is specified by the visitor when creating the SymTable and can vary based on the needs of the
visitor. The symbol table manages a global scope of symbols as well as a stack of local scopes. To create or destroy a local scope the visitor
must call the enter_scope and exit_scope methods respectively. To insert a symbol the visitor calls the insert method, to insert a global symbol
(no matter the current scope) the visitor calls the insert_global method, and to lookup a symbol the visitor calls the get method. The insert and get
methods search for the symbol by first iterating the stack of local scopes, starting with the most recent first, and then finally check the global
scope.
*NOTE: I really don't love this symbol table design and API, and it probably isn't the easiest to follow if you look at the code, but for the time
being it gets the job done. So sorry in advance for those who try to understand it!
### semantic checking
The semantic checking is performed by the TypeChecker, which is an AstVisitor defined in [typechecker.rs](./compiler/src/typechecker.rs). The typechecker
uses a definition of types provided in [types.rs](./compiler/src/types.rs). The typechecker utilizes a symbol table instance that maps identifier strings
to a Types type. The typechecker performs all of the scope-related semantic checks plus all of the type checks. The typechecker also mutates the ast in
place by adding type coercion nodes where needed to convert operands to the correct type, for example an IntToFloat node to convert an operand of integer
type to float type. Similar to the parser, the typechecker can generate a log or errors or warnings as it walks the ast, and again like the parser, you
query these errors with a call to get_errors after the typechecker has completed its walk.
- The unit test suite for the typechecker is also included in [typechecker.rs](./compiler/src/typechecker.rs).
### code generation
The LLVM IR code generation is performed by the CodeGen, which is an AstVisitor defined in [codegen.rs](./compiler/src/codegen.rs). The codegen makes
heavy use of the [Inkwell](https://crates.io/crates/inkwell) crate, which is a safe Rust wrapper around the LLVM C API. The CodeGen type wraps up a
Builder, Context, and Module type from Inkwell, which correlate to the same objects in the LLVM API. Throughout its walk of the ast, the codegen uses
the Builder to build up a valid LLVM IR Module, including a main function definition, all the function definitions in the source file, all the global
variables in the source file, all the string constants in the source file, as well as function declarations for all the functions defined externally
in the runtime library. The codegen utilizes two symbol table instances. One for variables that maps identifier strings to a PointerValue type. A PointerValue
is used to reference variable locations on the stack or in the global segment. The other is for procedures and maps identifier strings to FunctionValue types.
*NOTE: The Inkwell library is still in v0. It was highly recommended as the best available option for Rust projects, and it ultimately did get the job done,
but it was very difficult to use. Easily half of my time spent on this project was on the code gen phase. In hindsight I would have just generated
LLVM IR myself and then wrapped up command line calls to LLVM opt and clang. But hey, that's part of the learning process.
## runtime
The runtime directory contains the implementation of the language runtime library. The 9 functions defined in the
language spec are implemented, as well as an addition function strcmp, used for testing equality of strings. These
functions are implemented in Rust in [./runtime/src/lib.rs](./runtime/src/lib.rs), and we use the extern keyword and
the types provided in the [foreign function interface module of the standard library](https://doc.rust-lang.org/std/ffi/)
to produce C-like object code. These runtime functions are compiled into a static library libruntime.a found in ./target/debug.
# Features
All of the features specified in the language spec have been implemented.
The only obvious thing that is missing - which I don't think is a hard requirement - is column numbers reported in error and warning messages.
