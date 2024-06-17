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
## llparser
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
## runtime
The runtime directory contains the implementation of the language runtime library. The 9 functions defined in the
language spec are implemented, as well as an addition function strcmp, used for testing equality of strings. These
functions are implemented in Rust in [./runtime/src/lib.rs](./runtime/src/lib.rs), and we use the extern keyword and
the types provided in the [foreign function interface module of the standard library](https://doc.rust-lang.org/std/ffi/)
to produce C-like object code. These runtime functions are compiled into a static library libruntime.a found in ./target/debug.
