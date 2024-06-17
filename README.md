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
## runtime
The runtime directory contains the implementation of the language runtime library. The 9 functions defined in the
language spec are implemented, as well as an addition function strcmp, used for testing equality of strings. These
functions are implemented in Rust, and we use the extern keyword and the types provided in the [foreign function
interface module of the standard library](https://doc.rust-lang.org/std/ffi/) to produce C-like object code. These
runtime functions are compiled into a static library libruntime.a found in ./target/debug.
