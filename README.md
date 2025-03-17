# WACC Compiler (AArch64 Backend)  

This project implements a full compiler for the **WACC** programming language, targeting the **AArch64** (ARMv8-A) architecture. The compiler follows a standard four-stage process:  

1. **Lexical Analysis** - Tokenizing the input source code.  
2. **Syntactic Analysis** - Parsing tokens into an abstract syntax tree (AST).  
3. **Semantic Analysis** - Checking for type and scoping errors.  
4. **Code Generation** - Producing AArch64 assembly code.  

The front-end ensures syntactic and semantic correctness, while the back-end generates assembly code that can be assembled and executed using GCC and QEMU. The compiler correctly handles variables, expressions, conditionals, loops, functions, and memory management (heap and stack).  

**Features:**  
- Error detection with meaningful diagnostics.  
- Fully functional assembler output for AArch64.  
- Automated testing via CI and LabTS integration.  

To compile and run WACC programs:  
```sh
make  
./compile program.wacc  
aarch64-linux-gnu-gcc -o program program.s  
qemu-aarch64 -L /usr/aarch64-linux-gnu/ program  
```  
