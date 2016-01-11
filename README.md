# CFlat
Basic programming language compiler built for CS536 (Intro to Compilers) at UW-Madison

Created by Colin Samplawski and Sam Mergendahl (github.com/smergendahl)


#Description

* Syntax:
  * Code blocks are enclosed in {} 
  * Lines must end with semicolons
  * Only // comments are allowed


* Variables:
  * Can be global or local
  * Can be integer and boolean variables
  * Strings can be literal values

* Statements:
  * Supports if, if-else, and while statements with any amount of nesting
  * Supports basic logical ops (!, ==, !=, <, >=, etc)
  * Supports ++, --, uniary minus, and uniary not 


* Functions:
  * Can return int, bool, or void
 * All args are pass by value
  * Recursion is supported

* Errors:
  * The compiler will catch name and type errors at compile type
  * All programs must have a main function to compile


#Usage
Make sure that the deps/ and src/ directories in the Java classpath

A configure.sh file is included that adds these directories to the classpath, but we can't guarantee that it will work on all machines

The deps directory contains the needed files for JLex and Java CUP
More info about these tools can be found here:
http://www.cs.princeton.edu/~appel/modern/java/JLex
http://www2.cs.tum.edu/projects/cup/

Then just run 'make' to build the compiler with the supplied Makefile

Call the compiler via 'java CFlat <input file>.cf <outfile>.s'

This will generate MIPS code that can be ran on a MIPS simulator such as QtSpim
More info about this tool can be found here:
http://pages.cs.wisc.edu/~larus/spim.html
