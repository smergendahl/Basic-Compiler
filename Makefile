###
# This Makefile can be used to build the CFlat language compiler
# This will generate, the CFlat.class file, which can be ran with
# java CFlat <inputfile> <outputfile>
#
# In order for successful compilation, the deps/ directory will need to be
# in the Java classpath. A configure script file is included, but it is not
# guaranteed to work on all systems.
#
# make clean removes all generated files.
#
###

JC = javac

CFlat.class: CFlat.java parser.class Yylex.class ASTnode.class
	$(JC) -g CFlat.java

parser.class: src/parser.java ASTnode.class Yylex.class ErrMsg.class
	$(JC) src/parser.java

Yylex.class: src/CFlat.jlex.java sym.class ErrMsg.class
	$(JC) src/CFlat.jlex.java

ASTnode.class: src/ast.java src/Type.java
	$(JC) -g src/ast.java

sym.class: src/sym.java
	$(JC) -g src/sym.java

ErrMsg.class: src/ErrMsg.java
	$(JC) src/ErrMsg.java

Sym.class: src/Sym.java src/Type.java src/ast.java
	$(JC) -g src/Sym.java
	
SymTable.class: src/SymTable.java src/Sym.java src/DuplicateSymException.java src/EmptySymTableException.java
	$(JC) -g src/SymTable.java
	
Type.class: src/Type.java
	$(JC) -g src/Type.java

DuplicateSymException.class: src/DuplicateSymException.java
	$(JC) -g src/DuplicateSymException.java
	
EmptySymTableException.class: src/EmptySymTableException.java
	$(JC) -g src/EmptySymTableException.java

###
# clean
###
clean:
	rm -f *~ *.class
	rm -f src/*.class
