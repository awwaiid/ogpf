
# SUPER majikal makefile

OCAMLMAKEFILE = OcamlMakefile
LIBDIRS += ../untyped-lambda-calc
INCDIRS += ../untyped-lambda-calc

SOURCES =

# Extras
SOURCES += genotype/perlGenotype/pnode.ml
SOURCES += genotype/perlGenotype/parser.mly
SOURCES += genotype/perlGenotype/lexer.mll


#SOURCES += genotype/functions.ml
# Parameters
SOURCES += run/runParameters.ml

# Genotypes
SOURCES += genotype/genotype.ml
#SOURCES += genotype/fgenotype.ml
#SOURCES += genotype/lgenotype.ml
#SOURCES += genotype/stringGenotype.ml
#SOURCES += genotype/funcTree.ml
#SOURCES += genotype/lambdaGenome.ml
SOURCES += genotype/perlGenotype.ml

# Population Controllers
SOURCES += pop/population.ml
SOURCES += pop/grabBag.ml
SOURCES += pop/pop.ml

# Fitness Tests (problems)
#SOURCES += fitnessTest/soundex.ml
SOURCES += fitnessTest/fitnessTest.ml
#SOURCES += fitnessTest/rosesProblem.ml
#SOURCES += fitnessTest/trigIdent.ml
#SOURCES += fitnessTest/ticTacToe.ml
#SOURCES += fitnessTest/factorial.ml
SOURCES += fitnessTest/perlFactorial.ml

# Selection Methods
SOURCES += selectionMethod/selectionMethod.ml
SOURCES += selectionMethod/continuousTournament.ml

# Run Specification
SOURCES += run/runSpecification.ml

# Main entrypoint
SOURCES += main.ml


RESULT = gp

OCAMLBLDFLAGS = unix.cma
OCAMLNLDFLAGS = unix.cmxa

#LIBS = untyped-lambda-calc

#all: debug-code-library byte-code-library native-code-library
#all: native-code
#all: debug-code

-include $(OCAMLMAKEFILE)


