
# SUPER majikal makefile

OCAMLMAKEFILE = OcamlMakefile
#LIBDIRS += ../untyped-lambda-calc
#INCDIRS += ../untyped-lambda-calc

SOURCES =

# Genotypes
SOURCES += genotype/genotype.ml
SOURCES += genotype/fgenotype.ml
#SOURCES += genotype/stringGenotype.ml
SOURCES += genotype/funcTree.ml

# Population Controllers
SOURCES += pop/population.ml
SOURCES += pop/grabBag.ml
SOURCES += pop/pop.ml

# Fitness Tests (problems)
#SOURCES += fitnessTest/soundex.ml
SOURCES += fitnessTest/fitnessTest.ml
#SOURCES += fitnessTest/rosesProblem.ml
SOURCES += fitnessTest/trigIdent.ml

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

# LIBS = untyped-lambda-calc

#all: debug-code-library byte-code-library native-code-library
all: native-code
#all: debug-code

-include $(OCAMLMAKEFILE)


