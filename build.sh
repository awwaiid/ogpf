#!/bin/sh

experiment=$1
ocamlbuild -Is experiment/$1,genotype,pop,selectionMethod gp.native

