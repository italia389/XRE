# Test makefile for XRE library.	Ver. 1.0.0

# Set "InclDir" and "Lib" below to the correct paths, then type "make".
# The "txre" executable will be created in the current directory.

Hdrs = tutil.h
InclDir = ../xrelib/include
#InclDir = /usr/local/include
Lib = ../xrelib/lib/libxre.a
#Lib = /usr/local/lib/libxre.a

CC = cc
CFLAGS = $(COPTS) -funsigned-char -W -Wall -Wunused -Wno-bitwise-op-parentheses\
 -Wno-comment -Wno-missing-field-initializers -Wno-missing-braces\
 -Wno-pointer-sign -Wno-unused-parameter\
 -O2 -I$(InclDir)

# Targets.
txre: txre.o tutil.o
	$(CC) -o $@ txre.o tutil.o $(Lib)
	strip txre

txre.o: txre.c $(Hdrs)
	$(CC) -c -o $@ $(CFLAGS) txre.c

tutil.o: tutil.c $(Hdrs)
	$(CC) -c -o $@ $(CFLAGS) tutil.c
