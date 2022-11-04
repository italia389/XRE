# Makefile for XRE library test utilities.	Ver. 1.1.0

# Set "InclDir" and "XRELib" below to the correct paths, then type "make".
# The "txre" executable will be created in the current directory.

ObjDir = obj
SrcDir = src
TestInclDir = include
Hdrs = $(TestInclDir)/tutil.h
Binaries = txre

# XRE library include directory.
InclDir = /usr/local/include

# XRE library file.
XRELib = /usr/local/lib/libxre.a

CC = cc
CFLAGS = $(COPTS) -funsigned-char -W -Wall -Wunused\
 -Wno-comment -Wno-missing-field-initializers -Wno-missing-braces\
 -Wno-pointer-sign -Wno-unused-parameter\
 -O2 -I$(TestInclDir) -I$(InclDir)

# Targets.
.PHONY: all clean

all: $(ObjDir) txre

$(ObjDir):
	@if [ ! -d $@ ]; then \
		mkdir $@;\
		echo "Created directory '$@'." 1>&2;\
	fi

txre: $(ObjDir)/txre.o $(ObjDir)/tutil.o
	$(CC) -o $@ $(ObjDir)/txre.o $(ObjDir)/tutil.o $(XRELib)
	strip $@

$(ObjDir)/txre.o: $(SrcDir)/txre.c $(Hdrs)
	$(CC) -c -o $@ $(CFLAGS) $(SrcDir)/txre.c

$(ObjDir)/tutil.o: $(SrcDir)/tutil.c $(Hdrs)
	$(CC) -c -o $@ $(CFLAGS) $(SrcDir)/tutil.c

clean:
	@rm -f $(Binaries) $(ObjDir)/*.o
