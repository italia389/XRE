# Makefile for XRE library.		Ver. 1.0.0

# Definitions.
LibName = libxre.a
SrcDir = ../src
ObjDir = obj
InclDir = ../include

# Options and arguments to the C compiler.
CC = cc
#DFlags = -DXRE_Debug		# Full debugging (to standard error).
#DFlags =			# No debugging, keep assertions.
DFlags = -DNDEBUG		# No debugging or assertions (Production).

#CFLAGS = $(DFlags) -std=c99 -funsigned-char -W -Wall -Wunused -Wno-comment\
# -Wno-missing-field-initializers -Wno-missing-braces -Wno-pointer-sign\
# -O2 -I$(InclDir)
CFLAGS = $(DFlags) -std=c99 -funsigned-char -W -Wall -Wunused -Wno-pointer-sign -O2 -I$(InclDir)

# List of header files.
HdrFiles =\
 $(InclDir)/xre.h\
 $(InclDir)/internal.h\
 $(InclDir)/mem.h\
 $(InclDir)/parse.h

# List of object files.
ObjFiles =\
 $(ObjDir)/compile.o\
 $(ObjDir)/match.o\
 $(ObjDir)/mem.o\
 $(ObjDir)/parse.o\
 $(ObjDir)/xreg.o

# Targets.
$(LibName): $(ObjDir) $(ObjFiles)
	-[ -f $(LibName) ] && rm $(LibName)
	@if [ "`uname -s`" = Darwin ]; then \
		echo 'libtool -static -o $(LibName) $(ObjFiles)' 1>&2;\
		libtool -static -o $(LibName) $(ObjFiles);\
	else \
		echo 'ar r $(LibName) $(ObjFiles)' 1>&2;\
		ar r $(LibName) $(ObjFiles);\
		echo 'ranlib $(LibName)' 1>&2;\
		ranlib $(LibName);\
	fi

$(ObjDir):
	@if [ ! -d $@ ]; then \
		mkdir $@ && echo "Directory '$@' created." 1>&2;\
	fi

$(ObjDir)/compile.o: $(SrcDir)/compile.c $(HdrFiles)
	$(CC) -c -o $(ObjDir)/compile.o $(CFLAGS) $(SrcDir)/compile.c
$(ObjDir)/match.o: $(SrcDir)/match.c $(HdrFiles)
	$(CC) -c -o $(ObjDir)/match.o $(CFLAGS) $(SrcDir)/match.c
$(ObjDir)/mem.o: $(SrcDir)/mem.c $(HdrFiles)
	$(CC) -c -o $(ObjDir)/mem.o $(CFLAGS) $(SrcDir)/mem.c
$(ObjDir)/parse.o: $(SrcDir)/parse.c $(HdrFiles)
	$(CC) -c -o $(ObjDir)/parse.o $(CFLAGS) $(SrcDir)/parse.c
$(ObjDir)/xreg.o: $(SrcDir)/xreg.c $(HdrFiles)
	$(CC) -c -o $(ObjDir)/xreg.o $(CFLAGS) $(SrcDir)/xreg.c

clean:
	@rm -f $(LibName) $(ObjDir)/*.o
