# Makefile for XRE library.	Ver. 1.2.0

# Definitions.
MAKEFLAGS = --no-print-directory
ProjName = XRE
LibName = libxre.a
LibGlob = libxre*
SrcDir = src
ObjDir = obj
InclDir = include
InclFlags = -I$(InclDir)
LibDir = lib
HdrFileGlob = {xre,regex,stdos}.h
Install1 = /usr/local
ManDir = share/man
OldManGlob = man3/{xre_{approx,core,info,reverse},xreg{info,{,n,w,wn}rev}}.3
SrcManDir = man
SrcTestDir = test
TestFiles = Makefile include/tutil.h src/tutil.c src/txre.c
SrcTestInstructions = doc/UserInstall.txt
DestTestDir = xre_test
DestTestInstructions = Install.txt

# Options and arguments to the C compiler.
CC = cc
#DFlags = -DXRE_Debug=1		# Full debugging (to standard error).
#DFlags =			# No debugging, keep assertions.
DFlags = -DNDEBUG		# No debugging or assertions (Production).

CFLAGS = $(DFlags) -std=c99 -funsigned-char -W -Wall -Wextra -Wunused\
 -Wno-comment -Wno-missing-field-initializers -Wno-missing-braces -Wno-parentheses\
 -Wno-pointer-sign -Wno-unused-parameter $(COPTS)\
 -O2 $(InclFlags)

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
.PHONY: all build-msg uninstall install user-install clean

all: build-msg $(LibName)

build-msg:
	@if [ ! -f $(LibName) ]; then \
		echo "Building $(ProjName) library..." 1>&2;\
	fi

$(LibName): $(ObjDir) $(ObjFiles)
	@if [ -f $(LibName) ]; then \
		rm $(LibName);\
	fi;\
	if [ "`uname -s`" = Darwin ]; then \
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
		mkdir $@ && echo "Created directory '$@'." 1>&2;\
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

uninstall:
	@echo 'Uninstalling...' 1>&2;\
	if [ -n "$(INSTALL)" ] && [ -f "$(INSTALL)/$(LibDir)/$(LibName)" ]; then \
		destDir="$(INSTALL)";\
	elif [ -f "$(Install1)/$(LibDir)/$(LibName)" ]; then \
		destDir=$(Install1);\
	else \
		destDir=;\
	fi;\
	if [ -z "$$destDir" ]; then \
		echo "'$(LibName)' library not found." 1>&2;\
	else \
		for f in "$$destDir"/$(LibDir)/$(LibGlob); do \
			rm -f "$$f" 2>/dev/null && echo "Deleted '$$f'" 1>&2;\
		done;\
		for f in "$$destDir"/include/$(HdrFileGlob) "$$destDir"/$(ManDir)/$(OldManGlob); do \
			if [ -e "$$f" ] || [ -L "$$f" ]; then \
				rm -rf "$$f" && echo "Deleted '$$f'" 1>&2;\
			fi;\
		done;\
		cd $(SrcManDir) || exit $$?;\
		for n in 3 7; do \
			for x in *.$$n; do \
				f="$$destDir"/$(ManDir)/man$$n/$$x;\
				rm -f "$$f" 2>/dev/null && echo "Deleted '$$f'" 1>&2;\
			done;\
		done;\
	fi;\
	echo 'Done.' 1>&2

install: uninstall
	@echo 'Beginning installation...' 1>&2;\
	umask 022;\
	myID=`id -u`;\
	comp='-C' permCheck=true destDir=$(INSTALL);\
	if [ -z "$$destDir" ]; then \
		destDir=$(Install1);\
	else \
		[ $$myID -ne 0 ] && permCheck=false;\
		if [ ! -d "$$destDir" ]; then \
			mkdir "$$destDir" || exit $$?;\
		fi;\
		destDir=`cd "$$destDir"; pwd`;\
	fi;\
	if [ $$permCheck = false ]; then \
		own= grp= owngrp=;\
		dmode=755 fmode=644;\
	else \
		if [ `uname -s` = Darwin ]; then \
			fmt=-f perm='%p';\
		else \
			fmt=-c perm='%a';\
		fi;\
		eval `stat $$fmt 'own=%u grp=%g' "$$destDir"`;\
		if [ $$own -ne $$myID ] && [ $$myID -ne 0 ]; then \
			owngrp=;\
		else \
			owngrp="-o $$own -g $$grp";\
		fi;\
		eval `stat $$fmt $$perm "$$destDir" |\
		 sed 's/^/000/; s/^.*\(.\)\(.\)\(.\)\(.\)$$/p3=\1 p2=\2 p1=\3 p0=\4/'`;\
		dmode=$$p3$$p2$$p1$$p0 fmode=$$p3$$(($$p2 & 6))$$(($$p1 & 6))$$(($$p0 & 6));\
		[ $$p3 -gt 0 ] && comp=;\
	fi;\
	[ -f $(LibName) ] || { echo "Error: File '`pwd`/$(LibName)' does not exist" 1>&2; exit 1; };\
	[ -d "$$destDir"/$(LibDir) ] || install -v -d $$owngrp -m $$dmode "$$destDir"/$(LibDir) 1>&2;\
	install -v $$comp $$owngrp -m $$fmode $(LibName) "$$destDir"/$(LibDir) 1>&2;\
	[ -d "$$destDir/$(InclDir)" ] || install -v -d $$owngrp -m $$dmode "$$destDir/$(InclDir)" 1>&2;\
	install -v $$comp $$owngrp -m $$fmode $(InclDir)/$(HdrFileGlob) "$$destDir/$(InclDir)" 1>&2;\
	cd $(SrcManDir) || exit $$?;\
	[ -d "$$destDir"/$(ManDir) ] || install -v -d $$owngrp -m $$dmode "$$destDir"/$(ManDir) 1>&2;\
	for n in 3 7; do \
		[ -d "$$destDir"/$(ManDir)/man$$n ] || install -v -d $$owngrp -m $$dmode "$$destDir"/$(ManDir)/man$$n 1>&2;\
		for x in *.$$n; do \
			if [ -L $$x ]; then \
				umask 133;\
				cp -v -R $$x "$$destDir"/$(ManDir)/man$$n 1>&2;\
				if [ -n "$$owngrp" ] && [ `uname -s` = Darwin ]; then \
					chown -h "$$own:$$grp" "$$destDir"/$(ManDir)/man$$n 1>&2;\
				fi;\
			else \
				umask 022;\
				install -v $$comp $$owngrp -m $$fmode $$x "$$destDir"/$(ManDir)/man$$n 1>&2;\
			fi;\
		done;\
	done;\
	echo "Done.  $(ProjName) library files installed in '$$destDir'." 1>&2

user-install:
	@echo "Beginning test software installation..." 1>&2;\
	[ -d ~/$(DestTestDir) ] || install -v -d ~/$(DestTestDir) 1>&2;\
	cd $(SrcTestDir) || exit $$?;\
	for f in $(TestFiles); do \
		d=`dirname $$f`;\
		if [ -n "$$dirname" ]; then \
			[ -d ~/$(DestTestDir)/$$d ] || install -v -d ~/$(DestTestDir)/$$d 1>&2;\
		else \
			d=.;\
		fi;\
		install -v -C -m 640 -b $$f ~/$(DestTestDir)/$$d 1>&2;\
	done;\
	cd ..;\
	install -v -C -m 640 $(SrcTestInstructions) ~/$(DestTestDir)/$(DestTestInstructions) 1>&2;\
	echo "Done.  $(ProjName) test files installed in '`cd; pwd`/$(DestTestDir)'." 1>&2

clean:
	@rm -f $(LibName) $(ObjDir)/*.o;\
	echo '$(ProjName) binaries deleted.' 1>&2
