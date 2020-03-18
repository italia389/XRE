# Root makefile for XRE library.	Ver. 1.0.0

# Definitions.
Lib = libxre.a
Libname = xre
InclDir = include
Install1 = /usr/local
ManDir = man
TestFiles = tutil.h tutil.c txre.c
TestDir = test
UserTestDir = xre_test

# Targets.
all:	xrelib link

xrelib:
	@if [ `uname -s` = Darwin ]; then platform=macos; else platform=linux; fi;\
	if [ ! -f $$platform/$(Lib) ]; then \
		echo "Building $$platform XRE library..." 1>&2;\
		cd $$platform || exit $$?;\
		exec make;\
	fi

link:
	@if [ `uname -s` = Darwin ]; then platform=macos; else platform=linux; fi;\
	rm -f $(Lib);\
	ln -s $$platform/$(Lib) $(Lib);\
	[ $$platform = macos ] && chmod -h 640 $(Lib);\
	echo "'$(Lib)' link file created -> $$platform/$(Lib)" 1>&2

uninstall:
	@echo 'Uninstalling...' 1>&2;\
	if [ -n "$(INSTALL)" ] && [ -f "$(INSTALL)/lib/$(Lib)" ]; then \
		destDir="$(INSTALL)";\
	elif [ -f "$(Install1)/lib/$(Lib)" ]; then \
		destDir=$(Install1);\
	else \
		destDir=;\
	fi;\
	if [ -z "$$destDir" ]; then \
		echo "'$(Lib)' library not found." 1>&2;\
	else \
		for f in $$destDir/lib/$(Lib) $$destDir/share/man/man[37]/$(Libname)*\
		 $$destDir/include/{$(Libname),regex,stdos}.h; do \
			if [ -e "$$f" ]; then \
				rm -rf "$$f" && echo "Deleted '$$f'" 1>&2;\
			fi;\
		done;\
	fi;\
	echo 'Done.' 1>&2

install: uninstall
	@echo 'Beginning installation...' 1>&2;\
	umask 022;\
	comp='-C' permCheck=true destDir=$(INSTALL);\
	if [ `id -u` -ne 0 ]; then \
		if [ -z "$$destDir" ]; then \
			if uname -v | fgrep -qi debian; then \
				echo 'Error: You must be root to perform a site-wide install' 1>&2;\
				exit 1;\
			fi;\
		else \
			permCheck=false;\
		fi;\
	fi;\
	[ -z "$$destDir" ] && destDir=$(Install1);\
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
		owngrp="-o $$own -g $$grp";\
		eval `stat $$fmt $$perm "$$destDir" |\
		 sed 's/^/000/; s/^.*\(.\)\(.\)\(.\)\(.\)$$/p3=\1 p2=\2 p1=\3 p0=\4/'`;\
		dmode=$$p3$$p2$$p1$$p0 fmode=$$p3$$(($$p2 & 6))$$(($$p1 & 6))$$(($$p0 & 6));\
		[ $$p3 -gt 0 ] && comp=;\
	fi;\
	[ -f $(Lib) ] || { echo "Error: File '`pwd`/$(Lib)' does not exist" 1>&2; exit 1; };\
	[ -d "$$destDir"/lib ] || install -v -d $$owngrp -m $$dmode "$$destDir"/lib 1>&2;\
	install -v $$owngrp -m $$fmode $(Lib) "$$destDir"/lib 1>&2;\
	[ -d "$$destDir/$(InclDir)" ] || install -v -d $$owngrp -m $$dmode "$$destDir/$(InclDir)" 1>&2;\
	install -v $$owngrp -m $$fmode $(InclDir)/{$(Libname),regex,stdos}.h "$$destDir/$(InclDir)" 1>&2;\
	cd $(ManDir) || exit $$?;\
	for n in 3 7; do \
		[ -d "$$destDir"/share/man/man$$n ] || install -v -d $$owngrp -m $$dmode "$$destDir"/share/man/man$$n 1>&2;\
		for x in *.$$n; do \
			install -v $$comp $$owngrp -m $$fmode $$x "$$destDir"/share/man/man$$n 1>&2;\
		done;\
	done;\
	echo "Done.  XRE files installed in '$$destDir'." 1>&2

user-install:
	@echo 'Beginning test program installation...' 1>&2;\
	cd $(TestDir) || exit $$?;\
	[ -d ~/$(UserTestDir) ] || install -v -d ~/$(UserTestDir) 1>&2;\
	install -v -C -m 640 -b Makefile $(TestFiles) ~/$(UserTestDir) 1>&2;\
	echo "Done.  XRE test files installed in '`cd; pwd`/$(UserTestDir)'." 1>&2

clean:
	@if [ `uname -s` = Darwin ]; then platform=macos; else platform=linux; fi;\
	cd $$platform || exit $$?;\
	make $@; cd ..;\
	echo "'$$platform' binaries deleted." 1>&2;\
	if [ -L $(Lib) ]; then \
		rm -f $(Lib) && echo "'$(Lib)' link file deleted." 1>&2;\
	fi
