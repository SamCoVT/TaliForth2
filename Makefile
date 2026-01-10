# Makefile for Tali Forth 2
# This version: 07. Sep 2025

# Notes: The manual is not automatically updated because not everybody
# can be expected to have the asciidoc toolchain and ditaa installed.
# Tali requires python 3.x, 64tass, and GNU make to build the 65C02
# binary image.

# Example uses, where $ is the prompt (yours might be C:\>):
#
# Build tailforth-py65mon.bin for use with the py65mon simulator.
#
#   $ make
#
# Build Taliforth 2 for a different platform (steckschwein shown here).
# There must be a matching platform file in the platform folder.
#
#   $ make taliforth-steckschwein.bin
#
# Run tests
#
#   $ make tests
# or (much faster, but needs gcc installed)
#   $ make ctests
#
# Build and run Taliforth
#
#   $ make sim
# or (much faster, but needs gcc installed)
#   $ make csim
#
# The cxxx targets use the C-based c65 simulator rather than the default
# py65mon python simulator.  This runs 10-100x faster but
# lacks py65mon's monitor facilities for debugging.
# See https://github.com/patricksurry/c65
# c65 should build automatically as a submodule in `tools/c65/`.
# It's been tested on posix-based systems like OS X and Windows WSL
# (see https://learn.microsoft.com/en-us/windows/wsl/install).
# A native Windows port for mingw is still TODO
# (see https://github.com/SamCoVT/TaliForth2/issues/74).

# Determine which python launcher to use (python3 on Linux and OSX,
# "py -3" on Windows) and other OS-specific commands (rm vs del).
ifdef OS
	RM := del
	PYTHON := py -3
	TODAY := "\"$(shell date /t)\""
else
	RM := rm -f
	PYTHON := python3
	TODAY := "\"$(shell date +%Y-%m-%d)\""
endif

# Identify git version (or set to "unknown")
GIT_IDENT := "\"$(shell git describe --dirty --always --tags 2>/dev/null || echo unknown)\""

# enumerate the known platforms
PLATFORMS := $(patsubst platform/%/,%,$(dir $(wildcard platform/*/platform.asm)))

# prepend hyphen to VARIANT if defined
_VARIANT := $(if $(VARIANT),-${VARIANT},)


COMMON_SOURCES=taliforth.asm definitions.asm $(wildcard words/*.asm) stringtable.asm
TEST_SUITE=tests/core_a.fs tests/core_b.fs tests/core_c.fs tests/string.fs tests/double.fs \
    tests/facility.fs tests/ed.fs tests/asm.fs tests/tali.fs \
    tests/tools.fs tests/block.fs tests/search.fs tests/user.fs tests/cycles.fs
TEST_SOURCES=tests/talitest.py $(TEST_SUITE)

C65_DIR=tools/c65
C65=$(C65_DIR)/c65

all: taliforth-py65mon.bin docs/WORDLIST.md
clean:
	$(RM) *.bin *.prg
	make -C $(C65_DIR) clean

platforms:
	@echo Available platforms: $(PLATFORMS)

# create a phony target for each platform, so we can do make <platformname> `
.PHONY: $(PLATFORMS)

# For known platforms, the dummy target should build the binary for the selected variant
# e.g. make sbc VARIANT=dbg should build taliforth-sbc-dbg.bin
$(PLATFORMS): %: taliforth-%${_VARIANT}.bin

# Specific rule for PCE platform to output taliforth.pce
taliforth-pce${_VARIANT}.bin: platform/pce/*.asm platform/pce/*/*.asm platform/pce/platform_forth.asc $(COMMON_SOURCES)
	64tass --nostart \
	-D GIT_IDENT=${GIT_IDENT} \
	-D TODAY=${TODAY} \
	--list=platform/pce/pce${_VARIANT}-listing.txt \
	--vice-labels \
	--labels=platform/pce/pce${_VARIANT}-labelmap.txt \
	-D VARIANT:=\"${VARIANT}\" \
	--output taliforth.pce \
	$<
	python3 tools/sort_vice_labels.py platform/pce/pce${_VARIANT}-labelmap.txt

# Note, the _VARIANT variable may be empty if no variant defined.
taliforth-%${_VARIANT}.bin: platform/%/*.asm platform/%/*/*.asm platform/%/platform_forth.asc $(COMMON_SOURCES)
	64tass --nostart \
	-D GIT_IDENT=${GIT_IDENT} \
	-D TODAY=${TODAY} \
	--list=platform/$*/$*${_VARIANT}-listing.txt \
	--vice-labels \
	--labels=platform/$*/$*${_VARIANT}-labelmap.txt \
	-D VARIANT:=\"${VARIANT}\" \
	--output $@ \
	$<
	python3 tools/sort_vice_labels.py platform/$*/$*${_VARIANT}-labelmap.txt

taliforth-%${_VARIANT}.prg: platform/%/*.asm platform/%/*/*.asm platform/%/platform_forth.asc $(COMMON_SOURCES)
	64tass --cbm-prg \
	-D GIT_IDENT=${GIT_IDENT} \
	-D TODAY=${TODAY} \
	--list=platform/$*/$*${_VARIANT}-listing.txt \
	--labels=platform/$*/$*${_VARIANT}-labelmap.txt \
	-D VARIANT:=\"${VARIANT}\" \
	--output $@ \
	$<
	python3 tools/sort_vice_labels.py platform/$*/$*${_VARIANT}-labelmap.txt

# Compact the Forth word definitons for inclusion in the binary.
# This will only process the file if it exists.
# The || true on the end causes make to always think this command succeeds.
platform/%/platform_forth.asc: platform/%/platform_forth.fs
	test -f $< && $(PYTHON) tools/forth_pack.py -i $< > $@ || true

# Allow platform_forth.fs and platform_words.asm to be missing.
platform/%/platform_forth.fs:
	@echo No platform_forth.fs for this platform.
platform/%/platform_words.asm:
	@echo No platform_words.asm for this platform.

# Allow project subdirectories containing .asm files to be be missing.
platform/%/*/*.asm:
	@echo No platform subdirectories containing assembly for this platform.



# Automatically update the wordlist which also gives us the status of the words
# We need for the binary to be generated first or else we won't be able to find
# new words in the label listing
docs/WORDLIST.md: tools/generate_wordlist.py taliforth-py65mon.bin
	$(PYTHON) tools/generate_wordlist.py > docs/WORDLIST.md


# Some convenience targets to make running the tests and simulation easier.

# Build the c65 simulator
# After a normal git clone of Taliforth, c65 is still an empty folder
# so init and update the module if the Makefile is missing
$(C65_DIR)/Makefile:
ifeq (, $(shell git --version 2>/dev/null))
	$(error 'git' not found, can't initialize c65 submodule)
else
	git submodule init
	git submodule update $(C65_DIR)
endif

# Always check to see if c65 needs rebuilt
# but also make sure we have sources checked out first
.PHONY: c65check

$(C65): $(C65_DIR)/Makefile c65check
	make -C $(C65_DIR)

# Convenience target for regular tests.
tests:	tests/results.txt

# Run all of the tests.
ctests: $(C65) taliforth-c65.bin $(TEST_SOURCES)
	cd tests && $(PYTHON) ./talitest_c65.py

tests/results.txt:	taliforth-py65mon.bin $(TEST_SOURCES)
	cd tests && $(PYTHON) ./talitest.py

# Convenience target for parallel tests (Linux only)
ptests:	taliforth-py65mon.bin $(TEST_SOURCES)
	cd tests && ./ptest.sh

# Convenience target to run the py65mon simulator.
# Because taliforth-py65mon.bin is listed as a dependency, it will be
# reassembled first if any changes to its sources have been made.
sim: taliforth-py65mon.bin
	py65mon -m 65c02 -r taliforth-py65mon.bin

csim: $(C65) taliforth-c65.bin
	$(C65) -qq -r taliforth-c65.bin

cdbg: $(C65) taliforth-c65.bin
	$(C65) -r taliforth-c65.bin -l platform/c65/c65-labelmap.txt

# Some convenience targets for the documentation.
docs/manual.html: docs/*.adoc
	cd docs && asciidoctor -a toc=left manual.adoc

docs/ch_glossary.adoc: tools/generate_glossary.py $(wildcard words/*.asm)
	$(PYTHON) tools/generate_glossary.py > docs/ch_glossary.adoc

# The diagrams use ditaa to generate pretty diagrams from text files.
# They have their own makefile in the docs/pics directory.
docs-diagrams: docs/pics/*.txt
	cd docs/pics && $(MAKE)

docs: docs/manual.html docs-diagrams

# This one is experimental at the moment.
docsmd: docs/manual.html
	cd docs && ./asciidoc_to_markdown.sh

docspdf:	docs
	cd docs && asciidoctor-pdf -v manual.adoc

# A convenience target for preparing for a git commit.
gitready: docs all ctests
