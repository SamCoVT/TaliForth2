# Makefile for Tali Forth 2
# This version: 14. Jan 2020

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
# or
#   $ make ctests
#
# Build and run Taliforth
#
#   $ make sim
# or
#   $ make csim
#
# The cxxx targets use the C-based c65 simulator rather than the default
# py65mon python simulator.  This runs 10-100x faster but
# lacks py65mon's monitor facilities for debugging.
# c65 should build automatically from the sources in `c65/`.
# It's been tested on posix-based systems like OS X and Windows WSL
# (see https://learn.microsoft.com/en-us/windows/wsl/install).
# A native Windows port for mingw is still TODO
# (see https://github.com/SamCoVT/TaliForth2/issues/74).

# Determine which python launcher to use (python3 on Linux and OSX,
# "py -3" on Windows) and other OS-specific commands (rm vs del).
ifdef OS
	RM = del
	PYTHON = py -3
else
	RM = rm -f
	PYTHON = python3
endif

COMMON_SOURCES=taliforth.asm definitions.asm $(wildcard words/*.asm) strings.asm forth_words.asc user_words.asc opcodes.asm
TEST_SUITE=tests/core_a.fs tests/core_b.fs tests/core_c.fs tests/string.fs tests/double.fs \
    tests/facility.fs tests/ed.fs tests/asm.fs tests/tali.fs \
    tests/tools.fs tests/block.fs tests/search.fs tests/user.fs tests/cycles.fs
TEST_SOURCES=tests/talitest.py $(TEST_SUITE)

C65=c65/c65
C65_SOURCES=c65/*.c c65/*.h

all: taliforth-py65mon.bin docs/WORDLIST.md
clean:
	$(RM) *.bin *.prg
	make -C c65 clean

taliforth-%.bin: platform/platform-%.asm $(COMMON_SOURCES)
	64tass --nostart \
	--list=docs/$*-listing.txt \
	--labels=docs/$*-labelmap.txt \
	--output $@ \
	$<

taliforth-%.prg: platform/platform-%.asm $(COMMON_SOURCES)
	64tass --cbm-prg \
	--list=docs/$*-listing.txt \
	--labels=docs/$*-labelmap.txt \
	--output $@ \
	$<

# Convert the high-level Forth words to ASCII files that Ophis can include
%.asc: forth_code/%.fs
	$(PYTHON) forth_code/forth_to_ophisbin.py -i $< > $@

# Automatically update the wordlist which also gives us the status of the words
# We need for the binary to be generated first or else we won't be able to find
# new words in the label listing
docs/WORDLIST.md: tools/generate_wordlist.py taliforth-py65mon.bin
	$(PYTHON) tools/generate_wordlist.py > docs/WORDLIST.md


# Some convenience targets to make running the tests and simulation easier.

# Build the c65 simulator
$(C65): $(C65_SOURCES)
	make -C c65

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
	$(C65) -r taliforth-c65.bin

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
