This is where all of Tali's native forth words are defined.

At the top level `taliforth.asm` includes `all.asm` here which defines
the triad COLD, ABORT, QUIT and then includes the remaining word groups.
Some words are conditionally included according to the platform
configuration of TALIFORTH_OPTIONAL_WORDS (see platform/README.md).
Dictionary entries (name tokens) for all words are defined in `headers.asm`.
Note there are several distinct wordlists.

The words are organized following https://forth-standard.org/standard/words
along with some words specific to Tali Forth 2.  These extensions include:

- `tali.asm` low-level builtin helper words
- `assembler.asm` a 65c02 assembler
- `disasm.asm` a 65c02 disassembler
- `ed.asm` a line-based editor
- `editor.asm` a block-based editor


