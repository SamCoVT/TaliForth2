# Platform configurations for Tali Forth 2

First version: 17. Oct 2018
This version: 01. Jun 2024

This folder contains platform-specific configurations for building Tali Forth 2.
The default configuration for testing with the py65 emulator is
`platform-py65mon.asm`.   A similar configuration supporting external block
IO with the c65 emulator is `platform-c65.asm`.   These both assume a memory layout
with at least 32Kb of ROM.  The `platform-minimal.asm` configuration strips out
a number of optional features to run in 12-16Kb of ROM.

Other configurations are included to make life easier for individual developers
and as examples for people who want to port Tali to their own hardware.
Those not mentioned above may not be up to date with the latest changes: *caveat emptor*.

A configuration file is simply a [64tass](https://tass64.sourceforge.net/)
`.asm` file that customizes Tali's memory layout and feature set by
overriding default parameters.  The configuration then includes the global
`taliforth.asm` and finally defines the basic I/O routines that Tali uses
to get and put characters.

By default the top-level `make` will build `platform-py65mon.bin`.
To build for a different platform simply specify the binary corresponding
to the platform's `.asm` file here. For example build a 16K image with:

    make taliforth-minimal.bin

## Creating or modifying a configuration

The configuration has these responsibilities:

1. Customize the memory layout.  Where does RAM end and ROM begin?  What
   should be reserved for Tali or your own use?

2. Which Tali Forth 2 features should be included?

3. Provide the I/O routines `kernel_init`, `kernel_bye`, `kernel_getc`, `kernel_putc`
   and `kernel_kbhit`, along with the `s_kernel_id` string to show on startup.

4. Define the 65c02 reset and interrupt vectors at $fffa-$ffff.

### Tali Forth 2 memory layout

Tali's memory usage is highly configurable although you probably won't need to
change much at first.  See "Memory map" in the Developer's Guide for further discussion.
The diagram below sketches how Tali uses memory.
Note: it's not only ugly, but also not to scale.
The addresses shown on the left refer to the standard py65/c65 configuration.
You can modify for your needs by customizing the variables shown on the right.

As you can see in simulator.asm, shared by the py65mon and c65 platforms,
you likely only need to define ram_end and choose the starting location
for Tali's ROM code.

    $0000  +-------------------+  ram_start, zpage, user0
           |   Tali zp vars    |  (see cold_zp_table)
           +-------------------+
           |                   |
           +~~~~~~~~~~~~~~~~~~~+  <-- dsp
           |  ^  Forth Data    |
           |  |    Stack       |
    $0078  +-------------------+  dsp0
           |    Flood plain    |
    $007F  +-------------------+
           |                   |
           |   (free space)    |
           |                   |
    $0100  +-------------------+  stack0, lcbstack0
           |  |  Loop control  |
           |  v                |  <-- loopctrl
           |                   |
           |  ^  Return Stack  |  <-- rsp
           |  |                |
    $0200  +-------------------+  rsp0, buffer, buffer0
           |    Input Buffer   |
    $0300  +-------------------+  cp0, up
           | Native forth vars |  (see cold_user_table)
    $0400  +-------------------+  blkbuffer_offset
           |  1K block buffer  |
    $0800  +-------------------+  Starting cp value
           |  |  Dictionary    |
           |  v    (RAM)       |
           |                   |
    ...    ~~~~~~~~~~~~~~~~~~~~~  <-- cp aka HERE
           |                   |
           |                   |
    $7C00  +-------------------+  hist_buff, cp_end
           |   Input History   |
           |    for ACCEPT     |  (see TALI_OPTION_HISTORY)
           |  8x128B buffers   |
    $7fff  +-------------------+  ram_end

    $8000  +-------------------+  forth, code0, xt_cold
           |                   |
           |                   |
           | Tali Forth 2 Code |
           |                   |
           |                   |
    $dfff  ~~~~~~~~~~~~~~~~~~~~~  code_end (approximately $dfff)
           |                   |
           |   (free space)    |
           |                   |
    $f000  +-------------------+  io_start
           |  Virtual IO h/w   |  (specific to py65/c65)
    $f010  +-------------------+  io_end
           |  Kernel IO code   |
           ~~~~~~~~~~~~~~~~~~~~~
           |    (free space)   |
           +-------------------+
    $fffa  |  v_nmi            |  6502 vectors
    $fffc  |  v_reset          |
    $fffe  |  v_irq            |
           +-------------------+

(Note for developers: some of the py65/c65 defaults are hard-coded in
the testing routines, particularly the size of the input history buffer, the
offset for PAD, and the total RAM size. If these defaults are changed, the tests will
have to be changed as well.)

### Optional features

Tali Forth 2 is a bit of a beast, expecting about 24K of ROM space for all features.
For some applications, the user might not need certain words and would
prefer to have the memory back instead.  If you define the list
TALI_OPTIONAL_WORDS then only those features will be
assembled.  If TALI_OPTIONAL_WORDS is not defined in your platform file,
you will get everything.
```
TALI_OPTIONAL_WORDS := [
    "ed", "editor", "ramdrive", "block",
    "environment?", "assembler", "disassembler", "wordlist"
]
```
The currently available groups are:

- `ed` is a string editor. (~1.5K)
- `editor` is a block editor. The EDITOR-WORDLIST will also be removed. (~0.25K)
- `ramdrive` is for testing block words without a block device. (~0.3K)
- `block` is the optional BLOCK words. (~1.4K)
- `environment?` is the ENVIRONMENT? word.  While this is a core word
   for ANS-2012, it uses a lot of strings and therefore takes up a lot of memory. (~0.2K)
- `assembler` is a 65c02 assembler.
   The ASSEMBLER-WORDLIST will also be removed if the assembler is removed. (~3.2K)
- `disassembler` is the disassembler word DISASM. (~0.6K)
   If both the assembler and dissasembler are removed, the tables
   (used for both assembling and disassembling) will be removed
   for additional memory savings. (extra ~1.6K)
- `wordlist` is for the optional SEARCH-ORDER words (eg. wordlists)
   Note: Without "wordlist", you will not be able to use any words from
   the EDITOR or ASSEMBLER wordlists (they should probably be disabled
   by also removing "editor" and "assembler"), and all new words will
   be compiled into the FORTH wordlist. (~0.9K)

You can control what character(s) are printed by the word
CR in order to move the cursor to the next line.  The default is "lf"
for a line feed character (#10).  "cr" will use a carriage return (#13).
Having both will use a carriage return followed by a line feed.  This
only affects output.  Either CR or LF can be used to terminate lines
on the input.
```
TALI_OPTION_CR_EOL := [ "lf" ]
; TALI_OPTION_CR_EOL := [ "cr" ]
; TALI_OPTION_CR_EOL := [ "cr", "lf" ]
```
By default Tali provides editable input history buffers via ctrl-n/ctrl-p.
These can be disabled by setting `TALI_OPTION_HISTORY` to 0, saving about ~0.2K of Tali ROM and 1K RAM.
```
TALI_OPTION_HISTORY := 1
; TALI_OPTION_HISTORY := 0      ; disable history
```
Finally, setting `TALI_OPTION_TERSE` to 1 strips or shortens various strings to reduce the memory
footprint, saving about ~0.5K.
```
TALI_OPTION_TERSE := 0
; TALI_OPTION_TERSE := 1        ; shorten/remove strings
```

### Kernel I/O routines

Tali needs platform specific I/O routines to get input from the user
and display output back to them.  It expects the platform configuration
to provide the following kernel routines:

- `kernel_init` Initialize the low-level hardware.  This is normally also
  the reset vector.  It should set up your hardware, print the startup
  message and `jmp forth`.
- `kernel_getc` Get a single character from the keyboard and return
  it in the accumulator.  It should block until a character is ready,
  and should preserve the X and Y registers.
- `kernel_putc` Send the character in A to the output device (e.g. screen).
  It should preserve the X and Y registers, but need not preserve A.
- `kernel_kbhit` Return a non-zero value in the accumulator if an input character
  is ready (i.e. kernel_getc won't block).  It should preserve the X and Y registers.
- `kernel_bye` Exit forth, e.g. to a monitor program or just `brk` to reset.
- `s_kernel_id` Labels a zero-terminated string to be printed at startup.

You can use the examples in `simulator.asm` shared by `platform-py65mon.asm`
and `platform-c65.asm` for inspiration as you configure for your own hardware.

Note that `kernel_kbhit` only returns a flag that a character is ready,
but doesn't actually return the character.  It's only required if you use the KEY? word.
If your hardware requires you to read the character while checking whether one is
ready, you should buffer it and make sure that `kernel_getc` checks the buffer.
(See `platform-py65mon.asm` as one example.)
If your hardware doesn't support kbhit, or you don't care about KEY?,
taliforth.asm will provide a default implementation which always returns true,
so that any subequent call to KEY would block until a key is actually ready.

### Reset and interrupt vectors

Your platform configuration should define the 6502 NMI, Reset and IRQ vectors
at $fffa-$ffff.  Typically at least Reset ($fffc) should point to `kernel_init`.

## Contributing

To submit your configuration file, pick a name with the form `platform-*.asm`
that is not taken yet and initiate a pull request with it. A few comment lines
at the beginning with some background information would be nice. You'll probably
want to include your own boot string (see the bottom of the file) because that's
pretty cool.

Submitting your code implies that you are okay with other people using or
adapting it for their own systems. If your routines contain code for control of
your supervillain hide-out, the Evil League of Evil suggests you keep it off of
GitHub.

Note that this is being provided as a service only. As always, we take no
resposibility for anything, and you'll have to keep an eye on the code
yourself.
