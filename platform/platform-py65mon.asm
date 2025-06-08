; Platform file for Tali Forth 2 for the py65mon simulator (https://github.com/mnaberez/py65)
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; Patrick Surry
; First version: 19. Jan 2014
; This version: 27. Feb 2025


        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (e.g. ASCII)
        .enc "none"

TALI_ARCH := "py65mon"

; Set the address for the end of RAM
; In simulators or all-RAM systems, this will generally be at $7FFF
; This address is for 32K of RAM
; The code for Tali will generally live above this address (e.g. in ROM)
ram_end = $7fff

        ; Set the origin for Tali Forth 2 in ROM (or RAM if loading it)
        ; This will be labeled `forth` aka `xt_cold`
        * = $8000


; OPTIONAL WORDSETS

; Tali Forth 2 is a bit of a beast, expecting about 24K of ROM space.
; For some applications, the user might not need certain words and would
; prefer to have the memory back instead.  Remove any of the items in
; TALI_OPTIONAL_WORDS to remove the associated words when Tali is
; assembled.  If TALI_OPTIONAL_WORDS is not defined in your platform file,
; you will get all of the words.

TALI_OPTIONAL_WORDS := [ "ed", "editor", "ramdrive", "block", "environment?", "assembler", "disassembler", "wordlist" ]

; "ed" is a string editor. (~1.5K)
; "editor" is a block editor. (~0.25K)
;     The EDITOR-WORDLIST will also be removed.
; "ramdrive" is for testing block words without a block device. (~0.3K)
; "block" is the optional BLOCK words. (~1.4K)
; "environment?" is the ENVIRONMENT? word.  While this is a core word
;     for ANS-2012, it uses a lot of strings and therefore takes up a lot
;     of memory. (~0.2K)
; "assembler" is an assembler. (~3.2K)
;     The ASSEMBLER-WORDLIST will also be removed if the assembler is removed.
; "disassembler" is the disassembler word DISASM. (~0.6K)
;     If both the assembler and dissasembler are removed, the tables
;     (used for both assembling and disassembling) will be removed
;     for additional memory savings. (extra ~1.6K)
; "wordlist" is for the optional SEARCH-ORDER words (e.g. wordlists)
;     Note: Without "wordlist", you will not be able to use any words from
;     the EDITOR or ASSEMBLER wordlists (they should probably be disabled
;     by also removing "editor" and "assembler"), and all new words will
;     be compiled into the FORTH wordlist. (~0.9K)


; TALI_OPTION_CR_EOL sets the character(s) that are printed by the word
; CR in order to move the cursor to the next line.  The default is "lf"
; for a line feed character (#10).  "cr" will use a carriage return (#13).
; Having both will use a carriage return followed by a line feed.  This
; only affects output.  Either carriage returns or line feeds can be used
; to terminate lines on the input.

TALI_OPTION_CR_EOL := [ "lf" ]
;TALI_OPTION_CR_EOL := [ "cr" ]
;TALI_OPTION_CR_EOL := [ "cr", "lf" ]

; TALI_OPTION_HISTORY enables editable input history buffers via ctrl-n/ctrl-p
; These buffers are disabled when set to 0 (saving about ~0.2K Tali ROM, 1K RAM)

TALI_OPTION_HISTORY := 1
;TALI_OPTION_HISTORY := 0

; TALI_OPTION_TERSE strips or shortens various strings to reduce the memory
; footprint when set to 1 (~0.5K)

TALI_OPTION_TERSE := 0
;TALI_OPTION_TERSE := 1


; =====================================================================
; Include Tali Forth 2 code
; Make sure the above options are set BEFORE this include.

.include "../taliforth.asm" ; zero page variables, definitions

; Now we've got all of Tali's native code.  This requires about 24Kb
; with all options, or as little as 12Kb for a minimal build.
; In the default configuraiton, we've filled ROM from $8000
; to about $dfff, leaving about 8Kb.

; Both py65mon and c65 use $f000-$f010 as their default IO interface
; and we don't want to change that because it would make it harder to
; use out of the box, so we just advance past the virtual hardware addresses.
; (We could also choose to put our kernel routines before the IO addresses.)

io_start = $f000                ; virtual hardware addresses for the simulators

* = io_start

; Define the py65mon magic IO addresses relative to $f000
                .byte ?
io_putc:        .byte ?         ; $f001     write byte to stdout
                .byte ?
                .byte ?         ;
io_getc:        .byte ?         ; $f004     non-blocking read input character (0 if no key)
                .byte ?
io_clk_start:   .byte ?         ; $f006     *read* to start cycle counter
io_clk_stop:    .byte ?         ; $f007     *read* to stop the cycle counter
io_clk_cycles:  .word ?,?       ; $f008-b   32-bit cycle count in NUXI order
                .word ?,?

.cerror * != io_start + $10, "Mismatched magic IO interface"


; Here we add the required kernel routines to interface to
; our "hardware" which is a simulator in this configuration.
; Only kernel_init, kernel_bye, kernel_getc, and kernel_putc, are required.
; kernel_kbhit is optional and only affects the Forth word KEY?

kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have the 6502 stack ready yet. With
        ; py65mon, of course, this is really easy. At the end, we JMP
        ; back to the label `forth` to start the Forth system.
        ; This will also typically be the target of the reset vector.
        ; """
                ; Since the default case for Tali is the py65mon emulator, we
                ; have no use for interrupts. If you are going to include
                ; them in your system in any way, you're going to have to
                ; do it from scratch. Sorry.
                sei             ; Disable interrupts

                ; We've successfully set everything up, so print the kernel
                ; string
                ldx #0
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc
                inx
                bra -
_done:
                jmp forth

; The kernel_bye routine will be called when the Forth word BYE is run.
; It should go back to the OS or monitor, if there is one.
; Here, we just run the BRK instruction, which stops the simulation on some
; simulators and will end up restarting Tali on others.
kernel_bye:
        ; """Forth shutdown called from BYE"""
                brk

kernel_putc:
        ; """Print a single character (in the A register) to the console.
        ;
        ; Note this routine must preserve X, Y but that's easy here.
        ; If your code is more complex, wrap it with PHX, PHY ... PLY, PHX
        ; """
                sta io_putc
                rts

; c65 and py65mon have different implementations of kernel_getc and kernel_kbhit


; py65mon doesn't have kbhit so we roll our own, using a spare byte in the IO area
; as a single byte buffer.
io_bufc = io_putc+1

kernel_getc:
        ; """Get a single character from the keyboard and return in A register.
        ; py65mon's io_getc is non-blocking, returning 0 when no key is pressed.
        ; We'll convert to blocking by waiting for a non-zero result.
        ; We also check the single character io_bufc buffer (used by kbhit) first
        ;
        ; Note this routine must preserve X and Y but that's easy here.
        ; If your code is more complex, wrap it with PHX, PHY ... PLY, PHX
        ; """
                lda io_bufc             ; first check the buffer
                stz io_bufc
                bne _done
_loop:                                  ; otherwise wait for a character
                lda io_getc
                beq _loop
_done:
                rts

kernel_kbhit:
        ; """Check if a character is available.  py65mon doesn't have a native kbhit
        ; so we buffer the result of the non-blocking io_getc instead
        ; This routine is only required if you use the KEY? word.
        ; If you do not implement this word, KEY? always returns TRUE.
        ; """
                lda io_bufc             ; do we already have a character?
                bne _done

                lda io_getc             ; otherwise check and buffer the result
                sta io_bufc
_done:
                rts


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot

s_kernel_id:
        .text "Tali Forth 2 default kernel for py65mon (27. Feb 2025)", AscLF, 0


; Define the interrupt vectors.  For the simulator we redirect them all
; to the kernel_init routine and restart the system hard.  If you want to
; use them on actual hardware, you'll likely have to redefine them.

* = $fffa

v_nmi   .word kernel_init
v_reset .word kernel_init
v_irq   .word kernel_init

; END
