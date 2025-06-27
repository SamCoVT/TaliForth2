; Skeleton configuration
; This version: 26. Jun 2025

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

ram_end = $7fff

        ; Set the origin for Tali Forth 2 in ROM (or RAM if loading it)
        ; This will be labeled `forth` aka `xt_cold`
        * = $8000

; Explicitly list the optional features we want, or omit to get all features by default

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
; "wordlist" is for the optional SEARCH-ORDER words (eg. wordlists)
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
; Kernel routines (adapt these to your hardware)

; These are the required kernel routines to interface to
; our "hardware" which is a simulator in this example configuration.
; Modify these to suit your hardware.

kernel_init:
        ; """Initialize the hardware. At the end, we JMP
        ; to the label `forth` to start the Forth system.
        ; This will also typically be the target of the reset vector.
        ; """
        ; Since the default case for Tali is the py65mon emulator, we
        ; have no use for interrupts. If you are going to include
        ; them in your system in any way, you're going to have to
        ; do it from scratch.
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

kernel_bye:
        ; """Forth shutdown called from BYE
        ; If you have a monitor or OS to go back to, put the code
        ; to do that here."""
                brk


kernel_putc:
        ; """Print a single character to the console.
        ;
        ; Note this routine must preserve X and Y.
        ; If your code is more complex, wrap it with PHX, PHY ... PLY, PHX
        ; """
                sta $F001 ; $F001 is used in many simulators for character output.
                rts

kernel_getc:
        ; """Get a single character from the keyboard.
        ; The c65 io_getc is non-blocking, returning 0 if no key is pressed.
        ; We convert to a blocking version by waiting for a
        ; non-zero result.
        ;
        ; Note this routine must preserve X and Y but that's easy here.
        ; If your code is more complex, wrap it with PHX, PHY ... PLY, PHX
        ; """
_loop:
                lda $F004 ; $F004 is used in many simulators for charater input.
                beq _loop
                rts

kernel_kbhit:
        ; """Check if a character is available to be read.
        ; This should return non-zero when a key is available and 0 otherwise.
        ; It doesn't consume or return the character itself.
        ; This routine is only required if you use the KEY? word.
        ; If you are not going to use the KEY? word, then just return 1.
        ; """
                lda #1 ; This always says a key is available
                rts


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot

s_kernel_id:
        .text "Tali Forth 2 default kernel for skeleton platform (27. Jun 2025)", AscLF, 0


; =====================================================================
; Include any forth words written in assembly.  These will be added to
; the FORTH-WORDLIST.  This must be done BEFORE including taliforth.asm
; below.
.include "platform_words.asm"

.include "../../taliforth.asm" ;

; Now we've got all of Tali's native code.  This requires about 24Kb
; with all options, or as little as 12Kb for a minimal build.
; In the default configuration, we've filled ROM from $8000
; to about $dfff, leaving about 8Kb.

; =====================================================================
; Include any forth words written in forth.  Your forth code goes into
; the platform_forth.fs file.  The Makefile will turn that into
; platform_forth.asc, which is the same code but with all comments
; removed and all whitespace reduced to single space (to reduce size).
; It's this reduced-size version of the code that we bring in here.
; Note that Make normally deletes this .asc file after the build
; process completes.
forth_words_start:
user_words_start:
.binary "platform_forth.asc"
forth_words_end:
user_words_end:


; Define the interrupt vectors.  For the simulator we redirect them all
; to the kernel_init routine and restart the system hard.  If you want to
; use them on actual hardware, you'll likely have to redefine them.

* = $fffa

v_nmi   .word kernel_init
v_reset .word kernel_init
v_irq   .word kernel_init

; END
