; Platform file for Tali Forth 2 for the kowalski simulator
; Sam Colwell
; 2025-03-24

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"


; Set the address for the end of RAM
; In simulators or all-RAM systems, this will generally be at $7FFF
; This address is for 32K of RAM
; The code for Tali will generally live above this address (eg. in ROM)
ram_end = $7fff


        ; Set the origin for Tali Forth 2 in ROM (or RAM if loading it)
        ; This will be labeled `forth` aka `xt_cold`
        * = $8000

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
; Include Tali Forth 2 code
; Make sure the above options are set BEFORE this include.

.include "../../taliforth.asm" ; zero page variables, definitions


; =====================================================================
; FINALLY

; Of the 32 KiB we use, 24 KiB are reserved for Tali (from $8000 to $DFFF)
; and the last eight (from $E000 to $FFFF) are left for whatever the user
; wants to use them for.

; This section attempts to isolate the hardware-dependent parts of Tali
; Forth 2 to make it easier for people to port it to their own machines.
; Ideally, you shouldn't have to touch any other files. There are three
; routines and one string that must be present for Tali to run:
;
;       kernel_init - Initialize the low-level hardware
;       kernel_getc - Get single character in A from the keyboard (blocks)
;       kernel_putc - Prints the character in A to the screen
;       s_kernel_id - The zero-terminated string printed at boot
;

; The main file of Tali got us to $e000. However, the Kowalski simulator by	default puts
; the basic I/O routines at the beginning of $e000. We don't want to change
; that because it would make using it out of the box harder, so we just
; advance past the virtual hardware addresses.
 * = $f000

; All vectors currently end up in the same place - we restart the system
; hard. If you want to use them on actual hardware, you'll have to redirect
; them all.
v_nmi:
v_reset:
v_irq:
kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have anything set up for that yet. With
        ; py65mon, of course, this is really easy. -- At the end, we JMP
        ; back to the label forth to start the Forth system.
        ; """
                ; Since the default case for Tali is the py65mon emulator, we
                ; have no use for interrupts. If you are going to include
                ; them in your system in any way, you're going to have to
                ; do it from scratch. Sorry.
                ; This also makes sense for the Kowalski simulator.
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


kernel_getc:
        ; """Get a single character from the keyboard. By default, Kowalski
        ; is set to $e004, which we just keep. Note that Kowalski's getc routine
        ; is non-blocking, so it will return '00' even if no key has been
        ; pressed. We turn this into a blocking version by waiting for a
        ; non-zero character.
        ; """
_loop:
        lda $e004
        beq _loop
        rts



kernel_putc:
        ; """Print a single character to the console. By default, Kowalski
        ; is set to $e001, which we just keep.
        ; """
        sta $e001
        ;  The Kowalski simulator needs CR added to any LF from Tali.
        ; Check to see if it's a LF.
        cmp #$0A
        bne kernel_putc_done
        ; It's a LF, so send a CR as well.
        lda #$0D
        sta $e001
kernel_putc_done:   
    
        rts

; The kernel_bye routine will be called when the Forth word BYE is run.
; It should go back to the OS or monitor, if there is one.
; Here, we just run the BRK instruction, which stops the simulation on some
; simulators and will end up restarting Tali on others.
kernel_bye:
    brk
    

; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text "Tali Forth 2 kernel for Kowalski Simulator (2025-03-24)", AscLF, 0


; Add the interrupt vectors
* = $fffa

.word v_nmi
.word v_reset
.word v_irq

; END

