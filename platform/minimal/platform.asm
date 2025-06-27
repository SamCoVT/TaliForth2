; This illustrates a minimal configuration of TaliForth requiring a little under 12K of ROM.
; The 16K image leaves $f000-$ffff empty other than c65 I/O and the interrupt vectors.
; Build and run in the c65 simulator like:

;       make taliforth-minimal.bin
;       c65 -r taliforth-py65mon.bin

; See README.md if you want to customize this to run on your own hardware or simulator,

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (e.g. ASCII)
        .enc "none"

TALI_ARCH := "c65"

ram_end = $c000-1

; Where to start Tali Forth 2 in ROM (or RAM if loading it)
        * = $c000

; see README.md for configuration options

TALI_OPTIONAL_WORDS := [ ]              ; Drop all optional features for minimal build
TALI_OPTION_CR_EOL := [ "lf" ]
TALI_OPTION_HISTORY := 0
TALI_OPTION_TERSE := 1

; =====================================================================

; =====================================================================
; Include Tali Forth 2 code
; Make sure the above options are set BEFORE this include.

.include "../../taliforth.asm" ; zero page variables, definitions

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

; Define the c65 magic IO addresses relative to $f000
                .byte ?
io_putc:        .byte ?         ; $f001     write byte to stdout
                .byte ?
io_kbhit:       .byte ?         ; $f003     read non-zero on key ready (c65 only)
io_getc:        .byte ?         ; $f004     non-blocking read input character (0 if no key)
                .byte ?
io_clk_start:   .byte ?         ; $f006     *read* to start cycle counter
io_clk_stop:    .byte ?         ; $f007     *read* to stop the cycle counter
io_clk_cycles:  .word ?,?       ; $f008-b   32-bit cycle count in NUXI order
                .word ?,?

.cerror * != io_start + $10, "Mismatched magic IO interface"


; These magic block IO addresses are only implemented by c65 (not py65mon)
; see c65/README.md for more detail

io_blk_action:  .byte ?         ; $f010     Write to act (status=0 read=1 write=2)
io_blk_status:  .byte ?         ; $f011     Read action result (OK=0)
io_blk_number:  .word ?         ; $f012     Little endian block number 0-ffff
io_blk_buffer:  .word ?         ; $f014     Little endian memory address

.cerror * != io_start + $16, "Mismatched magic block IO interface"


; Now we're safe to inject the required kernel routines to interface to
; our "hardware" which is a simulator in this configuration.

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

kernel_bye:
        ; """Forth shutdown called from BYE"""
                brk

kernel_putc:
        ; """Print a single character to the console.
        ;
        ; Note this routine must preserve X, Y but that's easy here.
        ; If your code is more complex, wrap it with PHX, PHY ... PLY, PHX
        ; """
                sta io_putc
                rts

; c65 and py65mon have different implementations of kernel_getc and kernel_kbhit


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
                lda io_getc
                beq _loop
                rts

kernel_kbhit:
        ; """Check if a character is available to be read.
        ; This should return non-zero when a key is available and 0 otherwise.
        ; It doesn't consume or return the character itself.
        ; This routine is only required if you use the KEY? word.
        ; """
                lda io_kbhit
                rts


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot

s_kernel_id:
        .text "TF2", AscLF, 0

; Define the interrupt vectors.  For the simulator we redirect them all
; to the kernel_init routine and restart the system hard.  If you want to
; use them on actual hardware, you'll likely have to redefine them.

* = $fffa

v_nmi   .word kernel_init
v_reset .word kernel_init
v_irq   .word kernel_init

; END
