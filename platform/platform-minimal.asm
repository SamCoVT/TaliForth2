; This illustrates a minimal configuration of TaliForth requiring a little under 12K of ROM.
; The 16K image leaves $f000-$ffff empty other than c65 I/O and the interrupt vectors.
; Build and run in the c65 simulator like:

;       make taliforth-minimal.bin
;       c65 -r taliforth-py65mon.bin

; See README.md if you want to customize this to run on your own hardware or simulator,

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

ram_end = $c000-1

; Where to start Tali Forth 2 in ROM (or RAM if loading it)
        * = $c000

; see README.md for configuration options

TALI_OPTIONAL_WORDS := [ ]              ; Drop all optional features for minimal build
TALI_OPTION_CR_EOL := [ "lf" ]
TALI_OPTION_HISTORY := 0
TALI_OPTION_TERSE := 1

; =====================================================================

.include "simulator.asm"        ; shared py65/c65 I/O routines

; custom getc/kbhit for c65

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
