; Default configuration for c65 (see c65/README.md)

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

; define what the CR word should emit to kernel_putc at the end of each line

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

.include "simulator.asm"

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
        .text "Tali Forth 2 default kernel for c65 (01. Jun 2024)", AscLF, 0

; Define the interrupt vectors.  For the simulator we redirect them all
; to the kernel_init routine and restart the system hard.  If you want to
; use them on actual hardware, you'll likely have to redefine them.

* = $fffa

v_nmi   .word kernel_init
v_reset .word kernel_init
v_irq   .word kernel_init

; END
