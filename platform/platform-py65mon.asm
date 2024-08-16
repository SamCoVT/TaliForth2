; Default configuration for [py65mon](https://github.com/mnaberez/py65)

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

TALI_ARCH := "py65mon"

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

; TALI_OPTION_COLOR_INPUT uses ANSI escape codes to color input (and output)
; Requires a terminal that supports ANSI.  Colors can be changed by modifying
; s_color_pre_accept and s_color_post_accept

TALI_OPTION_COLOR_INPUT := 0
;TALI_OPTION_COLOR_INPUT := 1

; These strings can be used to change the colors and font effects.  You can set
; multiple options with semicolons between them.  See your favorite reference
; for available ANSI codes.

; The input_color_code is called at the beginning of ACCEPT and will color
; most input.

input_color_code := "97;1" ; WHITE and BOLD

; The outout_color_code is called at the end of ACCEPT and will color most
; output.

output_color_code := "0"   ; DEFAULT (light gray in many terminals)


.include "simulator.asm"

; py65mon doesn't have kbhit so we roll our own, using a spare byte in the IO area
io_bufc = io_putc+1

kernel_getc:
        ; """Get a single character from the keyboard.
        ; py65mon's io_getc is non-blocking, returning 0 when no key is pressed.
        ; We'll convert to blocking by waiting for a non-zero result.
        ; We also check the single character io_bufc buffer used by kbhit
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
        .text "Tali Forth 2 default kernel for py65mon (04. Dec 2022)", AscLF, 0


; Define the interrupt vectors.  For the simulator we redirect them all
; to the kernel_init routine and restart the system hard.  If you want to
; use them on actual hardware, you'll likely have to redefine them.

* = $fffa

v_nmi   .word kernel_init
v_reset .word kernel_init
v_irq   .word kernel_init

; END
