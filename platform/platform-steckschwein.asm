        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

; from the steckos jumptable
krn_chrout = $FFB3
krn_getkey = $FFB0
krn_uart_tx  = $FFDD
krn_uart_rx  = $FFE0

; steckOS uses the prg format used on the C64 with the first
; two bytes containing the load address
; This is now handled in the makefile by running:
;   make taliforth-steckschwein.prg
;* = $7FFE
;.word $8000


ram_end   = $8000-1        ; end of installed RAM

* = $8000

kernel_init:
        ; """Initialize the hardware.
        ; There is really not a lot to do as we use the steckOS kernel which already has done
        ; all the dirty work for us.
        ; We put this right before including the "real" taliforth as kernel_init is not called
        ; through any vector. Also, we save a few bytes as we need no jmp to kernel_init and no jmp to forth
        ; """

                ; We've successfully set everything up, so print the kernel
                ; string
                ldx #0
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc
                inx
                bra -
_done:
                ;jmp forth

.include "../taliforth.asm" ; zero page variables, definitions


kernel_bye:
        jmp $e800


kernel_getc:
        ; """Get a single character from the keyboard.
        ; krn_getkey does not block and uses the carry flag to signal
        ; if there was a byte. We need a small wrapper routine.
        ; """

-       jsr krn_getkey
        bcc -
        rts


; we need no wrapper routine but alias krn_chrout as kernel_putc as they are compatible
kernel_putc = krn_chrout


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text "Tali Forth 2 default kernel for steckOS (19. Oct 2018)", AscLF, 0


; Add the interrupt vectors

; END
