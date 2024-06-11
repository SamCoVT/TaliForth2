        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

; This file is based upon the steckschwein version which in turn seems to
; be based on the py65mon platform. Thanks for the good groundwork.

; Sorbus does not utilize a load address.
; Just build with:
;   make taliforth-sorbus.bin
; and rename the file to tali4th2.sx4

* = $0400   ; start of SX4 file

; from the Sorbus memory map
BANK       = $DF00      ; select bank to use
krn_chrout = $FF03      ; output a character to UART
krn_getkey = $FF00      ; read character from UART, C=1, Z=1, A=0: no data

kernel_init:
        ; """Initialize the hardware.
        ; There is really not a lot to do as we use the Sorbus kernel which
        ; already has done all the dirty work for us. Also there is not much
        ; to setup.
        ; We put this right before including the "real" taliforth as
        ; kernel_init is not called through any vector. Also, we save a few
        ; bytes as we need no jmp to kernel_init and no jmp to Forth.
        ; """

                ; We've successfully set everything up, so print the kernel
                ; string
                brk                 ; software interrupt
                .byte $0b           ; to copy BIOS from ROM to RAM
                stz BANK            ; switch to RAM

                ldx #$00
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc     ; print kernel id message
                inx
                bra -
_done:
                ;jmp forth


;    $0000  +-------------------+
;           |  (Reserved for    |
;           | Sorbus kernel,I/O)|
;    $0010  +-------------------+  ram_start, zpage, user0
;           |  user variables   |
;           +-------------------+
;           |                   |
;           |                   |
;           +~~~~~~~~~~~~~~~~~~~+  <-- dsp
;           |                   |
;           |  ^  data stack    |
;           |  |                |
;    $0078  +-------------------+  dsp0, stack
;           |                   |
;           |     (unused)      |
;           |                   |
;    $0100  +-------------------+
;           |                   |
;           |  ^  return stack  |  <-- rsp
;           |  |                |
;    $0200  +-------------------+  rsp0
;           |                   |
;           | (disk buffer for  |
;           |  saving only)     |
;           |                   |
;    $0300  +-------------------+
;           |                   |
;           | (disk buffer for  |
;           |  all access)      |
;           |                   |
;    $0360  +-------------------+
;           |  |                |
;           |  v  Input Buffer  |
;           |                   |
;    $0400  +-------------------+
;           |                   |
;           |  Forth program    |
;           |       code        |
;           |                   |
;   ~$6000  +-------------------+  cp0
;           |  |                |
;           |  v  dictionary    |
;           |       (RAM)       |
;           |                   |
;   (...)   ~~~~~~~~~~~~~~~~~~~~~  <-- cp
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;    $D000  +-------------------+  ram_end, cp_end
;           |                   |
;           |   hardware I/O    |
;           |                   |
;    $E000  +-------------------+  hist_buff
;           |   input history   |
;           |    for ACCEPT     |
;           |  8x128B buffers   |
;    $E400  +-------------------+  buffer, buffer0
;           |                   |
;           |     free RAM      |
;           |   (any ideas?)    |
;           |                   |
;    $FF00  +-------------------+
;           |                   |
;           |   I/O functions   |
;           |       (BIOS)      |
;           |                   |
;    $FFFF  +-------------------+


user0     = $0010          ; user and system variables
ram_end   = $D000-1        ; end of installed RAM
hist_buff = $E000          ; begin of "RAM under kernel"

cp0       = tali_end       ; Dictionary starts after code
cp_end    = ram_end        ; Last RAM byte available for code

.include "../taliforth.asm" ; zero page variables, definitions


; routine called when leaving Tali Forth 2: just call the reset vector.
kernel_bye:
        jmp ($fffc)


kernel_getc:
        ; """Get a single character from the keyboard.
        ; krn_getkey does not block and uses the carry flag to signal
        ; if there was a byte. We need a small wrapper routine.
        ; """

-       jsr krn_getkey
        bcs -
        rts

; we need no wrapper routine but alias krn_chrout as kernel_putc as they are compatible
kernel_putc = krn_chrout


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text AscLF, "Tali Forth 2 default kernel for Sorbus (2024-01-09)"
        .text AscLF, 0

tali_end:

; END
