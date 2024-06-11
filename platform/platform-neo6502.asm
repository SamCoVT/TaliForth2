; Programmer  : Sam Colwell
; File        : platform-neo6502.asm
; Date        : 2024-02
; Description : A platform file for the Neo6502 board.
; use "make taliforth-neo6502.bin" from main Tali directory to generate
; binary.  For the simulator (which starts in Neo6502 BASIC) place the
; binary file in the "storage" folder and use the following commands to
; load and start Tali Forth 2.
;
; load "taliforth-neo6502.bin", $a000
; sys $a000
;
; simulator commandline:
; neo taliforth-neo6502.bin@a000 run@a000

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

ram_end = $a000-1
        ; Where to start Tali Forth 2 in ROM (or RAM if loading it)
        * = $a000

; I/O facilities are handled in these separate kernel files because of their


; OPTIONAL WORDSETS
TALI_OPTIONAL_WORDS := [ "ed", "editor", "ramdrive", "block", "environment?", "assembler", "wordlist" ]
; Neo6502 uses CR
TALI_OPTION_CR_EOL := [ "cr" ]

; Put the kernel init first (at $a000)

kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have anything set up for that yet. With
        ; py65mon, of course, this is really easy. -- At the end, we JMP
        ; back to the label forth to start the Forth system.
        ; """

                ; Nothing special to set up here.

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


; Map kernel_getc and kernel_putc to Neo6502 API routines.
ReadCharacter = $ffee
WriteCharacter = $fff1
kernel_getc = ReadCharacter
kernel_putc = WriteCharacter

kernel_bye:
                brk


; Put the guts of Tali Forth 2 here.
.include "../taliforth.asm" ; zero page variables, definitions

; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text "Tali Forth 2 kernel for Neo6502 (2024-02-06)", AscLF, 0


; END

