; This is the platform file for 65C02 based Apple 1 machines
; This version has a memory layout for RAM based TaliForth2
; Jump to $2900 from the WOZMON with "2900R" after loading the
; Forth into RAM.
; The original Apple 1 has a 6502, so TaliForth2 will not work
; on an origial Apple 1. But some replica machines (such as the
; Replica 1 from Vince Briel) have a 65C02.
; There are also Apple 1 emulators containing emulation for 65C02
; based Apple 1 machines:
;  * Pom 1 enhanced by Ken Wessen:
;    http://school.anhb.uwa.edu.au/personalpages/kwessen/apple1/krusader.htm
;  * lua_6502, an 65C02 Emulator written in Lua 5.3+
;    https://github.com/JorjBauer/lua-6502

        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

ram_end = $28ff
        * = $2900


.include "../taliforth.asm" ; zero page variables, definitions


kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have anything set up for that yet.
        ; In an Apple 1, the machine is already initialized from WOZROM
        ; so we just print the Kernel message and leave.
        ; """
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
        ; """The high bit in the Apple 1 Keyboard Control Register KBDCR
        ; indicates a waiting keypress which will be read from the keyboard
        ; register KBD. Since the Apple 1 only knows upper case characters,
        ; and TaliForth2 needs lower case Forth words, we shift all upper case
        ; ASCII characters between 'A' and 'Z' to lower case 'a' to 'z'.
        ; """
KBD   = $D010		; Apple 1 keyboard register
KBDCR = $D011 		; Apple 1 keyboard control register

_loop:
  lda KBDCR 			; key press waiting?
  bpl _loop
  lda KBD			; read key
  and #$7F			; clear bit 7
  cmp #$41                      ; large 'A'
  bcc _exit                     ; below 'A'
  cmp #$5B                      ; large 'Z'+1
  bcs _exit                     ; above 'Z'
  eor #$20                      ; make lower case
_exit:
  rts

DSP = $D012 		; Display output register

kernel_putc:
                                ; """Print a single character to the console.
                                ; the Apple 1 can only display upper case
	                        ; characters. If the character to be printed
				; is between 'a' and 'z', it will be shifted to
				; upper case.
                                ; """

  bit DSP			; is the Display ready to receive a char?
  bmi kernel_putc		; no, loop
  cmp #$61                      ; little 'a'
  bcc out			; lower than 'a'
  cmp #$7B                      ; little 'z'+1
  bcs out			; higher than 'z'
  and #$DF			; clear bit 6 (make upper case)
out:
  cmp #AscLF			; Line feet?
  bne nolf
  lda #AscCR			; change to carriage return
nolf:
  sta DSP			; write out char
  rts

; platform dependend "bye" behaviour. for now, brk is retained like in platform-py65mon
kernel_bye:
    brk


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text AscCR, AscCR, "Tali Forth 2 default kernel for Apple 1 (15.06.2019)", AscCR, 0

_taliend:	NOP

;TODO these don't have wozmon or vectors?

; END


