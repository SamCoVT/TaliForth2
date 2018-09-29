.org $8000
.require "taliforth.asm"      ; Top-level definitions, memory map

; =====================================================================
; FINALLY

; Of the 32 KiB we use, 24 KiB are reserved for Tali (from $8000 to $DFFF)
; and the last eight (from $E000 to $FFFF) are left for whatever the user
; wants to use them for.

.advance $e000

; Default kernel file for Tali Forth 2 
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 19. Jan 2014
; This version: 18. Feb 2018
;
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
; This default version Tali ships with is written for the py65mon machine 
; monitor (see docs/MANUAL.md for details). 

; The main file of Tali got us to $e000. However, py65mon by default puts
; the basic I/O routines at the beginning of $f000. We don't want to change
; that because it would make using it out of the box harder, so we just 
; advance past the virtual hardware addresses.
.advance $f006

; All vectors currently end up in the same place - we restart the system
; hard. If you want to use them on actual hardware, you'll have to redirect
; them all.
v_nmi:
v_reset:
;v_irq: ; IRQ redirected to SERVICE_ACIA
kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have anything set up for that yet. With
        ; py65mon, of course, this is really easy. -- At the end, we JMP
        ; back to the label forth to start the Forth system.
        ; """
.scope

                jsr Init_ACIA
                ; We've successfully set everything up, so print the kernel
                ; string
                ldx #0
*               lda s_kernel_id,x
                beq _done
                jsr kernel_putc
                inx
                bra -
_done:
                jmp forth
.scend



    ;; Defines for hardware:
.alias ACIA_DATA    $7F80
.alias ACIA_STATUS  $7F81
.alias ACIA_COMMAND $7F82
.alias ACIA_CTRL    $7F83
    ;; Defines for the 256 byte circular buffer with two 8-bit pointers.
.alias ACIA_BUFFER acia_buff+2
.alias ACIA_RD_PTR acia_buff+0
.alias ACIA_WR_PTR acia_buff+1

	;; Init ACIA to 19200 8,N,1
    ;; Uses: A (not restored)
Init_ACIA:	
	    lda #$1F
	    sta ACIA_CTRL
	    lda #$09    ; RX interrupt on.  RTS low (asserted).
	    sta ACIA_COMMAND
        ;; Initialize the buffer
        stz ACIA_RD_PTR
        stz ACIA_WR_PTR
        ; Turn on interrupts.
        cli
	    rts

;; Helper routines for the ACIA buffer
;; from http://wilsonminesco.com/6502interrupts/index.html
WR_ACIA_BUF:
        ; Put value in increment pointer.
        ldx ACIA_WR_PTR
        sta ACIA_BUFFER,X
        inc ACIA_WR_PTR
        rts

RD_ACIA_BUF:
        ; Read value and increment pointer.
        ldx ACIA_RD_PTR
        lda ACIA_BUFFER,X
        inc ACIA_RD_PTR
        rts

ACIA_BUF_DIF:
        ; Subtract the buffer pointers (wrap around is fine)
        lda ACIA_WR_PTR
        sec
        sbc ACIA_RD_PTR
        rts

v_irq:                          ; IRQ handler (only handling ACIA RX)
SERVICE_ACIA:
        pha
        phx

        lda ACIA_STATUS
        ;and #$07    ; Check for errors.
        ;bne SERVICE_ACIA_END    ; Ignore errors.
        and #$08 ; Check for RX byte available
        beq SERVICE_ACIA_END ; No byte available.

        ; There is a byte to get.
        lda ACIA_DATA
        jsr WR_ACIA_BUF

        ; Check how many bytes in the buffer are used.
        jsr ACIA_BUF_DIF
        cmp #$F0
        bcc SERVICE_ACIA_END

        ; There are only 15 chars left - de-assert RTS
        lda #$01
        sta ACIA_COMMAND

SERVICE_ACIA_END:
        plx
        pla
        rti


        ;; Get_Char - get a character from the serial port into A.
        ;; Set the carry flag if char is valid.
        ;; Return immediately with carry flag clear if no char available.
        ;; Uses: A (return value)
Get_Char:
        ;;  Check to see if there is a character.
        jsr ACIA_BUF_DIF
        beq no_char_available
char_available:
        ;; See if RTS should be asserted (low)
        ;; buffer bytes in use in A from above.
        cmp #$E0
        bcs buf_full
        lda #$09
        sta ACIA_COMMAND
buf_full:
        phx                         ; Reading from buffer messes with X.
	    jsr RD_ACIA_BUF             ; Get the character.
        plx
	    ;; jsr Send_Char				; Echo
        sec                         ; Indicate it's valid.
        rts
no_char_available:
        clc                         ; Indicate no char available.
        rts

    
kernel_getc:
        ; """Get a single character from the keyboard (waits for key). 
        ; """
	    ;; Get_Char_Wait - same as Get_Char only blocking.
        ;; Uses: A (return value)
Get_Char_Wait:	
	    jsr Get_Char
	    bcc Get_Char_Wait
	    rts

 

kernel_putc:
        ; """Print a single character to the console. """
	    ;; Send_Char - send character in A out serial port.
        ;; Uses: A (original value restored)
Send_Char:
        sei
	    pha                         ;Save A (required for ehbasic)
wait_tx:			            ; Wait for the TX buffer to be free.    
	    lda ACIA_STATUS
        
        ; A byte may come in while we are trying to transmit.
        ; Because we have disabled interrupts, and we've just read from
        ; the status register (which clears an interrupt),
        ; we might have to deal with it ourselves.
        pha             ; Save the status for checking the TRDE bit later.
        and #$08        ; Check for byte recieved
        beq check_tx    ; No bye received, continue to check TRDE bit.

        ; A byte was recieved while we are trying to transmit.
        ; Process it and then go back to checking for TX ready.
        phx
        lda ACIA_DATA
        jsr WR_ACIA_BUF
        ; Check how many bytes in the buffer are used.
        jsr ACIA_BUF_DIF
        cmp #$F0
        bcc tx_keep_rts_active

        ; There are only 15 chars left - de-assert RTS
        lda #$01
        sta ACIA_COMMAND
tx_keep_rts_active:    
        plx             ; Restore the ACIA_STATUS value to A.

check_tx:
        ; Check to see if we can transmit yet.
        pla
	    and #$10
	    beq wait_tx		            ; TRDE is not set - byte still being sent.
        ; Send the byte.
	    pla	
	    sta ACIA_DATA
        cli
	    rts			               
        
        
; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id: 
        .byte "Tali Forth 2 default kernel for SamCo's SBC (2018-09-26)", AscLF, 0


; Add the interrupt vectors 
.advance $fffa

.word v_nmi
.word v_reset
.word v_irq

; END
	
