; --- Hardware Transfer Stub Allocation ---
hwt_stub = zpage+$80  ; 8 bytes: [Opcode] [SrcL/H] [DstL/H] [LenL/H] [RTS]

; ( src dst len -- )
#nt_header tii_exec, "(tii)", AN
xt_tii_exec:
    lda #$73          ; TII Opcode
    bra xt_block_run
z_tii_exec:           ; TaliForth needs this label right after the bra

; ( src dst len -- )
#nt_header tdd_exec, "(tdd)", AN
xt_tdd_exec:
    lda #$C3          ; TDD Opcode
    bra xt_block_run
z_tdd_exec:

; ( src vdc_reg len -- )
#nt_header tin_exec, "(tin)", AN
xt_tin_exec:
    lda #$D3          ; TIN Opcode
    bra xt_block_run
z_tin_exec:

; ( src vdc_reg len -- )
#nt_header tia_exec, "(tia)", AN
xt_tia_exec:
    lda #$E3          ; TIA Opcode
    bra xt_block_run
z_tia_exec:

; --- Shared Engine Logic ---
xt_block_run:
    sta hwt_stub      
    
    ; Move stack parameters to ZP stub
    lda zpage+4,x
    sta hwt_stub+1    ; Source Low
    lda zpage+5,x
    sta hwt_stub+2    ; Source High
    
    lda zpage+2,x
    sta hwt_stub+3    ; Destination Low
    lda zpage+3,x
    sta hwt_stub+4    ; Destination High
    
    lda zpage+0,x
    sta hwt_stub+5    ; Length Low
    lda zpage+1,x
    sta hwt_stub+6    ; Length High

    ; Store RTS and Execute
    lda #$60
    sta hwt_stub+7
    jsr hwt_stub

    ; Cleanup 3 stack cells (6 bytes)
    txa
    clc
    adc #6
    tax
    rts

; ( addr len char -- )
; High-speed override of core FILL using TII ripple
#nt_header pce_fill, "fill", AN
xt_pce_fill:
    ; 1. Seed the first byte
    lda zpage+4,x       ; addr low
    sta hwt_stub+1      
    lda zpage+5,x       ; addr high
    sta hwt_stub+2
    
    lda zpage+0,x       ; get char (TOS)
    ldy #0
    sta (hwt_stub+1), y ; Standard 6502 indirect syntax
    
    ; 2. Prepare stack for (tii) engine
    ; SRC (zpage+4,x) stays 'addr'

    ; DST (zpage+2,x) becomes addr + 1
    lda zpage+4,x
    clc
    adc #1
    sta zpage+2,x
    lda zpage+5,x
    adc #0
    sta zpage+3,x

    ; LEN (zpage+0,x) becomes len - 1
    ; Note: original 'len' was at zpage+2,x
    lda zpage+2,x
    sec
    sbc #1
    sta zpage+0,x
    lda zpage+3,x
    sbc #0
    sta zpage+1,x

    ; 3. Execute the hardware engine
    jsr xt_tii_exec
z_pce_fill:
    rts

