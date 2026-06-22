; ( -- )
; Wait for the START of the next V-blank period.
#nt_header v_sync, "vsync", AN
xt_v_sync:
@wait_end:
    lda $0000       ; Read VDC Status
    and #$20        ; Check V-blank bit
    bne @wait_end   ; If bit is 1, we are IN vblank, wait for it to finish
@wait_start:
    lda $0000
    and #$20
    beq @wait_start ; If bit is 0, we are drawing, wait for vblank to start
    rts

; ( src len -- )
#nt_header vram_store, "vram!", AN
xt_vram_store:
    lda zpage+2,x 
    sta .vsrc
    lda zpage+3,x 
    sta .vsrc+1
    lda zpage+0,x 
    sta .vlen
    lda zpage+1,x 
    sta .vlen+1
    tin .vsrc, $0002, .vlen
    txa 
    clc 
    adc #4 
    tax
    rts
.vsrc .word 0
.vlen .word 0

; ( src len -- )
#nt_header pal_store, "pal!", AN
xt_pal_store:
    lda zpage+2,x 
    sta .psrc
    lda zpage+3,x 
    sta .psrc+1
    lda zpage+0,x 
    sta .plen
    lda zpage+1,x 
    sta .plen+1
    tin .psrc, $0404, .plen
    txa 
    clc 
    adc #4 
    tax
    rts
.psrc .word 0
.plen .word 0

