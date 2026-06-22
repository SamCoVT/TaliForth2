; ( -- mask )
; Returns 8-bit mask: I, II, Select, Run, Up, Right, Down, Left
#nt_header joy_get, "joy@", AN
xt_joy_get:
    dex
    dex
    lda #$01
    sta $1000       ; Select D-Pad
    pha : pla       ; Delay
    lda $1000
    and #$0f
    sta zpage,x
    stz $1000       ; Select Buttons
    pha : pla       ; Delay
    lda $1000
    and #$0f
    asl a : asl a : asl a : asl a
    ora zpage,x
    eor #$ff        ; Flip bits so 1 = pressed
    sta zpage,x
    stz zpage+1,x
    rts

