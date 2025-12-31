; PC-Engine (HuC6280) Extra Opcodes for TaliForth
; Includes MMU, Speed Control, and Bitwise operations
; Notice: not all of these have been tested yet

; --- MMU Control ---
tam     .macro
        .byte $53, \1      ; Transfer Accumulator to MPR (1-byte mask)
        .endm

tma     .macro
        .byte $43, \1      ; Transfer MPR to Accumulator (1-byte mask)
        .endm

; --- System Control ---
csh     .macro
        .byte $d4          ; Change Speed High (7.16 MHz)
        .endm

csl     .macro
        .byte $54          ; Change Speed Low (1.79 MHz)
        .endm

setd    .macro
        .byte $f4          ; SET Drive (Enable internal I/O)
        .endm

; --- Block Transfer Instructions ---
; Format: OPCODE source, dest, length
; These are 7-byte instructions!
tii     .macro
        .byte $73
        .word \1, \2, \3   ; Transfer Inc-Inc (Memory to Memory)
        .endm

tdd     .macro
        .byte $c3
        .word \1, \2, \3   ; Transfer Dec-Dec
        .endm

tin     .macro
        .byte $d3
        .word \1, \2, \3   ; Transfer Inc-Fixed (Memory to I/O)
        .endm

tia     .macro
        .byte $e3
        .word \1, \2, \3   ; Transfer Inc-Alternate (Memory to Video RAM)
        .endm

tai     .macro
        .byte $f3
        .word \1, \2, \3   ; Transfer Alternate-Inc (Video RAM to Memory)
        .endm

; --- Bit Testing (Zero Page) ---
; TST #immediate, addr
tst     .macro
        .byte $83, \1, \2  ; Test bits with immediate mask (ZP)
        .endm

tst_x   .macro
        .byte $a3, \1, \2  ; Test bits (ZP, X)
        .endm

tst_abs .macro
        .byte $93, \1      ; Test bits (Absolute)
        .word \2
        .endm

; --- Set/Reset Memory Bit (Zero Page) ---
rmb     .macro
        .byte ($07 | (\1 << 4)), \2
        .endm

smb     .macro
        .byte ($87 | (\1 << 4)), \2
        .endm

; --- Branch on Bit Set/Reset ---
bbr     .macro
        .byte ($0f | (\1 << 4)), \2, (\3 - * - 1)
        .endm

bbs     .macro
        .byte ($8f | (\1 << 4)), \2, (\3 - * - 1)
        .endm

; --- Stop / Wait ---
stz_abs .macro             ; Store Zero (Absolute) - Often missing in standard 65c02
        .byte $9c
        .word \1
        .endm

cla     .macro
        .byte $62          ; Clear Accumulator
        .endm

clx     .macro
        .byte $82          ; Clear X
        .endm

cly     .macro
        .byte $c2          ; Clear Y
        .endm

say     .macro
        .byte $02          ; Swap Accumulator and Y
        .endm

sax     .macro
        .byte $22          ; Swap Accumulator and X
        .endm

sxy     .macro
        .byte $42          ; Swap X and Y
        .endm

