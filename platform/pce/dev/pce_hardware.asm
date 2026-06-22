; =====================================================================
; TaliForth 2 - PC Engine Hardware Dev Kit
; =====================================================================

; Macro for Tali Native Constants
tali_const .macro label, name, val
#nt_header \label, \name, AN
xt_\label:
    dex
    dex
    lda #<\val
    sta zpage+0,x
    lda #>\val
    sta zpage+1,x
z_\label:
    rts
.endmacro

; --- [ PHYSICAL I/O PORTS ] ------------------------------------------
tali_const vdc_reg,   "vdc-reg",   $0000 ; [8|W]  Select Register
tali_const vdc_stat,  "vdc-stat",  $0000 ; [8|R]  Status Register
tali_const vdc_data,  "vdc-data",  $0002 ; [16|RW] Register Data Window

tali_const vce_ctrl,  "vce-ctrl",  $0400 ; [8|RW] Dot clock / Blur
tali_const vce_addr,  "vce-addr",  $0402 ; [8|RW] Palette Index
tali_const vce_data,  "vce-data",  $0404 ; [16|RW] Palette Data

tali_const psg_port,  "psg-port",  $0800 ; [8|W]  Sound Port
tali_const joy_port,  "joy",       $1000 ; [8|RW] Joypad Port
tali_const timer_cnt, "timer-cnt", $0c00 ; [8|RW] Timer Value
tali_const timer_en,  "timer-en",  $0c01 ; [8|W]  Timer Toggle
tali_const irq_ack,   "irq-ack",   $1403 ; [8|RW] Interrupt Ack

; --- [ VDC REGISTER INDICES ] ---------------------------------------
; Write index to vdc-reg, then access vdc-data
tali_const v_mawr, "v-mawr", $00 ; [16|W]  VRAM Write Pointer
tali_const v_marr, "v-marr", $01 ; [16|W]  VRAM Read Pointer
tali_const v_vwr,  "v-vwr",  $02 ; [16|RW] VRAM Data
tali_const v_cr,   "v-cr",   $05 ; [8|W]   Control
tali_const v_rcr,  "v-rcr",  $06 ; [16|W]  Raster Compare
tali_const v_bxr,  "v-bgx",  $07 ; [16|W]  BG X Scroll
tali_const v_byr,  "v-bgy",  $08 ; [16|W]  BG Y Scroll
tali_const v_mwr,  "v-mwr",  $09 ; [8|W]   BG Map Size

; --- [ DISPLAY GEOMETRY ] -------------------------------------------
tali_const v_hsr,  "v-hsr",  $0a ; [16|W]  H-Sync
tali_const v_hdr,  "v-hdr",  $0b ; [16|W]  H-Display
tali_const v_vsr,  "v-vsr",  $0c ; [16|W]  V-Sync
tali_const v_vdr,  "v-vdr",  $0d ; [16|W]  V-Display
tali_const v_vde,  "v-vde",  $0e ; [16|W]  V-Display End

; 256x240 Reference Values
tali_const r_hsr,  "r-hsr",  $0202
tali_const r_hdr,  "r-hdr",  $031f
tali_const r_vsr,  "r-vsr",  $0f02
tali_const r_vdr,  "r-vdr",  $00ef
tali_const r_vde,  "r-vde",  $0003

; --- [ VRAM AUTO-INCREMENT ] ----------------------------------------
tali_const v_inc1,   "v-inc1",   $0000 ; Step +1
tali_const v_inc32,  "v-inc32",  $0800 ; Step +32
tali_const v_inc64,  "v-inc64",  $1000 ; Step +64
tali_const v_inc128, "v-inc128", $1800 ; Step +128

; --- [ DMA ] --------------------------------------------------------
tali_const v_dcr,  "v-dcr",  $0f ; [8|W]   DMA Control
tali_const v_sour, "v-sour", $10 ; [16|W]  DMA Source
tali_const v_dest, "v-dest", $11 ; [16|W]  DMA Destination
tali_const v_len,  "v-len",  $12 ; [16|W]  DMA Length
tali_const v_sat,  "v-sat",  $13 ; [16|W]  SATB Address

; --- [ PSG OFFSETS ] ------------------------------------------------
; All PSG registers are [8|W]
tali_const s_ch,     "s-ch",     0 ; Channel
tali_const s_main,   "s-main",   1 ; Global Vol
tali_const s_freql,  "s-freql",  2 ; Freq L
tali_const s_freqh,  "s-freqh",  3 ; Freq H
tali_const s_ctrl,   "s-ctrl",   4 ; Enable/DDA
tali_const s_pan,    "s-pan",    5 ; Pan
tali_const s_wave,   "s-wave",   6 ; Wave Data

