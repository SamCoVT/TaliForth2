; Definitions for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Modified by Patrick Surry
; First version: 01. Apr 2016 (Liara Forth)
; This version: 1. Jun 2024

; This file is included by taliforth.asm. These are the general
; definitions; platform-specific definitions such as the
; memory map are kept in the platform folder.

; HARD PHYSICAL ADDRESSES

; Some of these are somewhat silly for the 65c02, where for example
; the location of the Zero Page is fixed by hardware. However, we keep
; these for easier comparisons with Liara Forth's structure and to
; help people new to these things.

ram_start = $0000          ; start of installed RAM, must include zpage
zpage     = ram_start      ; begin of Zero Page ($0000-$00ff)
stack0    = $0100          ; begin of Return Stack ($0100-$01ff)

; weak constants can be overridden by the including platform configuration
.weak
zpage_end = $7F            ; end of Zero Page used ($0000-$007f)
ram_end   = $8000-1        ; end of installed RAM
hist_buff = ram_end-$03ff  ; begin of history buffers
.endweak

; SOFT PHYSICAL ADDRESSES

; Tali currently doesn't have separate user variables for multitasking. To
; prepare for this, though, we've already named the location of the user's
; Zero-Page System Variables user0. Note cp0 starts one byte further down so
; that it currently has the address $300 and not $2FF. This avoids crossing
; the page boundry when accessing the RAM System Variables table, which would
; cost an extra cycle.

rsp0      = $ff              ; initial Return Stack Pointer (65c02 stack)
bsize     = $ff              ; size of input/output buffers

.weak
user0     = zpage            ; TaliForth2 system variables
buffer0   = stack0+$100      ; input buffer ($0200-$02ff)
cp0       = buffer0+bsize+1  ; Dictionary starts after last buffer
                             ; The RAM System Variables and BLOCK buffer are
                             ; placed right at the beginning of the dictionary.
.if TALI_OPTION_HISTORY
cp_end    = hist_buff        ; Last RAM byte available for code
.else
cp_end    = ram_end
.endif
padoffset = $ff              ; offset from CP to PAD (holds number strings)

dsp0      = zpage_end-7    ; initial Data Stack Pointer
.endweak


; ZERO PAGE ADDRESSES/VARIABLES

; TaliForth reserves the first part of zero page ($0 - zpage_end) which is
; configured by zpage_end and normally 128 bytes.
; The rest of zero page is free for kernel/external use (zpage_end+1 to $FF)

; TaliForth zeropage usage is as follows:
;   zero page variables: 30 words = 60 bytes ($0000-$0038) - see cold_user_table
;   Forth Data Stack: 128 - 60 - 8 = 60 bytes or 30 words
;   Data Stack floodplain: 8 bytes after stack (to avoid catastrophic underflow)

; This table defines all of the zero page variables along with their initial
; values except the uninitialized temporaries at the end. The most important
; variables are defined first since the Data Stack grows towards this area
; from dsp0: If there is an overflow, the later, less important variables
; will be clobbered first, giving the system a chance to recover.
; In other words, they are part of the overflow floodplain.
; This table is relocated to zero page by COLD.

cold_zp_table:
        .logical user0              ; make labels refer to relocated address

cp:         .word cp0+256+1024      ; Compiler Pointer
                                    ; moved to make room for user vars and block buffer
dp:         .word dictionary_start  ; Dictionary Pointer
ip:         .word 0                 ; Instruction Pointer (current xt)
workword:   .word 0                 ; nt (not xt!) of word being compiled, except in
                                    ; a :NONAME declared word (see status)
up:         .word cp0               ; Forth user vars at start of available RAM

; The four variables insrc, cib, ciblen, and toin must stay together in this
; sequence for the words INPUT>R and R>INPUT to work correctly.

insrc:      .word 0                 ; input source for SOURCE-ID (0 for keyboard)
cib:        .word buffer0           ; address of current input buffer
ciblen:     .word 0                 ; length of current input buffer
toin:       .word 0                 ; pointer to CIB (>IN in Forth)

output:     .word kernel_putc       ; vector for EMIT
input:      .word kernel_getc       ; vector for KEY
havekey:    .word kernel_kbhit      ; vector for KEY?

base:       .word 10                ; number radix, default decimal
state:      .word 0                 ; STATE: -1 compile, 0 interpret

status:     .word 0                 ; internal status used by : :NONAME ; ACCEPT
        ; Bit 7 = Redefined word message postpone
        ;         When set before calling CREATE, it will
        ;         not print the "redefined xxxx" message if
        ;         the word exists. Instead, this bit will
        ;         be reused and after CREATE has run, it will
        ;         be set if the word was redefined and 0 if
        ;         not. This bit should be 0 when not in use.
        ; Bit 6 = 1 for normal ":" definitions
        ;         WORKWORD contains nt of word being compiled
        ;       = 0 for :NONAME definitions
        ;         WORKWORD contains xt of word being compiled
        ; Bit 5 = 1 for NUMBER returning a double word
        ;       = 0 for NUMBER returning a single word
        ; Bit 3 = 1 makes CTRL-n recall current history
        ;       = 0 CTRL-n recalls previous history
        ; Bit 2 = Current history buffer msb
        ; Bit 1 = Current history buffer (0-7, wraps)
        ; Bit 0 = Current history buffer lsb
        ;
        ; status+1 is used by ACCEPT to hold history lengths.

; The remaining ZP variables are uninitialized temporaries.

    .virtual
tmpdsp:     .byte ?         ; temporary DSP (X) storage (single byte)

; Loop control data

loopctrl:   .byte ?         ; Offset from lcbstack0 to current loop control block for DO/LOOP/+LOOP
loopidx0    .byte ?         ; cached LSB of current loop index for LOOP (not +LOOP)

lcbstack0 = stack0
loopindex = lcbstack0+0     ; loop control block index for adjusted loopindex
loopfufa  = lcbstack0+2     ; loop control block offset for limit fudge factor

    ; Each loop needs two control words (loopindex and loopfufa)
    ; which are stored in a 4-byte (dword) loop control block (LCB).
    ; Remembering state across nested loops means we need a stack of LCBs.
    ; A traditional Forth stores these blocks directly on the return stack
    ; but it's simpler for us to use a separate stack.
    ; This requires the same storage but is easier for us to manage.
    ; We use the space underneath the return stack with our stack
    ; growing upward towards it, but the specific location is arbitrary.
    ; The loopctrl byte is our loop stack pointer with current LCB offset.
    ; The loopidx0 byte caches the LSB of the current loopindex
    ; which helps us avoid some indexed operations.
    ;
    ; Here's how the various loop words interact to manage the loop:
    ;
    ; DO adds 4 to loopctrl to assign the next LCB.
    ; It writes the initial values of loopindex and loopfufa to the LCB and
    ; copies the LSB of loopindex to loopidx0
    ;
    ; LOOP usually just increments the LSB in loopidx0, avoiding the LCB
    ; altogether unless the LSB overflows.
    ;
    ; +LOOP updates loopidx0 and (if needed) the loopindex MSB. it can
    ; also avoid some checks if we have no carry and the step is < 256
    ;
    ; UNLOOP subtracts 4 from loopctrl to drop the current LCB.
    ; it restores LSB of the now current loopindex to loopidx0
    ; so that any enclosing loop has the correct value
    ;
    ; the I and J words use 16bit math to calculate the index from the LCB
    ; I can use loopidx0 for the LSB of loopindex

loopleave:  .word ?         ; tmp for LEAVE chaining ;TODO could it use existing tmp?
tmptos:     .word ?         ; temporary TOS storage
tmp1:       .word ?         ; temporary storage
tmp2:       .word ?         ; temporary storage
tmp3:       .word ?         ; temporary storage (especially for print)
tohold:     .word ?         ; pointer for formatted output
scratch:    .word ?,?,?,?   ; 8 byte scratchpad (see UM/MOD)
.if "ed" in TALI_OPTIONAL_WORDS
tmped:      .word ?,?,?     ; temporary for editors
.endif
    .endvirtual

    .endlogical
cold_zp_table_end:


; RAM System Variables

; This table defines the initial values for forth user variables above zero page,
; which are relocated to 'up' (defaulting to cp0) by COLD.

cold_user_table:
    .logical 0          ; make labels here offsets that we can add to 'up'

nc_limit_offset:        .word 20        ; byte limit for Native Compile size
uf_strip_offset:        .word 0         ; flag to strip underflow detection (0 off)

; Block variables
blk_offset:             .word 0         ; BLK
scr_offset:             .word 0         ; SCR

; Wordlists
max_wordlists = 12    ; Maximum number of wordlists supported (4 built-in, 8 user wordlists)

marker_start_offset:                    ; MARKER saves wordlist state from here to market_end_offset
current_offset:         .byte 0         ; CURRENT = FORTH-WORDLIST (compilation wordlist)
num_wordlists_offset:   .byte 4         ; #WORDLISTS (FORTH EDITOR ASSEMBLER ROOT)

wordlists_offset:
    .word dictionary_start              ; FORTH-WORDLIST
    .word editor_dictionary_start       ; EDITOR-WORDLIST
    .word assembler_dictionary_start    ; ASSEMBLER-WORDLIST
    .word root_dictionary_start         ; ROOT-WORDLIST
    .word 0,0,0,0,0,0,0,0               ; Space for 8 User wordlists

num_order_offset:       .byte 1         ; #ORDER (Number of wordlists in search order)
search_order_offset:
    .byte 0,0,0,0,0,0,0,0,0             ; SEARCH-ORDER (9 bytes to keep offsets even)
marker_end_offset:

.if "block" in TALI_OPTIONAL_WORDS
; Block buffer variables

blkbuffer_offset:       .word cp0+256   ; Address of buffer (right after USER vars)
buffblocknum_offset:    .word 0         ; Block number current in buffer
buffstatus_offset:      .word 0         ; Buffer status (bit 0 = used, bit 1 = dirty) (not in use)

; Block I/O vectors

blockread_offset:       .word xt_block_word_error   ; Vector to block reading routine
blockwrite_offset:      .word xt_block_word_error   ; Vector to block writing routine
.endif
    .endlogical
cold_user_table_end:


; ASCII CHARACTERS
AscCC   = $03  ; break (CTRL-c)
AscBELL = $07  ; bell sound
AscBS   = $08  ; backspace
AscLF   = $0A  ; line feed
AscCR   = $0D  ; carriage return
AscESC  = $1B  ; escape
AscSP   = $20  ; space
AscDEL  = $7F  ; delete (CTRL-h)
AscCP   = $10  ; CTRL-p (used to recall previous input history)
AscCN   = $0E  ; CTRL-n (used to recall next input history)

; OPCODES
; some common instructions we use when emitting code

OpJSR   = $20
OpJMP   = $4C
OpBNE   = $D0
OpBEQ   = $F0
OpRTS   = $60
OpBRA   = $80

; DICTIONARY FLAGS
; The first two bits are currently unused
CO = 1  ; Compile Only
AN = 2  ; Always Native Compile
IM = 4  ; Immediate Word
NN = 8  ; Never Native Compile
UF = 16 ; Includes Underflow Check (RESERVED)
HC = 32 ; Word has Code Field Area (CFA)
ST = 64 ; Stack juggling to be stripped for native compile


; VARIOUS
MAX_LINE_LENGTH  = 79      ; assumes 80 character lines

; END
