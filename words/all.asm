; Low-level Forth word routines
; Tali Forth 2 for the 65c02
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; Patrick Surry
; First version: 19. Jan 2014
; This version: 21. Apr 2024

; The words are grouped similarly to https://forth-standard.org/standard/words
; Each group of words is ordered alphabetically by the names of their XT symbol,
; not their strings (so "!" is sorted as "STORE"). However, we start off with COLD,
; ABORT, and QUIT as the natural start sequence. Other word groups are included below.
; Each word has two special status lines that begins with "; ## " which
; allows auto-generation of the WORDLIST.md file and other entries in the docs folder.
; Status entries are:

;       TBA --> fragment --> coded --> tested --> auto

; "Auto" means that the word is automatically tested by the test suite (good),
; "tested" means that it was tested by hand in some way (okay), "coded" means
; it hasn't been tested at all (bad). See the test suite for more details.


; ## COLD ( -- ) "Reset the Forth system"
; ## "cold"  tested  Tali Forth
;       """Reset the Forth system. Does not restart the kernel,
;       use the 65c02 reset for that. Flows into ABORT.
;       """
xt_cold:
w_cold:
                cld

                ; Initialize 65c02 stack (Return Stack)
                ldx #rsp0
                txs

                ; Clear Data Stack. This is repeated in ABORT, but this way we
                ; can load high-level words with EVALUATE
                ldx #dsp0

                ; Set the OUTPUT vector to the default kernel_putc
                ; We do this really early so we can print error messages
                ; during debugging
                lda #<kernel_putc
                sta output
                lda #>kernel_putc
                sta output+1

                ; Load all of the important zero page variables from ROM
                ldy #cold_zp_table_end-cold_zp_table-1

_load_zp_loop:
                ; This loop loads them back to front.
                lda cold_zp_table,y
                sta user0,y
                dey
                bpl _load_zp_loop           ; <128 bytes so safe to loop until y<0

                ; Initialize the user variables.
                ldy #cold_user_table_end-cold_user_table-1
_load_user_vars_loop:
                ; Like the zero page variables, these are initialized
                ; back to front.
                lda cold_user_table,y
                sta (up),y
                dey
                bpl _load_user_vars_loop    ; again we have <128 bytes so bpl is safe

                jsr w_cr

                ; Define high-level words in forth_words.asc via EVALUATE,
                ; followed by any user-defined words from user_words.asc.
                ; These are stored sequentially in ROM so we can evaluate them together.
                ; If you have neither, this section can be commented out.
                jsr push_inline_addru_literal
                .byte user_words_end-user_words_start  ; length goes TOS
                .word user_words_start                 ; start address goes NOS
                jsr w_evaluate

.if TALI_OPTION_HISTORY
                ; Initialize all of the history buffers by putting a zero in
                ; each length byte.
                stz hist_buff
                stz hist_buff+$80
                stz hist_buff+$100
                stz hist_buff+$180
                stz hist_buff+$200
                stz hist_buff+$280
                stz hist_buff+$300
                stz hist_buff+$380
.endif
                ; fall through to ABORT


; ## ABORT ( -- ) "Reset the Data Stack and restart the CLI"
; ## "abort"  tested  ANS core
        ; """https://forth-standard.org/standard/core/ABORT
        ; Clear Data Stack and continue into QUIT. We can jump here via
        ; subroutine if we want to because we are going to reset the 65c02's
        ; stack pointer (the Return Stack) anyway during QUIT. Note we don't
        ; actually delete the stuff on the Data Stack.
        ; """
xt_abort:
w_abort:
                ldx #dsp0

                ; fall through to QUIT


; ## QUIT ( -- ) "Reset the input and get new input"
; ## "quit"  tested  ANS core
        ; """https://forth-standard.org/standard/core/QUIT
        ; Rest the input and start command loop
        ; """
xt_quit:
w_quit:
                ; Clear the Return Stack. This is a little screwed up
                ; because the 65c02 can only set the Return Stack via X,
                ; which is our Data Stack pointer. The ANS specification
                ; demands, however, that ABORT reset the Data Stack pointer
                txa             ; Save the DSP that we just defined
                ldx #rsp0
                txs
                tax             ; Restore the DSP. Dude, seriously.

                ; SOURCE-ID is zero (keyboard input)
                stz insrc
                stz insrc+1

                ; BLK is zero
                lda #0
                ldy #blk_offset
                sta (up),y
                iny
                sta (up),y

                ; initialize loopctrl to indicate no active loop
                ; see definitions.asm
                lda #(256-4)
                sta loopctrl

                ; STATE is zero (interpret, not compile)
                stz state
                stz state+1
_get_line:
                lda #<buffer0   ; input buffer, this is paranoid
                sta cib
                lda #>buffer0
                sta cib+1

                ; Size of current input buffer (CIB) is zero
                stz ciblen
                stz ciblen+1

                ; Accept a line from the current import source. This is how
                ; modern Forths do it.
                jsr w_refill           ; ( -- f )

                ; Test flag: LSB of TOS
                lda 0,x
                bne _success

                ; If REFILL returned a FALSE flag, something went wrong and we
                ; need to print an error message and reset the machine. We
                ; don't need to save TOS because we're going to clobber it
                ; anyway when we go back to ABORT.
                lda #err_refill
                jmp error

_success:
                ; Assume we have successfully accepted a string of input from
                ; a source, with address cib and length of input in ciblen. We
                ; arrive here still with the TRUE flag from REFILL as TOS
                inx                     ; drop
                inx

                ; Main compile/execute routine
                jsr interpret

                ; Test for Data Stack underflow. Tali Forth doesn't explicitly check for
                ; overflow because it is so rare but the `bpl` test will trigger a
                ; wraparound "underflow" error if the stack exceeds 64 words.
                cpx #dsp0+1
                bpl underflow_error      ; DSP must always be smaller than DSP0

                ; Display system prompt if all went well. If we're interpreting,
                ; this is " ok", if we're compiling, it's " compiled". Note
                ; space at beginning of the string.
                lda state
                beq _print

                lda #1                  ; number for "compile" string
_print:
                jsr print_string_n
                jsr w_cr

                ; Awesome line, everybody! Now get the next one.
                bra _get_line

z_cold:
z_abort:
z_quit:         ; no RTS required


underflow_error:
                ; Entry for COLD/ABORT/QUIT
                lda #err_underflow      ; fall through to error

error:
        ; """Given the error number in a, display the error and call abort. Uses tmp3.
        ; """
                pha                     ; save error
                jsr print_error_n
                jsr w_cr
                pla
                cmp #err_underflow      ; should we display return stack?
                bne w_abort

                lda #err_returnstack
                jsr print_error_n

                ; dump return stack from SP...$1FF to help debug source of underflow
                ; the data stack pointer in X is already corrupted so safe to reuse here
                tsx
-
                inx
                beq +
                jsr w_space
                lda $100,x
                jsr byte_to_ascii
                bra -
+
                jsr w_cr

                bra w_abort            ; no jsr, as we clobber return stack


; Underflow tests. We jump to the label with the number of cells (not: bytes)
; required for the word. This routine flows into the generic error handling
; code
; Note that using bpl will also generate an error if the stack is more than 64 items (128 bytes)
; beyond the comparison point (effectively an overflow).
; An alternative is to switch bpl to bcs everywhere which allows a slightly larger stack.
; However in that case dsp0 must be at most $f0 or these tests will fail in unexpected ways.
; See discussion in https://github.com/SamCoVT/TaliForth2/issues/148
underflow_1:
        ; """Make sure we have at least one cell on the Data Stack"""
                cpx #dsp0-1
                bpl underflow_error
                rts
underflow_2:
        ; """Make sure we have at least two cells on the Data Stack"""
                cpx #dsp0-3
                bpl underflow_error
                rts
underflow_3:
        ; """Make sure we have at least three cells on the Data Stack"""
                cpx #dsp0-5
                bpl underflow_error
                rts
underflow_4:
        ; """Make sure we have at least four cells on the Data Stack"""
                cpx #dsp0-7
                bpl underflow_error
                rts


.include "core.asm"
.include "literals.asm"
.include "compile.asm"
.include "tools.asm"
.include "tali.asm"
.include "double.asm"
.include "string.asm"
.if "assembler" in TALI_OPTIONAL_WORDS || "disassembler" in TALI_OPTIONAL_WORDS
    .include "assembler.asm"
.endif
.if "disassembler" in TALI_OPTIONAL_WORDS
    .include "disasm.asm"
.endif
.if "ed" in TALI_OPTIONAL_WORDS
    .include "ed.asm"        ; Line-based editor ed6502
.endif
.if "block" in TALI_OPTIONAL_WORDS
    .include "block.asm"
    .if "editor" in TALI_OPTIONAL_WORDS
        .include "editor.asm"
    .endif
.endif
.if "wordlist" in TALI_OPTIONAL_WORDS
    .include "wordlist.asm"
.endif
