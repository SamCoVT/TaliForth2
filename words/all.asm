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
                clc
                ; to warm start into a preloaded RAM image the platform init routine
                ; should sec and jump to forth_warm
forth_warm:
                ; 65c02 resets with this clear, but just in case kernel messed with it...
                cld

                ; Initialize 65c02 stack (Return Stack)
                ldx #rsp0
                txs

                ; Clear Data Stack. This is repeated in ABORT, but this way we
                ; can load high-level words with EVALUATE
                ldx #dsp0

                ; on warm start from valid memory image, skip the rest of setup
                bcs _turnkey

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
                ; This loop loads them back to front. We can use X here
                ; because Tali hasn't started using the stack yet.
                lda cold_zp_table,y
                sta zpage,y
                dey
                bpl _load_zp_loop       ; <128 bytes so loop until y<0

                ; Initialize the user variables.
                ldy #cold_user_table_end-cold_user_table-1

_load_user_vars_loop:
                ; Like the zero page variables, these are initialized
                ; back to front.
                lda cold_user_table,y
                sta (up),y
                dey
                bne _load_user_vars_loop

                ; Copy the 0th element.
                lda cold_user_table
                sta (up)
                jsr xt_cr

                ; Define high-level words in forth_words.asc via EVALUATE,
                ; followed by any user-defined words from user_words.asc.
                ; These are stored sequentially in ROM so we can evaluate them together.
                ; If you have neither, this section can be commented out.
                dex
                dex
                dex
                dex

                ; start address goes NOS
                lda #<forth_words_start
                sta 2,x
                lda #>forth_words_start
                sta 3,x

                ; length goes TOS; let the assembler do the math
                lda #<(user_words_end-forth_words_start)
                sta 0,x
                lda #>(user_words_end-forth_words_start)
                sta 1,x

                jsr xt_evaluate
_turnkey:
                lda turnkey+1
                beq _no_turnkey
                dex
                dex
                sta 1,x
                lda turnkey
                sta 0,x
                jsr xt_execute
_no_turnkey:

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
                ldx #dsp0

                ; fall through to QUIT


; ## QUIT ( -- ) "Reset the input and get new input"
; ## "quit"  tested  ANS core
        ; """https://forth-standard.org/standard/core/QUIT
        ; Rest the input and start command loop
        ; """
xt_quit:
                ; Clear the Return Stack. This is a little screwed up
                ; because the 65c02 can only set the Return Stack via X,
                ; which is our Data Stack pointer. The ANS specification
                ; demands, however, that ABORT reset the Data Stack pointer
                txa             ; Save the DSP that we just defined
                ldx #rsp0
                txs
                tax             ; Restore the DSP. Dude, seriously.

                ; make sure instruction pointer is empty
                stz ip
                stz ip+1

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
                jsr xt_refill           ; ( -- f )

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

                ; Test for Data Stack underflow. Tali Forth does not check for
                ; overflow because it is so rare
                cpx #dsp0
                beq _stack_ok
                bcc _stack_ok           ; DSP must always be smaller than DSP0

                jmp underflow_error

_stack_ok:
                ; Display system prompt if all went well. If we're interpreting,
                ; this is " ok", if we're compiling, it's " compiled". Note
                ; space at beginning of the string.
                lda state
                beq _print

                lda #1                  ; number for "compile" string
_print:
                jsr print_string

                ; Awesome line, everybody! Now get the next one.
                bra _get_line

z_cold:
z_abort:
z_quit:         ; no RTS required


.include "core.asm"
.include "compile.asm"
.include "tools.asm"
.include "tali.asm"
.include "double.asm"
.include "string.asm"
.if "disassembler" in TALI_OPTIONAL_WORDS
    .include "disasm.asm"
.endif
.if "assembler" in TALI_OPTIONAL_WORDS
    .include "assembler.asm"
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
