; Core Forth word routines
; Tali Forth 2 for the 65c02
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; Patrick Surry
; First version: 19. Jan 2014
; This version: 21. Apr 2024


; ## ABORT_QUOTE ( "string" -- ) "If flag TOS is true, ABORT with message"
; ## "abort""  tested  ANS core
        ; """https://forth-standard.org/standard/core/ABORTq
        ; Abort and print a string.
        ; """

xt_abort_quote:
w_abort_quote:
                ; save the string
                jsr w_s_quote          ; S"

                ; compile run-time part
                ldy #>abort_quote_runtime
                lda #<abort_quote_runtime
                jsr cmpl_subroutine     ; may not be JMP as JSR/RTS

z_abort_quote:  rts


abort_quote_runtime:
        ; """Runtime aspect of ABORT_QUOTE"""
                ; We arrive here with ( f addr u )
                lda 4,x
                ora 5,x
                beq _done       ; if FALSE, we're done

                ; We're true, so print string and ABORT. We follow Gforth
                ; in going to a new line after the string
                jsr w_type
                jsr w_cr
                jmp w_abort    ; not JSR, so never come back
_done:
                ; Drop three entries from the Data Stack
                txa
                clc
                adc #6
                tax

                rts


; ## ABS ( n -- u ) "Return absolute value of a number"
; ## "abs"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ABS
        ; Return the absolute value of a number.
        ; """
xt_abs:
                jsr underflow_1
w_abs:
                lda 1,x
                bpl _done       ; positive number, easy money!

                ; negative: calculate 0 - n
                sec
                lda #0
                sbc 0,x         ; LSB
                sta 0,x

                lda #0          ; MSB
                sbc 1,x
                sta 1,x

_done:
z_abs:          rts


; ## ACCEPT ( addr n -- n ) "Receive a string of characters from the keyboard"
; ## "accept"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ACCEPT
        ; Receive a string of at most n1 characters, placing them at
        ; addr. Return the actual number of characters as n2. Characters
        ; are echoed as they are received. ACCEPT is called by REFILL in
        ; modern Forths.
        ; """
xt_accept:
                jsr underflow_2
w_accept:
                ; Abort if we were asked to receive 0 chars
                lda 0,x
                ora 1,x
                bne _not_zero

                inx
                inx
                stz 0,x
                stz 1,x

                jmp accept_done

_not_zero:
                lda 0,x         ; number of chars to get in tmp2 ...
                sta tmp2
                stz tmp2+1      ; ... but we only accept max 255 chars

                lda 2,x         ; address of buffer is NOS, to tmp1
                sta tmp1
                lda 3,x
                sta tmp1+1

                inx
                inx

                ldy #0

                ; Select the next history buffer. Clear bit 3 first (so overflow
                ; from bit 2 to 3 is OK)
                lda status
                and #$F7

                ; Increment the buffer number (overflow from 7 to 0 OK)
                ina

                ; Set bit 3 for detecting if CTRL-n has been pressed the first
                ; time. This bit will be cleared on the first CTRL-n or CTRL-p
                ; received and won't be used to calculate the history buffer
                ; offset.
                ora #%00001000
                sta status

accept_loop:
                ; Out of the box, py65mon catches some CTRL sequences such as
                ; CTRL-c. We also don't need to check for CTRL-l because a
                ; vt100 terminal clears the screen automatically.

                ; This is the internal version of KEY without all the mucking
                ; about with the Data Stack while still using the input vector
                jsr key_a

                ; We quit on both line feed and carriage return
                cmp #AscLF
                beq _eol
                cmp #AscCR
                beq _eol

                ; BACKSPACE and DEL do the same thing for the moment
                cmp #AscBS
                beq _backspace
                cmp #AscDEL     ; (CTRL-h)
                beq _backspace

.if TALI_OPTION_HISTORY
                ; Check for CTRL-p and CTRL-n to recall input history
                cmp #AscCP
                beq _ctrl_p
                cmp #AscCN
                beq _ctrl_n
.endif
                ; That's enough for now. Save and echo character.
                sta (tmp1),y
                iny

                ; EMIT_A sidesteps all the fooling around with the Data Stack
                jsr emit_a

                cpy tmp2        ; reached character limit?
                bne accept_loop       ; fall through if buffer limit reached
                bra _buffer_full

_eol:
                jsr w_space    ; print final space

_buffer_full:
                ; REFILL updates ciblen and toin, we don't need to do it here
                sty 0,x         ; Y contains number of chars accepted already
                stz 1,x         ; we only accept 256 chars

                jmp accept_done

_backspace:
                ; Handle backspace and delete kex, which currently do the same
                ; thing
                cpy #0          ; buffer empty?
                bne +

                lda #AscBELL    ; complain and don't delete beyond the start of line
                jsr emit_a
                iny
+
                dey
                lda #AscBS      ; move back one
                jsr emit_a
                lda #AscSP      ; print a space (rubout)
                jsr emit_a
                lda #AscBS      ; move back over space
                jsr emit_a

                bra accept_loop

.if TALI_OPTION_HISTORY
_ctrl_p:
                ; CTRL-p was pressed. Recall the previous input buffer.

                ; Select the previous buffer
                lda status

                ; Check for 0 (need to wrap back to 7)
                and #7
                bne _ctrl_p_dec

                ; We need to wrap back to 7.
                lda status
                ora #7
                sta status
                bra _recall_history

_ctrl_p_dec:
                ; It's safe to decrement the buffer index directly.
                dec status
                bra _recall_history

_ctrl_n:
                ; CTRL-n was pressed. Recall the next input buffer. Select
                ; the next buffer Check bit 3. If it's set, this is the first
                ; time CTRL-n has been pressed and we should select the CURRENT
                ; history buffer.
                lda #$8
                bit status
                bne _recall_history

                ; This isn't the first time CTRL-n has been pressed, select the
                ; next history buffer. Clear bit 3 first (so overflow is OK)
                lda status
                and #$F7

                ; Increment the buffer number (overflow from 7 to 0 OK)
               ina

                ; Bit 3 (if it got set by going from buffer 7 to 0) will
                ; be cleared below.
                sta status

                ; Falls through to _recall_history

_recall_history:
                ; Clear bit 3 (first time ctrl-n recall) bit in status
                lda #%00001000
                trb status

                jsr accept_total_recall

                ; tmp3 now has the address of the previous history buffer.
                ; First byte of buffer is length. Clear the line by sending
                ; CR, Y spaces, then CR.
                lda #AscCR
                jsr emit_a

input_clear:
                cpy #0
                beq input_cleared

                lda #AscSP
                jsr emit_a
                dey
                bra input_clear

input_cleared:
                lda #AscCR
                jsr emit_a

                ; Save the history length byte into histinfo+1
                ; ldy #0        ; Y is already 0 by clearing the line.
                lda (tmp3),y
                sta status+1

                ; Increment the tmp3 pointer so we can use ,y addressing
                ; on both tmp1 (the input buffer) and tmp3 (the history
                ; buffer)
                inc tmp3
                bne +           ; Increment the upper byte on carry.
                inc tmp3+1
+
                ; Copy the history buffer into the input buffer,
                ; sending the characters to the output as we go.
                lda #AscCR
                jsr emit_a

_history_loop:
                ; See if we have reached the end of the history buffer.
                cpy status+1
                bne +
                jmp accept_loop       ; Needs a long jump
+
                ; See if we have reached the end of the input buffer.
                ; (only comparing to lower byte as we currently limit
                ; to 255 characters max)
                cpy tmp2
                beq _hist_filled_buffer

                ; Copy a character and echo.
                lda (tmp3),y
                sta (tmp1),y
                jsr emit_a

                ; Move to the next character.
                iny
                bra _history_loop

_hist_filled_buffer:
                ; We don't want a history recall to EOL our buffer,
                ; so back up one character and return to editing.
                dey
                jmp accept_loop

accept_done:
                ; Copy the input buffer into the currently
                ; selected history buffer.
                jsr accept_total_recall
                sta status+1

                ; Also save it in the first buffer byte.
                ldy #0
                sta (tmp3),y

                ; Move path the count to the data bytes
                inc tmp3
                bne +           ; Increment the upper byte on carry.
                inc tmp3+1
+
                ; Copy the characters from the input buffer to the
                ; history buffer.

_save_history_loop:
                cpy status+1
                beq _save_history_done

                lda (tmp1),y
                sta (tmp3),y
                iny
                bra _save_history_loop

_save_history_done:
.else
accept_done:            ; nothing to do if we're not saving history
.endif

z_accept:
                rts

.if TALI_OPTION_HISTORY
accept_total_recall:
        ; """Internal subroutine for ACCEPT that recalls history entry"""

                ; Generate the address of the buffer in tmp3. Start with the
                ; base address.
                lda #<hist_buff
                sta tmp3
                lda #>hist_buff
                sta tmp3+1

                ; This is a bit annoying as some bits go into each byte.
                ; .....xxx gets put into address like ......xx x.......
                lda status
                ror
                and #3
                clc
                adc tmp3+1
                sta tmp3+1

                lda status
                ror             ; Rotate through carry into msb.
                ror
                and #$80
                clc
                adc tmp3
                sta tmp3
                bcc +           ; Increment the upper byte on carry.
                inc tmp3+1
+
                ; Save the current length of the input buffer in
                ; histinfo+1 temporarily.  Reduce to 127 if larger.
                tya
                cmp #$80
                bcc +
                lda #$7F
+
                rts
.endif


; ## ACTION_OF ( "name" -- xt ) "Get named deferred word's xt"
; ## "action-of"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/ACTION-OF"""
xt_action_of:
w_action_of:
                ; This is a state aware word with differet behavior
                ; when used while compiling vs interpreting.
                ; Check STATE
                lda state
                ora state+1
                beq _interpreting

                ; Run ['] to compile the xt of the next word
                ; as a literal.
                jsr w_bracket_tick

                ; Postpone DEFER@ by compiling a JSR to it.
                ldy #>w_defer_fetch
                lda #<w_defer_fetch
                jsr cmpl_subroutine
                bra _done

_interpreting:
                jsr w_tick
                jsr w_defer_fetch

_done:
z_action_of:           rts


; ## AGAIN ( addr -- ) "Code backwards branch to address left by BEGIN"
; ## "again"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/AGAIN"""
xt_again:
                jsr underflow_1
w_again:
                ; Compile a JMP back to TOS address.
                jsr cmpl_jump_tos

z_again:        rts



; ## ALIGN ( -- ) "Make sure CP is aligned on word size"
; ## "align"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ALIGN
        ; On a 8-bit machine, this does nothing. ALIGNED uses this
        ; routine as well, and also does nothing
        ; """



; ## ALIGNED ( addr -- addr ) "Return the first aligned address"
; ## "aligned"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ALIGNED"""
xt_align:
xt_aligned:
w_align:
w_aligned:
z_align:
z_aligned:
                rts             ; stripped out during native compile



; ## ALLOT ( n -- ) "Reserve or release memory"
; ## "allot"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ALLOT
        ; Reserve a certain number of bytes (not cells) or release them.
        ; If n = 0, do nothing. If n is negative, release n bytes, but only
        ; to the beginning of the Dictionary. If n is positive (the most
        ; common case), reserve n bytes, but not past the end of the
        ; Dictionary. See http://forth-standard.org/standard/core/ALLOT
        ; """
xt_allot:
                jsr underflow_1
w_allot:
                ; Releasing memory is going to be a very rare operation,
                ; so we check for it at the beginning and try to make
                ; the most common case as fast as possible
                lda 1,x
                bmi _release

                ; Common case: We are reserving memory, not releasing it
                clc
                lda cp
                adc 0,x
                sta cp

                lda cp+1
                adc 1,x
                sta cp+1

                ; Wait, did we just grant more space than we have? This is
                ; a check we only do here, not for other situations like cmpl_a
                ; where smaller amounts are reserved.
                ldy #<cp_end
                cpy cp
                lda #>cp_end
                sbc cp+1
                bcs _done               ; we're fine.

                ; Oops, that was too much, we're beyond the end of
                ; legal Dictionary RAM. Reduce to max memory and report
                ; an error
                sty cp                  ; still #<cp_end
                lda #>cp_end
                sta cp+1

                lda #err_allot
                jmp error

_release:
   		; The ANS standard doesn't really say what to do if too much
                ; memory is freed ("negatively alloted"). In fact, there isn't
                ; even an official test. Gforth is little help either. The good
                ; news is, this is going to be a rare case. We want to use as
                ; few bytes as possible.

                ; What we do is let the user free anything up to the beginning
                ; of the RAM area assigned to the Dicionary (CP0), but at
                ; their own risk. This means that the Dictionary pointer DP
                ; might end up pointing to garbage. However, an attempt to
                ; free more than RAM than CP0 will lead to CP being set to CP0,
                ; the DP pointing to the last word in RAM (should be DROP) and
                ; an error message.

                ; We arrive here with ( n ) which is negative. First step,
                ; subtract the number TOS from the CP for a new CP
                dex
                dex
                lda cp
                sta 0,x
                lda cp+1
                sta 1,x

                jsr w_plus                     ; new CP is now TOS

                ; Second step, see if we've gone too far. We compare the new
                ; CP on TOS (which, if we've really screwed up, might be
                ; negative) with CP0. This is a signed comparison
                dex
                dex                             ; new CP now NOS
                lda #<cp0
                sta 0,x
                lda #>cp0
                sta 1,x                         ; CP0 is TOS

                jsr compare_16bit               ; still ( CP CP0 )

                ; If CP (NOS) is smaller than CP0 (TOS), we're in trouble.
                ; This means we want Z=1 or N=1
                beq _nega_done
                bmi _nega_done

                ; Yep, we're in trouble. Set CP to CP0, set DP to the first
                ; word in ROM (should be DROP), and abort with an error
                lda #<cp0
                sta cp
                lda #>cp0
                sta cp+1

                lda #<dictionary_start
                sta dp
                lda #>dictionary_start
                sta dp+1

                lda #err_negallot
                jmp error

_nega_done:
                ; Save new CP, which is NOS
                lda 2,x
                sta cp
                lda 3,x
                sta cp+1

                inx
                inx                     ; drop through to _done
_done:
                inx
                inx
z_allot:
                rts



; ## AND ( n n -- n ) "Logically AND TOS and NOS"
; ## "and"  auto  ANS core
        ; """https://forth-standard.org/standard/core/AND"""
xt_and:
                jsr underflow_2
w_and:
                lda 0,x
                and 2,x
                sta 2,x

                lda 1,x
                and 3,x
                sta 3,x

                inx
                inx

z_and:          rts



; ## AT_XY ( n m -- ) "Move cursor to position given"
; ## "at-xy"  tested  ANS facility
        ; """https://forth-standard.org/standard/facility/AT-XY
        ; On an ANSI compatible terminal, place cursor at row n colum m.
        ; ANSI code is ESC[<n>;<m>H
        ;
        ; Do not use U. to print the numbers because the
        ; trailing space will not work with xterm
        ; """
xt_at_xy:
                jsr underflow_2
w_at_xy:
                ; Save the BASE and change to decimal as the ANSI escape code
                ; values need to be in decimal.
                lda base
                pha
                lda #10
                sta base

                lda #AscESC
                jsr emit_a
                lda #'['
                jsr emit_a
                jsr w_one_plus ; AT-XY is zero based, but ANSI is 1 based
                jsr print_u
                lda #';'
                jsr emit_a
                jsr w_one_plus ; AT-XY is zero based, but ANSI is 1 based
                jsr print_u
                lda #'H'
                jsr emit_a

                ; Restore the base
                pla
                sta base

z_at_xy:        rts



; ## BACKSLASH ( -- ) "Ignore rest of line"
; ## "\"  auto  ANS block ext
        ; """https://forth-standard.org/standard/block/bs"""
xt_backslash:
w_backslash:
                ; Check BLK to see if we are interpreting a block
                ldy #blk_offset
                lda (up),y
                iny
                ora (up),y
                beq backslash_not_block

                ; We are in a block.  Move toin to next multiple of 64.

                ; First, however, we have to see if we are at an exact
                ; multiple of 64+1, which happens when a \ is at the end
                ; of a line (in which case we do nothing).  We also have
                ; to check for exact multiple of 64, which will happen with
                ; a backslash at the very end of a block.
                lda toin
                and #$3F
                beq z_backslash
                cmp #$1
                beq z_backslash

                ; Not at the end of the line (beginning of next line,
                ; after parsing the \, technically), so move to the
                ; next line.
                lda toin
                and #$C0        ; Clear lower bits to move to beginning of line.

                clc             ; Add $40 (64 decimal) to move to next line.
                adc #$40
                sta toin
                bcc z_backslash
                inc toin+1
                bra z_backslash

backslash_not_block:
                lda ciblen
                sta toin
                lda ciblen+1
                sta toin+1

z_backslash:    rts



; ## BASE ( -- addr ) "Push address of radix base to stack"
; ## "base"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BASE
        ; The ANS Forth standard sees the base up to 36, so we can cheat and
        ; ingore the MSB
        ; """
xt_base:
w_base:
                dex
                dex
                lda #<base
                sta 0,x         ; LSB
                stz 1,x         ; MSB is always 0

z_base:         rts



; ## BEGIN ( -- addr ) "Mark entry point for loop"
; ## "begin"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BEGIN
        ;
        ; This is a dummy header, BEGIN shares the actual code with HERE
        ; It could as well be coded in Forth as
        ;       : BEGIN HERE ; IMMEDIATE COMPILE-ONLY
        ; """



; ## BL ( -- c ) "Push ASCII value of SPACE to stack"
; ## "bl"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BL"""
xt_bl:
w_bl:
                dex
                dex
                lda #AscSP
                sta 0,x
                stz 1,x

z_bl:           rts



; ## BRACKET_CHAR ( "c" -- ) "Compile character"
; ## "[char]"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BracketCHAR
        ; Compile the ASCII value of a character as a literal. This is an
        ; immediate, compile-only word.
        ;
        ; A definition given in
        ; http://forth-standard.org/standard/implement is
        ; : [CHAR]  CHAR POSTPONE LITERAL ; IMMEDIATE
        ; """
xt_bracket_char:
w_bracket_char:
                jsr w_char
                jsr w_literal
z_bracket_char: rts



; ## BRACKET_TICK ( -- ) "Store xt of following word during compilation"
; ## "[']"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BracketTick"""
xt_bracket_tick:
w_bracket_tick:
                jsr w_tick
                jsr w_literal
z_bracket_tick: rts



; ## BUFFER_COLON ( u "<name>" -- ; -- addr ) "Create an uninitialized buffer"
; ## "buffer:"  auto  ANS core ext
                ; """https://forth-standard.org/standard/core/BUFFERColon
                ; Create a buffer of size u that puts its address on the stack
                ; when its name is used.
                ; """
xt_buffer_colon:
w_buffer_colon:
                jsr w_create
                jsr w_allot
z_buffer_colon: rts



; ## C_COMMA ( c -- ) "Store one byte/char in the Dictionary"
; ## "c,"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CComma"""
xt_c_comma:
                jsr underflow_1
w_c_comma:
                lda 0,x
                jsr cmpl_a

                inx
                inx

z_c_comma:      rts



; ## C_FETCH ( addr -- c ) "Get a character/byte from given address"
; ## "c@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CFetch"""
xt_c_fetch:
                jsr underflow_1
w_c_fetch:
                lda (0,x)
                sta 0,x
                stz 1,x         ; Ignore LSB

z_c_fetch:      rts



; ## C_STORE ( c addr -- ) "Store character at address given"
; ## "c!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CStore"""
xt_c_store:
                jsr underflow_2
w_c_store:
                lda 2,x
                sta (0,x)

                inx
                inx
                inx
                inx

z_c_store:      rts



; ## CASE (C: -- 0) ( -- ) "Conditional flow control"
; ## "case"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/CASE
        ;
        ; This is a dummy header, CASE shares the actual code with ZERO.
        ; """



; ## CELL_PLUS ( u -- u ) "Add cell size in bytes"
; ## "cell+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CELLPlus
        ; Add the number of bytes ("address units") that one cell needs.
        ; Since this is an 8 bit machine with 16 bit cells, we add two bytes.
        ; """
xt_cell_plus:
                jsr underflow_1
w_cell_plus:
                inc 0,x
                bne +
                inc 1,x
+
                inc 0,x
                bne _done
                inc 1,x
_done:
z_cell_plus:    rts



; ## CELLS ( u -- u ) "Convert cells to size in bytes"
; ## "cells"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CELLS
        ;
        ; Dummy entry for the CELLS word, the code is the same as for
        ; 2*, which is where the header directs us to
        ; """



; ## CHAR ( "c" -- u ) "Convert character to ASCII value"
; ## "char"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CHAR"""
xt_char:
w_char:
                ; get character from string, returns ( addr u )
                jsr w_parse_name

                ; if we got back a zero, we have a problem
                lda 0,x
                ora 1,x
                bne _not_empty

                lda #err_noname
                jmp error

_not_empty:
                inx             ; drop number of characters, leave addr
                inx
                lda (0,x)       ; get character (equivalent to C@)
                sta 0,x
                stz 1,x         ; MSB is always zero

z_char:         rts



; ## CHAR_PLUS ( addr -- addr+1 ) "Add the size of a character unit to address"
; ## "char+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CHARPlus
        ;
        ; This is a dummy entry, the code is shared with ONE_PLUS
        ; """



; ## CHARS ( n -- n ) "Number of bytes that n chars need"
; ## "chars"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CHARS
        ; Return how many address units n chars are. Since this is an 8 bit
        ; machine, this does absolutely nothing and is included for
        ; compatibility with other Forth versions
        ; """
xt_chars:
                ; Checking for underflow seems a bit stupid because this
                ; routine does nothing on this machine. However, the user
                ; should be warned that there is something wrong with the
                ; code if this occurs.
                jsr underflow_1
w_chars:
z_chars:        rts



; ## COLON ( "name" -- ) "Start compilation of a new word"
; ## ":"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Colon
        ;
        ; Use the CREATE routine and fill in the rest by hand.
        ; """
xt_colon:
w_colon:
                ; If we're already in the compile state, complain
                ; and quit
                lda state
                ora state+1
                beq +

                lda #err_state
                jmp error
+
                ; switch to compile state
                dec state
                dec state+1

                ; Set bit 6 in status to tell ";" and RECURSE this is a normal
                ; word
                lda #%01000000
                tsb status

                ; CREATE is going to change the DP to point to the new word's
                ; header. While this is fine for (say) variables, it would mean
                ; that FIND-NAME etc would find a half-finished word when
                ; looking in the Dictionary. To prevent this, we save the old
                ; version of DP and restore it later. The new DP is placed in
                ; the variable WORKWORD until we're finished with a SEMICOLON.
                jsr current_to_dp
                lda dp+1            ; CREATE uses a lot of variables
                pha
                lda dp
                pha

                ; Tell create not to print warning for duplicate name.
                lda #%10000000
                tsb status

                jsr w_create

                ; Get the nt (not the xt!) of the new word as described above.
                ; Only COLON, SEMICOLON and RECURSE get to access WORKWORD
                jsr current_to_dp   ; This might be able to be omitted
                lda dp
                sta workword
                lda dp+1
                sta workword+1

                ; Restore original DP
                pla
                sta dp
                pla
                sta dp+1
                jsr dp_to_current

                ; CREATE includes a subroutine jump to DOVAR by default. We
                ; back up three bytes and overwrite that.
                lda cp
                sec
                sbc #3
                sta cp
                bcs _done
                dec cp+1
_done:
z_colon:        rts



; ## COLON_NONAME ( -- ) "Start compilation of a new word""
; ## ":NONAME"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ColonNONAME
        ; Compile a word with no nt.  ";" will put its xt on the stack.
        ; """
xt_colon_noname:
w_colon_noname:
                ; If we're already in the compile state, complain
                ; and quit
                lda state
                ora state+1
                beq +

                lda #err_state
                jmp error
+
                ; switch to compile state
                dec state
                dec state+1

                ; Clear bit 6 in status to tell ";" and RECURSE this is
                ; a :NONAME word.
                lda #%01000000
                trb status

                ; Put cp (the xt for this word) in WORKWORD. The flag above
                ; lets both ";" and RECURSE know that is is an xt instead of an
                ; nt and they will modify their behavior.
                lda cp
                sta workword
                lda cp+1
                sta workword+1
z_colon_noname:        rts



; ## COMMA ( n -- ) "Allot and store one cell in memory"
; ## ","  auto  ANS core
        ; """https://forth-standard.org/standard/core/Comma
        ; Store TOS at current place in memory.
        ;
        ; Since this an eight-bit machine, we can ignore all alignment issues.
        ; """
xt_comma:
                jsr underflow_1
w_comma:
                ldy #2
_twice:         lda 0,x
                jsr cmpl_a
                inx
                dey
                bne _twice

z_comma:        rts



; ## COMPILE_ONLY ( -- ) "Mark most recent word as COMPILE-ONLY"
; ## "compile-only"  tested  Tali Forth
        ; """Set the Compile Only flag (CO) of the most recently defined
        ; word.
        ;
        ; The alternative way to do this is to define a word
        ; ?COMPILE that makes sure  we're in compile mode
        ; """
xt_compile_only:
w_compile_only:
                jsr current_to_dp
                ldy #1          ; offset for status byte
                lda (dp),y
                ora #CO        ; make sure bit 7 is set
                sta (dp),y

z_compile_only: rts


; ## CONSTANT ( n "name" -- ) "Define a constant"
; ## "constant"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CONSTANT
        ;
        ; Forth equivalent is  CREATE , DOES> @  but we do
        ; more in assembler and let CREATE do the heavy lifting.
        ; See http://www.bradrodriguez.com/papers/moving3.htm for
        ; a primer on how this works in various Forths. This is the
        ; same code as VALUE in our case.
        ; """
xt_value:
xt_constant:
                jsr underflow_1
w_value:
w_constant:
                jsr w_create

            	; CREATE by default installs a subroutine jump to DOVAR,
                ; but we want DOCONST for constants. Go back two bytes and
                ; replace the subroutine jump target
                sec
                lda cp
                sbc #2
                sta tmp1
                lda cp+1
                sbc #0
                sta tmp1+1

                lda #<doconst           ; LSB of DOCONST
                sta (tmp1)
                ldy #1
                lda #>doconst           ; MSB of DOCONST
                sta (tmp1),y

                ; Now we save the constant number itself in the next cell
                jsr w_comma            ; drop through to adjust_z

adjust_z:
                ; Now the length of the complete word (z_word) has increased by
                ; two. We need to update that number or else words such as SEE
                ; will ignore the PFA. We use this same routine for VARIABLE,
                ; VALUE and DEFER
                jsr w_latestnt         ; gives us ( -- nt )

                ; z_word is six bytes further down
                lda 0,x
                sta tmp1
                lda 1,x
                sta tmp1+1

                ldy #6
                lda (tmp1),y
                clc
                adc #2
                sta (tmp1),y
                iny
                lda (tmp1),y
                adc #0                  ; only need carry
                sta (tmp1),y

                inx
                inx

z_value:
z_constant:     rts



; ## COUNT ( c-addr -- addr u ) "Convert character string to normal format"
; ## "count"  auto  ANS core
        ; """https://forth-standard.org/standard/core/COUNT
        ; Convert old-style character string to address-length pair. Note
        ; that the length of the string c-addr is stored in character length
        ; (8 bit), not cell length (16 bit). This is rarely used these days,
        ; though COUNT can also be used to step through a string character by
        ; character.
        ; """
xt_count:
                jsr underflow_1
w_count:
                lda (0,x)       ; Get number of characters (255 max)
                tay

                ; move start address up by one
                inc 0,x         ; LSB
                bne +
                inc 1,x         ; MSB

                ; save number of characters to stack
+               tya
                dex
                dex
                sta 0,x         ; LSB
                stz 1,x         ; MSB, always zero

z_count:        rts



; ## CR ( -- ) "Print a line feed"
; ## "cr"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CR"""
xt_cr:
w_cr:
.if "cr" in TALI_OPTION_CR_EOL
                lda #AscCR
                jsr emit_a
.endif
.if "lf" in TALI_OPTION_CR_EOL
                lda #AscLF
                jsr emit_a
.endif
z_cr:           rts


; ## CREATE ( "name" -- ) "Create Dictionary entry for 'name'"
; ## "create"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CREATE
        ;
        ; See the drawing in headers.asm for details on the header
        ; """
xt_create:
w_create:
                ; get string
                jsr w_parse_name       ; ( addr u )

                ; if we were given an empty string, we complain and quit
                lda 0,x
                ora 1,x
                bne _got_name

                lda #err_noname
                jmp error

_got_name:
                ; Enforce maximal length of string by overwriting the MSB of
                ; the length. There is a possible error here: If the string
                ; is exactly 255 chars long, then a lot of the following
                ; additions will fail because of wrapping
                stz 1,x

                ; Check to see if this name already exists.
                jsr w_two_dup          ; ( addr u addr u )
                jsr w_find_name        ; ( addr u flag ) (non-zero nt as flag)

                lda 0,x
                ora 1,x
                beq _new_name           ; We haven't seen this one before.

                ; This name already exists.  See if we are supposed to print
                ; the message for it.
                inx                     ; Drop flag (nt) from find-name.
                inx

                ; Check bit 7
                bit status
                bpl _redefined_name     ; Bit 7 is zero, so print the message.

                ; We aren't supposed to print the redefined message ourselves,
                ; but we should indicate that it is redefined (for ; to print
                ; later).
                lda #$80                ; Set bit 7 to indicate dup
                ora status
                sta status
                bra _process_name

_redefined_name:
                ; Print the message that the name is redefined.
                lda #str_redefined
                jsr print_string_no_lf

                jsr w_two_dup           ; ( addr u addr u )
                jsr w_type
                jsr w_space

                bra _process_name

_new_name:
                inx                     ; Drop flag (0) from find-name.
                inx
                lda #$7F                ; Clear bit 0 of status to indicate new word.
                and status
                sta status

_process_name:
                lda 0,x
                sta tmp2                ; store length of string in tmp2

                ; remember the first free byte of memory as the start of
                ; the new word
                lda cp
                sta tmp1
                lda cp+1
                sta tmp1+1

                ; We need 8 bytes plus the length of the string for our new header.
                ; This is also the offset for the start of the code field (the
                ; xt_ label) so we need to remember it. Otherwise, we could
                ; just allot the space afterwards
                lda 0,x
                clc
                adc #8
                sta tmp3                ; total header length

                ; We overwrite the length of the string returned by PARSE-NAME
                ; and then call ALLOT.
                ; We'll compile the CFA later
                sta 0,x
                stz 1,x         ; max header size is 255 chars
                jsr w_allot    ; ( addr )

                ; Get the CURRENT dictionary pointer.
                jsr current_to_dp

                ; Now we walk through the header with Y as the index, adding
                ; information byte-by-byte
                ldy #0

                ; HEADER BYTE 0: Length of string
                lda tmp2
                sta (tmp1),y

                ; HEADER BYTE 1: Status byte. By default, we set all new words
                ; to "never native", user will have to decide if they should
                ; be inlined
                lda #NN

                ; Also, words defined by CREATE are marked in the header has
                ; having a Code Field Area (CFA), which is a bit tricky for
                ; Subroutine Threaded Code (STC). We do this so >BODY works
                ; correctly with DOES> and CREATE. See the discussion at
                ; http://forum.6502.org/viewtopic.php?f=9&t=5182 for details
                ora #HC
                iny
                sta (tmp1),y
                iny

                ; HEADER BYTE 2,3: Next header. This is the current last word
                ; in the Dictionary
                lda dp
                sta (tmp1),y
                iny
                lda dp+1
                sta (tmp1),y
                iny

                ; Interlude: Make old CP new DP (new start of Dictionary)
                lda tmp1+1
                sta dp+1
                lda tmp1
                sta dp

                ; HEADER BYTE 4,5: Start of the code field ("xt_" of this word).
                ; This begins after the header so we take the length of the
                ; header, which we cleverly saved in tmp3, and use it as an
                ; offset to the address of the start of the word. We come here
                ; with tmp1 in A
                clc
                adc tmp3        ; add total header length
                sta (tmp1),y
                pha             ; we need this in the next step
                iny

                lda tmp1+1
                adc #0          ; only need the carry
                sta (tmp1),y
                iny

                ; HEADER BYTE 6,7: End of code ("z_" of this word). By default,
                ; we execute a jump to the DOVAR routine, so we need to move three
                ; bytes down, and then one more byte so that the z_ label points
                ; to the (still fictional) RTS instruction for correct compilation
                pla             ; LSB of "z_" address
                clc
                adc #3
                sta (tmp1),y

                dey             ; get the MSB of xt back
                lda (tmp1),y
                adc #0          ; only need the carry
                iny
                iny
                sta (tmp1),y
                iny

                ; HEADER BYTE 8: Start of name string. The address is TOS, the
                ; length in tmp2. We subtract 8 from the address so we can
                ; use the same loop index, which is already 8 byte ahead at
                ; this point
                lda 0,x
                sec
                sbc #8
                sta tmptos

                lda 1,x
                sbc #0          ; only need carry
                sta tmptos+1

_name_loop:
                lda (tmptos),y

                ; Make sure it goes into the dictionary in lower case.
                cmp #'Z'+1
                bcs _store_name
                cmp #'A'
                bcc _store_name

                ; An uppercase letter has been found so make it lowercase.
                ora #$20

                ; Fall into _store_name.

_store_name:
                sta (tmp1),y
                iny
                dec tmp2
                bne _name_loop

                ; After the name string comes the code field, starting at the
                ; current xt of this word, which is initially a jump to the
                ; subroutine to DOVAR.
                ldy #>dovar
                lda #<dovar
                jsr cmpl_subroutine

                ; Update the CURRENT wordlist with the new DP.
                ; We do this down here because this routine uses Y.
                jsr dp_to_current

                ; And we're done. Restore stack
                inx
                inx

z_create:       rts



; ## DECIMAL ( -- ) "Change radix base to decimal"
; ## "decimal"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DECIMAL"""
xt_decimal:
w_decimal:
                lda #10
                sta base
                stz base+1              ; paranoid

z_decimal:      rts


; ## DEFER ( "name" -- ) "Create a placeholder for words by name"
; ## "defer"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/DEFER
        ; Reserve an name that can be linked to various xt by IS.
        ;
        ; The ANS reference implementation is
        ;       CREATE ['] ABORT , DOES> @ EXECUTE ;
        ; But we use this routine as a low-level word so things go faster

xt_defer:
w_defer:
                jsr w_create

                ; CREATE by default installs a subroutine jump to DOVAR,
                ; but we actually want DODEFER this time. Go back two
                ; bytes and repace the subroutine jump target
                lda cp          ; LSB
                sec
                sbc #2
                sta tmp1

                lda cp+1        ; MSB
                sbc #0          ; we only care about the borrow
                sta tmp1+1

                ; Save the target address
                ldy #0
                lda #<dodefer   ; LSB
                sta (tmp1),y
                iny
                lda #>dodefer   ; MSB
                sta (tmp1),y


                ; DODEFER executes the next address it finds after
                ; its call. As default, we include the error
                ; "Defer not defined"
                lda #<defer_error
                sta (cp)
                inc cp
                bne +
                inc cp+1
+
                lda #>defer_error
                sta (cp)
                inc cp
                bne +
                inc cp+1
+
                jsr adjust_z    ; adjust header to correct length

z_defer:        rts



; ## DEFER_FETCH ( xt1 -- xt2 ) "Get the current XT for a deferred word"
; ## "defer@"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/DEFERFetch"""

xt_defer_fetch:
                jsr underflow_1
w_defer_fetch:
                jsr w_to_body
                jsr w_fetch
z_defer_fetch:  rts



; ## DEFER_STORE ( xt2 x1 -- ) "Set xt1 to execute xt2"
; ## "defer!"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/DEFERStore"""

xt_defer_store:
                jsr underflow_2
w_defer_store:
                jsr w_to_body
                jsr w_store
z_defer_store:  rts



; ## DEPTH ( -- u ) "Get number of cells (not bytes) used by stack"
; ## "depth"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DEPTH"""
xt_depth:
w_depth:
                lda #dsp0
                stx tmpdsp
                sec
                sbc tmpdsp

                ; divide by two because each cell is two bytes
                lsr

                dex
                dex
                sta 0,x
                stz 1,x

z_depth:        rts



; ## QUESTION_DO (C: -- ) ( limit start -- ) "Conditional loop start"
; ## "?do"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/qDO"""
xt_question_do:
w_question_do:
                ; ?DO shares most of its code with DO.
                ; But first compile its runtime.
                dex
                dex
                lda #<question_do_runtime
                sta 0,x
                lda #>question_do_runtime
                sta 1,x
                jsr w_dup              ; xt and xt' are the same
                dex
                dex
                lda #question_do_runtime_size
                sta 0,x
                stz 1,x
                jsr cmpl_by_limit
                bcc _native

                ; for subroutine compile, write placeholder for jmp-target and save its address
                jsr w_here
                jsr w_zero
                jsr w_comma
                bra do_common

_native:
                ; for native compile, add the placeholder jump, saving its address
                jsr cmpl_jump_later
                bra do_common


; ## DO (C: -- ) ( limit start -- )  "Start a loop"
; ## "do"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DO
        ;
        ; Compile-time part of DO. Could be realized in Forth as
        ;       : DO POSTPONE (DO) HERE ; IMMEDIATE COMPILE-ONLY
        ; but we do it in assembler for speed. See
        ; the Control Flow section of the manual for details.
        ;
        ; This is never native compile. Don't check for a stack underflow
        ; """

xt_do:
w_do:
                jsr w_zero             ; push 0 TOS

do_common:
                ; the stack is ( 0 | jmp-target ) depending
                ; on whether we arrived from DO or ?DO

                ; The word LEAVE can be used to exit LOOP/+LOOP from
                ; anywhere inside the loop body, zero or more times.
                ; We'll use loopleave as the head of a linked list
                ; which points at the latest LEAVE address
                ; that we need to patch.  The xt_leave compilation
                ; will link backward to any prior LEAVE.
                ; To handle nested loops we stack the current value
                ; of loopleave here and restore it in xt_loop
                ; after we write any chained jumps for the current loop

                ; save current loopleave in case we're nested
                dex
                dex
                lda loopleave
                sta 0,x
                lda loopleave+1
                sta 1,x

                ; For now there is no LEAVE addr to patch, which we
                ; flag with MSB=0 (zero page) which is never a compilation target
                stz loopleave+1

                ; compile runtime part of DO.
                ; do this as a subroutine since it only happens once and is a big chunk of code
                ldy #>do_runtime
                lda #<do_runtime
                jsr cmpl_subroutine

                ; Now we're ready for the loop body.  We also push HERE
                ; to the Data Stack so LOOP/+LOOP knows where to repeat back
                ; ( qdo-skip old-loopleave repeat-addr )

                jmp w_here
z_question_do:
z_do:


question_do_runtime:
        ; """This is called (?DO) in some Forths. See the explanation at
        ; do_runtime for the background on this design
        ; """
                ; if TOS == NOS we skip the loop and drop the limits
                lda 0,x
                cmp 2,x
                bne _begin
                lda 1,x
                cmp 3,x
                bne _begin
                inx                     ; drop loop limits and skip
                inx
                inx
                inx
question_do_runtime_size = * - question_do_runtime
                ; for native compilation we stop here and ?DO will tack on a JMP <skip-addr>
                ; for subroutine compile we set up A for zbranch_runtime
                lda #0
                .byte $2c               ; BIT llhh to hide the lda #1
_begin:         lda #1
                jmp zbranch_runtime



do_runtime:
        ; """Runtime routine for DO loop. Note that ANS loops quit when the
        ; boundary of limit-1 and limit is reached, a different mechanism than
        ; the FIG Forth loop (you can see which version you have by running
        ; a loop with start and limit as the same value, for instance
        ; 0 0 DO -- these will walk through the number space). We use a
        ; "fudge factor" for the limit that makes the Overflow Flag trip when
        ; it is reached; see http://forum.6502.org/viewtopic.php?f=9&t=2026
        ; for further discussion of this. The source given there for
        ; this idea is Laxen & Perry F83. -- This routine is called (DO)
        ; in some Forths.
        ; """
                ldy loopctrl
                bmi +                   ; is this the first LCB?
                lda loopidx0            ; no, write cached LSB
                sta loopindex,y         ; back to loopindex in the LCB
+
                iny                     ; Reserve 4 bytes for next LCB
                iny
                iny
                iny
                sty loopctrl            ; Udpate LCB stack pointer

                ; data stack has ( limit index -- )
                ;
                ; We're going to calculate adjusted loop bounds
                ; and store the values in the current LCB:
                ;
                ;   loopfufa = $8000 - limit
                ;   loopindex = loopfufa + index
                ;
                ; The idea is that once we've incremented this adjusted
                ; index at least limit-index times we'll get:
                ;
                ;   loopindex' = $8000 - limit + index + (limit - index)
                ;              = $8000
                ;
                ; which will trigger LOOP's overflow test

                sec
                lda #0
                sbc 2,x             ; LSB of limit
                sta loopfufa,y      ; write to loop control block
                lda #$80
                sbc 3,x             ; MSB of limit
                sta loopfufa+1,y

                ; Second step: index is FUFA plus original index
                clc
                lda 0,x             ; LSB of original index
                adc loopfufa,y
                sta loopidx0        ; write LSB to cache not LCB
                lda 1,x             ; MSB of orginal index
                adc loopfufa+1,y
                sta loopindex+1,y

                inx                 ; clean up the stack
                inx
                inx
                inx

                rts


; ## DOES ( -- ) "Add payload when defining new words"
; ## "does>"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DOES
        ; Create the payload for defining new defining words. See
        ; http://www.bradrodriguez.com/papers/moving3.htm and
        ; the Developer Guide in the manual for a discussion of
        ; DOES>'s internal workings. This uses tmp1 and tmp2.
        ; """

xt_does:
w_does:
                ; compile a subroutine jump to runtime of DOES>
                ldy #>does_runtime
                lda #<does_runtime
                jsr cmpl_subroutine

                ; compile a subroutine jump to DODOES. In traditional
                ; terms, this is the Code Field Area (CFA) of the new
                ; word
                ldy #>dodoes
                lda #<dodoes
                jsr cmpl_subroutine

z_does:         rts


does_runtime:
        ; """Runtime portion of DOES>. This replaces the subroutine jump
        ; to DOVAR that CREATE automatically encodes by a jump to the
        ; address that contains a subroutine jump to DODOES. We don't
        ; jump to DODOES directly because we need to work our magic with
        ; the return addresses. This routine is also known as "(DOES)" in
        ; other Forths
        ; """

                ply             ; LSB
                pla             ; MSB

                iny
                bne +
                ina
+
                sty tmp1
                sta tmp1+1

                ; CREATE has also already modified the DP to point to the new
                ; word. We have no idea which instructions followed the CREATE
                ; command if there is a DOES> so the CP could point anywhere
                ; by now. The address of the word's xt is four bytes down.
                jsr current_to_dp   ; Grab the DP from the CURRENT wordlist.
                lda dp
                clc
                adc #4
                sta tmp2
                lda dp+1
                adc #0          ; we only care about the carry
                sta tmp2+1

                ; Now we get that address and add one byte to skip over the JSR
                ; opcode
                lda (tmp2)
                clc
                adc #1
                sta tmp3
                ldy #1
                lda (tmp2),y
                adc #0          ; we only care about the carry
                sta tmp3+1

                ; Replace the DOVAR address with our own
                lda tmp1        ; LSB
                sta (tmp3)
                lda tmp1+1
                sta (tmp3),y    ; Y is still 1

                ; Since we removed the return address that brought us here, we
                ; go back to whatever the main routine was. Otherwise, we we
                ; smash into the subroutine jump to DODOES.
                rts



; ## DOT ( u -- ) "Print TOS"
; ## "."  auto  ANS core
        ; """https://forth-standard.org/standard/core/d"""

xt_dot:
                jsr underflow_1
w_dot:
                jsr w_dup                      ; ( n n )
                jsr w_abs                      ; ( n u )
                jsr w_zero                     ; ( n u 0 )
                jsr w_less_number_sign         ; ( n u 0 )
                jsr w_number_sign_s            ; ( n ud )
                jsr w_rot                      ; ( ud n )
                jsr w_sign                     ; ( ud )
                jsr w_number_sign_greater      ; ( addr u )
                jsr w_type
                jsr w_space

z_dot:          rts



; ## DOT_PAREN ( -- ) "Print input up to close paren .( comment )"
; ## ".("  auto  ANS core
        ; """http://forth-standard.org/standard/core/Dotp"""

xt_dot_paren:
w_dot_paren:
                ; Put a right paren on the stack.
                dex
                dex
                lda #41     ; Right parenthesis
                sta 0,x
                stz 1,x

                jsr w_parse
                jsr w_type

z_dot_paren:    rts



; ## DOT_QUOTE ( "string" -- ) "Print string from compiled word"
; ## ".""  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Dotq
        ; Compile string that is printed during run time. ANS Forth wants
        ; this to be compile-only, even though everybody and their friend
        ; uses it for everything. We follow the book here, and recommend
        ; `.(` for general printing.
        ; """

xt_dot_quote:
w_dot_quote:
                ; we let S" do the heavy lifting. Since we're in
                ; compile mode, it will save the string and reproduce it
                ; during runtime
                jsr w_s_quote

                ; We then let TYPE do the actual printing
                ldy #>w_type
                lda #<w_type
                jsr cmpl_subroutine

z_dot_quote:    rts



; ## DOT_R ( n u -- ) "Print NOS as unsigned number with TOS with"
; ## ".r"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/DotR
        ;
        ; Based on the Forth code
        ;  : .R  >R DUP ABS 0 <# #S ROT SIGN #> R> OVER - SPACES TYPE ;
        ; """

xt_dot_r:
                jsr underflow_2
w_dot_r:
                jsr w_to_r
                jsr w_dup
                jsr w_abs
                jsr w_zero
                jsr w_less_number_sign
                jsr w_number_sign_s
                jsr w_rot
                jsr w_sign
                jsr w_number_sign_greater
                jsr w_r_from
                jsr w_over
                jsr w_minus
                jsr w_spaces
                jsr w_type

z_dot_r:        rts



; ## DROP ( u -- ) "Pop top entry on Data Stack"
; ## "drop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DROP"""
xt_drop:
                jsr underflow_1
w_drop:
                inx
                inx

z_drop:         rts



; ## DUP ( u -- u u ) "Duplicate TOS"
; ## "dup"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DUP"""
xt_dup:
                jsr underflow_1
w_dup:
                dex
                dex

                lda 2,x         ; LSB
                sta 0,x
                lda 3,x         ; MSB
                sta 1,x

z_dup:          rts



; ## ELSE (C: orig -- orig' ) ( -- ) "Conditional flow control"
; ## "else"  auto  ANS core
        ; """http://forth-standard.org/standard/core/ELSE
        ;
        ; The code is shared with ENDOF and most of THEN
        ; """

xt_else:
xt_endof:
                jsr underflow_1
w_else:
w_endof:
                ; Add an unconditional branch with target filled in later
                jsr cmpl_jump_later

                ; stash the branch target for later
                ; and then calculate the forward branch from orig
                jsr w_swap              ; ( target orig )
                bra w_then              ; fall through to then

xt_then:
                jsr underflow_1
w_then:
                ; (C: orig -- ) ( -- )

                ; This is a compile-time word that writes the target address
                ; of an earlier forward branch.  For example xt_if writes
                ; zbranch <placeholder> which wants to skip forward to here
                ; if the condition is false.  The orig argument on the stack
                ; is the address of the placeholder which needs to point here.
                ;
                ; Note this is also used by several other words that write
                ; a forward branch, like xt_else and xt_while.

                ; Just stuff HERE in for the branch address back
                ; at the IF or ELSE (origination address is on stack).
                jsr w_here
                jsr w_swap
                jsr w_store

z_else:
z_endof:
z_then:         rts



; ## EMIT ( char -- ) "Print character to current output"
; ## "emit"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EMIT
        ; Run-time default for EMIT. The user can revector this by changing
        ; the value of the OUTPUT variable. We ignore the MSB completely, and
        ; do not check to see if we have been given a valid ASCII character.
        ; Don't make this native compile.
        ; """

xt_emit:
                jsr underflow_1
w_emit:
                lda 0,x
                inx
                inx

emit_a:
        ; We frequently want to print the character in A without fooling
        ; around with the Data Stack. This is emit_a's job, which still
        ; allows the output to be vectored. Call it with JSR as you
        ; would XT_EMIT
                jmp (output)            ; JSR/RTS

z_emit:         ; never reached



; ## ENDCASE (C: case-sys -- ) ( x -- ) "Conditional flow control"
; ## "endcase"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/ENDCASE"""

xt_endcase:
                jsr underflow_1
w_endcase:
                ; Postpone DROP to remove the item
                ; being checked.
                ldy #>w_drop
                lda #<w_drop
                jsr cmpl_subroutine

                ; There are a number of address (of branches that need their
                ; jump addressed filled in with the address of right here).
                ; Keep calling THEN to deal with them until we reach the
                ; 0 that CASE put on the stack at the beginning.
_endcase_loop:
                ; Check for 0 on the stack.
                lda 0,x
                ora 1,x
                beq _done

                jsr w_then
                bra _endcase_loop
_done:
                ; Remove the 0 from the stack.
                inx
                inx
z_endcase:      rts



; ## ENDOF (C: case-sys1 of-sys1-- case-sys2) ( -- ) "Conditional flow control"
; ## "endof"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/ENDOF
        ; This is a dummy entry, the code is shared with ELSE
        ; """



.if "environment?" in TALI_OPTIONAL_WORDS
; ## ENVIRONMENT_Q  ( addr u -- 0 | i*x true )  "Return system information"
; ## "environment?"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ENVIRONMENTq
        ;
        ; By ANS definition, we use upper-case strings here, see the
        ; string file for details. This can be realized as a high-level
        ; Forth word as
        ;
        ; : STRING_OF POSTPONE 2OVER POSTPONE COMPARE POSTPONE 0=
        ;    POSTPONE IF POSTPONE 2DROP ; IMMEDIATE COMPILE-ONLY
        ; HEX
        ; : ENVIRONMENT? ( C-ADDR U -- FALSE | I*X TRUE )
        ; CASE
        ; S" /COUNTED-STRING"    STRING_OF    FF TRUE ENDOF
        ; S" /HOLD"              STRING_OF    FF TRUE ENDOF
        ; S" /PAD"               STRING_OF    54 TRUE ENDOF ( 84 DECIMAL )
        ; S" ADDRESS-UNIT-BITS"  STRING_OF     8 TRUE ENDOF
        ; S" FLOORED"            STRING_OF FALSE TRUE ENDOF ( WE HAVE SYMMETRIC )
        ; S" MAX-CHAR"           STRING_OF   255 TRUE ENDOF
        ; S" MAX-D"              STRING_OF
                                     ; 7FFFFFFF. TRUE ENDOF
        ; S" MAX-N"              STRING_OF  7FFF TRUE ENDOF
        ; S" MAX-U"              STRING_OF  FFFF TRUE ENDOF
        ; S" MAX-UD"             STRING_OF
                                     ; FFFFFFFF. TRUE ENDOF
        ; S" RETURN-STACK-CELLS" STRING_OF    80 TRUE ENDOF
        ; S" STACK-CELLS"        STRING_OF    20 TRUE ENDOF ( FROM DEFINITIONS.ASM )
        ; ( DEFAULT ) 2DROP FALSE FALSE ( ONE FALSE WILL DROPPED BY ENDCASE )
        ; ENDCASE ;
        ;
        ; but that uses lots of memory and increases the start up time. This
        ; word is rarely used so we can try to keep it short at the expense
        ; of speed.
        ; """
xt_environment_q:
                jsr underflow_1
w_environment_q:
                ; This code is table-driven: We walk through the list of
                ; strings until we find one that matches, and then we take
                ; the equivalent data from the results table. This is made
                ; a bit harder by the fact that some of these return a
                ; double-cell number and some a single-cell one.

                ; We will walk through the table with variables that return
                ; a single-cell result
                ldy #00                 ; counter for table

                ; We use a flag on the the stack to signal if we have a single-cell
                ; or double-cell number. We use 0 to signal single-cell and 1 for
                ; double-cell.
                phy
_table_loop:
                ; We arrived here with the address of the string to be checked
                ; on the stack. We make a copy. Index is in Y
                jsr w_two_dup          ; ( addr u addr u ) 2DUP does not use Y

                ; We do our work on the TOS to speed things up
                dex
                dex                     ; ( addr u addr u ? )

                ; Get address of string to check from table
                lda env_table_single,y
                sta 0,x
                iny
                lda env_table_single,y
                sta 1,x                 ; ( addr u addr u addr-t )
                iny

                ; See if this is the last entry. The LSB is still in A
                ora 0,x
                beq _table_done

                ; We have a string entry. The address there is stored in
                ; old-style address format, that is, the first byte is the
                ; length of the string
                phy                     ; save Y, which is used by COUNT
                jsr w_count            ; ( addr u addr u addr-s u-s )
                jsr w_compare          ; ( addr u f )
                ply

                ; If we found a match (flag is zero -- COMPARE is weird
                ; that way), return the result
                lda 0,x
                ora 1,x
                beq _got_result

                ; Flag is not zero, so not a perfect match, so try next
                ; word
                inx                     ; DROP, now ( addr u )
                inx

                bra _table_loop

_got_result:
                ; We arrive here with ( addr u -1 ) and know that we've found
                ; a match. The index of the match+2 is in Y.
                inx                     ; drop flag, now ( addr u )
                inx
                dey                     ; go back to index we had
                dey

                ; See if this is a single-cell word.
                pla
                bne _double_result

                ; Single-cell result
                lda env_results_single,y
                sta 2,x
                iny
                lda env_results_single,y
                sta 3,x                 ; ( res u )

                bra _set_flag

_double_result:
                ; This is a double-celled result, which means we have to
                ; fool around with the index some more. We also need a
                ; further cell on the stack
                dex                     ; ( addr u ? )
                dex

                ; We have 11 single-cell words we check, plus the 0000 as
                ; a marker for the end of the table, so we arrive here
                ; with Y as 22 or more. To get the index for the double-
                ; cell words, we move the result
                tya
                sec
                sbc #24

                ; We have four bytes per entry in the table, but the index
                ; keeps increasing by two, so we only have to multiply by
                ; two (shift left once) to get the right result
                asl
                tay

                lda env_results_double,y
                sta 2,x
                iny
                lda env_results_double,y
                sta 3,x                 ; ( res u ? )
                iny

                lda env_results_double,y
                sta 4,x
                iny
                lda env_results_double,y
                sta 5,x                 ; ( res res ? )

                ; fall through to _set_flag
_set_flag:
                lda #$FF
                sta 0,x
                sta 1,x                 ; ( res f )

                bra _done
_table_done:
                ; We're done with a table, because the entry was a zero.
                ; We arrive here with ( addr u addr u 0 )

                ; We take the flag from stack and increase it by one. If the
                ; flag is zero, we have just completed the single-cell number
                ; strings, so we in increase the flag and try again. Otherwise,
                ; we're done with the double-cell table without having found
                ; a match, and we're done
                pla
                bne _no_match

                ; Flag is zero, increase it to one and start over to check
                ; double-cell values
                ina
                pha

                txa
                clc
                adc #6                  ; skip six bytes
                tax                     ; ( addr u )

                bra _table_loop
_no_match:
                ; Bummer, not found. We arrive here with
                ; ( addr u addr u 0 ) and need to return just a zero
                txa
                clc
                adc #10
                tax                     ; ( addr ) - not ( 0 ) !

                jsr w_false
_done:
z_environment_q:
                rts


; Tables for ENVIRONMENT?. We use two separate ones, one for the single-cell
; results and one for the double-celled results. The zero cell at the
; end of each table marks its, uh, end. The strings themselves are defined
; in strings.asm. Note if we add more entries to the single-cell table, we
; have to adapt the result code for double printout, where we subtract 22
; (two bytes each single-cell string and two bytes for the end-of-table
; marker 0000
env_table_single:
        .word envs_cs, envs_hold, envs_pad, envs_aub, envs_floored
        .word envs_max_char, envs_max_n, envs_max_u, envs_rsc
        .word envs_sc, envs_wl, 0000

env_table_double:
        .word envs_max_d, envs_max_ud, 0000

env_results_single:
        .word $00FF     ; /COUNTED-STRING
        .word $00FF     ; /HOLD
        .word $0054     ; /PAD (this is 84 decimal)
        .word $0008     ; ADDRESS-UNIT-BITS (keep "$" to avoid octal!)
        .word 0000      ; FLOORED ("FALSE", we have symmetric)
        .word $00FF     ; MAX-CHAR
        .word $7FFF     ; MAX-N
        .word $FFFF     ; MAX-U
        .word $0080     ; RETURN-STACK-CELLS
        .word $0020     ; STACK-CELLS (from definitions.asm)
        .word $0009     ; WORDLISTS

env_results_double:
        .word $7FFF, $FFFF      ; MAX-D
        .word $FFFF, $FFFF      ; MAX-UD
.endif


; ## EQUAL ( n n -- f ) "See if TOS and NOS are equal"
; ## "="  auto  ANS core
        ; """https://forth-standard.org/standard/core/Equal"""

xt_equal:
                jsr underflow_2
w_equal:
                lda 0,x                 ; LSB
                cmp 2,x
                bne _false

                lda 1,x                 ; MSB
                cmp 3,x
                bne _false

                lda #$FF
                bra _done

_false:         lda #0                  ; drop thru to done

_done:          sta 2,x
                sta 3,x

                inx
                inx

z_equal:        rts



; ## BLANK ( addr u -- ) "Fill memory region with spaces"
; ## "blank"  auto  ANS string
        ; """https://forth-standard.org/standard/string/BLANK"""
xt_blank:
                jsr underflow_2
w_blank:
                dex
                dex
                lda #AscSP
                sta 0,x
                stz 1,x
                bra w_fill     ; skip over code for ERASE


; ## ERASE ( addr u -- ) "Fill memory region with zeros"
; ## "erase"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/ERASE
        ; Note that ERASE works with "address" units
        ; (bytes), not cells.
        ; """

xt_erase:
                jsr underflow_2
w_erase:
                dex
                dex
                stz 0,x
                stz 1,x

                ; fall through to FILL
                bra w_fill

; ## FILL ( addr u char -- ) "Fill a memory region with a character"
; ## "fill"  auto  ANS core
        ; """https://forth-standard.org/standard/core/FILL
        ; Fill u bytes of memory with char starting at addr. Note that
        ; this works on bytes, not on cells. On an 8-bit machine such as the
        ; 65c02, this is a serious pain in the rear. It is not defined what
        ; happens when we reach the end of the address space
        ; """
xt_fill:
                jsr underflow_3
w_fill:
                ; We use tmp1 to hold the address
                lda 4,x         ; LSB
                sta tmp1
                lda 5,x
                sta tmp1+1

                ; We use tmp2 to hold the counter
                lda 2,x
                sta tmp2
                lda 3,x
                sta tmp2+1

                ; We use Y to hold the character
                lda 0,x
                tay
_loop:
                ; Unfortunately, we also need to make sure that we don't
                ; write further than the end of the RAM. So RAM_END must
                ; be larger or equal to the current address
                lda #>ram_end           ; MSB
                cmp tmp1+1
                bcc _done               ; RAM_END < TMP1, so leave
                bne _check_counter      ; RAM_END is not smaller and not equal

                lda #<ram_end           ; LSB, because MSBs were equal
                cmp tmp1
                bcc _done               ; RAM_END < TMP1, so leave

_check_counter:
                ; See if our counter has reached zero
                lda tmp2
                ora tmp2+1
                beq _done

                ; We're not in ROM and we still have stuff on the counter, so
                ; let's actually do what we came here to do
                tya
                sta (tmp1)

                ; Adjust the counter
                lda tmp2
                bne +
                dec tmp2+1
+               dec tmp2

                ; Next address
                inc tmp1
                bne _loop
                inc tmp1+1

                bra _loop

_done:
                ; Drop three cells off the Data Stack. This uses one byte
                ; less than six times INX
                txa
                clc
                adc #6
                tax
z_blank:
z_erase:
z_fill:         rts



; ## EXECUTE ( xt -- ) "Jump to word based on execution token"
; ## "execute"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EXECUTE"""
xt_execute:
                jsr underflow_1
w_execute:
                jsr doexecute   ; do not combine to JMP (native coding)

z_execute:      rts

doexecute:
                lda 0,x
                sta ip
                lda 1,x
                sta ip+1

                inx
                inx

                ; we don't need a RTS here because we highjack the RTS of
                ; the word we're calling to get back to xt_execute
                jmp (ip)

; end of doexecute



; ## EXIT ( -- ) "Return control to the calling word immediately"
; ## "exit"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EXIT
        ; If we're in a loop, user should UNLOOP first to clean up
        ; any loop control. This should be natively compiled.
        ; """

xt_exit:
w_exit:
                rts             ; keep before z_exit
z_exit:                         ; never reached


; ## FALSE ( -- f ) "Push flag FALSE to Data Stack"
; ## "false"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/FALSE
        ;
        ; This is a dummy header, FALSE shares the actual code with ZERO.
        ; """


; ## FETCH ( addr -- n ) "Push cell content from memory to stack"
; ## "@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Fetch"""
xt_fetch:
                jsr underflow_1
w_fetch:
                lda (0,x)               ; LSB
                tay
                inc 0,x
                bne +
                inc 1,x
+
                lda (0,x)               ; MSB
                sta 1,x
                sty 0,x

z_fetch:        rts


; ## FIND ( caddr -- addr 0 | xt 1 | xt -1 ) "Find word in Dictionary"
; ## "find"  auto  ANS core
        ; """https://forth-standard.org/standard/core/FIND
        ; Included for backwards compatibility only, because it still
        ; can be found in so may examples. It should, however, be replaced
        ; by FIND-NAME. Counted string either returns address with a FALSE
        ; flag if not found in the Dictionary, or the xt with a flag to
        ; indicate if this is immediate or not. FIND is a wrapper around
        ; FIND-NAME, we get this all over with as quickly as possible. See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Lists.html
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; """

xt_find:
                jsr underflow_1
w_find:
                ; Save address in case conversion fails. We use the
                ; Return Stack instead of temporary variables like TMP1
                ; because this is shorter and anybody still using FIND
                ; can't be worried about speed anyway
                lda 1,x                 ; MSB
                pha
                lda 0,x                 ; LSB
                pha

                ; Convert ancient-type counted string address to
                ; modern format
                jsr w_count            ; ( caddr -- addr u )
                jsr w_find_name        ; ( addr u -- nt | 0 )

                lda 0,x
                ora 1,x
                bne _found_word

                ; No word found. Return address of the string and a false
                ; flag
                jsr w_false            ; ( 0 0 )

                ; The address needs to be restored.
                pla                     ; LSB of address
                sta 2,x
                pla
                sta 3,x                 ; MSB of address

                bra _done               ; ( addr 0 )

_found_word:
                ; We don't need the address after all, dump it
                pla
                pla

                ; We arrive here with ( nt ) on the TOS. Now we have to
                ; convert the return values to FIND's format
                jsr w_dup              ; ( nt nt )
                jsr w_name_to_int      ; ( nt xt )
                jsr w_swap             ; ( xt nt )

                ldy #0                  ; Prepare flag

                ; The flags are in the second byte of the header
                inc 0,x
                bne +
                inc 1,x                 ; ( xt nt+1 )
+
                lda (0,x)               ; ( xt char )
                and #IM
                bne _immediate          ; bit set, we're immediate

                lda #$FF                ; We're not immediate, return -1
                sta 0,x
                sta 1,x
                bra _done

_immediate:
                lda #1                  ; We're immediate, return 1
                sta 0,x
                stz 1,x
_done:
z_find:         rts



; ## FM_SLASH_MOD ( d n1  -- rem n2 ) "Floored signed division"
; ## "fm/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/FMDivMOD
        ; Note that by default, Tali Forth uses SM/REM for most things.
        ;
        ; There are various ways to realize this. We follow EForth with
        ;    DUP 0< DUP >R  IF NEGATE >R DNEGATE R> THEN >R DUP
        ;    0<  IF R@ + THEN  R> UM/MOD R> IF SWAP NEGATE SWAP THEN
        ; See (http://www.forth.org/eforth.html). However you can also
        ; go FM/MOD via SM/REM (http://www.figuk.plus.com/build/arith.htm):
        ;     DUP >R  SM/REM DUP 0< IF SWAP R> + SWAP 1+ ELSE  R> DROP THEN
        ; """

xt_fm_slash_mod:
                jsr underflow_3
w_fm_slash_mod:
                ; if sign of n1 is negative, negate both n1 and d
                stz tmp2        ; default: n is positive
                lda 1,x         ; MSB of n1
                bpl _check_d

                inc tmp2        ; set flag to negative for n1
                jsr w_negate   ; NEGATE
                jsr w_to_r     ; >R
                jsr w_dnegate  ; DNEGATE
                jsr w_r_from   ; R>

_check_d:
                ; If d is negative, add n1 to high cell of d
                lda 3,x         ; MSB of high word of d
                bpl _multiply

                clc
                lda 0,x         ; LSB of n1
                adc 2,x         ; LSB of dh
                sta 2,x

                lda 1,x         ; MSB of n1
                adc 3,x         ; MSB of dh
                sta 3,x

_multiply:
                jsr w_um_slash_mod     ; ( d n1 -- rem n2 )

                ; if n was negative, negate the result
                lda tmp2
                beq _done

                inx             ; pretend that we SWAP
                inx
                jsr w_negate
                dex
                dex
_done:
z_fm_slash_mod: rts



; This is a special jsr target to skip the zeroing of BLK at the beginning
; of evaluate.  It's used by LOAD to allow setting BLK while the block is
; being evaluated.  Evaluate's normal behavior is to zero BLK.
load_evaluate:
                ; Set a flag (using tmp1) to not zero BLK
                lda #$FF
                sta tmp1
                bra load_evaluate_start

; ## EVALUATE ( addr u -- ) "Execute a string"
; ## "evaluate"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EVALUATE
        ; Set SOURCE-ID to -1, make addr u the input source, set >IN to zero.
        ; After processing the line, revert to old input source. We use this
        ; to compile high-level Forth words and user-defined words during
        ; start up and cold boot. In contrast to ACCEPT, we need to, uh,
        ; accept more than 255 characters here, even though it's a pain in
        ; the 8-bit.
        ; """

xt_evaluate:
                jsr underflow_2
w_evaluate:
                ; Clear the flag to zero BLK.  Only LOAD will set the flag,
                ; and will set the block number.
                stz tmp1

                ; If u is zero (which can happen a lot for the user-defined
                ; words), just leave again
                lda 0,x
                ora 1,x
                bne evaluate_got_work

                inx
                inx
                inx
                inx

                bra evaluate_done

; Special entry point for LOAD to bypass the zeroing of BLK.
load_evaluate_start:
evaluate_got_work:
                ; Save the current value of BLK on the return stack.
                ldy #blk_offset+1
                lda (up),y
                pha
                dey
                lda (up),y
                pha

                ; See if we should zero BLK.
                lda tmp1
                bne _nozero

                ; Set BLK to zero.
                ; lda #0        ; A is already zero from loading tmp1
                sta (up),y
                iny
                sta (up),y

_nozero:
                ; Save the input state to the Return Stack
                jsr w_input_to_r

                ; set SOURCE-ID to -1
                lda #$FF
                sta insrc
                sta insrc+1

                ; set >IN to zero
                stz toin
                stz toin+1

                ; move TOS and NOS to input buffers
                lda 0,x
                sta ciblen
                lda 1,x
                sta ciblen+1

                lda 2,x
                sta cib
                lda 3,x
                sta cib+1

                inx             ; A clean stack is a clean mind
                inx
                inx
                inx

                jsr interpret   ; ( -- )

                ; restore variables
                jsr w_r_to_input

                ; Restore BLK from the return stack.
                ldy #blk_offset
                pla
                sta (up),y
                iny
                pla
                sta (up),y

evaluate_done:
z_evaluate:     rts



; ## GREATER_THAN ( n n -- f ) "See if NOS is greater than TOS"
; ## ">"  auto  ANS core
        ; """https://forth-standard.org/standard/core/more"""

xt_greater_than:
                jsr underflow_2
w_greater_than:
                ldy #0          ; default false
                jsr compare_16bit

                ; for signed numbers, NOS>TOS gives us Z=0 and N=1
                beq _false
                bpl _false

                ; true
                dey
_false:
                tya

                inx
                inx
                sta 0,x
                sta 1,x

z_greater_than: rts



; ## HERE ( -- addr ) "Put Compiler Pointer on Data Stack"
; ## "here"  auto  ANS core
        ; """https://forth-standard.org/standard/core/HERE
        ; This code is also used by the assembler directive ARROW
        ; ("->") though as immediate
        ; and by HERE as an immediate compile word"""
xt_here:
xt_begin:
xt_asm_arrow:
w_here:
w_begin:
w_asm_arrow:
                dex
                dex
                lda cp
                sta 0,x
                lda cp+1
                sta 1,x

z_here:
z_begin:
z_asm_arrow:
                rts



; ## HEX ( -- ) "Change base radix to hexadecimal"
; ## "hex"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/HEX"""
xt_hex:
w_hex:
                lda #16
                sta base
                stz base+1              ; paranoid

z_hex:          rts



; ## HOLD ( char -- ) "Insert character at current output"
; ## "hold"  auto  ANS core
        ; """https://forth-standard.org/standard/core/HOLD
        ; Insert a character at the current position of a pictured numeric
        ; output string on
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ;
        ; Forth code is : HOLD  -1 HLD +!  HLD @ C! ;  We use the the internal
        ; variable tohold instead of HLD.
        ; """
xt_hold:
                jsr underflow_1
w_hold:
                lda tohold
                bne +
                dec tohold+1
+
                dec tohold

                lda 0,x
                sta (tohold)
                inx
                inx

z_hold:         rts



; ## I ( -- n )  "Copy loop counter to stack"
; ## "i"  auto  ANS core
        ; """https://forth-standard.org/standard/core/I
        ; See definitions.asm and the Control Flow section of the manual.
        ;
        ; This word can be native compiled or not since
        ; it no longer depends on the return stack.
        ; """

xt_i:
w_i:
                dex
                dex

                ; The fudged index and offset are stored in the current
                ; loop control block, with loopidx0 cached in zp

                ldy loopctrl
                sec
                lda loopidx0        ; cached LSB of loopindex
                sbc loopfufa,y
                sta 0,x
                lda loopindex+1,y
                sbc loopfufa+1,y
                sta 1,x

z_i:            rts



; ## IF (C: -- orig) (flag -- ) "Conditional flow control"
; ## "if"  auto  ANS core
        ; """http://forth-standard.org/standard/core/IF"""

xt_if:
w_if:
                jsr cmpl_0branch_later
z_if:           rts



; ## IMMEDIATE ( -- ) "Mark most recent word as IMMEDIATE"
; ## "immediate"  auto  ANS core
        ; """https://forth-standard.org/standard/core/IMMEDIATE
        ; Make sure the most recently defined word is immediate. Will only
        ; affect the last word in the dictionary. Note that if the word is
        ; defined in ROM, this will have no affect, but will not produce an
        ; error message.
        ; """
xt_immediate:
w_immediate:
                jsr current_to_dp
                ldy #1          ; offset for status byte
                lda (dp),y
                ora #IM        ; make sure bit 7 is set
                sta (dp),y

z_immediate:    rts



; ## INVERT ( n -- n ) "Complement of TOS"
; ## "invert"  auto  ANS core
        ; """https://forth-standard.org/standard/core/INVERT"""
xt_invert:
                jsr underflow_1
w_invert:
                lda #$FF
                eor 0,x         ; LSB
                sta 0,x

                lda #$FF
                eor 1,x         ; MSB
                sta 1,x

z_invert:       rts



; ## IS ( xt "name" -- ) or (C: "name" ) "Set named word to execute xt"
; ## "is"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/IS"""

xt_is:
w_is:
                ; This is a state aware word with differet behavior
                ; when used while compiling vs interpreting.
                ; Check STATE
                lda state
                ora state+1
                beq _interpreting

                ; Run ['] to compile the xt of the next word as a literal.
                jsr w_bracket_tick

                ; Postpone DEFER! by compiling a JSR to it.
                ldy #>w_defer_store
                lda #<w_defer_store
                jsr cmpl_subroutine

                bra _done

_interpreting:
                jsr w_tick
                jsr w_defer_store
_done:
z_is:           rts



; ## J ( -- n ) "Copy second loop counter to stack"
; ## "j"  auto  ANS core
        ; """https://forth-standard.org/standard/core/J
        ; Copy second loop counter from Return Stack to stack. Note we use
        ; a fudge factor for loop control; see the Control Flow section of
        ; the manual for more details.
        ;
        ; This can be native compiled or not since it no longer uses the stack
        ; """

xt_j:
w_j:
                dex                 ; make space on the stack
                dex

                ; subtract four to get the enclosing LCB offset
                lda loopctrl
                sec
                sbc #4
                tay
                sec
                lda loopindex,y
                sbc loopfufa,y
                sta 0,x
                lda loopindex+1,y
                sbc loopfufa+1,y
                sta 1,x
z_j:            rts



; ## KEY ( -- char ) "Get one character from the input"
; ## "key"  tested  ANS core
xt_key:
w_key:
        ; """https://forth-standard.org/standard/core/KEY
        ; Get a single character of input from the vectored
        ; input without echoing.
        ; """
                jsr key_a               ; returns char in A

                dex
                dex
                sta 0,x
                stz 1,x

z_key:          rts

key_a:
        ; The 65c02 doesn't have a JSR (ADDR,X) instruction like the
        ; 65816, so we have to fake the indirect jump to vector it.
        ; This is depressingly slow. We use this routine internally
        ; to avoid manipulating the Data Stack when we just want a
        ; character
                jmp (input)             ; JSR/RTS


; ## KEY? ( -- char ) "Return true if a character is available"
; ## "key?"  tested  ANS core
xt_keyq:
w_keyq:
        ; """https://forth-standard.org/standard/core/KEYq
        ; Check if a key is available from the vectored havekey.
        ; Use KEY to fetch it.
        ; """
                ldy #0
                jsr keyq_a
                beq +           ; A=0 => Y=0
                dey             ; A<>0 => Y=#$ff
+
                dex
                dex
                sty 0,x         ; store either $0000 or $ffff
                sty 1,x

z_keyq:         rts

keyq_a:         jmp (havekey)


; ## LEAVE ( -- ) "Leave DO/LOOP construct"
; ## "leave"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LEAVE
        ; Note that this does not work with anything but a DO/LOOP in
        ; contrast to other versions such as discussed at
        ; http://blogs.msdn.com/b/ashleyf/archive/2011/02/06/loopty-do-i-loop.aspx
        ;
        ;       : LEAVE POSTPONE BRANCH HERE SWAP 0 , ; IMMEDIATE COMPILE-ONLY
        ; See definitions.asm and the Control Flow section in the manual
        ; for details of how this works.
        ; This must be native compile and not IMMEDIATE
        ; """

xt_leave:
w_leave:
                ; LEAVE will eventually jump forward to the unloop.
                ; We don't know where that is at compile time
                ; so we'll write a JMP to be patched later.
                ; Since LEAVE is allowed multiple times we'll
                ; use the JMP placeholder address to keep a linked list
                ; of all LEAVE addresses to update, headed by loopleave

                lda loopleave
                ldy loopleave+1
                jsr cmpl_jump   ; emit the JMP chaining prior leave address

                ; set head of the list to point to our placeholder
                sec
                lda cp
                sbc #2
                sta loopleave
                lda cp+1
                bcs +
                dea
+               sta loopleave+1

z_leave:
                rts



; ## LEFT_BRACKET ( -- ) "Enter interpretation state"
; ## "["  auto  ANS core
        ; """https://forth-standard.org/standard/core/Bracket
        ; This is an immediate and compile-only word
        ; """
xt_left_bracket:
w_left_bracket:
                stz state
                stz state+1

z_left_bracket: rts



; ## LESS_NUMBER_SIGN ( -- ) "Start number conversion"
; ## "<#"  auto  ANS core
        ; """https://forth-standard.org/standard/core/num-start
        ; Start the process to create pictured numeric output.
        ;
        ; The new
        ; string is constructed from back to front, saving the new character
        ; at the beginning of the output string. Since we use PAD as a
        ; starting address and work backward (!), the string is constructed
        ; in the space between the end of the Dictionary (as defined by CP)
        ; and the PAD. This allows us to satisfy the ANS Forth condition that
        ; programs don't fool around with the PAD but still use its address.
        ; Based on pForth
        ; http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
        ; pForth is in the pubic domain. Forth is : <# PAD HLD ! ; we use the
        ; internal variable tohold instead of HLD.
        ; """
xt_less_number_sign:
w_less_number_sign:
                jsr w_pad      ; ( addr )

                lda 0,x
                sta tohold
                lda 1,x
                sta tohold+1

                inx
                inx

z_less_number_sign:
                rts



; ## LESS_THAN ( n m -- f ) "Return true if NOS < TOS"
; ## "<"  auto  ANS core
        ; """https://forth-standard.org/standard/core/less"""

xt_less_than:
                jsr underflow_2
w_less_than:
                ldy #0          ; default false
                jsr compare_16bit

                ; for signed numbers, NOS < TOS if Z=0 and N=0
                beq _false
                bmi _false

                ; true
                dey
_false:
                tya

                inx
                inx
                sta 0,x
                sta 1,x

z_less_than:    rts



; ## LITERAL ( n -- ) "Store TOS to be push on stack during runtime"
; ## "literal"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LITERAL
        ; Compile-only word to store TOS so that it is pushed on stack
        ; during runtime. This is a immediate, compile-only word. At runtime,
        ; it works by calling literal_runtime by compling JSR LITERAL_RT.
        ;
        ; Note the cmpl_ routines use TMPTOS
        ; """
xt_literal:
                jsr underflow_1
w_literal:
                lda #template_push_tos_size
                jsr check_nc_limit
                bcc _inline

                ldy #>literal_runtime
                lda #<literal_runtime
                jsr cmpl_subroutine

                ; Compile the value that is to be pushed on the Stack during
                ; runtime
                jsr w_comma
                bra z_literal

_inline:
                ; we'll need the MSB (if non-zero) and LSB to fill in the template
                ; which we set up on the stack in reverse order
                ; first we need the STZ/STY opcode for the end of the template
                ldy #$94        ; STY opcode
                lda 1,x         ; MSB
                bne +
                ldy #$74        ; STZ opcode
+               phy

                lda 0,x         ; LSB
                pha

                ; if MSB is non-zero, stack it, otherwise skip first two bytes of template
                ldy #2
                lda 1,x         ; MSB
                beq _copy
                ldy #0
                pha

_copy:          lda template_push_tos,y
                cmp #$ff        ; is it a placeholder?
                bne +
                pla
+               jsr cmpl_a
                iny
                cpy #template_push_tos_size
                bne _copy

                inx             ; drop the literal
                inx

z_literal:      rts

template_push_tos:
                ldy #$ff        ; we'll omit this if MSB is zero
                lda #$ff
                dex
                dex
                sta 0,x
                .byte $ff, 1    ; this will become either sty 1,x or stz 1,x
template_push_tos_size = * - template_push_tos


literal_runtime:
                ; During runtime, we push the value following this word back
                ; on the Data Stack. The subroutine jump that brought us
                ; here put the address to return to on the Return Stack -
                ; this points to the data we need to get. This routine is
                ; also called (LITERAL) in some Forths
                dex
                dex

            	; The 65c02 stores <RETURN-ADDRESS>-1 on the Return Stack,
                ; so we are actually popping the address-1 of the literal
                pla             ; LSB
                sta tmp1
                pla             ; MSB
                sta tmp1+1

                ; Fetch the actual literal value and push it on Data stack
                ldy #1
                lda (tmp1),y    ; LSB
                sta 0,x
                iny
                lda (tmp1),y    ; MSB
                sta 1,x

                ; Adjust return address and push back on the Return Stack
                tya
                clc
                adc tmp1
                tay
                lda tmp1+1
                adc #0
                pha
                phy

                rts


; ## LOOP ( -- ) "Finish loop construct"
; ## "loop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LOOP
        ; Compile-time part of LOOP. This is specialized to
        ; increment by one.
        ;
        ; In Forth, this is
        ;       : LOOP  POSTPONE 1 POSTPONE (+LOOP) , POSTPONE UNLOOP ;
        ;       IMMEDIATE ; COMPILE-ONLY
        ; """
xt_loop:
w_loop:
                ; Compile LOOP-specific runtime
                dex
                dex
                dex
                dex
                lda #<loop_runtime
                sta 2,x
                lda #>loop_runtime
                sta 3,x
                lda #loop_runtime_size
                sta 0,x
                stz 1,x

                ; Now compile the runtime shared with +LOOP
                bra loop_common



; ## PLUS_LOOP ( -- ) "Finish loop construct"
; ## "+loop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/PlusLOOP
        ;
        ; Compile-time part of +LOOP, also used for LOOP. Is usually
        ;       : +LOOP POSTPONE (+LOOP) , POSTPONE UNLOOP ; IMMEDIATE
        ;       COMPILE-ONLY
        ; in Forth. LOOP uses this routine as well. We jump here with the
        ; address for looping as TOS and the address for aborting the loop
        ; (LEAVE) as the second double-byte entry on the Return Stack (see
        ; DO and the Control Flow section of the manual for details).
        ; """

xt_plus_loop:
w_plus_loop:
                ; Compile +LOOP-specific runtime
                dex
                dex
                dex
                dex
                lda #<plus_loop_runtime
                sta 2,x
                lda #>plus_loop_runtime
                sta 3,x
                lda #plus_loop_runtime_size
                sta 0,x
                stz 1,x

                ; fall through to shared runtime

loop_common:
                jsr w_over
                jsr w_swap             ; xt and xt' are the same
                ; ( xt xt u )
                jsr cmpl_by_limit

                ; The address we need to loop back to is TOS
                ; ( qdo-skip old-loopleave repeat-addr )

                bcc _native

                ; if non-native, just write repeat-addr as payload after the call
                jsr w_comma
                bra +

_native:
                ; if native, write the JMP repeat-addr after either loop runtime
                jsr cmpl_jump_tos
+

                ; any LEAVE words want to jmp to the unloop we'll write here
                ; so follow the linked list and update them
                lda loopleave+1         ; MSB=0 means we're done
                beq _noleave
_next:
                ; stash current LEAVE addr which links to
                ; the previous one (if any) and replace it with HERE
                ldy #1
                lda (loopleave),y
                pha
                lda cp+1
                sta (loopleave),y
                dey
                lda (loopleave),y
                pha
                lda cp
                sta (loopleave),y

                ; follow the chain backward
                pla
                sta loopleave
                pla
                sta loopleave+1
                bne _next
_noleave:
                ; restore loopleave in case we were nested
                lda 0,x
                sta loopleave
                lda 1,x
                sta loopleave+1

                ; reuse TOS

                ; Clean up the loop params by appending unloop
                lda #<w_unloop
                sta 0,x
                lda #>w_unloop
                sta 1,x
                jsr w_compile_comma

                ; Finally we're left with qdo-skip which either
                ; points at ?DO's "skip the loop" jmp address,
                ; wanting to skip past this whole mess to CP=HERE,
                ; or has MSB=0 from DO which we can just ignore
                lda 1,x                 ; MSB=0 means DO so nothing to do
                beq +
                jsr w_here
                jsr w_swap
                jmp w_store            ; write here as ?DO jmp target and return

+               inx                     ; drop the ignored word for DO
                inx
z_loop:
z_plus_loop:    rts


loop_runtime:
        ; """Runtime compile for LOOP when stepping by one.
        ; This must always be native compiled.
        ; """
                ; do_runtime has set up the loop control block with
                ; loopindex:    $8000-limit+index
                ; loopfufa:     $8000-limit

                ; so we need to increment loopindex and
                ; and look for overflow as explained in do_runtime

                inc loopidx0            ; increment the LSB of loopindex
                bne _repeat             ; avoid expensive test most of the time

                ; we might be done so need to inc and check the MSB

                ldy loopctrl
                ; for the +1 case we can increment MSB and test for #$80
                ; unlike the +LOOP case where we use V to flag crossing #$80
                lda loopindex+1,y
                ina
                cmp #$80
                beq _done
                sta loopindex+1,y
loop_runtime_size = * - loop_runtime
_repeat:
                ; for native compilation we stop here and LOOP/+LOOP will tack on a JMP <repeat-addr>
                ; for subroutine compile we set up A for zbranch_runtime
                lda #0
                .byte $2c               ; BIT llhh to hide the lda #1
_done:          lda #1
                jmp zbranch_runtime


plus_loop_runtime:
        ; """Runtime compile for +LOOP when we have an arbitrary step.
        ; See below for loop_runtime when step=1. Note we use a fudge factor for
        ; loop control so we can test with the Overflow Flag. See
        ; the Control Flow section of the manual for details.
        ; The step value is TOS in the loop.
        ; This must always be native compiled.
        ; In some Forths, this is a separate word called (+LOOP) or (LOOP)
        ; """

                clc
                lda 0,x                 ; LSB of step
                adc loopidx0
                sta loopidx0

                inx                     ; dump step from TOS before MSB test
                inx                     ; since we might skip it
                lda $FF,x               ; MSB of step since 1,x == -1,x+2
                bne _chkv               ; if it's non-zero we have to check
                bcc _repeat             ; but if 0 and no carry, we're good

_chkv:          clv
                ldy loopctrl            ; get LCB offset
                adc loopindex+1,y       ; MSB of index
                sta loopindex+1,y       ; put MSB of index back on stack

                ; If V flag is set, we're done looping and continue
                ; after the +LOOP instruction
                bvs _done               ; skip over JMP instruction
plus_loop_runtime_size = * - plus_loop_runtime
_repeat:
                ; for native compilation we stop here and LOOP/+LOOP will tack on a JMP <repeat-addr>
                ; for subroutine compile we set up A and continue with zbranch_runtime
                lda #0
                .byte $2c               ; BIT llhh to hide the lda #1
_done:          lda #1
                jmp zbranch_runtime



; ## LSHIFT ( x u -- u ) "Shift TOS left"
; ## "lshift"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LSHIFT"""

xt_lshift:
                jsr underflow_2
w_lshift:
                ; max shift 16 times
                lda 0,x
                and #%00001111
                beq _done

                tay

_loop:
                asl 2,x
                rol 3,x
                dey
                bne _loop

_done:
                inx
                inx

z_lshift:       rts



; ## M_STAR ( n n -- d ) "16 * 16 --> 32"
; ## "m*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MTimes
        ; Multiply two 16 bit numbers, producing a 32 bit result. All
        ; values are signed. Adapted from FIG Forth for Tali Forth.
        ;
        ; The original Forth is : M* OVER OVER XOR >R ABS SWAP ABS UM* R> D+- ;
        ; with  : D+- O< IF DNEGATE THEN ;
        ; """

xt_m_star:
                jsr underflow_2
w_m_star:
                ; figure out the sign
                lda 1,x         ; MSB of n1
                eor 3,x         ; MSB of n2

                ; UM* uses all kinds of temporary variables so we don't
                ; risk a conflict but just take the cycle hit and push
                ; this to the stack
                pha

                ; get the absolute value of both numbers so we can feed
                ; them to UM*, which does the real work
                jsr w_abs
                jsr w_swap
                jsr w_abs

                jsr w_um_star          ; ( d )

                ; handle the sign
                pla
                bpl _done

                jsr w_dnegate
_done:
z_m_star:       rts



; ## MARKER ( "name" -- ) "Create a deletion boundary"
; ## "marker"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/MARKER
        ; This word replaces FORGET in earlier Forths. Old entries are not
        ; actually deleted, but merely overwritten by restoring CP and DP.
        ; Run the named word at a later time to restore all of the wordlists
        ; to their state when the word was created with marker.  Any words
        ; created after the marker (including the marker) will be forgotten.
        ;
        ; To do this, we want to end up with a run-time component
        ; that reverts to the original CP, DP  and wordlist state:
        ;
        ;       jsr marker_runtime
        ;       <Original CP MSB>
        ;       <Original CP LSB>
        ;       <Original DP MSB> ( for CURRENT wordlist )
        ;       <Original DP LSB>
        ;       ; USER variables with wordlist state:
        ;       <CURRENT> (byte variable)
        ;       <All wordlists> (currently 12) (cell array)
        ;       <#ORDER> (byte variable)
        ;       <All search order> (currently 9) (byte array)
        ;
        ; This code uses tmp1 and tmp2
        ; """

xt_marker:
w_marker:
                ; Before we do anything, we need to save CP, which
                ; after all is the whole point of this operation. CREATE
                ; uses tmp1 and tmp2, so we take the speed hit and push stuff
                ; to the stack
                jsr current_to_dp

                lda dp
                pha
                lda dp+1
                pha

                lda cp
                pha
                lda cp+1
                pha

                jsr w_create

                ; By default, CREATE installs a subroutine jump to DOVAR, which
                ; we have to replace by a jump to marker_runtime. We back up
                ; two bytes and then overwrite the address
                lda cp          ; LSB
                sec
                sbc #2
                sta cp
                bcs +
                dec cp+1        ; we only care about the borrow
+
                ; Add the address of the runtime component
                ldy #>marker_runtime
                lda #<marker_runtime
                jsr cmpl_word

                ; Add original CP as payload
                ply                     ; MSB
                pla                     ; LSB
                jsr cmpl_word

                ; Add original DP as payload
                ply                     ; MSB
                pla                     ; LSB
                jsr cmpl_word

                ; Add the user variables for the wordlists and search order.
                ; We're compiling them in byte order.
                ldy #marker_start_offset
-
                lda (up),y
                jsr cmpl_a
                iny
                cpy #marker_end_offset
                bne -

z_marker:       rts


marker_runtime:
        ; """Restore Dictionary and memory (DP and CP) to where the were
        ; when this marker was defined. We arrive here with the return
        ; address on the Return Stack in the usual 65c02 format
        ; """

                ; Get the address of the string address off the stack and
                ; increase by one because of the RTS mechanics
                pla
                sta tmp1        ; LSB of address
                pla
                sta tmp1+1      ; MSB of address

                ldy #1          ; start at 1 due to RTS mechanics

                ; CP was stored first
                lda (tmp1),y
                sta cp
                iny
                lda (tmp1),y
                sta cp+1

                ; Next was DP
                iny
                lda (tmp1),y
                sta dp
                iny
                lda (tmp1),y
                sta dp+1

                ; We've consumed the first four bytes and now come the user vars.
                ; It's slightly tricky since we can only index indirectly with y
                ; so we'd like to copy from (tmp1),y to (up),y
                ; But currently tmp1 + 5 corresponds to up + marker_start_offset
                ; So we'll adjust tmp1 so that tmp1' + marker_start_offset == tmp1 + 5
                ; meaning that tmp1' = tmp1 - (marker_start_offset - 5).  Phew.
        .cerror marker_start_offset < 5, "MARKER assumes marker_start_offset >= 5"
                sec
                lda tmp1
                sbc #marker_start_offset - 5
                sta tmp1
                bcs +
                dec tmp1+1
+
                ; Restore previous wordlist state
                ldy #marker_start_offset
-
                ; Copy from the dictionary back on top of the wordlists
                ; and search order.
                lda (tmp1),y
                sta (up),y
                iny
                cpy #marker_end_offset
                bne -

                jsr dp_to_current       ; Move the CURRENT DP back.

                ; The return instruction takes us back to the original caller
                rts



; ## MAX ( n n -- n ) "Keep larger of two numbers"
; ## "max"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MAX
        ; Compare TOS and NOS and keep which one is larger. Adapted from
        ; Lance A. Leventhal "6502 Assembly Language Subroutines". Negative
        ; Flag indicates which number is larger. See also
        ; http://6502.org/tutorials/compare_instructions.html and
        ; http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        ; """

xt_max:
                jsr underflow_2
w_max:
                ; Compare LSB. We do this first to set the carry flag
                lda 0,x         ; LSB of TOS
                cmp 2,x         ; LSB of NOS, this sets the carry

                lda 1,x         ; MSB of TOS
                sbc 3,x         ; MSB of NOS
                bvc _no_overflow

                ; handle overflow, because we use signed numbers
                eor #$80        ; complement negative flag

_no_overflow:
                ; if negative, NOS is larger and needs to be kept
                bmi _keep_nos

                ; move TOS to NOS
                lda 0,x
                sta 2,x
                lda 1,x
                sta 3,x

_keep_nos:
                inx
                inx

z_max:          rts



; ## MIN ( n n -- n ) "Keep smaller of two numbers"
; ## "min"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MIN
        ; Adapted from Lance A. Leventhal "6502 Assembly Language
        ; Subroutines." Negative Flag indicateds which number is larger. See
        ; http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        ; """

xt_min:
                jsr underflow_2
w_min:
                ; compare LSB. We do this first to set the carry flag
                lda 0,x         ; LSB of TOS
                cmp 2,x         ; LSB of NOS, this sets carry

                lda 1,x         ; MSB of TOS
                sbc 3,x         ; MSB of NOS
                bvc _no_overflow

                ; handle overflow because we use signed numbers
                eor #$80

_no_overflow:
                ; if negative, NOS is larger and needs to be dumped
                bpl _keep_nos

                ; move TOS to NOS
                lda 0,x
                sta 2,x
                lda 1,x
                sta 3,x

_keep_nos:
                inx
                inx

z_min:          rts



; ## MINUS ( n n -- n ) "Subtract TOS from NOS"
; ## "-"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Minus"""
xt_minus:
                jsr underflow_2
w_minus:
                sec
                lda 2,x         ; LSB
                sbc 0,x
                sta 2,x

                lda 3,x         ; MSB
                sbc 1,x
                sta 3,x

                inx
                inx

z_minus:        rts



; ## MOD ( n1 n2 -- n ) "Divide NOS by TOS and return the remainder"
; ## "mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MOD
        ;
        ; The Forth definition of this word is  : MOD /MOD DROP ;
        ; so we just jump to xt_slash_mod and dump the actual result.
        ; """
xt_mod:
                jsr underflow_2
w_mod:
                jsr w_slash_mod

                inx             ; DROP
                inx
z_mod:
                rts



; ## MOVE ( addr1 addr2 u -- ) "Copy bytes"
; ## "move"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MOVE
        ; Copy u "address units" from addr1 to addr2. Since our address
        ; units are bytes, this is just a front-end for CMOVE and CMOVE>. This
        ; is actually the only one of these three words that is in the CORE
        ; set.
        ;
        ; This word must not be natively compiled.
        ; """

xt_move:
                jsr underflow_3
w_move:
                ; compare MSB first
                lda 3,x                 ; MSB of addr2
                cmp 5,x                 ; MSB of addr1
                beq _lsb                ; wasn't helpful, move to LSB
                bcs _to_move_up         ; we want CMOVE>

                jmp w_cmove            ; JSR/RTS

_lsb:
                ; MSB were equal, so do the whole thing over with LSB
                lda 2,x                 ; LSB of addr2
                cmp 4,x                 ; LSB of addr1
                beq _equal              ; LSB is equal as well
                bcs _to_move_up         ; we want CMOVE>

                jmp w_cmove            ; JSR/RTS

_to_move_up:
                jmp w_cmove_up         ; JSR/RTS
_equal:
                ; drop three entries from Data Stack
                txa
                clc
                adc #6
                tax

z_move:         rts



; ## NEGATE ( n -- n ) "Two's complement"
; ## "negate"  auto  ANS core
        ; """https://forth-standard.org/standard/core/NEGATE"""
xt_negate:
                jsr underflow_1
w_negate:
        	lda #0
                sec
                sbc 0,x         ; LSB
                sta 0,x

                lda #0
                sbc 1,x         ; MSB
                sta 1,x

z_negate:       rts



; ## NIP ( b a -- a ) "Delete NOS"
; ## "nip"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/NIP"""
xt_nip:
                jsr underflow_2
w_nip:
                lda 0,x         ; LSB
                sta 2,x
                lda 1,x         ; MSB
                sta 3,x

                inx
                inx

z_nip:          rts



; ## NOT_EQUALS ( n m -- f ) "Return a true flag if TOS != NOS"
; ## "<>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/ne
        ;
        ; This is just a variant of EQUAL, we code it separately
        ; for speed.
        ; """

xt_not_equals:
                jsr underflow_2
w_not_equals:
                ldy #0                  ; default is true

                lda 0,x                 ; LSB
                cmp 2,x
                bne _not_equal

                ; LSB is equal
                lda 1,x                 ; MSB
                cmp 3,x
                bne _not_equal

                lda #$FF
                bra _done

_not_equal:
                dey                     ; drop thru to done

_done:
                tya
                inx
                inx
                sta 0,x
                sta 1,x

z_not_equals:   rts



; ## NUMBER_SIGN ( ud -- ud ) "Add character to pictured output string"
; ## "#"  auto  ANS core
        ; """https://forth-standard.org/standard/core/num
        ; Add one char to the beginning of the pictured output string.
        ;
        ; Based on
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; Forth code  BASE @ UD/MOD ROT 9 OVER < IF 7 + THEN [CHAR] 0 + HOLD ;
        ; """
xt_number_sign:
                jsr underflow_2         ; double number
w_number_sign:
                ; The following is based on the ancient Forth word UD/MOD, which in
                ; various Forths (including Gforth) lives on under the hood,
                ; even though it's not an ANS standard word, it doesn't appear
                ; in the docs, it's only used here, and there are no tests for
                ; it. This is why we got rid of it. We'll be converting this
                ; mess to something more sane in the long run.

                ; Imagine we have the double word ud = 2^16 u + v which we want to
                ; write as qd m + r for some base m.  Let u = qu m + ru, with qu, ru
                ; caculated in forth via `u 0 m um/mod`, meaning that
                ; ud = 2^16 qu m + (2^16 ru + v).  Now write the "remainder" term
                ; 2^16 ru + v as qv m + rv, again calculating qv, rv
                ; in forth via `v ru m um/mod`.  (We know the quotient won't overflow
                ; a single word because the high word ru < m.)  This leaves
                ; ud = 2^16 qu m + qv m + rv = (2^16 qu + qv) m + rv
                ; so that qd is the double word (qv, qu) and r is rv.

                ; If, as is often the case, the most signficant word u is zero
                ; then qu = ru = 0 and we can skip the first pass and
                ; simply calculate v 0 m um/mod immediately.

                dex                     ; inline w_zero
                dex
                stz 0,x
                stz 1,x

                ; use msb of base as a flag to loop twice
                ; (we assume below base <= 36 so this is safe)
                inc base+1

                lda 2,x                 ; if msw is 0 we can skip the first pass
                ora 3,x
                beq _skip               ; enter with ( v 0 0 -rot -- 0 v 0 )

_loop:
                ; ( v u 0 ) on first pass, then ( qu v ru ) on second pass
                dex                     ; inline `base @`
                dex
                lda base                ; base <= 36
                sta 0,x
                stz 1,x
                jsr w_um_slash_mod      ; ( v u 0 base -- v ru qu )
_skip:          jsr w_not_rot           ; ( qu v ru )
                lsr base+1              ; 1 => 0 + C=1 => 0 + C=0
                bcs _loop               ; run two passes

                ; the second pass calculates:
                ; base @ ( qu v ru base )
                ; um/mod ( qu rv qv )
                ; -rot   ( qv qu rv ) aka ( ud rem )

                ; Convert the number that is left over to an ASCII character.
                ; We use a string lookup for speed (assumes base <= 36).

                lda 0,x
                tay
                lda s_abc_upper,y       ; upper case 0-9A-Z
                sta 0,x
                stz 1,x                 ; paranoid; now ( ud char )

                jsr w_hold

z_number_sign:
                rts



; ## NUMBER_SIGN_GREATER ( d -- addr u ) "Finish pictured number conversion"
; ## "#>"  auto  ANS core
        ; """https://forth-standard.org/standard/core/num-end
        ; Finish conversion of pictured number string, putting address and
        ; length on the Data Stack.
        ;
        ; Original Fort is  2DROP HLD @ PAD OVER -
        ; Based on
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; """
xt_number_sign_greater:
                jsr underflow_2         ; double number
w_number_sign_greater:
                ; The start address lives in tohold
                lda tohold
                sta 0,x         ; LSB of tohold
                sta 2,x
                lda tohold+1
                sta 1,x         ; MSB of addr
                sta 3,x         ; ( addr addr )

                ; The length of the string is pad - addr
                jsr w_pad      ; ( addr addr pad )

                sec
                lda 0,x         ; LSB of pad address
                sbc 2,x
                sta 2,x

                lda 1,x         ; MSB, which should always be zero
                sbc 3,x
                sta 3,x         ; ( addr u pad )

                inx
                inx

z_number_sign_greater:
                rts



; ## NUMBER_SIGN_S ( d -- addr u ) "Completely convert pictured output"
; ## "#s"  auto  ANS core
        ; """https://forth-standard.org/standard/core/numS
        ; Completely convert number for pictured numerical output.
        ;
        ; Based on
        ; https://github.com/philburk/pforth/blob/master/fth/system.fth
        ; Original Forth code  BEGIN # 2DUP OR 0= UNTIL
        ; """

xt_number_sign_s:
                jsr underflow_2
w_number_sign_s:
_loop:
                ; convert a single number ("#")
                jsr w_number_sign

                ; stop when double-celled number in TOS is zero:
                lda 0,x
                ora 1,x
                ora 2,x
                ora 3,x
                bne _loop

z_number_sign_s:
                rts



; ## OF (C: -- of-sys) (x1 x2 -- |x1) "Conditional flow control"
; ## "of"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/OF"""

xt_of:
w_of:
                ; Check if value is equal to this case.
                ; Postpone over (eg. compile a jsr to it)
                ldy #>w_over
                lda #<w_over
                jsr cmpl_subroutine

                ; Postpone = (EQUAL), that is, compile a jsr to it
                ldy #>w_equal
                lda #<w_equal
                jsr cmpl_subroutine

                jsr w_if

                ; If it's true, consume the original value.
                ; Postpone DROP (eg. compile a jsr to it)
                ldy #>w_drop
                lda #<w_drop
                jsr cmpl_subroutine

z_of:           rts



; ## ONE_MINUS ( u -- u-1 ) "Decrease TOS by one"
; ## "1-"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OneMinus"""

xt_one_minus:
                jsr underflow_1
w_one_minus:
                lda 0,x
                bne +
                dec 1,x
+
                dec 0,x

z_one_minus:    rts



; ## ONE_PLUS ( u -- u+1 ) "Increase TOS by one"
; ## "1+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OnePlus
        ;
        ; Code is shared with CHAR-PLUS
        ; """

xt_char_plus:
xt_one_plus:
                jsr underflow_1
w_char_plus:
w_one_plus:
                inc 0,x
                bne _done
                inc 1,x

_done:
z_char_plus:
z_one_plus:     rts



; ## OR ( m n -- n ) "Logically OR TOS and NOS"
; ## "or"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OR"
xt_or:
                jsr underflow_2
w_or:
                lda 0,x
                ora 2,x
                sta 2,x

                lda 1,x
                ora 3,x
                sta 3,x

                inx
                inx

z_or:           rts



; ## OVER ( b a -- b a b ) "Copy NOS to TOS"
; ## "over"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OVER"""
xt_over:
                jsr underflow_2
w_over:
                dex
                dex

                lda 4,x         ; LSB
                sta 0,x
                lda 5,x         ; MSB
                sta 1,x

z_over:         rts



; ## PAD ( -- addr ) "Return address of user scratchpad"
; ## "pad"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/PAD
        ; Return address to a temporary area in free memory for user. Must
        ; be at least 84 bytes in size (says ANS). It is located relative to
        ; the compile area pointer (CP) and therefore varies in position.
        ; This area is reserved for the user and not used by the system
        ; """
xt_pad:
w_pad:
                dex
                dex

                lda cp
                clc
                adc #padoffset  ; assumes padoffset one byte in size
                sta 0,x

                lda cp+1
                adc #0          ; only need carry
                sta 1,x

z_pad:          rts



; ## PAGE ( -- ) "Clear the screen"
; ## "page"  tested  ANS facility
        ; """https://forth-standard.org/standard/facility/PAGE
        ; Clears a page if supported by ANS terminal codes. This is
        ; Clear Screen ("ESC[2J") plus moving the cursor to the top
        ; left of the screen
        ; """
xt_page:
w_page:
                lda #AscESC
                jsr emit_a
                lda #'['
                jsr emit_a
                lda #'2'
                jsr emit_a
                lda #'J'
                jsr emit_a

                ; move cursor to top left of screen
                jsr w_zero
                jsr w_zero
                jsr w_at_xy

z_page:         rts



; ## PAREN ( -- ) "Discard input up to close paren ( comment )"
; ## "("  auto  ANS core
        ; """http://forth-standard.org/standard/core/p"""

xt_paren:
w_paren:
                ; Put a right paren on the stack.
                dex
                dex
                lda #41     ; Right parenthesis
                sta 0,x
                stz 1,x

                ; Call parse.
                jsr w_parse

                ; Throw away the result.
                inx
                inx
                inx
                inx

z_paren:        rts



; ## PARSE_NAME ( "name" -- addr u ) "Parse the input"
; ## "parse-name"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/PARSE-NAME
        ; Find next word in input string, skipping leading whitespace. This is
        ; a special form of PARSE and drops through to that word. See PARSE
        ; for more detail. We use this word internally for the interpreter
        ; because it is a lot easier to use. Reference implementations at
        ; http://forth-standard.org/standard/core/PARSE-NAME and
        ; http://www.forth200x.org/reference-implementations/parse-name.fs
        ; Roughly, the word is comparable to BL WORD COUNT. -- Note that
        ; though the ANS standard talks about skipping "spaces", whitespace
        ; is actually perfectly legal (see for example
        ; http://forth-standard.org/standard/usage#subsubsection.3.4.1.1).
        ; Otherwise, PARSE-NAME chokes on tabs.
        ; """

xt_parse_name:
w_parse_name:
                ; To enable the compilation of the high-level Forth words
                ; in forth-words.asm and user-words.asm at boot time,
                ; PARSE-NAME and PARSE must be able to deal with 16-bit string
                ; lengths. This is a pain on an 8-bit machine. The pointer
                ; to the current location is in toin (>IN). We need to check,
                ; worst case, the characters from cib+toin to cib+ciblen, and
                ; we can't just use Y as an index.

                ; The counter is CIBLEN-TOIN and stored in tmp1
                lda ciblen              ; LSB of counter
                sec
                sbc toin
                sta tmp1
                lda ciblen+1            ; MSB
                sbc toin+1
                sta tmp1+1

                ; Check the result for zero (TOIN is equal to CIBLEN)
                lda tmp1
                ora tmp1+1
                beq _empty_line

                ; We walk through the characters starting at CIB+TOIN, so we
                ; save a temp version of that in tmp2
                lda cib
                clc
                adc toin
                sta tmp2                ; LSB of first character
                lda cib+1
                adc toin+1
                sta tmp2+1              ; MSB

_skip_loop:
                lda (tmp2)              ; work copy of cib
                jsr is_whitespace
                bcc _char_found

                ; Char is still whitespace, continue
                inc tmp2
                bne +
                inc tmp2+1
+
                ; Adjust counter
                lda tmp1
                bne +
                dec tmp1+1
+               dec tmp1

                lda tmp1
                ora tmp1+1
                bne _skip_loop          ; fall through if empty line

_empty_line:
                ; Neither the ANS Forth nor the Gforth documentation say
                ; what to return as an address if a string with only
                ; spaces is given. For speed reasons, we just return junk
                ; NOS, with the TOS zero as per standard
                dex
                dex
                dex
                dex

                stz 0,x                 ; TOS is zero
                stz 1,x

                jmp z_parse_name        ; skip over PARSE

_char_found:
                ; We arrive here with tmp2 pointing to the first non-space
                ; character. This is where the word really starts, so
                ; we use it to calculate the new >IN by subtracting
                lda tmp2
                sec
                sbc cib
                sta toin
                lda tmp2+1
                sbc cib+1
                sta toin+1

                ; prepare Data Stack for PARSE by adding space
                ; as the delimiter
                dex
                dex

                lda #AscSP
                sta 0,x
                stz 1,x                 ; paranoid, now ( "name" c )



; ## PARSE ( "name" c -- addr u ) "Parse input with delimiter character"
; ## "parse"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/PARSE
        ; Find word in input string delimited by character given. Do not
        ; skip leading delimiters -- this is the main difference to PARSE-NAME.
        ; PARSE and PARSE-NAME replace WORD in modern systems. ANS discussion
        ; http://www.forth200x.org/documents/html3/rationale.html#rat:core:PARSE
        ;
        ;
        ;     cib  cib+toin   cib+ciblen
        ;      v      v            v
        ;     |###################|
        ;
        ;     |------>|  toin (>IN)
        ;     |------------------->|  ciblen
        ;
        ; The input string is stored starting at the address in the Current
        ; Input Buffer (CIB), the length of which is in CIBLEN. While searching
        ; for the delimiter, TOIN (>IN) points to the where we currently are.
        ; Since PARSE does not skip leading delimiters, we assume we are on a
        ; useful string if there are any characters at all. As with
        ; PARSE-NAME, we must be able to handle strings with a length of
        ; 16-bit for EVALUTE, which is a pain on an 8-bit machine.
        ; """

xt_parse:
                jsr underflow_1
w_parse:
                ; If the input buffer is empty, we just return
                lda ciblen
                ora ciblen+1
                beq _abort_parse

                ; If the pointer >IN is larger or equal to the length of
                ; the input buffer (CIBLEN), the line is done. Put
                ; differently, we only continue if >IN is smaller than
                ; CIBLEN
                lda toin+1              ; MSB
                cmp ciblen+1
                bcc _go_parse           ; unsigned comparison

                lda toin                ; LSB
                cmp ciblen
                bcc _go_parse

_abort_parse:
                ; Sorry, this line is over
                dex
                dex
                stz 0,x
                stz 1,x

                bra _done
_go_parse:
                ; We actually have work to do. Save the delimiter in
                ; tmptos.
                lda 0,x
                sta tmptos

                ; We can now prepare the Data Stack for the return value
                dex
                dex

                ; tmp1 is CIB+TOIN, the beginning of the current string
                ; tmp2 is initially the same as tmp1, then the work index
                ; tmp3 is CIB+CIBLEN, one char past the end of the string

                ; Calculate the beginning of the string, which is also the
                ; address to return
                lda cib
                clc
                adc toin        ; LSB
                sta tmp1
                sta tmp2
                sta 2,x

                lda cib+1
                adc toin+1      ; MSB
                sta tmp1+1
                sta tmp2+1
                sta 3,x

                ; Calculate the address where the input buffer ends plus 1, so
                ; we can compare it with TOIN, which is an index
                lda cib
                clc
                adc ciblen
                sta tmp3
                lda cib+1
                adc ciblen+1
                sta tmp3+1

                ; Initialize the offset we use to adjust EOL or found delimiter
                stz tmptos+1
_loop:
                ; If we are at the end of the string, quit
                lda tmp2
                cmp tmp3
                bne _not_empty

                lda tmp2+1
                cmp tmp3+1
                beq _eol
_not_empty:
                ; We have to do this the hard way. In fact, it's really
                ; hard since if we are dealing with a SPACE, the standard
                ; wants us to skip all whitespace, not just spaces. Otherwise,
                ; Tali would choke on tabs between words. For details, see
                ; http://forth-standard.org/standard/file#subsection.11.3.5
                ; In theory, we could make this faster by defining a delimiter
                ; that is 00 as the sign that we skip all whitespace, thereby
                ; avoiding having to test every time. However, somebody,
                ; somewhere might want to parse a zero-delimited list. Since
                ; any byte value could be chosen for that, we just test for
                ; a space every single time for the moment.
                lda (tmp2)

                ldy tmptos
                cpy #AscSP
                bne _not_whitespace

                ; The delimiter is a space, so we're looking for all
                ; whitespace
                jsr is_whitespace
                bcc _not_whitespace
                bra _found_delimiter

_not_whitespace:
                ; The delimiter is not a space, so we're looking for
                ; whatever it is
                cmp tmptos
                beq _found_delimiter

                ; Not a delimiter, next character
                inc tmp2
                bne _loop
                inc tmp2+1
                bra _loop

_found_delimiter:
                ; Increase the offset: If we've found a delimiter, we want
                ; TOIN to point to the character after it, not the delimiter
                ; itself
                inc tmptos+1
_eol:
                ; The length of the new string is tmp2-tmp1
                lda tmp2
                sec
                sbc tmp1
                sta 0,x

                lda tmp2+1
                sbc tmp1+1
                sta 1,x

                ; The new offset is tmp2-cib
                lda tmp2
                sec
                sbc cib
                sta toin
                lda tmp2+1
                sbc cib+1
                sta toin+1

                ; Add in the delimiter
                lda toin
                clc
                adc tmptos+1
                sta toin
                bcc +
                inc toin+1
+
_done:
z_parse_name:
z_parse:        rts



; ## PICK ( n n u -- n n n ) "Move element u of the stack to TOS"
; ## "pick"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/PICK
        ; Take the u-th element out of the stack and put it on TOS,
        ; overwriting the original TOS. 0 PICK is equivalent to DUP, 1 PICK to
        ; OVER. Note that using PICK is considered poor coding form. Also note
        ; that FIG Forth has a different behavior for PICK than ANS Forth.
        ; """

xt_pick:
w_pick:
                ; Checking for underflow is difficult because it depends on
                ; which element we want to grab. We could probably figure
                ; something out, but it wouldn't work with underflow stripping
                ; Since using PICK is considered poor form anyway, we just
                ; leave it as it is
                asl 0,x         ; we assume u < 128 (stack is small)
                txa
                adc 0,x
                tay

                lda 0002,y
                sta 0,x
                lda 0003,y
                sta 1,x

z_pick:         rts



; ## PLUS ( n n -- n ) "Add TOS and NOS"
; ## "+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Plus"""
xt_plus:
                jsr underflow_2
w_plus:
                clc
                lda 0,x         ; LSB
                adc 2,x
                sta 2,x

                lda 1,x         ; MSB. No CLC, conserve carry bit
                adc 3,x
                sta 3,x

                inx
                inx

z_plus:         rts



; ## PLUS_STORE ( n addr -- ) "Add number to value at given address"
; ## "+!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/PlusStore"""
xt_plus_store:
                jsr underflow_2
w_plus_store:
                ; move address to tmp1 so we can work with it
                lda 0,x
                sta tmp1
                lda 1,x
                sta tmp1+1

                ldy #0          ; LSB
                lda (tmp1),y
                clc
                adc 2,x
                sta (tmp1),y

                iny             ; MSB
                lda (tmp1),y
                adc 3,x
                sta (tmp1),y

                inx
                inx
                inx
                inx

z_plus_store:   rts



; ## POSTPONE ( -- ) "Change IMMEDIATE status (it's complicated)"
; ## "postpone"  auto   ANS core
        ; """https://forth-standard.org/standard/core/POSTPONE
        ; Add the compilation behavior of a word to a new word at
        ; compile time. If the word that follows it is immediate, include
        ; it so that it will be compiled when the word being defined is
        ; itself used for a new word. Tricky, but very useful.
        ;
        ; Because POSTPONE expects a word (not an xt) in the input stream (not
        ; on the Data Stack). This means we cannot build words with
        ; "jsr w_postpone, jsr <word>" directly.
        ; """

xt_postpone:
w_postpone:
                jsr w_parse_name               ; ( -- addr n )

                ; if there was no word provided, complain and quit
                lda 0,x
                ora 1,x
                bne +

                lda #err_noname
                jmp error
+
                jsr w_find_name                ; ( -- nt | 0 )

                ; if word not in Dictionary, complain and quit
                bne +
                lda #err_noname
                jmp error

+
                ; keep a copy of nt for later
                lda 0,x
                sta tmp1
                lda 1,x
                sta tmp1+1

                ; We need the xt instead of the nt
                jsr w_name_to_int              ; ( nt -- xt )

                ; See if this is an immediate word. This is easier
                ; with nt than with xt. The status byte of the word
                ; is nt+1
                inc tmp1
                bne +
                inc tmp1+1
+
                lda (tmp1)
                and #IM         ; mask all but Intermediate flag
                beq _not_immediate

                ; We're immediate, so instead of executing it right now, we
                ; compile it. xt is TOS, so this is easy. The RTS at the end
                ; takes us back to the original caller
                jsr w_compile_comma
                bra _done

_not_immediate:
                ; This is not an immediate word, so we enact "deferred
                ; compilation" by including ' <NAME> COMPILE, which we do by
                ; compiling the run-time routine of LITERAL, the xt itself, and
                ; a subroutine jump to COMPILE,
                jsr w_literal

                ; Last, compile COMPILE,
                ldy #>w_compile_comma
                lda #<w_compile_comma
                jsr cmpl_subroutine
_done:
z_postpone:     rts



; ## QUESTION_DUP ( n -- 0 | n n ) "Duplicate TOS non-zero"
; ## "?dup"  auto  ANS core
        ; """https://forth-standard.org/standard/core/qDUP"""

xt_question_dup:
                jsr underflow_1
w_question_dup:
                ; Check if TOS is zero
                lda 0,x
                ora 1,x
                beq _done

                ; not zero, duplicate
                dex
                dex
                lda 2,x
                sta 0,x
                lda 3,x
                sta 1,x
_done:
z_question_dup: rts



; ## R_FETCH ( -- n ) "Get copy of top of Return Stack"
; ## "r@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/RFetch
        ; This word is Compile Only in Tali Forth, though Gforth has it
        ; work normally as well
        ;
        ; An alternative way to write this word
        ; would be to access the elements on the stack directly like 2R@
        ; does, these versions should be compared at some point.
        ; """
xt_r_fetch:
w_r_fetch:
                ; --- START FOR JSR (save return address + 1) ---

                pla                     ; LSB
                ply                     ; MSB
                inc a
                sta tmp1                ; LSB
                bne +
                iny
+
                sty tmp1+1              ; MSB

                ; --- START FOR NATIVE COMPILE (via ST flag) ---

                ; get the actual top of Return Stack
                dex
                dex

                ply             ; LSB
                sty 0,x
                pla             ; MSB
                sta 1,x

                ; now we have to put that value back
                pha
                phy

                ; --- CUT FOR NATIVE COMPILE ---

z_r_fetch:      jmp (tmp1)




; ## R_FROM ( -- n )(R: n --) "Move top of Return Stack to TOS"
; ## "r>"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Rfrom
        ; Move Top of Return Stack to Top of Data Stack.
        ;
        ; We have to move
        ; the RTS address out of the way first. This word is handled
        ; differently for native and and subroutine compilation, see COMPILE,
        ; This is a compile-only word
        ; """
xt_r_from:
w_r_from:
                ; --- START FOR JSR (save return address + 1) ---

                pla                     ; LSB
                ply                     ; MSB
                inc a
                sta tmp1                ; LSB
                bne +
                iny
+
                sty tmp1+1              ; MSB

                ; --- START FOR NATIVE COMPILE (via ST flag) ---

                dex
                dex

                ; now we can access the actual data

                pla             ; LSB
                sta 0,x
                pla             ; MSB
                sta 1,x

                ; --- CUT FOR NATIVE COMPILE ---

z_r_from:       jmp (tmp1)




; ## RECURSE ( -- ) "Copy recursive call to word being defined"
; ## "recurse"  auto  ANS core
        ; """https://forth-standard.org/standard/core/RECURSE
        ;
        ; This word may not be natively compiled
        ; """

xt_recurse:
w_recurse:
                ; The whole routine amounts to compiling a reference to
                ; the word that is being compiled. First, we save the JSR
                ; instruction
                ldy #0

                lda #OpJSR
                sta (cp),y
                iny

                ; Next, we save the LSB and MSB of the xt of the word
                ; we are currently working on. We first need to see if
                ; WORKWORD has the nt (: started the word) or the
                ; xt (:NONAME started the word). Bit 6 in status tells us.
                bit status
                bvs _nt_in_workword

                ; This is a special :NONAME word. Just copy the xt
                ; from WORKWORD into the dictionary.
                lda workword
                sta (cp),y
                iny
                lda workword+1
                sta (cp),y
                iny
                bra _update_cp

_nt_in_workword:
                ; This is a regular : word, so the xt is four bytes down
                ; from the nt which we saved in WORKWORD. We could probably
                ; use NAME>INT here but this is going to be faster, and
                ; fast counts with recursion
                lda workword            ; LSB
                clc
                adc #4
                sta tmp1
                lda workword+1          ; MSB
                adc #0
                sta tmp1+1

                lda (tmp1)
                sta (cp),y
                phy
                ldy #1
                lda (tmp1),y
                ply
                iny
                sta (cp),y
                iny

_update_cp:
                tya
                clc
                adc cp
                sta cp
                bcc _done
                inc cp+1
_done:
z_recurse:      rts



; ## REFILL ( -- f ) "Refill the input buffer"
; ## "refill"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/REFILL
        ; Attempt to fill the input buffer from the input source, returning
        ; a true flag if successful. When the input source is the user input
        ; device, attempt to receive input into the terminal input buffer. If
        ; successful, make the result the input buffer, set >IN to zero, and
        ; return true. Receipt of a line containing no characters is considered
        ; successful. If there is no input available from the current input
        ; source, return false. When the input source is a string from EVALUATE,
        ; return false and perform no other action." See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Input-Stream.html
        ; and Conklin & Rather p. 156. Note we don't have to care about blocks
        ; because REFILL is never used on blocks - Tali is able to evaluate the
        ; entire block as a 1024 byte string.
        ; """"

xt_refill:
w_refill:
                ; Get input source from SOURCE-ID. This is an
                ; optimized version of a subroutine jump to SOURCE-ID
                lda insrc               ; cheat: We only check LSB
                bne _src_not_kbd

                ; SOURCE-ID of zero means we're getting stuff from the keyboard
                ; with ACCEPT, which wants the address of the current input
                ; buffer NOS and the max number of characters to accept TOS
                dex
                dex
                dex
                dex

                lda cib                 ; address of CIB is NOS
                sta 2,x
                lda cib+1
                sta 3,x

                stz ciblen              ; go in with empty buffer
                stz ciblen+1

                lda #bsize              ; max number of chars is TOS
                sta 0,x
                stz 1,x                 ; cheat: We only accept max 255

                jsr w_accept           ; ( addr n1 -- n2)

                ; ACCEPT returns the number of characters accepted, which
                ; belong in CIBLEN
                lda 0,x
                sta ciblen
                lda 1,x
                sta ciblen+1            ; though we only accept 255 chars

                ; make >IN point to beginning of buffer
                stz toin
                stz toin+1

                lda #$FF                ; overwrite with TRUE flag
                sta 0,x
                sta 1,x

                bra _done

_src_not_kbd:
                ; If SOURCE-ID doesn't return a zero, it must be a string in
                ; memory or a file (remember, no blocks in this version).
                ; If source is a string, we were given the flag -1 ($FFFF)
                ina
                bne _src_not_string

                ; Simply return FALSE flag as per specification
                dex
                dex
                stz 0,x
                stz 1,x

                bra z_refill

_src_not_string:
                ; Since we don't have blocks, this must mean that we are trying
                ; to read from a file. However, we don't have files yet, so we
                ; report an error and jump to ABORT.
                lda #err_badsource
                jmp error
_done:
z_refill:       rts



; ## REPEAT (C: orig dest -- ) ( -- ) "Loop flow control"
; ## "repeat"  auto  ANS core
        ; """http://forth-standard.org/standard/core/REPEAT"""

xt_repeat:
                jsr underflow_2
w_repeat:
                ; Code the jump back to begin
                jsr w_again

                ; Stuff HERE in for the branch address left by WHILE
                ; to get out of the loop
                jmp w_then
z_repeat:



; ## RIGHT_BRACKET ( -- ) "Enter the compile state"
; ## "]"  auto  ANS core
        ; """https://forth-standard.org/standard/right-bracket
        ; This is an immediate word.
        ; """
xt_right_bracket:
w_right_bracket:
                lda #$FF
                sta state
                sta state+1
z_right_bracket:
                rts



; ## ROT ( a b c -- b c a ) "Rotate first three stack entries downwards"
; ## "rot"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ROT
        ; Remember "R for 'Revolution'" - the bottom entry comes out
        ; on top!
        ; """

xt_rot:
                jsr underflow_3
w_rot:
                ldy 5,x         ; MSB first
                lda 3,x
                sta 5,x
                lda 1,x
                sta 3,x
                sty 1,x

                ldy 4,x         ; LSB next
                lda 2,x
                sta 4,x
                lda 0,x
                sta 2,x
                sty 0,x

z_rot:          rts



; ## RSHIFT ( x u -- x ) "Shift TOS to the right"
; ## "rshift"  auto  ANS core
        ; """https://forth-standard.org/standard/core/RSHIFT"""
xt_rshift:
                jsr underflow_2
w_rshift:
                ; We shift maximal by 16 bits, mask everything else
                lda 0,x
                and #%00001111
                beq _done               ; if 0 shifts, quit

                tay
_loop:
                lsr 3,x
                ror 2,x
                dey
                bne _loop
_done:
                inx
                inx

z_rshift:       rts


; ## S_BACKSLASH_QUOTE ( "string" -- )( -- addr u ) "Store string in memory"
; ## "s\""  auto  ANS core
        ; """https://forth-standard.org/standard/core/Seq
        ; Store address and length of string given, returning ( addr u ).
        ; ANS core claims this is compile-only, but the file set expands it
        ; to be interpreted, so it is a state-sensitive word, which in theory
        ; are evil. We follow general usage. This is just like S" except
        ; that it allows for some special escaped characters.
        ; """

xt_s_backslash_quote:
w_s_backslash_quote:
                ; tmp2 will be used to determine if we are handling
                ; escaped characters or not. In this case, we are,
                ; so set it to $FF (the upper byte will be used to
                ; determine if we just had a \ and the next character
                ; needs to be modifed as an escaped character).
                lda #$FF
                sta tmp2
                stz tmp2+1

                ; Now that the flag is set, jump into s_quote to process
                ; the string.
                jsr s_quote_start
z_s_backslash_quote:
                rts


; This is a helper function for s_backslash_quote to convert a character
; from ASCII to the corresponding hex value, eg 'F'->15
convert_hex_value:

        cmp #'A'
        bcc _digit

        ; It's A-F
        and #$DF                ; Make it uppercase.
        sec
        sbc #'7'                ; gives value 10 for 'A'
        bra _done

_digit:
        ; It's 0-9
        sec
        sbc #'0'

_done:
        rts



; ## S_QUOTE ( "string" -- )( -- addr u ) "Store string in memory"
; ## "s""  auto  ANS core
        ; """https://forth-standard.org/standard/core/Sq
        ; Store address and length of string given, returning ( addr u ).
        ; ANS core claims this is compile-only, but the file set expands it
        ; to be interpreted, so it is a state-sensitive word, which in theory
        ; are evil. We follow general usage.
        ;
        ; Can also be realized as
        ;     : S" [CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE
        ; but it is used so much we want it in code.
        ; """

xt_s_quote:
w_s_quote:
                ; tmp2 will be used to determine if we are handling
                ; escaped characters or not.  In this case, we are
                ; not, so set it to zero.  (cf S_BACKSLASH_QUOTE)
                stz tmp2
                stz tmp2+1

s_quote_start:
                ; Put a jmp past the string data to be filled in later.
                jsr cmpl_jump_later
                jsr w_here             ; the start of the string
                ; ( jmp-target addr )

_savechars_loop:
                ; Start saving the string into the dictionary up to the
                ; ending double quote. First, check to see if the input
                ; buffer is empty.
                lda toin+1              ; MSB
                cmp ciblen+1
                bcc _input_fine         ; unsigned comparison

                lda toin                ; LSB
                cmp ciblen
                bcc _input_fine

                ; Input buffer is empty. Refill it. Refill calls accept,
                ; which uses tmp2 and tmp3. Save and restore them.
                lda tmp2
                pha
                lda tmp2+1
                pha
                lda tmp3    ; Only tmp3 used, so don't bother with tmp3+1
                pha

                jsr w_refill           ; ( -- f )

                pla
                sta tmp3
                pla
                sta tmp2+1
                pla
                sta tmp2

                ; Check result of refill.
                lda 0,x
                ora 1,x
                bne _refill_ok

                ; Something when wrong with refill.
                lda #err_refill
                jmp error

_refill_ok:
                ; Remove the refill flag from the data stack.
                inx
                inx

                ; For refill success, jump back up to the empty check, just in
                ; case refill gave us an empty buffer (eg. empty/blank line of
                ; input)
                bra _savechars_loop

_input_fine:
                ; There should be at least one valid char to use.
                ; Calculate it's address at CIB+TOIN into tmp1
                lda cib
                clc
                adc toin        ; LSB
                sta tmp1
                lda cib+1
                adc toin+1      ; MSB
                sta tmp1+1

                ; Get the character
                lda (tmp1)

                ; Check to see if we are handling escaped characters.
                bit tmp2
                bmi _handle_escapes    ; Only checking bit 7
                jmp _regular_char

_handle_escapes:
                ; We are handling escaped characters.  See if we have
                ; already seen the backslash.
                bit tmp2+1
                bmi _escaped
                jmp _not_escaped

_escaped:

                ; We have seen a backslash (previous character). Check to see if
                ; we are in the middle of a \x sequence (bit 6 of tmp2+1 will
                ; be clear in that case )
                bvs _check_esc_chars

                ; We are in the middle of a \x sequence. Check to see if we
                ; are on the first or second digit.
                lda #1
                bit tmp2+1
                bne _esc_x_second_digit

                ; First digit.
                inc tmp2+1  ; Adjust flag for second digit next time.
                lda (tmp1)  ; Get the char again.

                ; Convert to hex
                jsr convert_hex_value

                ; This is the upper nybble, so move it up.
                asl
                asl
                asl
                asl
                sta tmp3    ; Save it for later.
                jmp _next_character

_esc_x_second_digit:

                ; We are on the second hex digit of a \x sequence. Clear the
                ; escaped character flag (because we are handling it right
                ; here)
                stz tmp2+1
                lda (tmp1)

                ; Convert to hex, combine with value in tmp3
                jsr convert_hex_value
                ora tmp3

                bra _save_character

_check_esc_chars:
                ; Clear the escaped character flag (because we are
                ; handling it right here)
                stz tmp2+1

                ; is it character a-z ?
                cmp #'a'
                bmi _check_esc_quote
                cmp #'z'+1
                bpl _check_esc_quote
                ; check translation table
                tay
                lda escape_tr_table - 'a',y   ; fake base address to index with a-z directly
                bne _esc_replace
                tya                     ; revert if no translation
                bra _check_esc_quote

_esc_replace:   bpl _save_character     ; simple replacement
                ; handle specials with hi bit set (NUL and CR/LF)
                and #$7F                ; clear hi bit
                beq _save_character     ; NUL we can just output
                jsr cmpl_a              ; else output first char (CR)
                lda #10                 ; followed by LF
                bra _save_character

_check_esc_quote:
                cmp #'"'
                beq _save_character

                cmp #'x'
                bne _check_esc_backslash

                ; This one is difficult. We need to get the next TWO
                ; characters (which might require a refill in the middle)
                ; and combine them as two hex digits. We do this by
                ; clearing bit 6 of tmp2+1 to indicate we are in a digit
                ; and using bit 0 to keep track of which digit we are on.
                lda #%10111110        ; Clear bits 6 and 0
                sta tmp2+1
                bra _next_character

_check_esc_backslash:
                cmp #'\'
                bne _regular_char
                bra _save_character

_not_escaped:
                ; Check for the backslash to see if we should escape
                ; the next char.
                cmp #'\'
                bne _regular_char

                ; We found a backslash.  Don't save anyhing, but set
                ; a flag (in tmp2+1) to handle the next char. We don't
                ; try to get the next char here as it may require a
                ; refill of the input buffer.
                lda #$FF
                sta tmp2+1
                bra _next_character

_regular_char:
                ; Check if the current character is the end of the string.
                cmp #'"'
                beq _found_string_end

_save_character:
                ; If we didn't reach the end of the string, compile this
                ; character into the dictionary
                jsr cmpl_a

_next_character:
                ; Move on to the next character.
                inc toin
                bne _savechars_loop_longjump
                inc toin+1

_savechars_loop_longjump:
                jmp _savechars_loop

_found_string_end:
                ; Use up the delimiter.
                inc toin
                bne +
                inc toin+1
+
                ; We currently have ( jmp-target addr )
                ; We need to calculate the length of string
                ; and update the jump target.

                jsr w_here
                jsr w_rot
                jsr w_store    ; Update the jmp target

                jsr w_here
                jsr w_over
                jsr w_minus    ; HERE - addr gives string length

                ; What happens next depends on the state (which is bad, but
                ; that's the way it works at the moment). If we are
                ; interpreting, we save the string to a transient buffer
                ; and return that address (used for file calls, see
                ; https://forth-standard.org/standard/file/Sq ). If we're
                ; compiling, we just need SLITERAL
                lda state
                ora state+1             ; paranoid
                beq _done

                ; Jump into the middle of the sliteral word, after the
                ; string data has been compiled into the dictionary,
                ; because we've already done that step.
                jsr cmpl_sliteral         ; ( addr u -- )

_done:
z_s_quote:      rts



escape_tr_table:
    ; 26 character translation for simple escapes
    ; 0 indicates no translation, hi bit indicates special
    .byte   7               ; a -> BEL (ASCII value 7)
    .byte   8               ; b -> Backspace (ASCII value 8)
    .byte   0,0             ; c, d no escape
    .byte   27              ; e -> ESC (ASCII value 27)
    .byte   12              ; f -> FF (ASCII value 12)
    .byte   0,0,0,0,0       ; g,h,i,j,k
    .byte   10              ; l -> LF (ASCII value 10)
    .byte   13+128          ; m -> CR/LF pair (ASCII values 13, 10)
    ; n has configurable behavior which we hard-code in the table
.if "cr" in TALI_OPTION_CR_EOL
.if "lf" in TALI_OPTION_CR_EOL
    .byte   13+128          ; n behaves like m --> cr/lf
.else
    .byte   13              ; n behaves like r --> cr
.endif
.else
    .byte   10              ; n behaves like l --> lf
.endif
    .byte   0,0             ; o,p
    .byte   34              ; q -> Double quote (ASCII value 34)
    .byte   13              ; r ->  CR (ASCII value 13)
    .byte   0               ; s
    .byte   9               ; t -> Horizontal TAB (ASCII value 9)
    .byte   0               ; u
    .byte   11              ; v -> Vertical TAB (ASCII value 11)
    .byte   0,0,0           ; w,x,y   (x is a special case later)
    .byte   0+128           ; z -> NULL (ASCII value 0)



; ## S_TO_D ( u -- d ) "Convert single cell number to double cell"
; ## "s>d"  auto  ANS core
        ; """https://forth-standard.org/standard/core/StoD"""

xt_s_to_d:
                jsr underflow_1
w_s_to_d:
                dex
                dex
                stz 0,x
                stz 1,x

                lda 3,x
                bpl _done

                ; negative, extend sign
                dec 0,x
                dec 1,x
_done:
z_s_to_d:       rts



; ## SEMICOLON ( -- ) "End compilation of new word"
; ## ";"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Semi
        ; End the compilation of a new word into the Dictionary.
        ;
        ; When we
        ; enter this, WORKWORD is pointing to the nt_ of this word in the
        ; Dictionary, DP to the previous word, and CP to the next free byte.
        ; A Forth definition would be (see "Starting Forth"):
        ; : POSTPONE EXIT  REVEAL POSTPONE ; [ ; IMMEDIATE  Following the
        ; practice of Gforth, we warn here if a word has been redefined.
        ; """

xt_semicolon:
w_semicolon:
                ; Check if this is a : word or a :NONAME word.
                bit status
                bvs _colonword

                ; This is a :NONAME word - just put an RTS on the end and
                ; the address (held in workword) on the stack.
                lda #OpRTS
                jsr cmpl_a

                dex
                dex
                lda workword
                sta 0,x
                lda workword+1
                sta 1,x
                bra _semicolon_done

_colonword:
                ; CP is the byte that will be the address we use in the
                ; header as the end-of-compile address (z_word). This is
                ; six bytes down from the header
                ldy #6
                lda cp
                sta (workword),y
                iny
                lda cp+1
                sta (workword),y

                ; Allocate one further byte and save the RTS instruction
                ; there
                lda #OpRTS
                jsr cmpl_a

                ; Before we formally add the word to the Dictionary, we
                ; check to see if it is already present, and if yes, we
                ; warn the user.

                ; See if word already in Dictionary.
                ; (STATUS bit 7 will be high as CREATE already
                ;  checked for us.)
                bit status
                bpl _new_word   ; Bit 7 is clear = new word

                ; We start by putting the string of the
                ; word we're defining on the stack
                dex
                dex
                dex
                dex

                ; WORKWORD points to the beginning of the head of our new
                ; word, where the first byte is the length of the string
                ; We can't use LATESTNT because we haven't added the new
                ; word to the Dictionary yet
                lda (workword)
                sta 0,x
                stz 1,x

                ; Eight bytes below WORKWORD is the actual beginning of
                ; the string
                lda workword
                clc
                adc #8
                sta 2,x
                lda workword+1
                adc #0                  ; only want carry
                sta 3,x

                ; This word is already in the Dictionary, so we print a
                ; warning to the user.
                lda #str_redefined       ; address of string "redefined"
                jsr print_string_no_lf

                ; Now we print the offending word.
                jsr w_type
                jsr w_space

                ; Clear bit 7 of status (so future words will print message
                ; by defaut)
                lda #%10000000
                trb status

_new_word:
                ; Let's get this over with. Save beginning of our word
                ; as new last word in the Dictionary
                lda workword
                sta dp
                lda workword+1
                sta dp+1
                jsr dp_to_current       ; Save the updated DP to the
                                        ; CURRENT wordlist.
_semicolon_done:
                ; Word definition complete. Return compile flag to zero
                ; to return to interpret mode
                stz state
                stz state+1

z_semicolon:    rts



; ## SIGN ( n -- ) "Add minus to pictured output"
; ## "sign"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SIGN
        ;
        ; Code based on
        ; http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
        ; Original Forth code is   0< IF ASCII - HOLD THEN
        ; """

xt_sign:
                jsr underflow_1
w_sign:
                lda 1,x         ; check MSB of TOS
                bmi _minus

                inx
                inx
                bra _done
_minus:
                lda #'-'
                sta 0,x         ; overwrite TOS
                stz 1,x         ; paranoid

                jsr w_hold
_done:
z_sign:         rts



; ## SLASH ( n1 n2 -- n ) "Divide NOS by TOS"
; ## "/"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Div
        ;
        ; Forth code is either  >R S>D R> FM/MOD SWAP DROP
        ; or >R S>D R> SM/REM SWAP DROP -- we use SM/REM in Tali Forth.
        ; This code is currently unoptimized. This code without the SLASH
        ; DROP at the end is /MOD, so we share the code as far as possible.
        ; """

xt_slash:
                jsr underflow_2
w_slash:
                ; With all the multiplication going on, it would be hard to
                ; make sure that one of our temporary variables is not
                ; overwritten. We make sure that doesn't happen by taking the
                ; hit of pushing the flag to the 65c02's stack
                lda #0
                bra slashmod_common

xt_slash_mod:
                jsr underflow_2
w_slash_mod:
                ; Note that /MOD accesses this code
                lda #$FF                ; falls through to _common

slashmod_common:
                pha
                jsr w_to_r             ; >R
                jsr w_s_to_d           ; S>D
                jsr w_r_from           ; R>
                jsr w_sm_slash_rem     ; SM/REM

                ; Get the flag back from the 65c02's stack. Zero is SLASH,
                ; $FF is SLASH MOD
                pla
                bne _done

                ; The following code is for SLASH only
                jsr w_swap
                inx             ; DROP
                inx
_done:
z_slash_mod:
z_slash:        rts


; ## SLASH_MOD ( n1 n2 -- n3 n4 ) "Divide NOS by TOS with a remainder"
; ## "/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DivMOD
        ;
        ; This is a dummy entry, the actual code is shared with SLASH
        ; """



; ## SM_SLASH_REM ( d n1 -- n2 n3 ) "Symmetric signed division"
; ## "sm/rem"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SMDivREM
        ; Symmetric signed division. Compare FM/MOD. Based on F-PC 3.6
        ; by Ulrich Hoffmann. See http://www.xlerb.de/uho/ansi.seq
        ;
        ; Forth:
        ; OVER >R 2DUP XOR 0< >R ABS >R DABS R> UM/MOD R> ?NEGATE SWAP
        ; R> ?NEGATE SWAP
        ; """

xt_sm_slash_rem:
                jsr underflow_3 ; contains double number
w_sm_slash_rem:
                ; push MSB of high cell of d to Data Stack so we can check
                ; its sign later
                lda 3,x
                pha

                ; XOR the MSB of the high cell of d and n1 so we figure out
                ; its sign later as well
                lda 1,x
                eor 3,x
                pha

                ; Prepare division by getting absolute of n1 and d
                jsr w_abs
                inx             ; pretend we pushed n1 to R
                inx

                jsr w_dabs
                dex
                dex

                jsr w_um_slash_mod     ; UM/MOD

                ; if the XOR compiled above is negative, negate the
                ; quotient (n3)
                pla
                bpl +
                jsr w_negate
+
                ; if d was negative, negate the remainder (n2)
                pla
                bpl _done

                inx             ; pretend we pushed quotient to R
                inx
                jsr w_negate
                dex
                dex

_done:
z_sm_slash_rem: rts



; ## SOURCE ( -- addr u ) "Return location and size of input buffer""
; ## "source"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SOURCE"""
xt_source:
w_source:
                ; add address
                dex
                dex
                lda cib
                sta 0,x
                lda cib+1
                sta 1,x

                ; add size
                dex
                dex
                lda ciblen
                sta 0,x
                lda ciblen+1
                sta 1,x

z_source:       rts



; ## SOURCE_ID ( -- n ) "Return source identifier"
; ## "source-id"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/SOURCE-ID Identify the
        ; input source unless it is a block (s. Conklin & Rather p. 156). This
        ; will give the input source: 0 is keyboard, -1 ($FFFF) is character
        ; string, and a text file gives the fileid.
        ; """
xt_source_id:
w_source_id:
                dex
                dex

                lda insrc
                sta 0,x
                lda insrc+1
                sta 1,x

z_source_id:    rts



; ## SPACE ( -- ) "Print a single space"
; ## "space"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SPACE"""
xt_space:
w_space:
                lda #AscSP
                jsr emit_a

z_space:        rts



; ## SPACES ( u -- ) "Print a number of spaces"
; ## "spaces"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SPACES"""

xt_spaces:
                jsr underflow_1
w_spaces:
                lda 1,x         ; ANS says this word takes a signed value
                bmi _done       ; but prints no spaces for negative values.

                ldy 0,x
                beq _msb
_loop:                          ; loop to zero out LSB
                lda #AscSP
                jsr emit_a      ; user routine preserves X and Y
                dey
                bne _loop       ; Y is zero on exit so looping again emits 256 more spaces
_msb:
                dec 1,x         ; when decrementing MSB goes negative, it was zero so we're done
                bpl _loop       ; otherwise emit another 256 spaces

_done:          inx
                inx
z_spaces:       rts


; ## STAR ( n n -- n ) "16*16 --> 16 "
; ## "*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Times
        ; Multiply two signed 16 bit numbers, returning a 16 bit result.
        ;
        ; This is nothing  more than UM* DROP
        ; """

xt_star:
                jsr underflow_2
w_star:
                jsr w_um_star
                inx
                inx

z_star:         rts



; ## STAR_SLASH  ( n1 n2 n3 -- n4 ) "n1 * n2 / n3 -->  n"
; ## "*/"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TimesDiv
        ; Multiply n1 by n2 and divide by n3, returning the result
        ; without a remainder. This is */MOD without the mod.
        ;
        ; This word
        ; can be defined in Forth as : */  */MOD SWAP DROP ; which is
        ; pretty much what we do here
        ; """
xt_star_slash:
                jsr underflow_3
w_star_slash:
                jsr w_star_slash_mod
                jsr w_swap
                inx
                inx
z_star_slash:
                rts


; ## STAR_SLASH_MOD  ( n1 n2 n3 -- n4 n5 ) "n1 * n2 / n3 --> n-mod n"
; ## "*/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TimesDivMOD
        ; Multiply n1 by n2 producing the intermediate double-cell result d.
        ; Divide d by n3 producing the single-cell remainder n4 and the
        ; single-cell quotient n5.
        ;
        ; In Forth, this is
        ; : */MOD  >R M* >R SM/REM ;  Note that */ accesses this routine.
        ; """
xt_star_slash_mod:
                jsr underflow_3
w_star_slash_mod:
                jsr w_to_r
                jsr w_m_star
                jsr w_r_from
                jsr w_sm_slash_rem

z_star_slash_mod:
                rts



; ## STATE ( -- addr ) "Return the address of compilation state flag"
; ## "state"  auto  ANS core
        ; """https://forth-standard.org/standard/core/STATE
        ; STATE is true when in compilation state, false otherwise. Note
        ; we do not return the state itself, but only the address where
        ; it lives. The state should not be changed directly by the user; see
        ; http://forth.sourceforge.net/standard/dpans/dpans6.htm#6.1.2250
        ; """
xt_state:
w_state:
                dex
                dex
                lda #<state
                sta 0,x
                lda #>state
                sta 1,x

z_state:        rts



; ## STORE ( n addr -- ) "Store TOS in memory"
; ## "!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Store"""
xt_store:
                jsr underflow_2
w_store:
                lda 2,x         ; LSB
                sta (0,x)

                inc 0,x
                bne +
                inc 1,x
+
                lda 3,x         ; MSB
                sta (0,x)

                inx             ; 2DROP
                inx
                inx
                inx

z_store:        rts



; ## SWAP ( b a -- a b ) "Exchange TOS and NOS"
; ## "swap"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SWAP"""
xt_swap:
                jsr underflow_2
w_swap:
                lda 0,x         ; LSB
                ldy 2,x
                sta 2,x
                sty 0,x

                lda 1,x         ; MSB
                ldy 3,x
                sta 3,x
                sty 1,x

z_swap:         rts



; ## THEN (C: orig -- ) ( -- ) "Conditional flow control"
; ## "then"  auto  ANS core
        ; """http://forth-standard.org/standard/core/THEN
        ; This is a dummy entry, the code is shared with xt_else
        ; """



; ## TICK ( "name" -- xt ) "Return a word's execution token (xt)"
; ## "'"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Tick"""

xt_tick:
w_tick:
                jsr w_parse_name       ; ( -- addr u )

                ; if we got a zero, there was a problem getting the
                ; name of the word
                lda 0,x
                ora 1,x
                bne +

                lda #err_noname
                jmp error
+
                jsr w_find_name        ; ( addr u -- nt )

                ; If we didn't find the word in the Dictionary, abort
                lda 0,x
                ora 1,x
                bne +

                lda #err_syntax
                jmp error
+
                jsr w_name_to_int      ; ( nt -- xt )

z_tick:         rts



; ## TO ( n "name" -- ) or ( "name" ) "Change a value"
; ## "to"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TO
        ; Gives a new value to a, uh, VALUE.
        ;
        ; One possible Forth
        ; implementation is  ' >BODY !  but given the problems we have
        ; with >BODY on STC Forths, we do this the hard way. Since
        ; Tali Forth uses the same code for CONSTANTs and VALUEs, you
        ; could use this to redefine a CONSTANT, but that is a no-no.
        ;
        ; Note that the standard has different behaviors for TO depending
        ; on the state (https://forth-standard.org/standard/core/TO).
        ; This makes TO state-dependent (which is bad) and also rather
        ; complex (see the Gforth implementation for comparison). This
        ; word may not be natively compiled and must be immediate. Frankly,
        ; it would have made more sense to have two words for this.
        ; """

xt_to:
w_to:
                ; One way or the other, we need the xt of the word after this
                ; one. At this point, we don't know if we are interpreted or
                ; compile, so we don't know if there is a value n on the stack,
                ; so we can't do an underflow check yet
                jsr w_tick             ; ( [n] xt )

                ; The PFA (DFA in this case) is three bytes down,
                ; after the jump to DOCONST
                lda 0,x                 ; LSB
                clc
                adc #3
                sta tmp1
                lda 1,x                 ; MSB
                adc #0                  ; we just want the carry
                sta tmp1+1

                ; Now check which state we are in
                lda state
                ora state+1
                beq _interpret

                ; Compiling, so we arrive with just ( xt ) on the stack.
                ; We need to generate code that writes a number
                ; from TOS to the address in tmp1
                ; i.e. LITERAL tmp1 !

                lda tmp1            ; replace TOS with tmp1
                sta 0,x
                lda tmp1+1
                sta 1,x

                jsr w_literal      ; generate the runtime for LITERAL tmp1

                ldy #>w_store      ; write the runtime for !
                lda #<w_store
                jsr cmpl_subroutine

                bra _done

_interpret:
                ; We're interpreting, so we arrive here with ( n xt )
                ; on the stack. This is an annoying place to put
                ; the underflow check because we can't
                ; automatically strip it out
                jsr underflow_2

                inx
                inx                     ; leaving just ( n )

                ; We skip over the jump to DOCONST and store the number
                ; in the Program Field Area (PDF, in this case more a
                ; Data Field Area
                lda 0,x
                sta (tmp1)              ; LSB

                ldy #1
                lda 1,x                 ; MSB
                sta (tmp1),y            ; fall through to common

                inx                     ; DROP
                inx
_done:
z_to:           rts



; ## TO_BODY ( xt -- addr ) "Return a word's Code Field Area (CFA)"
; ## ">body"  auto  ANS core
        ; """https://forth-standard.org/standard/core/toBODY
        ; Given a word's execution token (xt), return the address of the
        ; start of that word's parameter field (PFA). This is defined as the
        ; address that HERE would return right after CREATE.
        ;
        ; This is a
        ; difficult word for STC Forths, because most words don't actually
        ; have a Code Field Area (CFA) to skip. We solve this by having CREATE
        ; add a flag, "has CFA" (HC), in the header so >BODY know to skip
        ; the subroutine jumps to DOVAR, DOCONST, or DODOES
        ; """

xt_to_body:
                jsr underflow_1
w_to_body:
                ; Ideally, xt already points to the CFA. We just need to check
                ; the HC flag for special cases
                jsr w_dup              ; ( xt xt )
                jsr w_int_to_name      ; ( xt nt )

                ; The status byte is nt+1
                inc 0,x
                bne +
                inc 1,x
+
                lda (0,x)               ; get status byte
                and #HC
                beq _no_cfa

                ; We've got a DOVAR, DOCONST, DODEFER, DODOES or whatever,
                ; so we add three to xt, which is NOS
                clc
                lda 2,x         ; LSB
                adc #3
                sta 2,x
                bcc _no_cfa
                inc 3,x         ; MSB
_no_cfa:
                inx             ; get rid of the nt
                inx
z_to_body:      rts



; ## TO_IN ( -- addr ) "Return address of the input pointer"
; ## ">in"  auto  ANS core
xt_to_in:
w_to_in:
                dex
                dex

                lda #<toin
                sta 0,x
                lda #>toin      ; paranoid, should be zero
                sta 1,x

z_to_in:        rts



; ## TO_NUMBER ( ud addr u -- ud addr u ) "Convert a number"
; ## ">number"  auto  ANS core
        ; """https://forth-standard.org/standard/core/toNUMBER
        ; Convert a string to a double number. Logic here is based on the
        ; routine by Phil Burk of the same name in pForth, see
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; for the original Forth code. We arrive here from NUMBER which has
        ; made sure that we don't have to deal with a sign and we don't have
        ; to deal with a dot as a last character that signalizes double -
        ; this should be a pure number string.
        ;
        ; This routine calles UM*, which uses tmp1, tmp2 and tmp3, so we
        ; cannot access any of those.
        ;
        ; For the math routine, we move the inputs to the scratchpad to
        ; avoid having to fool around with the Data Stack.
        ;
        ;     +-----+-----+-----+-----+-----+-----+-----+-----+
        ;     |   UD-LO   |   UD-HI   |     N     | UD-HI-LO  |
        ;     |           |           |           |           |
        ;     |  S    S+1 | S+2   S+3 | S+4   S+5 | S+6   S+7 |
        ;     +-----+-----+-----+-----+-----+-----+-----+-----+
        ;
        ; The math routine works by converting one character to its
        ; numerical value (N) via DIGIT? and storing it in S+4 for
        ; the moment. We then multiply the UD-HI value with the radix
        ; (from BASE) using UM*, which returns a double-cell result. We
        ; discard the high cell of that result (UD-HI-HI) and store the
        ; low cell (UD-HI-LO) in S+6 for now. -- The second part is
        ; multiplying UD-LO with the radix. The high cell (UD-LO-HI)
        ; gets put in S+2, the low cell (HD-LO-LO) in S. We then use
        ; a version of D+ to add ( S S+2 ) and ( S+4 S+6) together,
        ; storing the result back in S and S+2, before we start another
        ; round with it as the new UD-LO and UD-HI.
        ; """

xt_to_number:
                jsr underflow_4
w_to_number:
                ; Fill the scratchpad. We arrive with ( ud-lo ud-hi addr u ).
                ; After this step, the original ud-lo and ud-hi will still be on
                ; the Data Stack, but will be ignored and later overwritten
                ; If >NUMBER is called by NUMBER, these should be all zeros
                lda 6,x         ; ud-lo LSB
                sta scratch
                lda 7,x         ; ud-lo MSB
                sta scratch+1

                lda 4,x         ; ud-hi LSB
                sta scratch+2
                lda 5,x         ; ud-hi MSB
                sta scratch+3

                ; Push down one on the Data Stack to use TOS for character
                ; conversion ( ud-lo ud-hi addr u x )
                dex
                dex

_loop:
                ; Get one character based on address
                lda (4,x)
                sta 0,x                 ; ( ud-lo ud-hi addr u char )
                stz 1,x                 ; paranoid

                jsr w_digit_question   ; ( char -- n -1 | char 0 )

                ; This gives us ( ud-lo ud-hi addr u char f | n f ), so we
                ; check the flag. If it is zero, we return what we have and
                ; let the caller (usually NUMBER) complain
                lda 0,x
                bne _digit_ok

                inx
                inx
                bra _done       ; ( ud-lo ud-hi addr u char )

_digit_ok:
                ; Conversion was successful. We arrive here with
                ; ( ud-lo ud-hi addr u n -1 ) and can start the
                ; math routine

                ; Save n so we don't have to fool around with the
                ; Data Stack
                lda 2,x
                sta scratch+4
                lda 3,x
                sta scratch+5

                ; Now multiply ud-hi (the one in the scratchpad, not the
                ; original one on the Data Stack) with the radix from BASE.
                ; We can clobber TOS and NOS because we saved n
                lda scratch+2
                sta 2,x         ; NOS
                lda scratch+3
                sta 3,x

                lda base
                sta 0,x         ; TOS
                stz 1,x         ; now ( ud-lo ud-hi addr u ud-hi base)

                ; UM* returns a double-celled number
                jsr w_um_star  ; ( ud-lo ud-hi addr u ud-hi-lo ud-hi-hi )

                ; Move ud-hi-lo to safety
                lda 2,x         ; ud-hi-lo
                sta scratch+6
                lda 3,x
                sta scratch+7

                ; Now we multiply ud-lo, overwriting the stack entries
                lda scratch
                sta 2,x
                lda scratch+1
                sta 3,x         ; ( ud-lo ud-hi addr u ud-lo ud-hi-hi )

                lda base
                sta 0,x
                stz 1,x         ; ( ud-lo ud-hi addr u ud-lo base )

                jsr w_um_star  ; ( ud-lo ud-hi addr u ud-lo-lo ud-lo-hi )

                lda 0,x
                sta scratch+2
                lda 1,x
                sta scratch+3

                lda 2,x
                sta scratch
                lda 3,x
                sta scratch+1

                ; We add ud-lo and n, as well as ud-hi and ud-hi-lo,
                ; both in the scratch pad
                clc
                lda scratch     ; ud-lo LSB
                adc scratch+4   ; n LSB
                sta scratch     ; this is the new ud-lo
                lda scratch+1   ; ud-lo MSB
                adc scratch+5   ; n MSB
                sta scratch+1

                lda scratch+2   ; LSB
                adc scratch+6
                sta scratch+2   ; this is the new ud-hi
                lda scratch+3   ; MSB
                adc scratch+7
                sta scratch+3

                ; Clean up: Get rid of one of the two top elements on
                ; the Data Stack. We don't really care which one
                inx
                inx             ; ( ud-lo ud-hi addr u ud-lo-lo )

                ; One character down. Move address up
                inc 4,x
                bne +
                inc 5,x
+
                ; Decrease counter
                dec 2,x
                bne _loop

_done:
                ; Counter has reached zero or we have an error. In both
                ; cases, we clean up the Data Stack and return. Error gives
                ; us ( ud-lo ud-hi addr u char ), regular end is
                ; ( ud-lo ud-hi addr u ud-lo )
                inx
                inx             ; ( ud-lo ud-hi addr u )

                ; The new ud-lo and ud-hi are still on the scratch pad
                lda scratch     ; new ud-lo
                sta 6,x
                lda scratch+1
                sta 7,x

                lda scratch+2
                sta 4,x
                lda scratch+3
                sta 5,x

z_to_number:    rts



; ## TO_R ( n -- )(R: -- n) "Push TOS to the Return Stack"
; ## ">r"  auto  ANS core
        ; """https://forth-standard.org/standard/core/toR
        ; This word is handled differently for native and for
        ; subroutine coding, see `COMPILE,`. This is a complile-only
        ; word.
        ; """
xt_to_r:
                ; we can't avoid underflow check here due to the stack prologue
w_to_r:
                ; --- START FOR JSR (save return address + 1) ---

                pla                     ; LSB
                ply                     ; MSB
                inc a
                sta tmp1                ; LSB
                bne +
                iny
+
                sty tmp1+1              ; MSB

                ; --- START FOR NATIVE COMPILE (via ST flag) ---

                ; We check for underflow in the second step, so we can
                ; strip off the stack thrashing for native compiling first

                jsr underflow_1

                ; now we can do the actual work
                lda 1,x         ; MSB
                pha
                lda 0,x         ; LSB
                pha

                inx
                inx

                ; --- CUT FOR NATIVE COMPILE ---

z_to_r:         jmp (tmp1)



; ## TRUE ( -- f ) "Push TRUE flag to Data Stack"
; ## "true"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TRUE"""
xt_true:
w_true:
                dex
                dex
                lda #$FF
                sta 0,x
                sta 1,x

z_true:         rts


; ## TUCK ( b a -- a b a ) "Copy TOS below NOS"
; ## "tuck"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TUCK"""
xt_tuck:
                jsr underflow_2
w_tuck:
                dex
                dex

                ldy 4,x         ; LSB
                lda 2,x
                sta 4,x
                sty 2,x
                sta 0,x

                ldy 5,x         ; MSB
                lda 3,x
                sta 5,x
                sty 3,x         ; bba
                sta 1,x         ; baa

z_tuck:         rts



; ## TWO_DROP ( n n -- ) "Drop TOS and NOS"
; ## "2drop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoDROP"""
xt_two_drop:
                jsr underflow_2
w_two_drop:
                inx
                inx
                inx
                inx

z_two_drop:     rts



; ## TWO_DUP ( a b -- a b a b ) "Duplicate first two stack elements"
; ## "2dup"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoDUP"""
xt_two_dup:
                jsr underflow_2
w_two_dup:
                dex
                dex
                dex
                dex

                lda 4,x         ; TOS
                sta 0,x
                lda 5,x
                sta 1,x

                lda 6,x         ; NOS
                sta 2,x
                lda 7,x
                sta 3,x

z_two_dup:      rts



; ## TWO_FETCH ( addr -- n1 n2 ) "Fetch the cell pair n1 n2 stored at addr"
; ## "2@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoFetch
        ; Note n2 stored at addr and n1 in the next cell -- in our case,
        ; the next two bytes. This is equvalent to  `DUP CELL+ @ SWAP @`
        ; """
xt_two_fetch:
                jsr underflow_1
w_two_fetch:
                lda 0,x
                sta tmp1
                ldy 1,x
                sty tmp1+1

                dex             ; reuse one stack element
                dex

                lda (tmp1)      ; copy LSB
                sta 0,x
                ldy #1          ; copy next
                lda (tmp1),y
                sta 1,x
                iny             ; copy next
                lda (tmp1),y
                sta 2,x
                iny             ; copy next
                lda (tmp1),y
                sta 3,x

z_two_fetch:    rts



; ## TWO_OVER ( d1 d2 -- d1 d2 d1 ) "Copy double word NOS to TOS"
; ## "2over"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoOVER"""
xt_two_over:
                jsr underflow_4
w_two_over:
                dex
                dex
                dex
                dex

                lda 8,x
                sta 0,x

                lda 9,x
                sta 1,x

                lda 10,x
                sta 2,x

                lda 11,x
                sta 3,x

z_two_over:     rts



; ## TWO_R_FETCH ( -- n n ) "Copy top two entries from Return Stack"
; ## "2r@"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TwoRFetch
        ;
        ; This is R> R> 2DUP >R >R SWAP but we can do it a lot faster in
        ; assembler. We use trickery to access the elements on the Return
        ; Stack instead of pulling the return address first and storing
        ; it somewhere else like for 2R> and 2>R. In this version, we leave
        ; it as Never Native; at some point, we should compare versions to
        ; see if an Always Native version would be better
        ; """
xt_two_r_fetch:
w_two_r_fetch:
                ; --- START FOR JSR (save return address + 1) ---

                pla                     ; LSB
                ply                     ; MSB
                inc a
                sta tmp1                ; LSB
                bne +
                iny
+
                sty tmp1+1              ; MSB

                ; --- START FOR NATIVE COMPILE (via ST flag) ---

                ; copy four bytes from return stack to the data stack

                txa             ; arrange for Y = SP; X -= 4
                tsx
                phx             ; 65c02 has no TXY, so do it the hard way
                ply
                sec
                sbc #4
                tax

                lda $101,y
                sta 0,x
                lda $102,y
                sta 1,x
                lda $103,y
                sta 2,x
                lda $104,y
                sta 3,x

                ; --- CUT FOR NATIVE COMPILE ---

z_two_r_fetch:  jmp (tmp1)



; ## TWO_R_FROM ( -- n1 n2 ) (R: n1 n2 -- ) "Pull two cells from Return Stack"
; ## "2r>"  auto  ANS core ext
	    ; """https://forth-standard.org/standard/core/TwoRfrom
        ; Pull top two entries from Return Stack.
        ;
        ; Is the same as
        ; R> R> SWAP. As with R>, the problem with the is word is that
        ; the top value on the ReturnStack for a STC Forth is the
        ; return address, which we need to get out of the way first.
        ; Native compile needs to be handled as a special case.
        ; """
xt_two_r_from:
w_two_r_from:
                ; --- START FOR JSR (save return address + 1) ---

                pla
                ply                     ; MSB
                inc a
                sta tmp1                ; LSB
                bne +
                iny
+
                sty tmp1+1              ; MSB

                ; --- START FOR NATIVE COMPILE (via ST flag) ---

                ; In theory, we should test for underflow on the Return
                ; Stack. However, given the traffic there with an STC
                ; Forth, that's probably not really useful

		; make room on stack
                dex
                dex
                dex
                dex

                pla                     ; LSB
                sta 0,x
                pla                     ; MSB
                sta 1,x

                pla                     ; LSB
                sta 2,x
                pla                     ; MSB
                sta 3,x

                ; --- CUT FOR NATIVE COMPILE ---

z_two_r_from:   jmp (tmp1)



; ## TWO_SLASH ( n -- n ) "Divide TOS by two"
; ## "2/"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoDiv"""
xt_two_slash:
                jsr underflow_1
w_two_slash:
                ; We can't just LSR the LSB and ROR the MSB because that
                ; would do bad things to the sign
                lda 1,x
                asl                     ; save the sign
                ror 1,x
                ror 0,x

z_two_slash:    rts



; ## TWO_STAR ( n -- n ) "Multiply TOS by two"
; ## "2*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoTimes
        ;
        ; Also used for CELLS
        ; """
xt_two_star:
xt_cells:
                jsr underflow_1
w_two_star:
w_cells:
                asl 0,x
                rol 1,x
z_cells:
z_two_star:     rts



; ## TWO_STORE ( n1 n2 addr -- ) "Store two numbers at given address"
; ## "2!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoStore
        ; Stores so n2 goes to addr and n1 to the next consecutive cell.
        ; Is equivalent to  `SWAP OVER ! CELL+ !`
        ; """
xt_two_store:
                jsr underflow_3
w_two_store:
                lda 0,x
                sta tmp1
                ldy 1,x
                sty tmp1+1

                inx
                inx

                lda 0,x         ; copy MSB
                sta (tmp1)
                lda 1,x         ; copy next
                ldy #1
                sta (tmp1),y
                lda 2,x         ; copy next
                iny
                sta (tmp1),y
                lda 3,x         ; copy MSB
                iny
                sta (tmp1),y

                inx             ; 2DROP
                inx
                inx
                inx

z_two_store:    rts



; ## TWO_SWAP ( n1 n2 n3 n4 -- n3 n4 n1 n1 ) "Exchange two double words"
; ## "2swap"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoSWAP"""
xt_two_swap:
                jsr underflow_4
w_two_swap:
                ; 0 <-> 4
                lda 0,x
                ldy 4,x
                sta 4,x
                sty 0,x

                ; 1 <-> 5
                lda 1,x
                ldy 5,x
                sta 5,x
                sty 1,x

                ; 2 <-> 6
                lda 2,x
                ldy 6,x
                sta 6,x
                sty 2,x

                ; 3 <-> 7
                lda 3,x
                ldy 7,x
                sta 7,x
                sty 3,x

z_two_swap:     rts



; ## TWO_TO_R ( n1 n2 -- )(R: -- n1 n2 "Push top two entries to Return Stack"
; ## "2>r"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TwotoR
        ; Push top two entries to Return Stack.
        ;
        ; The same as SWAP >R >R
        ; except that if we jumped here, the return address will be in the
        ; way. May not be natively compiled unless we're clever and use
        ; special routines.
        ; """
xt_two_to_r:
                ; we can't avoid the underflow check here due to the stack prologue
w_two_to_r:
                ; --- START FOR JSR (save return address + 1) ---

                pla                     ; LSB
                ply                     ; MSB
                inc a
                sta tmp1                ; LSB
                bne +
                iny
+
                sty tmp1+1              ; MSB

                ; --- START FOR NATIVE COMPILE (via ST flag) ---

                jsr underflow_2

                ; now we can move the data
                lda 3,x         ; MSB
                pha
                lda 2,x         ; LSB
                pha

                ; now we can move the data
                lda 1,x         ; MSB
                pha
                lda 0,x         ; LSB
                pha

                inx
                inx
                inx
                inx

                ; --- CUT FOR NATIVE COMPILE ---

z_two_to_r:     jmp (tmp1)




; ## TYPE ( addr u -- ) "Print string"
; ## "type"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TYPE
        ; Works through EMIT to allow OUTPUT revectoring.
        ; """

xt_type:
                jsr underflow_2
w_type:
                ; Save the starting address into tmp1
                lda 2,x
                sta tmp1
                lda 3,x
                sta tmp1+1
_loop:
                ; done if length is zero
                lda 0,x
                ora 1,x
                beq _done

                ; Send the current character
                lda (tmp1)
                jsr emit_a      ; avoids stack foolery

                ; Move the address along (in tmp1)
                inc tmp1
                bne +
                inc tmp1+1
+
                ; Reduce the count (on the data stack)
                lda 0,x
                bne +
                dec 1,x
+
                dec 0,x

                bra _loop
_done:
                inx
                inx
                inx
                inx

z_type:         rts



; ## U_DOT ( u -- ) "Print TOS as unsigned number"
; ## "u."  tested  ANS core
        ; """https://forth-standard.org/standard/core/Ud
        ;
        ; This is : U. 0 <# #S #> TYPE SPACE ; in Forth
        ; We use the internal assembler function print_u followed
        ; by a single space
        ; """
xt_u_dot:
                jsr underflow_1
w_u_dot:
                jsr print_u
                lda #AscSP
                jsr emit_a

z_u_dot:        rts


; ## U_DOT_R ( u u -- ) "Print NOS as unsigned number right-justified with TOS width"
; ## "u.r"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/UDotR"""
xt_u_dot_r:
                jsr underflow_2
w_u_dot_r:
                jsr w_to_r
                jsr w_zero
                jsr w_less_number_sign
                jsr w_number_sign_s
                jsr w_number_sign_greater
                jsr w_r_from
                jsr w_over
                jsr w_minus
                jsr w_spaces
                jsr w_type

z_u_dot_r:      rts


; ## U_GREATER_THAN ( n m -- f ) "Return true if NOS > TOS (unsigned)"
; ## "u>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Umore"""
xt_u_greater_than:
                jsr underflow_2
w_u_greater_than:
                lda 0,x
                cmp 2,x
                lda 1,x
                sbc 3,x
                inx
                inx

                lda #0
                adc #$FF
                sta 0,x         ; store flag
                sta 1,x

z_u_greater_than:    rts


; ## U_LESS_THAN ( n m -- f ) "Return true if NOS < TOS (unsigned)"
; ## "u<"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Uless"""
xt_u_less_than:
                jsr underflow_2
w_u_less_than:
                lda 2,x
                cmp 0,x
                lda 3,x
                sbc 1,x
                inx
                inx

                lda #0
                adc #$FF
                sta 0,x         ; store flag
                sta 1,x

z_u_less_than:    rts


; ## UM_SLASH_MOD ( ud u -- ur u ) "32/16 -> 16 division"
; ## "um/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/UMDivMOD
        ; Divide double cell number by single cell number, returning the
        ; quotient as TOS and any remainder as NOS. All numbers are unsigned.
        ; This is the basic division operation all others use. Based on FIG
        ; Forth code, modified by Garth Wilson, see
        ; http://6502.org/source/integers/ummodfix/ummodfix.htm
        ;
        ; This uses tmp1, tmp1+1, and tmptos
        ; """

xt_um_slash_mod:
                jsr underflow_3
w_um_slash_mod:
                ; catch division by zero
                lda 0,x
                ora 1,x
                bne _not_zero

                lda #err_divzero
                jmp error

_not_zero:
                ; We loop 17 times
                lda #17
                sta tmptos

_loop:
                ; rotate low cell of dividend one bit left (LSB)
                rol 4,x
                rol 5,x

                ; loop control
                dec tmptos
                beq _done

                ; rotate high cell of dividend one bit left (MSB)
                rol 2,x
                rol 3,x

                stz tmp1        ; store the bit we got from hi cell (MSB)
                rol tmp1

                ; subtract dividend hi cell minus divisor
                sec
                lda 2,x
                sbc 0,x
                sta tmp1+1
                lda 3,x
                sbc 1,x

                tay
                lda tmp1
                sbc #0
                bcc _loop

                ; make result new dividend high cell
                lda tmp1+1
                sta 2,x
                sty 3,x         ; used as temp storage

                bra _loop
_done:
                inx
                inx

                jsr w_swap

z_um_slash_mod: rts



; ## UM_STAR ( u u -- ud ) "Multiply 16 x 16 -> 32"
; ## "um*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/UMTimes
        ; Multiply two unsigned 16 bit numbers, producing a 32 bit result.
        ; Old Forth versions such as FIG Forth call this U*
        ;
        ; This is based on modified FIG Forth code by Dr. Jefyll, see
        ; http://forum.6502.org/viewtopic.php?f=9&t=689 for a detailed
        ; discussion.
        ;
        ; We don't use the system scratch pad (SYSPAD) for temp
        ; storage because >NUMBER uses it as well, but instead tmp1 to
        ; tmp3 (tmp1 is N in the original code, tmp1+1 is N+1, etc).
        ;
        ; Consider switching to a table-supported version based on
        ; http://codebase64.org/doku.php?id=base:seriously_fast_multiplication
        ; http://codebase64.org/doku.php?id=magazines:chacking16#d_graphics_for_the_masseslib3d>
        ; http://forum.6502.org/viewtopic.php?p=205#p205
        ; http://forum.6502.org/viewtopic.php?f=9&t=689
        ; """

xt_um_star:
                jsr underflow_2
w_um_star:
                ; to eliminate clc inside the loop, the value at
                ; tmp1 is reduced by 1 in advance
                clc
                lda 0,x         ; copy TOS to tmp2
                sbc #0
                sta tmp2

                lda 1,x
                sbc #0
                bcc _zero       ; is TOS zero?
                sta tmp2+1

                lda #0
                sta tmp1
                stx tmp3        ; tested for exit from outer loop
                dex
                dex

_outer_loop:
                ldy #8          ; counter inner loop
                lsr 4,x         ; think "2,x" then later "3,x"

_inner_loop:
                bcc _no_add
                sta tmp1+1      ; save time, don't CLC
                lda tmp1
                adc tmp2
                sta tmp1
                lda tmp1+1
                adc tmp2+1

_no_add:
                ror
                ror tmp1
                ror 4,x         ; think "2,x" then later "3,x"

                dey
                bne _inner_loop ; go back for one more shift?

                inx
                cpx tmp3
                bne _outer_loop ; go back for eight more shifts?

                ; all done, store high word of result
                sta 1,x
                lda tmp1
                sta 0,x
                bra _done

_zero:
                stz 2,x
                stz 3,x
_done:
z_um_star:      rts



; ## UNLOOP ( -- ) "Drop current loop control block"
; ## "unloop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/UNLOOP"""
xt_unloop:
w_unloop:
                ; This is used as an epliogue to each LOOP/+LOOP
                ; as well as prior to EXIT'ng a loop
                ; We need to drop the current loop control block
                ; and restore the cached loopidx0 of the prior loop, if any

                ldy loopctrl
                dey
                dey
                dey
                dey
                sty loopctrl
                bmi z_unloop            ; no active loops?

                lda loopindex,y         ; else re-cache the LSB of loopindex
                sta loopidx0

z_unloop:       rts


; ## UNTIL (C: dest -- ) ( -- ) "Loop flow control"
; ## "until"  auto  ANS core
        ; """http://forth-standard.org/standard/core/UNTIL"""
xt_until:
                jsr underflow_1
w_until:
                ; The address to loop back to is on the stack.
                jsr cmpl_0branch_tos

z_until:        rts


; ## UNUSED ( -- u ) "Return size of space available to Dictionary"
; ## "unused"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/UNUSED
        ; UNUSED does not include the ACCEPT history buffers. Total RAM
        ; should be HERE + UNUSED + <history buffer size>, the last of which
        ; defaults to $400
        ; """
xt_unused:
w_unused:
                dex
                dex

                lda #<cp_end
                sec
                sbc cp
                sta 0,x

                lda #>cp_end
                sbc cp+1
                sta 1,x

z_unused:       rts



; ## VALUE ( n "name" -- ) "Define a value"
; ## "value"  auto  ANS core
        ; """https://forth-standard.org/standard/core/VALUE
        ;
        ; This is a dummy header for the WORDLIST. The actual code is
        ; identical to that of CONSTANT
        ; """


; ## VARIABLE ( "name" -- ) "Define a variable"
; ## "variable"  auto  ANS core
        ; """https://forth-standard.org/standard/core/VARIABLE
        ; There are various Forth definitions for this word, such as
        ; `CREATE 1 CELLS ALLOT`  or  `CREATE 0 ,`  We use a variant of the
        ; second one so the variable is initialized to zero
        ; """
xt_variable:
w_variable:
                ; we let CREATE do the heavy lifting
                jsr w_create

                ; there is no "STZ (CP)" so we have to do this the hard
                ; way
                lda #0

                sta (cp)
                inc cp
                bne +
                inc cp+1
+
                sta (cp)
                inc cp
                bne +
                inc cp+1
+
                ; Now we need to adjust the length of the complete word by two
                jsr adjust_z

z_variable:     rts


; ## WHILE ( C: dest -- orig dest ) ( x -- ) "Loop flow control"
; ## "while"  auto  ANS core
        ; """http://forth-standard.org/standard/core/WHILE"""
xt_while:
                jsr underflow_1
w_while:
                jsr cmpl_0branch_later          ; branch to location we'll determine later
                ; tuck the address of the branch placeholder under the repeat address left by begin
                jsr w_swap
                ; ( branch-target repeat-target )

z_while:        rts


; ## WITHIN ( n1 n2 n3 -- ) "Test n1 within range [n2, n3) or outwith [n3, n2)"
; ## "within"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/WITHIN
        ;
        ; This an assembler version of the ANS Forth implementation
        ; at https://forth-standard.org/standard/core/WITHIN which is
        ; OVER - >R - R> U<  note there is an alternative high-level version
        ; ROT TUCK > -ROT > INVERT AND
        ; """"
xt_within:
                jsr underflow_3
w_within:
                jsr w_over
                jsr w_minus
                jsr w_to_r
                jsr w_minus
                jsr w_r_from
                jsr w_u_less_than

z_within:       rts


; ## WORD ( char "name " -- caddr ) "Parse input stream"
; ## "word"  auto  ANS core
        ; """https://forth-standard.org/standard/core/WORD
        ; Obsolete parsing word included for backwards compatibility only.
        ; Do not use this, use `PARSE` or `PARSE-NAME`. Skips leading delimiters
        ; and copies word to storage area for a maximum size of 255 bytes.
        ; Returns the result as a counted string (requires COUNT to convert
        ; to modern format), and inserts a space after the string. See "Forth
        ; Programmer's Handbook" 3rd edition p. 159 and
        ; http://www.forth200x.org/documents/html/rationale.html#rat:core:PARSE
        ; for discussions of why you shouldn't be using WORD anymore.
        ;
        ; Forth
        ; would be   PARSE DUP BUFFER1 C! OUTPUT 1+ SWAP MOVE BUFFER1
        ; We only allow input of 255 chars. Seriously, use PARSE-NAME.
        ; """

xt_word:
                jsr underflow_1
w_word:
                ; Skip over leading delimiters - this is like PARSE-NAME,
                ; but unlike PARSE
                ldy toin                ; >IN
_loop:
                cpy ciblen              ; quit if end of input
                beq _found_char
                lda (cib),y
                cmp 0,x                 ; ASCII of delimiter
                bne _found_char

                iny
                bra _loop
_found_char:
                ; Save index of where word starts
                sty toin

                ; The real work is done by parse
                jsr w_parse            ; Returns ( addr u )

                ; Convert the modern ( addr u ) string format to obsolete
                ; ( caddr ) format. We just do this in the Dictionary
                lda 0,x
                sta (cp)                ; Save length of string
                pha                     ; Keep copy of length for later

                jsr w_dup              ; ( addr u u )
                lda cp
                clc
                adc #1
                sta 2,x                 ; LSB of CP
                lda cp+1
                adc #0
                sta 3,x                 ; ( addr cp+1 u )

                jsr w_move

                ; Return caddr
                dex
                dex
                lda cp
                sta 0,x
                lda cp+1
                sta 1,x

                ; Adjust CP
                pla                     ; length of string
                clc
                adc cp
                sta cp
                bcc z_word
                inc cp+1
z_word:         rts



; ## XOR ( n n -- n ) "Logically XOR TOS and NOS"
; ## "xor"  auto  ANS core
        ; """https://forth-standard.org/standard/core/XOR"""
xt_xor:
                jsr underflow_2
w_xor:
                lda 0,x
                eor 2,x
                sta 2,x

                lda 1,x
                eor 3,x
                sta 3,x

                inx
                inx

z_xor:          rts



; ## ZERO_EQUAL ( n -- f ) "Check if TOS is zero"
; ## "0="  auto  ANS core
        ; """https://forth-standard.org/standard/core/ZeroEqual"""

xt_zero_equal:
                jsr underflow_1
w_zero_equal:
                lda 0,x
                ora 1,x
                beq _zero       ; if 0, A is inverse of the TRUE (-1) we want
                lda #$FF        ; else set A inverse of the FALSE (0) we want
_zero:
                eor #$FF        ; now just invert:
                sta 0,x
                sta 1,x

z_zero_equal:   rts



; ## ZERO_GREATER ( n -- f ) "Return a TRUE flag if TOS is positive"
; ## "0>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Zeromore"""

xt_zero_greater:
                jsr underflow_1
w_zero_greater:
                ldy #0          ; Default is FALSE (TOS is negative)

                lda 1,x         ; MSB
                bmi _done       ; TOS is negative, keep FLASE
                ora 0,x
                beq _done       ; TOS is zero, keep FALSE

                dey             ; TOS is postive, make true
_done:
                tya
                sta 0,x
                sta 1,x

z_zero_greater: rts



; ## ZERO_LESS ( n -- f ) "Return a TRUE flag if TOS negative"
; ## "0<"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Zeroless"""

xt_zero_less:
                jsr underflow_1
w_zero_less:
                ldy #0          ; Default is FALSE (TOS positive)

                lda 1,x         ; MSB
                bpl _done       ; TOS is positive, so keep FALSE

                dey             ; TOS is negative, make TRUE
_done:
                tya
                sta 0,x
                sta 1,x

z_zero_less:    rts



; ## ZERO_UNEQUAL ( m -- f ) "Return TRUE flag if not zero"
; ## "0<>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Zerone"""

xt_zero_unequal:
                jsr underflow_1
w_zero_unequal:
                lda 0,x
                ora 1,x
                beq _zero
                lda #$FF
_zero:
                sta 0,x
                sta 1,x

z_zero_unequal: rts

; END
