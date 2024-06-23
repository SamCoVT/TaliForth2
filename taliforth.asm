; Tali Forth 2 for the 65c02
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; Patrick Surry
; First version: 19. Jan 2014 (Tali Forth 1)
; This version: 21. Apr 2024 (Version 1.1)

; This is the main file for Tali Forth 2


; These assignments are "weak" and will only assign if the label
; does not have anything assigned to it.  The user can override these
; defaults by assigning values in their platform file before
; including this file.

; Assemble all words unless overridden in the platform file.
TALI_OPTIONAL_WORDS :?= [ "ed", "editor", "ramdrive", "block", "environment?", "assembler", "wordlist" ]

; Default line ending is line feed.
TALI_OPTION_CR_EOL :?= [ "lf" ]

; Default to verbose strings
TALI_OPTION_TERSE :?= 0

; Default to ctrl-n/p accept history
TALI_OPTION_HISTORY :?= 1

; Label used to calculate UNUSED based on the hardware configuration in platform/
code0:

; Entry point for Tali Forth after kernel hardware setup
forth:

.include "words/all.asm"           ; Native Forth words. Starts with COLD
.include "definitions.asm"      ; Top-level definitions, memory map
                                ; included here to put relocatable tables after native words
.if "disassembler" in TALI_OPTIONAL_WORDS || "assembler" in TALI_OPTIONAL_WORDS
    .include "opcodes.asm"
.endif

; High-level Forth words, see forth_code/README.md
forth_words_start:
.if ! TALI_OPTION_TERSE         ; omit startup strings if terse
.binary "forth_words.asc"
.endif
forth_words_end:

; User-defined Forth words, see forth_code/README.md
user_words_start:
.binary "user_words.asc"
user_words_end:

.include "words/headers.asm"          ; Headers of native words
.include "strings.asm"          ; Strings, including error messages


; =====================================================================
; CODE FIELD ROUTINES

doconst:
        ; """Execute a CONSTANT: Push the data in the first two bytes of
        ; the Data Field onto the Data Stack
        ; """
                dex             ; make room for constant
                dex

                ; The value we need is stored in the two bytes after the
                ; JSR return address, which in turn is what is on top of
                ; the Return Stack
                pla             ; LSB of return address
                sta tmp1
                pla             ; MSB of return address
                sta tmp1+1

                ; Start LDY with 1 instead of 0 because of how JSR stores
                ; the return address on the 65c02
                ldy #1
                lda (tmp1),y
                sta 0,x
                iny
                lda (tmp1),y
                sta 1,x

                ; This takes us back to the original caller, not the
                ; DOCONST caller
                rts


dodefer:
        ; """Execute a DEFER statement at runtime: Execute the address we
        ; find after the caller in the Data Field
        ; """
                ; The xt we need is stored in the two bytes after the JSR
                ; return address, which is what is on top of the Return
                ; Stack. So all we have to do is replace our return jump
                ; with what we find there
                pla             ; LSB
                sta tmp1
                pla             ; MSB
                sta tmp1+1

                ldy #1
                lda (tmp1),y
                sta tmp2
                iny
                lda (tmp1),y
                sta tmp2+1

                jmp (tmp2)      ; This is actually a jump to the new target

defer_error:
                ; """Error routine for undefined DEFER: Complain and abort"""
                lda #err_defer
                jmp error

dodoes:
        ; """Execute the runtime portion of DOES>. See DOES> and
        ; docs/create-does.txt for details and
        ; http://www.bradrodriguez.com/papers/moving3.htm
        ; """
                ; Assumes the address of the CFA of the original defining word
                ; (say, CONSTANT) is on the top of the Return Stack. Save it
                ; for a later jump, adding one byte because of the way the
                ; 6502 works
                ply             ; LSB
                pla             ; MSB
                iny
                bne +
                ina
+
                sty tmp2
                sta tmp2+1

                ; Next on the Return Stack should be the address of the PFA of
                ; the calling defined word (say, the name of whatever constant we
                ; just defined). Move this to the Data Stack, again adding one.
                dex
                dex

                ply
                pla
                iny
                bne +
                ina
+
                sty 0,x         ; LSB
                sta 1,x         ; MSB

                ; This leaves the return address from the original main routine
                ; on top of the Return Stack. We leave that untouched and jump
                ; to the special code of the defining word. It's RTS instruction
                ; will take us back to the main routine
                jmp (tmp2)


dovar:
        ; """Execute a variable: Push the address of the first bytes of
        ; the Data Field onto the stack. This is called with JSR so we
        ; can pick up the address of the calling variable off the 65c02's
        ; stack. The final RTS takes us to the original caller of the
        ; routine that itself called DOVAR. This is the default
        ; routine installed with CREATE.
        ; """
                ; Pull the return address off the machine's stack, adding
                ; one because of the way the 65c02 handles subroutines
                ply             ; LSB
                pla             ; MSB
                iny
                bne +
                ina
+
                dex
                dex

                sta 1,x
                tya
                sta 0,x

                rts

; =====================================================================
; LOW LEVEL HELPER FUNCTIONS

push_upvar_tos:
        ; """Write addr of user page variable with offset A to TOS"""
                dex
                dex
                clc
                adc up
                sta 0,x
                lda up+1
                bcc +
                ina
+
                sta 1,x
                rts

byte_to_ascii:
        ; """Convert byte in A to two ASCII hex digits and EMIT them"""
                pha
                lsr             ; convert high nibble first
                lsr
                lsr
                lsr
                jsr _nibble_to_ascii
                pla

                ; fall through to _nibble_to_ascii

_nibble_to_ascii:
        ; """Private helper function for byte_to_ascii: Print lower nibble
        ; of A and and EMIT it. This does the actual work.
        ; """
                and #$F
                ora #'0'
                cmp #'9'+1
                bcc +
                adc #6

+               jmp emit_a

                rts


find_header_name:
        ; """Given a string on the stack ( addr  n ) with n at most 255
        ; and tmp1 pointing at an NT header, search each
        ; linked header looking for a matching name.
        ; Each header has length at NT, name at NT+8
        ; and next header pointer at NT+2 with 0 marking the end.
        ; On success tmp1 points at the matching NT, with A=$FF and Z=0.
        ; On failure tmp1 is 0, A=0 and Z=1.
        ; Stomps tmp2.  The stack is unchanged.
        ; """

                lda 2,x                 ; Copy mystery string to tmp2
                sta tmp2
                lda 3,x
                sta tmp2+1

_loop:
                ; first quick test: Are strings the same length?
                lda (tmp1)
                cmp 0,x
                bne _next_entry

                ; second quick test: could first characters be equal?
                lda (tmp2)      ; first character of mystery string
                ldy #8
                eor (tmp1),y    ; flag any mismatched bits
                and #%11011111  ; but ignore upper/lower case bit
                bne _next_entry ; definitely not equal if any bits differ

                ; Same length and probably same first character
                ; (though we still have to check properly).
                ; Suck it up and compare all characters. We go
                ; from back to front, because words like CELLS and CELL+ would
                ; take longer otherwise.

                ; The string of the word we're testing against is 8 bytes down
                lda tmp1
                pha             ; Save original address on the stack
                clc
                adc #8
                sta tmp1
                lda tmp1+1
                pha
                bcc +
                ina
                sta tmp1+1
+
                ldy 0,x         ; index is length of string minus 1
                dey

_next_char:
                lda (tmp2),y    ; last char of mystery string

                ; Lowercase the incoming charcter.
                cmp #'Z'+1
                bcs _check_char
                cmp #'A'
                bcc _check_char

                ; Convert uppercase letter to lowercase.
                ora #$20

_check_char:
                cmp (tmp1),y    ; last char of word we're testing against
                bne _reset_tmp1

                dey
                bpl _next_char

        ; if we fall through on success, and only then, Y is $FF
_reset_tmp1:
                pla
                sta tmp1+1
                pla
                sta tmp1

                tya             ; leave A = $FF on success
                iny             ; if Y was $FF, we succeeded
                beq _done

_next_entry:
                ; Otherwise move on to next header address
                ldy #2
                lda (tmp1),y
                pha
                iny
                lda (tmp1),y
                sta tmp1+1
                pla
                sta tmp1

                ; If we got a zero, we've walked the whole Dictionary and
                ; return as a failure, otherwise try again
                ora tmp1+1
                bne _loop

_done:          cmp #0      ; A is 0 on failure and $FF on success
                rts         ; so cmp #0 sets Z on failure and clears on success



compare_16bit:
        ; """Compare TOS/NOS and return results in form of the 65c02 flags
        ; Adapted from Leventhal "6502 Assembly Language Subroutines", see
        ; also http://www.6502.org/tutorials/compare_beyond.html
        ; For signed numbers, Z signals equality and N which number is larger:
        ;       if TOS = NOS: Z=1 and N=0
        ;       if TOS > NOS: Z=0 and N=0
        ;       if TOS < NOS: Z=0 and N=1
        ; For unsigned numbers, Z signals equality and C which number is larger:
        ;       if TOS = NOS: Z=1 and N=0
        ;       if TOS > NOS: Z=0 and C=1
        ;       if TOS < NOS: Z=0 and C=0
        ; Compared to the book routine, WORD1 (MINUED) is TOS
        ;                               WORD2 (SUBTRAHEND) is NOS
        ; """
                ; Compare LSB first to set the carry flag
                lda 0,x                 ; LSB of TOS
                cmp 2,x                 ; LSB of NOS
                beq _equal

                ; LSBs are not equal, compare MSB
                lda 1,x                 ; MSB of TOS
                sbc 3,x                 ; MSB of NOS
                bvs _overflow
                bra _not_equal
_equal:
                ; Low bytes are equal, so we compare high bytes
                lda 1,x                 ; MSB of TOS
                sbc 3,x                 ; MSB of NOS
                bvc _done
_overflow:
                ; Handle overflow because we use signed numbers
                eor #$80                ; complement negative flag
_not_equal:
                ora #1                  ; set Z=0 since we're not equal
_done:
                rts

current_to_dp:
        ; """Look up the current (compilation) dictionary pointer
        ; in the wordlist set and put it into the dp zero-page
        ; variable. Uses A and Y.
        ; """
                ; Determine which wordlist is current
                ldy #current_offset
                lda (up),y      ; current is a byte variable
                asl             ; turn it into an offset (in cells)

                ; Get the dictionary pointer for that wordlist.
                clc
                adc #wordlists_offset   ; add offset to wordlists base.
                tay
                lda (up),y              ; get the dp for that wordlist.
                sta dp
                iny
                lda (up),y
                sta dp+1

                rts


dp_to_current:
        ; """Look up which wordlist is current and update its pointer
        ; with the value in dp. Uses A and Y.
        ; """
                ; Determine which wordlist is current
                ldy #current_offset
                lda (up),y      ; current is a byte variable
                asl             ; turn it into an offset (in cells)

                ; Get the dictionary pointer for that wordlist.
                clc
                adc #wordlists_offset   ; add offset to wordlists base.
                tay
                lda dp
                sta (up),y              ; get the dp for that wordlist.
                iny
                lda dp+1
                sta (up),y

                rts

interpret:
        ; """Core routine for the interpreter called by EVALUATE and QUIT.
        ; Process one line only. Assumes that the address of name is in
        ; cib and the length of the whole input line string is in ciblen
        ; """
                ; Normally we would use PARSE here with the SPACE character as
                ; a parameter (PARSE replaces WORD in modern Forths). However,
                ; Gforth's PARSE-NAME makes more sense as it uses spaces as
                ; delimiters per default and skips any leading spaces, which
                ; PARSE doesn't
_loop:
                jsr w_parse_name       ; ( "string" -- addr u )

                ; If PARSE-NAME returns 0 (empty line), no characters were left
                ; in the line and we need to go get a new line
                lda 0,x
                ora 1,x
                beq _line_done

                ; Go to FIND-NAME to see if this is a word we know. We have to
                ; make a copy of the address in case it isn't a word we know and
                ; we have to go see if it is a number
                jsr w_two_dup          ; ( addr u -- addr u addr u )
                jsr w_find_name        ; ( addr u addr u -- addr u nt|0 )

                ; A zero signals that we didn't find a word in the Dictionary
                lda 0,x
                ora 1,x
                bne _got_name_token

                ; We didn't get any nt we know of, so let's see if this is
                ; a number.
                inx                     ; ( addr u 0 -- addr u )
                inx

                ; If the number conversion doesn't work, NUMBER will do the
                ; complaining for us
                jsr w_number           ; ( addr u -- u|d )

                ; Otherwise, if we're interpreting, we're done
                lda state
                beq _loop

                ; We're compiling, so there is a bit more work.  Check
                ; status bit 5 to see if it's a single or double-cell
                ; number.
                lda #%00100000
                bit status
                bne _double_number

                jsr w_literal
                ; That was so much fun, let's do it again!
                bra _loop

_double_number:
                ; It's a double cell number.
                jsr w_two_literal
                bra _loop

_got_name_token:
                ; We have a known word's nt TOS. We're going to need its xt
                ; though, which is four bytes father down.

                ; We arrive here with ( addr u nt ), so we NIP twice
                lda 0,x
                sta 4,x
                lda 1,x
                sta 5,x

                inx
                inx
                inx
                inx                     ; ( nt )

                ; Whether interpreting or compiling we'll need to check the
                ; status byte at nt+1 so let's save it now
                jsr w_one_plus
                lda (0,x)
                pha
                jsr w_one_minus
                jsr w_name_to_int      ; ( nt - xt )

                ; See if we are in interpret or compile mode, 0 is interpret
                lda state
                bne _compile

                ; We are interpreting, so EXECUTE the xt that is TOS. First,
                ; though, see if this isn't a compile-only word, which would be
                ; illegal. The status byte is the second one of the header.
                pla
                and #CO                 ; mask everything but Compile Only bit
                beq _interpret

                lda #err_compileonly
                jmp error

_interpret:
                ; We JSR to EXECUTE instead of calling the xt directly because
                ; the RTS of the word we're executing will bring us back here,
                ; skipping EXECUTE completely during RTS. If we were to execute
                ; xt directly, we have to fool around with the Return Stack
                ; instead, which is actually slightly slower
                jsr w_execute

                ; That's quite enough for this word, let's get the next one
                jmp _loop

_compile:
                ; We're compiling! However, we need to see if this is an
                ; IMMEDIATE word, which would mean we execute it right now even
                ; during compilation mode. Fortunately, we saved the nt so life
                ; is easier. The flags are in the second byte of the header
                pla
                and #IM                 ; Mask all but IM bit
                bne _interpret          ; IMMEDIATE word, execute right now

                ; Compile the xt into the Dictionary with COMPILE,
                jsr w_compile_comma
                bra _loop

_line_done:
                ; drop stuff from PARSE_NAME
                inx
                inx
                inx
                inx

                rts


is_printable:
        ; """Given a character in A, check if it is a printable ASCII
        ; character in the range from $20 to $7E inclusive. Returns the
        ; result in the Carry Flag: 0 (clear) is not printable, 1 (set)
        ; is printable. Keeps A. See
        ; http://www.obelisk.me.uk/6502/algorithms.html for a
        ; discussion of various ways to do this
                cmp #AscSP              ; $20
                bcc _done
                cmp #$7F + 1             ; '~'
                bcs _failed

                sec
                bra _done
_failed:
                clc
_done:
                rts


is_whitespace:
        ; """Given a character in A, check if it is a whitespace
        ; character, that is, an ASCII value from 0 to 32 (where
        ; 32 is SPACE). Returns the result in the Carry Flag:
        ; 0 (clear) is no, it isn't whitespace, while 1 (set) means
        ; that it is whitespace. See PARSE and PARSE-NAME for
        ; a discussion of the uses. Does not change A or Y.
                cmp #00         ; explicit comparison to leave Y untouched
                bcc _done

                cmp #AscSP+1
                bcs _failed

                sec
                bra _done
_failed:
                clc
_done:
                rts


; Underflow tests. We jump to the label with the number of cells (not: bytes)
; required for the word. This routine flows into the generic error handling
; code
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

underflow_error:
                ; Entry for COLD/ABORT/QUIT
                lda #err_underflow      ; fall through to error

error:
        ; """Given the error number in a, display the error and call abort. Uses tmp3.
        ; """
                pha                     ; save error
                jsr print_error
                jsr w_cr
                pla
                cmp #err_underflow      ; should we display return stack?
                bne _no_underflow

                lda #err_returnstack
                jsr print_error

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

_no_underflow:
                jmp w_abort            ; no jsr, as we clobber return stack

; =====================================================================
; PRINTING ROUTINES

; We distinguish two types of print calls, both of which take the string number
; (see strings.asm) in A:

;       print_string       - with a line feed
;       print_string_no_lf - without a line feed

; In addition, print_common provides a lower-level alternative for error
; handling and anything else that provides the address of the
; zero-terminated string directly in tmp3. All of those routines assume that
; printing should be more concerned with size than speed, because anything to
; do with humans reading text is going to be slow.

print_string_no_lf:
        ; """Given the number of a zero-terminated string in A, print it to the
        ; current output without adding a LF. Uses Y and tmp3 by falling
        ; through to print_common
        ; """
                ; Get the entry from the string table
                asl
                tay
                lda string_table,y
                sta tmp3                ; LSB
                iny
                lda string_table,y
                sta tmp3+1              ; MSB

                ; fall through to print_common
print_common:
        ; """Common print routine used by both the print functions and
        ; the error printing routine. Assumes string address is in tmp3. Uses
        ; Y.
        ; """
                ldy #0
_loop:
                lda (tmp3),y
                beq _done               ; strings are zero-terminated

                jsr emit_a              ; allows vectoring via output
                iny
                bra _loop
_done:
                rts


print_error:
        ; """Given the error number in a, print the associated error string. Uses tmp3.
        ; """
                asl
                tay
                lda error_table,y
                sta tmp3                ; LSB
                iny
                lda error_table,y
                sta tmp3+1              ; MSB

                jsr print_common
                rts


print_string:
        ; """Print a zero-terminated string to the console/screen, adding a LF.
        ; We do not check to see if the index is out of range. Uses tmp3.
        ; """
                jsr print_string_no_lf
                jmp w_cr               ; JSR/RTS because never compiled


print_u:
        ; """basic printing routine used by higher-level constructs,
        ; the equivalent of the forth word  0 <# #s #> type  which is
        ; basically u. without the space at the end. used for various
        ; outputs
        ; """
                jsr w_zero                     ; 0
                jsr w_less_number_sign         ; <#
                jsr w_number_sign_s            ; #S
                jsr w_number_sign_greater      ; #>
                jmp w_type                     ; JSR/RTS because never compiled

.weak
kernel_kbhit .proc
                lda #1
                rts
.endproc
.endweak

code_end:

; END
