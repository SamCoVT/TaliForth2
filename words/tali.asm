; ## ALLOW_NATIVE ( -- ) "Flag last word to allow native compiling"
; ## "allow-native"  auto  Tali Forth
xt_allow_native:
w_allow_native:
                jsr current_to_dp
                lda (dp)        ; header status flags are @ nt
                and #$FF-NN-AN  ; AN and NN flag is clear.
                sta (dp)
z_allow_native:
                rts



; ## ALWAYS_NATIVE ( -- ) "Flag last word as always natively compiled"
; ## "always-native"  auto  Tali Forth
xt_always_native:
w_always_native:
                jsr current_to_dp
                lda (dp)        ; header status flags are @ nt
                ora #AN         ; Make sure AN flag is set
                and #$FF-NN     ; and NN flag is clear.
                sta (dp)
z_always_native:
                rts



; ## BELL ( -- ) "Emit ASCII BELL"
; ## "bell"  auto  Tali Forth
xt_bell:
w_bell:
                lda #7          ; ASCII value for BELL (aka ctrl-G)
                jsr emit_a

z_bell:         rts



; ## BOUNDS ( addr u -- addr+u addr ) "Prepare address for looping"
; ## "bounds"  auto  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Memory-Blocks.html
        ; Given a string, return the correct Data Stack parameters for
        ; a DO/LOOP loop over its characters. This is realized as
        ; OVER + SWAP in Forth, but we do it a lot faster in assembler
        ; """
xt_bounds:
                jsr underflow_2
w_bounds:
                clc
                lda 0,x                 ; LSB u
                ldy 2,x                 ; LSB addr
                adc 2,x
                sta 2,x                 ; LSB addr+u
                sty 0,x

                lda 1,x                 ; MSB u
                ldy 3,x                 ; MSB addr
                adc 3,x
                sta 3,x                 ; MSB addr+u
                sty 1,x

z_bounds:       rts



; ## CLEAVE ( addr u -- addr2 u2 addr1 u1 ) "Split off word from string"
; ## "cleave"  auto  Tali Forth

        ; """Given a range of memory with words delimited by whitespace,return
        ; the first word at the top of the stack and the rest of the word
        ; following it.
        ;
        ; Example:
        ; s" w1 w2 w3" cleave  -> "w2 w3" "w1"
        ; s" w1" cleave        -> "" "w1"
        ;
        ; Since it will be used in loops a lot, we want it to work in pure
        ; assembler and be as fast as we can make it. Calls PARSE-NAME so we
        ; strip leading delimiters.
        ; """
xt_cleave:
                jsr underflow_2
w_cleave:
                ; We arrive here with ( addr u ). We need to strip any leading
                ; spaces by hand: PARSE-NAME does do that, but it doesn't
                ; remember how many spaces were stripped. This means we can't
                ; calculate the length of the remainder. Fortunately, Tali
                ; Forth has just the word we need for this:
                jsr w_minus_leading    ; -LEADING ( addr u )

                ; The main part we can turn over to PARSE-NAME, except that we
                ; have a string ( addr u ) and not stuff in the input buffer.
                ; We get around this by cheating: We place ( addr u ) in the
                ; input buffer and then call PARSE-NAME.
                jsr w_input_to_r       ; save old imput state

                lda 0,x         ; u is new ciblen
                sta ciblen
                lda 1,x
                sta ciblen+1

                lda 2,x         ; addr is new cib
                sta cib
                lda 3,x
                sta cib+1

                stz toin        ; >IN pointer is zero
                stz toin+1

                ; PARSE-NAME gives us back the substring of the first word
                jsr w_parse_name       ; ( addr u addr-s u-s )

                ; If we were given an empty string, then we're done. It's the
                ; resposibility of the user to catch this as a sign to end the
                ; any loop
                lda 0,x
                ora 1,x
                beq _done

                ; Now we have to adjust the original string
                lda 4,x         ; LSB of original u
                sec
                sbc 0,x
                sta 4,x

                lda 5,x         ; MSB of original u
                sbc 1,x
                sta 5,x

                lda 6,x         ; LSB of original addr
                clc
                adc 0,x
                sta 6,x

                lda 7,x         ; MSB of original addr
                adc 1,x
                sta 7,x

                ; There is one small problem: PARSE-NAME will probably have
                ; left the string with the rest of the words with leading
                ; delimiters. We use our magic -LEADING again
                jsr w_two_swap         ; ( addr-s u-s addr u )
                jsr w_minus_leading
                jsr w_two_swap         ; ( addr u addr-s u-s )
_done:
                ; Restore input
                jsr w_r_to_input

z_cleave:       rts



; ## DIGIT_QUESTION ( char -- u f | char f ) "Convert ASCII char to number"
; ## "digit?"  auto  Tali Forth
        ; """Inspired by the pForth instruction DIGIT, see
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; Rewritten from DIGIT>NUMBER in Tali Forth. Note in contrast to
        ; pForth, we get the base (radix) ourselves instead of having the
        ; user provide it. There is no standard name for this routine, which
        ; itself is not ANS; we use DIGIT? following pForth and Gforth.
        ; """

xt_digit_question:
                jsr underflow_1
w_digit_question:
                ; one way or another, we're going to need room for the
                ; flag on the stack
                dex
                dex
                stz 0,x                 ; default flag is failure
                stz 1,x
                stz 3,x                 ; paranoid

                ; Check the character, now in the LSB of NOS.
                lda 2,x
                jsr ascii_to_digit
                bcs _done               ; failure flag already set

                ; Found a legal number for current base
                sta 2,x                 ; put number in NOS
                dec 0,x                 ; set success flag
                dec 1,x
_done:
z_digit_question:
                rts



; ## EXECUTE_PARSING ( addr u xt -- ) "Pass a string to a parsing word"
; ## "execute-parsing"  auto  Gforth
        ; """https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Input-Stream.html
        ; Execute the parsing word defined by the execution token (xt) on the
        ; string as if it were passed on the command line. See the file
        ; tests/tali.fs for examples.
        ;
        ; Note that this word is coded completely
        ; different in its Gforth version, see the file execute-parsing.fs
        ; (in /usr/share/gforth/0.7.3/compat/ on Ubuntu 18.04 LTS) for details.
        ; """
xt_execute_parsing:
                jsr underflow_3
w_execute_parsing:
                jsr w_input_to_r       ; save normal input for later
                jsr w_not_rot          ; -ROT ( xt addr u )

                lda 0,x                 ; TOS is new ciblen
                sta ciblen
                lda 1,x
                sta ciblen+1

                lda 2,x                 ; NOS is new cib
                sta cib
                lda 3,x
                sta cib+1

                stz toin                ; Set >IN to zero
                stz toin+1

                jsr w_two_drop         ; 2DROP ( xt )
                jsr w_execute

                jsr w_r_to_input

z_execute_parsing:
                rts



; ## FIND_NAME ( addr u -- nt|0 ) "Get the name token of input word"
; ## "find-name"  auto  Gforth

xt_find_name:
        ; """www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; Given a string, find the Name Token (nt) of a word or return
        ; zero if the word is not in the dictionary. We use this instead of
        ; ancient FIND to look up words in the Dictionary passed by
        ; PARSE-NAME. Note this returns the nt, not the xt of a word like
        ; FIND. To convert, use NAME>INT. This is a Gforth word. See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; FIND calls this word
        ; """
                jsr underflow_2
w_find_name:
                ; check for special case of an empty string (length zero)
                lda 0,x
                ora 1,x
                beq _fail_done

_nonempty:
                ; Truncate names longer than the max allowed (31).
                dex
                dex
                lda #31
                sta 0,x
                stz 1,x
                jsr w_min

                ; Set up for traversing the wordlist search order.
                stz tmp3                ; Start at the beginning

_wordlist_loop:
                ldy #num_order_offset   ; Compare to byte variable #ORDER
                lda tmp3
                cmp (up),y              ; Check to see if we are done

                ; We ran out of wordlists to search.
                beq _fail_done

                ; set up first loop iteration

                ; Get the current wordlist id
                clc             ; SEARCH-ORDER is array of bytes.
                adc #search_order_offset
                tay
                lda (up),y      ; Get the id byte, which is the offset
                                ; into the cell array WORDLISTS

                ; Get the DP for that wordlist.
                asl                     ; Turn offset into cells offset.
                clc
                adc #wordlists_offset
                tay
                lda (up),y
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1

                jsr find_nt_by_name
                bne _success

                ; Move on to the next wordlist in the search order.
                inc tmp3
                bra _wordlist_loop

_success:
                ; The strings match. Put correct nt NOS, because we'll drop
                ; TOS before we leave
                lda tmp1
                sta 2,x
                lda tmp1+1
                sta 3,x

                bra _done

_fail_done:
                stz 2,x         ; failure flag
                stz 3,x
_done:
                inx
                inx

z_find_name:    rts


; ## HAVEKEY ( -- addr ) "Return address of key? vector"
; ## "havekey" tested Tali Forth

xt_havekey:
w_havekey:
                dex
                dex
                lda #<havekey
                sta 0,x
                lda #>havekey
                sta 1,x

z_havekey:      rts


; ## HEXSTORE ( addr1 u1 addr2 -- u2 ) "Store a list of numbers"
; ## "hexstore"  auto  Tali
        ; """Given a string addr1 u1 with numbers in the current base seperated
        ; by spaces, store the numbers at the address addr2, returning the
        ; number of elements. Non-number elements are skipped, an zero-length
        ; string produces a zero output.
        ; """

xt_hexstore:
                jsr underflow_3
w_hexstore:
                jsr w_dup              ; Save copy of original address
                jsr w_two_to_r         ; ( addr1 u1 ) ( R: addr2 addr2 )

_loop:
                ; Loop until string is totally consumed
                lda 0,x
                ora 1,x
                beq _done

                jsr w_cleave           ; ( addr1 u1 addr3 u3 ) ( R: addr2 addr2 )

                ; Prepare the conversion of the number.
                jsr w_two_to_r
                jsr w_zero
                jsr w_zero
                jsr w_two_r_from       ; ( addr1 u1 0 0 addr3 u3 ) ( R: addr2 addr2 )
                jsr w_to_number        ; ( addr1 u1 n n addr4 u4 ) ( R: addr2 addr2 )

                ; If u4 is not zero, we have leftover chars and have to do
                ; things differently
                lda 0,x
                ora 1,x
                bne _have_chars_left

                ; Normal case, this number is all done
                jsr w_two_drop         ; ( addr1 u1 n n ) ( R: addr2 addr2 )
                jsr w_d_to_s           ; ( addr1 u1 n ) ( R: addr2 addr2 )

                ; Store the new value
                jsr w_r_fetch          ; ( addr1 u1 n addr2 ) ( R: addr2 addr2 )
                jsr w_c_store          ; ( addr1 u1 ) ( R: addr2 addr2 )

                ; Increase counter
                jsr w_r_from           ; R>
                jsr w_one_plus         ; 1+
                jsr w_to_r             ; >R ( addr1 u1 ) ( R: addr2+1 addr2 )
                bra _loop

_have_chars_left:
                ; Pathological case: Drop the rest of this number off the stack
                ; and continue with the next word. Doesn't print a warning. We
                ; need to drop four cells, that is, eight bytes
                txa
                clc
                adc #8
                tax
                bra _loop

_done:
                ; Clean up return stack and calculate number of chars stored
                inx
                inx
                inx
                inx                     ; 2DROP

                jsr w_two_r_from       ; ( addr2+n addr2 )
                jsr w_swap
                jsr w_minus            ; ( n )

z_hexstore:     rts



; ## INPUT ( -- addr ) "Return address of input vector"
; ## "input" tested Tali Forth

xt_input:
w_input:
                dex
                dex
                lda #<input
                sta 0,x
                lda #>input
                sta 1,x

z_input:        rts



; ## INPUT_TO_R ( -- ) ( R: -- n n n n ) "Save input state to the Return Stack"
; ## "input>r"  tested  Tali Forth
   	; """Save the current input state as defined by insrc, cib, ciblen, and
        ; toin to the Return Stack. Used by EVALUATE.
        ;
        ; The naive way of doing
        ; this is to push each two-byte variable to the stack in the form of
        ;
        ;       lda insrc
        ;       pha
        ;       lda insrc+1
        ;       pha
        ;
        ; for a total of 24 byte of instruction in one direction and later
        ; a further 24 bytes to reverse the process. We shorten this at the
        ; cost of some speed by assuming the four variables are grouped
        ; together on the Zero Page and start with insrc (see definitions.asm
        ; for details). The reverse operation is r_to_input. These words must
        ; be flagged as Never Native. Uses tmp1
        ; """

xt_input_to_r:
w_input_to_r:
                ; We arrive here with the return address on the top of the
                ; 65c02's stack. We need to move it out of the way first
                pla
                sta tmp1
                pla
                sta tmp1+1

                ; This assumes that insrc is the first of eight bytes and
                ; toin+1 the last in the sequence we want to save from the Zero
                ; Page.
                ldy #7
_loop:
                lda insrc,y     ; insrc+7 is toin+1
                pha
                dey
                bpl _loop

                ; Restore address for return jump
                lda tmp1+1
                pha
                lda tmp1
                pha

z_input_to_r: 	rts



; ## INT_TO_NAME ( xt -- nt | 0 ) "Get name token from execution token"
; ## "int>name"  auto  Tali Forth
        ; """www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; This is called >NAME in Gforth, but we change it to
        ; INT>NAME to match NAME>INT
        ;
        ; Uses tmp1
        ; """

xt_int_to_name:
                jsr underflow_1
w_int_to_name:
                ; Unfortunately, to find the header, we have to walk through
                ; all of the wordlists, which we do in id order.

                stz tmptos              ; initialize loop counter with first wordlist id

_wordlist_loop:
                lda tmptos              ; fetch and increment wordlist id
                cmp #max_wordlists
                bcs _done               ; we're done when index >= max
                inc tmptos              ; bump index for next go round

                ; Get the DP for that wordlist.
                asl                     ; Turn offset into cells offset.
                clc
                adc #wordlists_offset
                tay
                lda (up),y              ; Save the DP (first nt) for this wordlist
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1

                jsr find_nt_by_xt
                beq _wordlist_loop      ; found?

_done:
                ; Write stashed nt (found or 0) to TOS
                lda tmp1
                sta 0,x
                lda tmp1+1
                sta 1,x

z_int_to_name:  rts



; ## LATESTNT ( -- nt ) "Push most recent nt to the stack"
; ## "latestnt"  auto  Tali Forth
        ; """www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; The Gforth version of this word is called LATEST
        ; """
xt_latestnt:
w_latestnt:
                dex
                dex

                jsr current_to_dp

                lda dp
                sta 0,x
                lda dp+1
                sta 1,x

z_latestnt:     rts


; ## LATESTXT ( -- xt ) "Push most recent xt to the stack"
; ## "latestxt"  auto  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Anonymous-Definitions.html"""
xt_latestxt:
w_latestxt:
                jsr w_latestnt         ; ( nt )
                jsr w_name_to_int      ; ( xt )

z_latestxt:     rts



; ## NAME_TO_INT ( nt -- xt ) "Convert Name Token to Execute Token"
; ## "name>int"  tested  Gforth
        ; """See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ;
        ; Uses tmp1
        ; """

xt_name_to_int:
                jsr underflow_1
w_name_to_int:
                lda 0,x                 ; copy nt to tmp1
                sta tmp1
                lda 1,x
                sta tmp1+1

                jsr nt_to_xt            ; get xt to Y/A

                sta 0,x
                sty 1,x
z_name_to_int:  rts



; ## NAME_TO_STRING ( nt -- addr u ) "Given a name token, return string of word"
; ## "name>string"  tested  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html"""

xt_name_to_string:
                jsr underflow_1
w_name_to_string:
                dex             ; make space for length
                dex

                lda (2,x)       ; grab status flags for header length
                and #DC+LC+FP   ; mask length bits
                lsr
                adc #4-1        ; calculate header length less one
                tay             ; stash temporarily

                inc 2,x         ; nt+1 has the name length
                bne +
                inc 3,x
+
                lda (2,x)       ; fetch name length byte

                sta 0,x         ; namelen is always <256
                stz 1,x

                tya
                adc 2,x         ; note C=0 from adc above
                sta 2,x         ; LSB of nameptr = nt+1 + hdrlen-1
                bcc +
                inc 3,x         ; MSB of nameptr
+
                ; ( addr n )
z_name_to_string:
                rts


; ## NC_LIMIT ( -- addr ) "Return address where NC-LIMIT value is kept"
; ## "nc-limit"  tested  Tali Forth

xt_nc_limit:
w_nc_limit:
                lda #nc_limit_offset
                jmp push_upvar_tos
z_nc_limit:



; ## NEVER_NATIVE ( -- ) "Flag last word as never natively compiled"
; ## "never-native"  auto  Tali Forth
xt_never_native:
w_never_native:
                jsr current_to_dp
                lda (dp)        ; header status flags are @ nt
                ora #NN         ; Make sure NN flag is set
                and #$FF-AN     ; and AN flag is clear.
                sta (dp)
z_never_native:
                rts



; ## NOT_ROT ( a b c -- c a b ) "Rotate upwards"
; ## "-rot"  auto  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Data-stack.html"""

xt_not_rot:
                jsr underflow_3
w_not_rot:
                ldy 1,x         ; MSB first
                lda 3,x
                sta 1,x

                lda 5,x
                sta 3,x
                sty 5,x

                ldy 0,x         ; LSB second
                lda 2,x
                sta 0,x

                lda 4,x
                sta 2,x
                sty 4,x

z_not_rot:      rts



; ## NUMBER ( addr u -- u | d ) "Convert a number string"
; ## "number"  auto  Tali Forth
        ; """Convert a number string to a double or single cell number. This
        ; is a wrapper for >NUMBER and follows the convention set out in the
        ; "Forth Programmer's Handbook" (Conklin & Rather) 3rd edition p. 87.
        ; Based in part on the "Starting Forth" code
        ; https://www.forth.com/starting-forth/10-input-output-operators/
        ; Gforth uses S>NUMBER? and S>UNUMBER? which return numbers and a flag
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Number-Conversion.html
        ; Another difference to Gforth is that we follow ANS Forth that the
        ; dot to signal a double cell number is required to be the last
        ; character of the string.
        ;
        ; Number calls >NUMBER which in turn calls UM*,
        ; which uses tmp1, tmp2, and tmp3, so we can't use them here, which is
        ; a pain.
        ;"""

xt_number:
                jsr underflow_2
w_number:
                ; we keep the flags for sign and double in tmpdsp because
                ; we've run out of temporary variables
                ; sign will be the sign bit, and double will be bit 1
                stz tmpdsp      ; %n000 000d

                ; Push the current base onto the stack.
                ; This is done to handle constants in a different base
                ; like #1234 and $ABCD and %0101
                lda base
                pha

                ; Make a copy of the addr u in case we need to print an error message.
                jsr w_two_dup

                ; Look at the first character.
                lda (2,x)

                cmp #'#'                ; decimal?
                bne _check_hex
                ; Switch temporarily to decimal
                lda #10
                bra _base_changed
_check_hex:
                cmp #'$'
                bne _check_binary
                ; Switch temporarily to hexadecimal
                lda #16
                bra _base_changed
_check_binary:
                cmp #'%'                ; binary?
                bne _check_char
                ; Switch temporarily to hexadecimal
                lda #2
                bra _base_changed
_check_char:
                cmp #"'"
                bne _check_minus
                ; Character constants should have a length of 3
                ; and another single quote in position 3.
                lda 0,x         ; Get the length
                cmp #3
                bne _not_a_char
                lda 1,x
                bne _not_a_char ; No compare needed to check for non-zero.
                ; Compute location of last character
                ; We know the string is 3 characters long, so last char
                ; is known to be at offset +2.
                lda 2,x         ; LSB of address
                clc
                adc #2          ; length of string
                sta tmptos
                lda 3,x
                adc #0          ; only need carry
                sta tmptos+1
                lda (tmptos)
                cmp #"'"
                bne _not_a_char
                ; The char we want is between the single quotes.
                inc 2,x
                bne +
                inc 3,x
+
                ; Grab the character and replace the string with just the char.
                lda (2,x)
                sta 2,x
                stz 3,x

                jmp _drop_original_string ; Single flag will drop the TOS for us.
_not_a_char:
                ; This label was just a bit too far away for a single bra from
                ; the character checking code, so we'll sneak it here and
                ; then bra again to get there.
                bra _number_error

_base_changed:
                sta base        ; Switch to the new base
                inc 2,x         ; start one character later
                bne +
                inc 3,x
+
                dec 0,x         ; decrease string length by one


                lda (2,x)       ; Load the first char again
_check_minus:
                ; If the first character is a minus, strip it off and set
                ; the flag
                cmp #'-'
                bne _check_dot

                ; It's a minus
                lda #$80
                sta tmpdsp      ; set the sign bit
                inc 2,x         ; start one character later
                bne +
                inc 3,x
+
                dec 0,x         ; decrease string length by one

_check_dot:
                ; If the last character is a dot, strip it off and set a
                ; flag. We can use tmptos as a temporary variable
                lda 2,x         ; LSB of address
                clc
                adc 0,x         ; length of string
                sta tmptos
                lda 3,x
                adc #0          ; only need carry
                sta tmptos+1

                ; tmptos now points to the first character after the string,
                ; but we need the last character
                lda tmptos
                bne +
                dec tmptos+1
+
                dec tmptos

                lda (tmptos)
                cmp #'.'
                bne _main

                ; We have a dot, which means this is a double number. Flag
                ; the fact and reduce string length by one
                inc tmpdsp
                dec 0,x

_main:
                ; Set up stack for subroutine jump to >NUMBER, which means
                ; we have to go ( addr u --> ud addr u )
                dex
                dex
                dex
                dex

                lda 4,x         ; LSB of length
                sta 0,x
                stz 1,x         ; MSB, max length 255 chars

                lda 6,x         ; LSB of address
                sta 2,x
                lda 7,x         ; MSB of address
                sta 3,x

                stz 4,x         ; clear space for ud
                stz 5,x
                stz 6,x
                stz 7,x

                jsr w_to_number        ; (ud addr u -- ud addr u )

                ; test length of returned string, which should be zero
                lda 0,x
                beq _all_converted

_number_error:
                ; Something went wrong, we still have characters left over,
                ; so we print an error and abort. If the NUMBER was called
                ; by INTERPRET, we've already checked for Forth words, so
                ; we're in deep trouble one way or another

                ; Drop the addr u from >NUMBER and the double
                ; (partially converted number) and print the unkown
                ; word using the original addr u we saved at the beginning.
                jsr w_two_drop ; >NUMBER modified addr u
                jsr w_two_drop ; ud   (partially converted number)

                lda #'>'
                jsr emit_a
                jsr w_type
                lda #'<'
                jsr emit_a
                jsr w_space

                ; Pull the base of the stack and restore it.
                pla
                sta base

                lda #err_syntax
                jmp error

_all_converted:
                ; We can drop the string info
                inx ; Drop the current addr u
                inx
                inx
                inx
_drop_original_string:
                jsr w_two_swap  ; Drop the original addr u
                jsr w_two_drop  ; (was saved for unknown word error message)

                ; We have a double-cell number on the Data Stack that might
                ; actually have a minus and might actually be single-cell
                lda tmpdsp      ; flag for double/minus
                ldy #%00100000  ; status bit 5 for double(1) or single(0)
                asl             ; %n000 000d => %0000 00d0, C=n, Z=d
                beq _single

                ; Set status bit 5 (A=%0010 0000) to indicate a double number
                tya
                tsb status

                ; This is a double cell number. If it had a minus (C=1) negate it
                bcc _done       ; no minus, all done

                jsr w_dnegate

                bra _done

_single:
                ; This is a single number, so we just drop the top cell
                inx
                inx

                ; Clear status bit 5 to indicate this is a single number
                tya
                trb status

                ; If we had a minus (C=1), we'll have to negate it
                bcc _done       ; no minus, all done

                jsr w_negate
_done:
                ; Restore the base (in case it was changed by #/$/%)
                pla
                sta base
z_number:       rts



; ## ONE ( -- n ) "Push the number 1 to the Data Stack"
; ## "1"  auto  Tali Forth
        ; """This is also the code for EDITOR-WORDLIST"""
xt_editor_wordlist:
xt_one:
w_editor_wordlist:
w_one:
                dex
                dex
                lda #1
                sta 0,x
                stz 1,x

z_editor_wordlist:
z_one:
                rts



; ## OUTPUT ( -- addr ) "Return the address of the EMIT vector address"
; ## "output"  tested  Tali Forth
xt_output:
w_output:
        ; """Return the address where the jump target for EMIT is stored (but
        ; not the vector itself). By default, this will hold the value of
        ; kernel_putc routine, but this can be changed by the user, hence this
        ; routine.
        ; """
                dex
                dex
                lda #<output
                sta 0,x
                lda #>output
                sta 1,x

z_output:       rts



; ## R_TO_INPUT ( -- ) ( R: n n n n -- ) "Restore input state from Return Stack"
; ## "r>input"  tested  Tali Forth
        ; """Restore the current input state as defined by insrc, cib, ciblen,
        ; and toin from the Return Stack.
        ;
        ; See INPUT_TO_R for a discussion of this word. Uses tmp1
        ; """

xt_r_to_input:
w_r_to_input:
                ; We arrive here with the return address on the top of the
                ; 65c02's stack. We need to move it out of the way first
                pla
                sta tmp1
                pla
                sta tmp1+1

                ; This assumes that insrc is the first of eight bytes and
                ; toin+1 the last in the sequence we want to save from the Zero
                ; Page. Since we went in reverse order, insrc is now on the top
                ; of the Return Stack.
                ldy #0
_loop:
                pla
                sta insrc,y
                iny
                cpy #8
                bne _loop

                ; Restore address for return jump
                lda tmp1+1
                pha
                lda tmp1
                pha

z_r_to_input: 	rts



; ## STRIP_UNDERFLOW ( -- addr ) "Return address where underflow flag is kept"
; ## "strip-underflow"  tested  Tali Forth
        ; """`STRIP-UNDERFLOW` is a flag variable that determines if underflow
        ; checking should be removed during the compilation of new words.
        ; Default is false.
        ; """
xt_strip_underflow:
w_strip_underflow:
                lda #uf_strip_offset
                jmp push_upvar_tos
z_strip_underflow:



; ## TWO ( -- u ) "Push the number 2 to stack"
; ## "2"  auto  Tali Forth
        ;
        ; This code is shared with ASSEMBLER-WORDLIST
xt_assembler_wordlist:
xt_two:
w_assembler_wordlist:
w_two:
                dex
                dex
                lda #2
                sta 0,x
                stz 1,x

z_assembler_wordlist:
z_two:          rts



; ## USERADDR ( -- addr ) "Push address of base address of user variables"
; ## "useraddr"  tested  Tali Forth
xt_useraddr:
w_useraddr:
                dex
                dex
                lda #<up
                sta 0,x
                lda #>up
                sta 1,x

z_useraddr:     rts



; ## WORDSIZE ( nt -- u ) "Get size of word in bytes"
; ## "wordsize"  auto  Tali Forth
        ; """Given an word's name token (nt), return the size of the
        ; word's payload size in bytes (CFA plus PFA) in bytes. Does not
        ; count the final RTS.
        ; """
xt_wordsize:
                jsr underflow_1
w_wordsize:
                ; Use header status flags to calculate offset to code size
                lda (0,x)       ; fetch status flag byte
                pha             ; stash the flags for later LC check
                and #DC+FP      ; mask length bits excluding LC
                lsr             ; preserve DC flag with FP in carry
                adc #3          ; offset to code size is 3 + 0-3

                adc 0,x         ; add offset to nt, carry clear from offset calc
                sta 0,x         ; LSB of size byte(s) address
                bcc +
                inc 1,x         ; MSB
+
                lda (0,x)       ; get LSB of body size
                tay             ; hold that thought
                pla             ; check if there's a MSB
                and #LC         ; extra byte if LC is set
                beq _small      ; otherwise it's zero

                inc 0,x         ; increment ptr
                bne +
                inc 1,x
+
                lda (0,x)       ; grab MSB

_small:         sty 0,x         ; Y has LSB
                sta 1,x         ; A has either MSB we fetched or 0 if there wasn't one

z_wordsize:     rts



; ## ZERO ( -- 0 ) "Push 0 to Data Stack"
; ## "0"  auto  Tali Forth
        ; """The disassembler assumes that this routine does not use Y. Note
        ; that CASE, FALSE, and FORTH-WORDLIST use the same routine to place
        ; a 0 on the data stack."""
xt_case:
xt_false:
xt_forth_wordlist:
xt_zero:
w_case:
w_false:
w_forth_wordlist:
w_zero:
                dex             ; push
                dex
                stz 0,x
                stz 1,x
z_case:
z_false:
z_forth_wordlist:
z_zero:
                rts
