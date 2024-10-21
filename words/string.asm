; ## CMOVE ( addr1 addr2 u -- ) "Copy bytes going from low to high"
; ## "cmove"  auto  ANS string
        ; """https://forth-standard.org/standard/string/CMOVE
        ; Copy u bytes from addr1 to addr2, going low to high (addr2 is
        ; larger than addr1). Based on code in Leventhal, Lance A.
        ; "6502 Assembly Language Routines", p. 201, where it is called
        ; "move left".
        ;
        ; There are no official tests for this word.
        ; """
xt_cmove:
                jsr underflow_3
w_cmove:
                ; move destination address to where we can work with it
                lda 2,x
                sta tmp2        ; use tmp2 because easier to remember
                lda 3,x
                sta tmp2+1

                ; move source address to where we can work with it
                lda 4,x
                sta tmp1        ; use tmp1 because easier to remember
                lda 5,x
                sta tmp1+1

                ldy #0
                lda 1,x         ; number of whole pages to move
                beq _dopartial

_page:
                lda (tmp1),y
                sta (tmp2),y
                iny
                bne _page

                inc tmp1+1
                inc tmp2+1
                dec 1,x
                bne _page

_dopartial:
                lda 0,x         ; length of last page
                beq _done

_partial:
                lda (tmp1),y
                sta (tmp2),y
                iny

                dec 0,x
                bne _partial

_done:          ; clear the stack
                txa
                clc
                adc #6
                tax

z_cmove:        rts


; ## CMOVE_UP ( addr1 addr2 u -- ) "Copy bytes from high to low"
; ## "cmove>"  auto  ANS string
        ; """https://forth-standard.org/standard/string/CMOVEtop
        ; Based on code in Leventhal, Lance A. "6502 Assembly Language
        ; Routines", p. 201, where it is called "move right".
        ;
        ; There are no official tests for this word.
        ; """
xt_cmove_up:
                jsr underflow_3
w_cmove_up:
                ; Move destination address to where we can work with it
                lda 2,x
                sta tmp2        ; use tmp2 because easier to remember
                lda 3,x
                clc
                adc 1,x
                sta tmp2+1      ; point to last page of destination

                ; Move source address to where we can work with it
                lda 4,x
                sta tmp1        ; use tmp1 because easier to remember
                lda 5,x
                clc
                adc 1,x
                sta tmp1+1      ; point to last page of source
                inc 1,x         ; allows us to use bne with dec 1,x below

                ; Move the last partial page first
                ldy 0,x         ; length of last page
                beq _nopartial

_outerloop:
                dey
                beq _finishpage

_innerloop:
                lda (tmp1),y
                sta (tmp2),y
                dey
                bne _innerloop

_finishpage:
                lda (tmp1)      ; handle y = 0 separately
                sta (tmp2)

_nopartial:
                dec tmp1+1      ; back up to previous pages
                dec tmp2+1
                dec 1,x
                bne _outerloop

                ; clear up the stack and leave
                txa
                clc
                adc #6
                tax

z_cmove_up:     rts



; ## COMPARE ( addr1 u1 addr2 u2 -- -1 | 0 | 1) "Compare two strings"
; ## "compare"   auto  ANS string
        ; """https://forth-standard.org/standard/string/COMPARE
        ; Compare string1 (denoted by addr1 u1) to string2 (denoted by
        ; addr2 u2).  Return -1 if string1 < string2, 0 if string1 = string2
        ; and 1 if string1 > string2 (ASCIIbetical comparison).  A string
        ; that entirely matches the beginning of the other string, but is
        ; shorter, is considered less than the longer string.
        ; """
xt_compare:
                jsr underflow_4
w_compare:
                ; Load the two string addresses into tmp1 and tmp2.
                lda 2,x
                sta tmp2
                lda 3,x
                sta tmp2+1
                lda 6,x
                sta tmp1
                lda 7,x
                sta tmp1+1
                ; The counts will be used in-place on the stack.

_compare_loop:
                ; Check to see if we are out of letters.

                ; Check string1
                lda 4,x
                ora 5,x
                beq _str1_done

                ; Check string2
                lda 0,x
                ora 1,x
                beq _greater    ; Str2 empty first

                ; Both strings have at least one letter left.
                ; Check the letters against each other.
                lda (tmp1)
                cmp (tmp2)
                bcc _less
                bne _greater

                ; On to the next letter.  Advance both tmp pointers
                ; and decrement the counts on the stack.
                inc tmp1
                bne +
                inc tmp1+1
+
                inc tmp2
                bne +
                inc tmp2+1
+
                ; Decrement count1 on the stack.
                lda 4,x
                bne +
                dec 5,x
+
                dec 4,x

                ; Decrement count2 on the stack.
                lda 0,x
                bne +
                dec 1,x
+
                dec 0,x

                ; Loop around and check again.
                bra _compare_loop

_str1_done:
                ; String 1 is out of letters. Check string 2.
                lda 0,x
                ora 1,x
                beq _equal      ; Both out of letters

                ; Falls into less (str1 is out but str2 has more)
_less:
                ; Return -1
                lda #$FF
                sta 6,x
                sta 7,x
                bra _done
_equal:
                ; Return 0
                stz 6,x
                stz 7,x
                bra _done
_greater:
                ; Return 1
                lda #1
                sta 6,x
                stz 7,x
                ; Falls into _done
_done:
                ; Remove all but the result from the stack.
                txa
                clc
                adc #6
                tax

z_compare:      rts



; ## MINUS_LEADING ( addr1 u1 -- addr2 u2 ) "Remove leading spaces"
; ## "-leading"  auto  Tali String
        ; """Remove leading whitespace. This is the reverse of -TRAILING
        ; """

xt_minus_leading:
                jsr underflow_2
w_minus_leading:
_loop:
                ; Quit if we were given an empty string. This also terminates
                ; the main loop
                lda 0,x
                ora 1,x
                beq _done

                lda (2,x)               ; get first character
                jsr is_whitespace
                bcc _done

                ; It's whitespace, move one down
                jsr slash_string_1      ; ( addr+1 u-1 )

                bra _loop
_done:
z_minus_leading:
                rts



; ## MINUS_TRAILING ( addr u1 -- addr u2 ) "Remove trailing spaces"
; ## "-trailing"  auto  ANS string
        ; """https://forth-standard.org/standard/string/MinusTRAILING
        ; Remove trailing spaces
        ; """

xt_minus_trailing:
                jsr underflow_2
w_minus_trailing:
                ; if length is zero we-re done
                lda 0,x         ; LSB of n
                ora 1,x         ; MSB of n
                beq _done

                ; Compute address past last character: addr + u1
                jsr w_two_dup
                jsr w_plus
                ; ( addr u addr' )

_loop:
                ; Move back to point at last character
                jsr w_one_minus

                ; While spaces are found,
                ; decrease the count on the data stack and repeat
                lda (0,x)
                jsr is_whitespace
                bcc _drop_done

                ; Decrement count by one.
                lda 2,x
                bne +
                dec 3,x
+
                dea
                sta 2,x
                ora 3,x         ; If count reaches zero - we're also done!
                bne _loop
_drop_done:
                inx             ; drop the end-of-string pointer
                inx
_done:
z_minus_trailing:
                rts



; ## SEARCH ( addr1 u1 addr2 u2 -- addr3 u3 flag) "Search for a substring"
; ## "search"   auto  ANS string
        ; """https://forth-standard.org/standard/string/SEARCH
        ; Search for string2 (denoted by addr2 u2) in string1 (denoted by
        ; addr1 u1). If a match is found the flag will be true and
        ; addr3 will have the address of the start of the match and u3 will have
        ; the number of characters remaining from the match point to the end
        ; of the original string1. If a match is not found, the flag will be
        ; false and addr3 and u3 will be the original string1's addr1 and u1.
        ; """

xt_search:
                jsr underflow_4
w_search:
                ; ANS says if the second string is a zero-length string it
                ; automatically matches.
                lda 0,x
                ora 1,x
                bne _start_search

                ; The second string is a zero length string.  Just remove
                ; the second string and put a true flag.
                inx             ; Remove u2
                inx
                lda #$FF        ; Turn addr2 into a true flag
                sta 0,x
                sta 1,x
                jmp z_search

_start_search:
                ; Put an offset (starting at zero) on the stack.
                jsr w_zero

_search_loop:
                ; We stop (not found) when u2 + offset > u1
                ; Calculate u2+offset into tmp1
                clc
                lda 0,x
                adc 2,x
                sta tmp1
                lda 1,x
                adc 3,x


                ; Compare to u1. Start with the high byte
                cmp 7,x
                bcc _init_comparison ; Obviously less
                bne _not_found

                ; The upper address byte matched - check the lower byte
                ; Load u1 first so we can use just a carry to check.
                lda 6,x
                cmp tmp1
                bcs _init_comparison

_not_found:
                ; The substring isn't in the main string.
                ; Return just the main string and a false flag.
                inx             ; Remove offset
                inx
                inx             ; Remove u2
                inx
                stz 0,x         ; Turn addr2 into a false flag
                stz 1,x
                bra z_search

_init_comparison:
                ; Use tmp1 to hold address in string 1.
                ; Use tmp2 to hold address in string 2.
                ; Use tmp3 to hold the number of characters left to check.

                ; Compute the starting address in string 1
                ; as addr1 + offset
                clc
                lda 8,x
                adc 0,x
                sta tmp1
                lda 9,x
                adc 1,x
                sta tmp1+1

                ; The starting address in string 2 is just addr2.
                lda 4,x
                sta tmp2
                lda 5,x
                sta tmp2+1

                ; The number of characters to check is u2.
                lda 2,x
                sta tmp3
                lda 3,x
                sta tmp3+1

_comparison_loop:
                ; Check to see if the current characters match.
                lda (tmp1)
                cmp (tmp2)
                beq _letters_match

                ; One of the letters didn't match.
                ; Increment the offset and try again.
                jsr w_one_plus
                bra _search_loop

_letters_match:
                ; The letters match.  Advance the pointers until the
                ; count reaches zero.
                inc tmp1
                bne +
                inc tmp1+1
+
                inc tmp2
                bne +
                inc tmp2+1
+
                ; Decrement the count of remaining letters to check.
                lda tmp3
                bne +
                dec tmp3+1
+
                dec tmp3

                ; Check if we've reached zero.
                lda tmp3
                ora tmp3+1
                bne _comparison_loop ; Check the next letter

                ; We've run out of letters and they all match!
                ; Return (addr1+offset) (u1-offset) true
                ; Add offset to addr1.
                clc
                lda 0,x
                adc 8,x
                sta 8,x
                lda 1,x
                adc 9,x
                sta 9,x

                ; Subtract offset from u1.
                sec
                lda 6,x
                sbc 0,x
                sta 6,x
                lda 7,x
                sbc 1,x
                sta 7,x

                ; Replace addr2, u2, and offset with a true flag.
                inx             ; drop offset
                inx
                inx             ; drop u2
                inx
                lda #$FF
                sta 0,x         ; Turn addr2 into a true flag.
                sta 1,x

z_search:       rts



; ## SLASH_STRING ( addr u n -- addr u ) "Shorten string by n"
; ## "/string"  auto  ANS string
        ; """https://forth-standard.org/standard/string/DivSTRING
        ;
        ; Forth code is
        ; : /STRING ( ADDR U N -- ADDR U ) ROT OVER + ROT ROT - ;
        ; Put differently, we need to add TOS and 3OS, and subtract
        ; TOS from NOS, and then drop TOS
        ; """

xt_slash_string:
                jsr underflow_3
w_slash_string:
                clc             ; 3OS+TOS
                lda 0,x
                adc 4,x
                sta 4,x

                lda 1,x
                adc 5,x
                sta 5,x

                sec             ; NOS-TOS
                lda 2,x
                sbc 0,x
                sta 2,x

                lda 3,x
                sbc 1,x
                sta 3,x

                inx
                inx

z_slash_string: rts

; for internal use we often need to remove a single character
slash_string_1:       ; ( addr u -- addr+1 u-1)
                inc 2,x
                bne +
                inc 3,x
+
                lda 0,x
                bne +
                dec 1,x
+
                dec 0,x
rts



; ## SLITERAL (C: addr u -- ) ( -- addr u ) "Compile a string for runtime"
; ## "sliteral" auto  ANS string
        ; """https://forth-standard.org/standard/string/SLITERAL
        ; Add the runtime for an existing string.
        ; """

xt_sliteral:
                jsr underflow_2
w_sliteral:
                ; We can't assume that ( addr u ) of the current string is in
                ; a stable area (eg. already in the dictionary.)
                ; We'll compile the length and string data into the dictionary
                ; using move along with runtime code that stacks the new ( addr' u )
                ;
                ;   jsr sliteral_runtime
                ;   .word u
                ;   .byte < u data bytes >

                ldy #>sliteral_runtime
                lda #<sliteral_runtime
                jsr cmpl_subroutine     ; jsr sliteral_runtime

                lda 0,x
                ldy 1,x
                jsr cmpl_word           ; .word u

                jsr w_here
                jsr w_swap
                ; ( addr addr' u )

                jsr w_dup               ; allocate space for the string
                jsr w_allot

                jsr w_move              ; .text < u bytes >

z_sliteral:     rts


sliteral_runtime:
        ; """Run time behaviour of SLITERAL: Push ( addr u ) of the string to
        ; the Data Stack.  The length and string data follows the JSR here,
        ; for example if we have
        ;
        ;       jsr sliteral_runtime
        ;       .word u
        ;    _addr:
        ;       .byte < u string bytes >
        ;
        ; Then we want to stack ( _str u ) and return past the end of the string.
        ; """
                dex             ; make space on the stack
                dex
                dex
                dex

                ; fetch return address which points one byte before u
                clc
                pla             ; LSB of return address
                sta tmp1
                adc #3          ; calculate string offset
                sta 2,x         ; LSB of string address
                ply             ; MSB of address
                sty tmp1+1
                bcc +
                iny
+
                sty 3,x         ; MSB of string address

                ldy #2          ; copy u to TOS
                lda (tmp1),y
                sta 1,x         ; MSB of u
                dey
                lda (tmp1),y
                sta 0,x         ; LSB of u

                ; we want to continue past the string, i.e. NOS+TOS
                clc             ; A still has LSB of u
                adc 2,x         ; LSB of continuation address
                sta tmp1
                lda 1,x
                adc 3,x
                sta tmp1+1

                jmp (tmp1)
