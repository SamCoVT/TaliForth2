; ## BYE ( -- ) "Break"
; ## "bye"  tested  ANS tools ext
        ; """https://forth-standard.org/standard/tools/BYE"""
xt_bye:
w_bye:
                ; Use the kernel_bye routine provided in the platform
                ; file.  For simulators, this is traditionally just a
                ; brk instruction, but platforms with another OS can
                ; arrange to jump back into that OS.  This routine
                ; does not return.
                jmp kernel_bye
z_bye:



; ## DOT_S ( -- ) "Print content of Data Stack"
; ## ".s"  auto  ANS tools
        ; """https://forth-standard.org/standard/tools/DotS
        ; Print content of Data Stack non-distructively. We follow the format
        ; of Gforth and print the number of elements first in brackets,
        ; followed by the Data Stack content (if any).
        ;
        ; Since this is for humans, we don't have to worry about speed.
        ; """

xt_dot_s:
w_dot_s:
                jsr w_depth    ; ( -- u )

                ; Print stack depth in brackets
                lda #'<'
                jsr emit_a

                ; We keep a copy of the number of the things on the stack
                ; to use as a counter later down. This assumes that there
                ; are less than 255 elements on the stack
                lda 0,x
                pha

                ; print unsigned number without the trailing space
                jsr w_dup

                jsr print_tos

                lda #'>'
                jsr emit_a
                jsr w_space

                inx
                inx

                ; There will be lots of cases where the stack is empty. If that
                ; is so, get out of here quickly
                cpx #dsp0
                beq _done

                ; We have at least one element on the stack. The depth of the
                ; stack is on the stack, we can use it as a counter. We go
                ; from bottom to top
                ply

                lda #+dsp0-1     ; go up one to avoid garbage
                sta tmp3
                stz tmp3+1      ; must be zero page on the 65c02
_loop:
                dex
                dex

                lda (tmp3)
                sta 1,x
                dec tmp3

                lda (tmp3)
                sta 0,x
                dec tmp3
                phy

                jsr w_dot

                ply
                dey
                bne _loop

                pha             ; dummy to balance stack
_done:
                pla
z_dot_s:        rts



; ## DUMP ( addr u -- ) "Display a memory region"
; ## "dump"  auto  ANS tools
        ; """https://forth-standard.org/standard/tools/DUMP
        ;
        ; DUMP's exact output is defined as "implementation dependent".
        ; This is in assembler because it is
        ; useful for testing and development, so we want to have it work
        ; as soon as possible. Uses tmp1, tmp2
        ; """

xt_dump:
                jsr underflow_2
w_dump:
_row:
                ; track current address in tmp2
                lda 3,x
                sta tmp2+1
                lda 2,x
                sta tmp2

                jsr w_cr

                ; set Y to number of characters for this row
                ldy #16                 ; max 16
                lda 1,x                 ; if u > 256 keep 16
                bne +

                lda 0,x                 ; if u = 0 we're done
                beq _done

                cmp #16                 ; if u < 16 do what's left
                bcs +
                tay
+
                sty tmp1                ; temporary storage for loop counter
                lda #$40                ; bit 6 set on first pass and bit 7 on second
                sta tmp1+1              ; so we can use bit tmp1+1 to check N flag

                ; print current address for the row
                ldy #1
-
                lda tmp2,y
                jsr byte_to_ascii
                dey
                bpl -

                jsr w_space
_pass:                                  ; loop once for bytes, then for ascii
                ldy #0
_bytes:                                 ; loop over each byte in the row
                tya
                and #7
                bne +
                jsr w_space             ; extra space before bytes 0 and 8
+
                ; dump the contents
                lda (tmp2),y
                bit tmp1+1              ; which pass are we on?
                bmi _ascii              ; bit 7 set on second pass

                jsr byte_to_ascii       ; show byte value
                jsr w_space
                bra _nextbyte
_ascii:
                jsr is_printable        ; show ascii char
                bcs +
                lda #'.'                ; use dot if not printable
+
                jsr emit_a
_nextbyte:
                iny
                cpy tmp1
                bne _bytes

                asl tmp1+1              ; $40 -> $80 -> 0
                beq +                   ; done both passes?

                ; add spaces to align partial lines
                ; after writing Y bytes, we need to add padding of
                ; of 3*(16-Y) + (1 if Y<9)
                dey                     ; Y-1 is 0...15
                tya
                eor #$f                 ; 15-(Y-1) is 16-Y
                sta tmpdsp
                asl a                   ; A is 2*(16-Y)
                ; y < 9 when 16-y > 7 and when 16-y >= 8
                ; so with A=2*(16-y), cmp #2*8 sets C=1 when true
                cmp #16
                adc tmpdsp              ; 3*(16-Y) + 1 if Y<9

                jsr push_a_tos
                jsr w_spaces

                bra _pass
+
                ; done this row, increment address and decrement count
                lda tmp1
                jsr push_a_tos
                jsr w_slash_string      ; ( addr n k -- addr+k n-k )

                bra _row                ; new row

_done:
                inx
                inx
                inx
                inx

z_dump:         rts



; ## QUESTION ( addr -- ) "Print content of a variable"
; ## "?"  auto  ANS tools
        ; """https://forth-standard.org/standard/tools/q
        ;
        ; Only used interactively. Since humans are so slow, we
        ; save size and just go for the subroutine jumps
        ; """
xt_question:
w_question:
                ; FETCH takes care of underflow check
                jsr w_fetch
                jsr w_dot

z_question:     rts



; ## SEE ( "name" -- ) "Print information about a Forth word"
; ## "see" auto  ANS tools
        ; """https://forth-standard.org/standard/tools/SEE
        ; SEE takes the name of a word and prints its name token (nt),
        ; execution token (xt), size in bytes, flags used, and then dumps the
        ; code and disassembles it.
        ; """

xt_see:
w_see:
                jsr w_parse_name       ; ( addr u )
                jsr w_find_name        ; ( nt | 0 )

                ; If we got back a zero we don't know that word and so we quit
                ; with an error
                lda 0,x
                ora 1,x
                bne +

                lda #err_noname
                jmp error
+
                jsr w_cr

                ; We have a legal word, so let's get serious. Save the current
                ; number base and use hexadecimal instead.
                lda base
                pha
                jsr w_hex

                lda #str_see_nt
                jsr print_string_n

                jsr w_dup               ; ( nt nt )
                jsr w_u_dot
                jsr w_space             ; ( nt )

                jsr w_dup               ; ( nt nt )
                jsr w_name_to_int       ; ( nt xt )

                lda #str_see_xt
                jsr print_string_n

                jsr w_dup               ; ( nt xt xt )
                jsr w_u_dot             ; ( nt xt )
                jsr w_space

                lda #str_see_header
                jsr print_string_n
                jsr w_over
                ; calculate header length from status flag byte
                lda (0,x)               ; fetch status byte
                and #DC+LC+FP           ; mask length bits
                lsr                     ; shift FP to carry flag, A = 2*DC + LC
                adc #4                  ; header length is 4 bytes + 2*DC + LC + FP
                tay
_show_header:
                lda (0,x)
                jsr byte_to_ascii
                jsr w_space
                jsr w_one_plus
                dey
                bne _show_header

                jsr w_cr
                jsr w_drop              ; ( nt xt )

                ; Show flag values from the status byte along with
                ; any calculated (synthetic) flag values
                lda (2,x)               ; grab status flags @ NT
                dex                     ; make some space
                dex                     ; ( nt xt flags )
                sta 0,x                 ; stash status flag byte
                stz 1,x                 ; placeholder for synthetic flags
                pha                     ; save a copy of flags for later

                                        ; ( nt xt flags )
                ; collect synthetic flags in reverse order for template
                and #ST                 ; calculate ST flag
                cmp #ST
                beq +                   ; C=1 when ST set
                clc
+
                ror 1,x                 ; add to flag byte

                jsr w_over
                jsr has_uf_check        ; C=1 when UF set
                ror 1,x                 ; add to flag byte

                lda #N_FLAGS            ; count off status byte flags
                sta tmptos
.if N_FLAGS < 8
-
                cmp #8                  ; discard any unused high bits
                beq +
                asl 0,x
                ina
                bra -
+
.endif
                ; use a zero-terminated template string to show flag names with placeholders
                ; marked by shifted characters
                lda #<see_flags_template
                sta tmp3                ; LSB
                lda #>see_flags_template
                sta tmp3+1              ; MSB

                ldy #0                  ; index the string
_show_flags:
                lda (tmp3),y            ; next char in template
                bpl _emit               ; normal char?  just show it

                ; otherwise insert a flag first
                and #$7f                ; clear hi bit and save char that follows flag
                pha

                ; for each flag, print "<space><flag><space>"
                jsr w_space             ; no stack effect

                dec tmptos
                bmi _synthetic          ; more core status flags?
                asl 0,x                 ; shift next flag bit into carry
                bra +
_synthetic:
                asl 1,x                 ; show synthetic flags after core ones
+
                lda #'0'                ; convert C=0/1 into '0' or '1'
                adc #0
                jsr emit_a              ; write the flag digit
                jsr w_space             ; and a space

                pla                     ; recover following character
                beq _done
_emit:
                jsr emit_a

                iny
                bne _show_flags
_done:

                jsr w_cr

                inx                     ; drop flags
                inx                     ; ( nt xt )

                ; Figure out the size
                lda #str_see_size
                jsr print_string_n

                jsr w_swap              ; ( xt nt )
                jsr w_wordsize          ; ( xt u )
                jsr w_dup               ; ( xt u u )
                jsr w_decimal

                ; for HC words we'll split out CFA/PFA
                pla                     ; fetch flag byte we saved earlier
                and #HC                 ; does it have CFA?
                pha                     ; we'll need to check once more
                beq +

                lda #str_see_cfapfa
                jsr print_string_n  ; print "CFA: 3  PFA: "

                sec
                lda 0,x                 ; reduce to u-3
                sbc #3
                sta 0,x                 ; assume u < 256
+
                jsr w_u_dot             ; print u (or u-3 for PFA)

                ; ( xt u )
                jsr w_cr

                ; Dump hex and disassemble
.if "disassembler" in TALI_OPTIONAL_WORDS
                jsr w_two_dup           ; ( xt u xt u )
.endif
                jsr w_hex
                jsr w_dump
                pla                     ; recover HC flag
.if "disassembler" in TALI_OPTIONAL_WORDS
                beq +
                lda #3
                sta 0,x                 ; for CFA words, just show three bytes
                stz 1,x
+
                jsr w_disasm
.endif
                pla
                sta base

z_see:          rts



; ## WORDS ( -- ) "Print known words from Dictionary"
; ## "words"  auto  ANS tools
        ; """https://forth-standard.org/standard/tools/WORDS
        ; This is pretty much only used at the command line so we can
        ; be slow and try to save space.
        ; """

xt_words:
w_words:
                ; we follow Gforth by starting on the next
                ; line
                jsr w_cr

                ; We pretty-format the output by inserting a line break
                ; before the end of the line. We can get away with pushing
                ; the counter to the stack because this is usually an
                ; interactive word and speed is not that important
                lda #0
                pha

                ; Set up for traversing the wordlist search order.
                dex                     ; Make room on the stack for
                dex                     ; a dictionary pointer.
                stz tmp3                ; Start at the beginning of
                                        ; the search order.
_wordlist_loop:
                ldy #num_order_offset   ; Check against byte variable #ORDER.
                lda tmp3
                cmp (up),y              ; See if we are done.
                bne _have_wordlist

                ; We ran out of wordlists to search.
                bra _words_done

_have_wordlist:
                ; start with last word in Dictionary
                ; Get the current wordlist id
                clc                     ; Index into byte array SEARCH-ORDER.
                adc #search_order_offset
                tay
                lda (up),y              ; Get the index into array WORDLISTS

                ; Get the DP for that wordlist.
                asl                     ; Turn offset into cells offset.
                clc
                adc #wordlists_offset
                tay
                lda (up),y              ; Save the DP for this wordlist
                sta 0,x                 ; on the stack. ( nt )
                iny
                lda (up),y
                sta 1,x

_loop:
                jsr w_dup              ; ( nt nt )
                jsr w_name_to_string   ; ( nt addr u )

                ; Insert line break if we're about to go past the end of the
                ; line
                pla
                clc
                adc 0,x
                ina                     ; don't forget the space between words
                cmp #MAX_LINE_LENGTH    ; usually 79
                bcc +

                jsr w_cr

                lda 0,x                 ; After going to next line, start
                ina                     ; with length of this word.
+
                pha
                jsr w_type             ; ( nt )

                lda #AscSP
                jsr emit_a

                lda 0,x
                sta tmp1
                lda 1,x
                sta tmp1+1
                jsr nt_to_nt
                beq _next_list          ; did we reach the end of the list?
                lda tmp1
                sta 0,x
                lda tmp1+1
                sta 1,x

                bra _loop
_next_list:
                ; Move on to the next wordlist in the search order.
                inc tmp3
                bra _wordlist_loop

_words_done:
                pla                     ; dump counter

                inx
                inx

z_words:        rts


