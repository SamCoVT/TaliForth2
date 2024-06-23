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
; ## ".s"  tested  ANS tools
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
                dex             ; DUP
                dex
                sta 0,x
                stz 1,x

                jsr print_u

                lda #'>'
                jsr emit_a
                lda #AscSP      ; ASCII for SPACE
                jsr emit_a

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

                lda #dsp0-1     ; go up one to avoid garbage
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
; ## "dump"  tested  ANS tools
        ; """https://forth-standard.org/standard/tools/DUMP
        ;
        ; DUMP's exact output is defined as "implementation dependent".
        ; This is in assembler because it is
        ; useful for testing and development, so we want to have it work
        ; as soon as possible. Uses TMP2
        ; """

xt_dump:
                jsr underflow_2
w_dump:
_row:
                ; start counter for 16 numbers per row
                ldy #16

                ; We use TMP2 as the index for the ASCII characters
                ; that we print at the and of the hex block. We
                ; start saving them at HERE (CP)
                stz tmp2

                jsr w_cr

                ; print address number
                lda 3,x
                jsr byte_to_ascii
                lda 2,x
                jsr byte_to_ascii

                jsr w_space
                jsr w_space
_loop:
                ; if there are zero bytes left to display, we're done
                lda 0,x
                ora 1,x
                beq _all_printed

                ; dump the contents
                lda (2,x)
                pha                     ; byte_to_ascii destroys A
                jsr byte_to_ascii
                jsr w_space
                pla

                ; Handle ASCII printing
                jsr is_printable
                bcs _printable
                lda #'.'                 ; Print dot if not printable
_printable:
                phy                     ; save counter
                ldy tmp2
                sta (cp),y
                inc tmp2
                ply

                ; extra space after eight bytes
                cpy #9
                bne _next_char
                jsr w_space

_next_char:
                inc 2,x
                bne _counter
                inc 3,x

_counter:
                ; loop counter
                lda 0,x
                bne +
                dec 1,x
+
                dec 0,x
                dey
                bne _loop               ; next byte

                ; Done with one line, print the ASCII version of these
                ; characters
                jsr w_space
                jsr dump_print_ascii

                bra _row                ; new row

_all_printed:
                ; See if there are any ASCII characters in the buffer
                ; left to print
                lda tmp2
                beq _done

                ; In theory, we could try to make the ASCII part line
                ; up with the line before it. But that is a hassle (we
                ; use three bytes for each missed hex entry, and
                ; then there is the gap after eight entries) and it
                ; makes it harder to read. We settle for one extra
                ; space instead for the moment
                jsr w_space
                jsr dump_print_ascii
_done:
                jsr w_two_drop         ; one byte less than 4x INX
z_dump:         rts


dump_print_ascii:
                ; Print the ASCII characters that we have saved from
                ; HERE (CP) to HERE plus whatever is in TMP2. This routine
                ; is not compiled (DUMP is probably never compiled anyway)
                ; but we keep it inside the scope of DUMP.
                ldy #0
_ascii_loop:
                lda (cp),y
                jsr emit_a
                iny

                ; extra space after eight chars
                cpy #8
                bne +
                jsr w_space
+
                dec tmp2
                bne _ascii_loop

                rts



; ## QUESTION ( addr -- ) "Print content of a variable"
; ## "?"  tested  ANS tools
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
; ## "see" tested  ANS tools
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
                jsr print_string_no_lf

                jsr w_dup              ; ( nt nt )
                jsr w_u_dot
                jsr w_space            ; ( nt )

                jsr w_dup              ; ( nt nt )
                jsr w_name_to_int      ; ( nt xt )

                lda #str_see_xt
                jsr print_string_no_lf

                jsr w_dup              ; ( nt xt xt )
                jsr w_u_dot
                jsr w_cr               ; ( nt xt )

                ; We print letters for flags and then later follow it with 1 or
                ; 0 to mark if which flag is set
                lda #str_see_flags
                jsr print_string_no_lf

                jsr w_over             ; ( nt xt nt )
                jsr w_one_plus         ; ( nt xt nt+1 )
                jsr w_fetch            ; ( nt xt flags )

                lda 0,x

                ; This is crude, but for the moment it is good enough
                ldy #6                  ; Not all bits are used
_flag_loop:
                pha
                and #%00000001
                clc
                adc #'0'
                jsr emit_a
                jsr w_space

                pla
                ror                     ; Next flag

                dey
                bne _flag_loop

                jsr w_cr

                inx
                inx                     ; ( nt xt )

                ; Figure out the size
                lda #str_see_size
                jsr print_string_no_lf

                jsr w_swap             ; ( xt nt )
                jsr w_wordsize         ; ( xt u )
                jsr w_dup              ; ( xt u u ) for DUMP and DISASM
                jsr w_decimal
                jsr w_u_dot            ; ( xt u )
                jsr w_hex
                jsr w_cr

                ; Dump hex and disassemble
.if "disassembler" in TALI_OPTIONAL_WORDS
                jsr w_two_dup          ; ( xt u xt u )
.endif
                jsr w_dump
                jsr w_cr
.if "disassembler" in TALI_OPTIONAL_WORDS
                jsr w_disasm
.endif
                pla
                sta base

z_see:          rts



; ## WORDS ( -- ) "Print known words from Dictionary"
; ## "words"  tested  ANS tools
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

                ; get next word, which begins two down
                jsr w_one_plus         ; 1+
                jsr w_one_plus         ; 1+
                jsr w_fetch            ; @ ( nt+1 )

                ; if next address is zero, we're done
                lda 0,x
                ora 1,x
                bne _loop

                ; Move on to the next wordlist in the search order.
                inc tmp3
                bra _wordlist_loop

_words_done:
                pla                     ; dump counter

                inx
                inx

z_words:        rts
