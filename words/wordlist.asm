; ## ALSO ( -- ) "Make room in the search order for another wordlist"
; ## "also"  auto  ANS search ext
        ; """http://forth-standard.org/standard/search/ALSO"""
xt_also:
w_also:
                jsr w_get_order
                jsr w_over
                jsr w_swap
                jsr w_one_plus
                jsr w_set_order

z_also:         rts



; ## ASSEMBLER_WORDLIST ( -- u ) "WID for the Assembler wordlist"
; ## "assembler-wordlist"  tested  Tali Assembler
        ; """ Commonly used like `assembler-wordlist >order` to add the
        ; assembler words to the search order so they can be used.
        ; See the tutorial on Wordlists and the Search Order for
        ; more information.
        ;
        ; This is a dummy entry, the code is shared with TWO
        ; """



; ## DEFINITIONS ( -- ) "Make first wordlist in search order the current wordlist"
; ## "definitions" auto ANS search
xt_definitions:
w_definitions:
                ldy #search_order_offset    ; Transfer byte variable
                lda (up),y                  ; SEARCH_ORDER[0] to
                ldy #current_offset         ; byte variable CURRENT.
                sta (up),y
z_definitions:  rts



; ## EDITOR_WORDLIST ( -- u ) "WID for the Editor wordlist"
; ## "editor-wordlist"  tested  Tali Editor
        ; """ Commonly used like `editor-wordlist >order` to add the editor
        ; words to the search order so they can be used.  This will need
        ; to be done before any of the words marked "Tali Editor" can be
        ; used.  See the tutorial on Wordlists and the Search Order for
        ; more information.

        ;
        ; This is a dummy entry, the code is shared with ONE
        ; """



; ## FORTH ( -- ) "Replace first WID in search order with Forth-Wordlist"
; ## "forth"  auto  ANS search ext
        ; """https://forth-standard.org/standard/search/FORTH"""
xt_forth:
w_forth:
                ldy #search_order_offset
                lda #0          ; The WID for Forth is 0.

                sta (up),y
z_forth:
                rts



; ## FORTH_WORDLIST ( -- u ) "WID for the Forth Wordlist"
; ## "forth-wordlist"  auto  ANS search
        ; """https://forth-standard.org/standard/search/FORTH-WORDLIST"""
        ; This is a dummy entry, the actual code is shared with ZERO.



; ## GET_CURRENT ( -- wid ) "Get the id of the compilation wordlist"
; ## "get-current" auto ANS search
        ; """https://forth-standard.org/standard/search/GET-CURRENT"""

xt_get_current:
w_get_current:
                ; This is a little different than some of the variables
                ; in the user area as we want the value rather than
                ; the address.
                dex
                dex
                ldy #current_offset
                lda (up),y
                sta 0,x         ; CURRENT is a byte variable
                stz 1,x         ; so the MSB is zero.

z_get_current:  rts



; ## GET_ORDER ( -- wid_n .. wid_1 n) "Get the current search order"
; ## "get-order" auto ANS search
        ; """https://forth-standard.org/standard/search/GET-ORDER"""

xt_get_order:
w_get_order:
                ; Get #ORDER - the number of wordlists in the search order.
                ldy #num_order_offset
                lda (up),y
                sta tmp1
                beq _done       ; If zero, there are no wordlists.

_loop:
                ; Count down towards the front of the list.
                ; By decrementing first, we also turn the length into an offset.
                dec tmp1        ; Count down by bytes.

                ; Get a pointer to the current wordlist, working back to front.
                lda #search_order_offset
                clc
                adc tmp1
                tay

                ; Put that wordlist id on the stack.
                dex
                dex
                lda (up),y
                sta 0,x         ; Search order array is bytes, so
                stz 1,x         ; put a zero in the high byte.

                ; See if that was the last one to process (first in the list).
                lda #0
                cmp tmp1
                bne _loop

_done:
                ; Put the number of items on the stack.
                dex
                dex
                ldy #num_order_offset
                lda (up),y
                sta 0,x
                stz 1,x         ; We only support 8 wordlists.

z_get_order:    rts



; ## ONLY ( -- ) "Set earch order to minimum wordlist"
; ## "only"  auto  ANS search ext
        ; """https://forth-standard.org/standard/search/ONLY"""

xt_only:
w_only:
                ; Put -1 on data stack.
                jsr w_true
                ; Invoke set-order to set the minimum search order.
                jsr w_set_order

z_only:         rts



; ## ORDER ( -- ) "Print current word order list and current WID"
; ## "order"  auto  ANS core
        ; """https://forth-standard.org/standard/search/ORDER
        ; Note the search order is displayed from first search to last
        ; searched and is therefore exactly the reverse of the order in which
        ; Forth stacks are displayed.
        ;
        ; A Forth implementation of this word is:
        ;
        ; 	: .wid ( wid -- )
        ; 	dup 0=  if ." Forth "  drop    else
        ; 	dup 1 = if ." Editor " drop    else
        ; 	dup 2 = if ." Assembler " drop else
        ; 	dup 3 = if ." Root " drop      else
        ; 	           . ( just print the number )
        ; 	then then then then ;
        ;
        ; : ORDER ( -- )
        ; 	cr get-order 0 ?do .wid loop
        ; 	space space get-current .wid ;
        ;
        ; This is an interactive program, so speed
        ; is not as important as size. We assume we do not have more than 255
        ; wordlists.
        ; """

xt_order:
w_order:
                jsr w_cr
                jsr w_get_order        ; ( wid_n ... wid_1 n )

                ; Paranoid: Check if there are no wordlists, a rather
                ; pathological case. this would mean ( 0 ) on the stack. In
                ; that case, we just drop n and run
                lda 0,x                 ; assumes no more than 255 wordlists
                beq _drop_done

                ; We arrive here with the LSB of TOS in A, the number of WIDs
                ; on the stack
                tay
_loop:
                inx
                inx                     ; DROP, now ( wid_n ... wid_1 )
                lda 0,x

                phy
                jsr order_print_wid_string   ; internal helper function
                ply

                dey
                bne _loop

                ; We've printed the wordlists, now we add the current wordlist.
                ; This follows the convention of Gforth
                jsr w_space
                jsr w_space
                jsr w_get_current      ; ( wid )

                lda 0,x
                jsr order_print_wid_string
                jsr w_cr

_drop_done:
                inx
                inx
z_order:
                rts

order_print_wid_string:
        ; """Helper function for ORDER: Given a WID in A, print the
        ; corresponding string. If there is no such word list defined, just
        ; print the number. Assumes we will not have more than 256 WIDs; also
        ; assumes we have just loaded A so Z reflects status of byte.  In
        ; theory, we could speed this up by having the WID be the same as the
        ; number of the strings. However, ORDER is used rather infrequently and
        ; this would make changes to the strings.asm file very dangerous, so we
        ; follow the slightly more complicated route with a translation table.
        ; """
                ; If the WID is larger than 3, we have no string avaliable and
                ; just print the number.
                ; See http://6502.org/tutorials/compare_instructions.html
                ; for details
                cmp #4
                bcc _output_string      ; less than 4, print a real string

                ; Our WID is not less than 4, that is, 4 or larger. We just
                ; print the number
                dex
                dex
                sta 0,x
                stz 1,x
                jmp w_u_dot            ; JSR/RTS as this routine is not compiled

_output_string:
                ; Get the string number based on WID 0 to 3
                tay
                lda _wid_data,y

                ; Print without a line feed
                jmp print_string_no_lf  ; JSR/RTS as this routine is not compiled

_wid_data:
        ; Table of string numbers (see strings.asm) indexed by the WID if
        ; less than 4.
        .byte str_wid_forth            ; WID 0: "Forth"
        .byte str_wid_editor           ; WID 1: "Editor"
        .byte str_wid_assembler        ; WID 2: "Assembler"
        .byte str_wid_root             ; WID 3: "Root"



; ## PREVIOUS ( -- ) "Remove the first wordlist in the search order"
; ## "previous"  auto  ANS search ext
        ; """http://forth-standard.org/standard/search/PREVIOUS"""

xt_previous:
w_previous:
                jsr w_get_order
                jsr w_nip
                jsr w_one_minus
                jsr w_set_order

z_previous:     rts



; ## ROOT_WORDLIST ( -- u ) "WID for the Root (minimal) wordlist"
; ## "root-wordlist"  tested  Tali Editor
xt_root_wordlist:
w_root_wordlist:
                dex             ; The WID for the Root wordlist is 3.
                dex
                lda #3
                sta 0,x
                stz 1,x

z_root_wordlist:
                rts



; ## SEARCH_WORDLIST ( caddr u wid -- 0 | xt 1 | xt -1) "Search for a word in a wordlist"
; ## "search-wordlist" auto ANS search
        ; """https://forth-standard.org/standard/search/SEARCH_WORDLIST"""

xt_search_wordlist:
                jsr underflow_3
w_search_wordlist:
                ; Set up tmp1 with the wordlist indicated by wid
                ; on the stack. Start by putting the base address
                ; of the wordlists in tmp2.
                lda up
                clc
                adc #wordlists_offset
                sta tmp2
                lda up+1
                adc #0          ; Adding carry
                sta tmp2+1

                ; Add the wid (in cells) to the base address.
                lda 0,x
                asl             ; Convert wid to offset in cells (x2)
                adc tmp2
                sta tmp2
                bcc +
                inc tmp2+1      ; Propagate carry if needed.

                ; tmp2 now holds the address of the dictionary pointer
                ; for the given wordlist.
+
                ; Remove the wid from the stack.
                inx
                inx

                ; check for special case of an empty string (length zero)
                lda 0,x
                ora 1,x
                beq _done

                ; Check for special case of empty wordlist
                ; (dictionary pointer, in tmp2, is 0)
                lda tmp2
                ora tmp2+1
                beq _done

                ; set up first loop iteration
                lda (tmp2)              ; nt of first word in Dictionary
                sta tmp1

                inc tmp2                ; Move to the upper byte
                bne +
                inc tmp2+1
+
                lda (tmp2)
                sta tmp1+1

                jsr find_header_name
                beq _fail_done

                ; The strings match. Drop the count and put correct nt TOS
                inx
                inx
                lda tmp1
                sta 0,x
                lda tmp1+1
                sta 1,x

                ; Change the nt into an xt, but save a copy of the nt
                ; to look up whether the word is immediate or not.
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
                bra _done_nodrop

_immediate:
                lda #1                  ; We're immediate, return 1
                sta 0,x
                stz 1,x

                bra _done_nodrop

_fail_done:
                stz 2,x         ; failure flag
                stz 3,x
_done:
                inx
                inx
_done_nodrop:
z_search_wordlist:
                rts



; ## SET_CURRENT ( wid -- ) "Set the compilation wordlist"
; ## "set-current" auto ANS search
        ; """https://forth-standard.org/standard/search/SET-CURRENT"""

xt_set_current:
                jsr underflow_1
w_set_current:
                ; Save the value from the data stack.
                ldy #current_offset
                lda 0,x         ; CURRENT is byte variable
                sta (up),y      ; so only the LSB is used.

                inx
                inx

z_set_current:  rts



; ## SET_ORDER ( wid_n .. wid_1 n -- ) "Set the current search order"
; ## "set-order" auto ANS search
        ; """https://forth-standard.org/standard/search/SET-ORDER"""

xt_set_order:
w_set_order:
                ; Test for -1 TOS
                lda #$FF
                cmp 1,x
                bne _start
                cmp 0,x
                bne _start

                ; There is a -1 TOS.  Replace it with the default
                ; search order, which is just the FORTH-WORDLIST.
                dex             ; Make room for the count.
                dex
                stz 3,x         ; ROOT-WORDLIST is 3
                lda #3
                sta 2,x
                stz 1,x         ; Count is 1.
                lda #1
                sta 0,x

                ; Continue processing with ( forth-wordlist 1 -- )
_start:
                ; Set #ORDER - the number of wordlists in the search order.
                ldy #num_order_offset
                lda 0,x
                sta (up),y      ; #ORDER is a byte variable.
                sta tmp1        ; Save a copy for zero check and looping.
                                ; Only the low byte is saved in tmp1 as
                                ; only 8 wordlists are allowed.

                inx             ; Drop the count off the data stack.
                inx

                ; Check if there are zero wordlists.
                lda tmp1
                beq _done       ; If zero, there are no wordlists.

                ; Move the wordlist ids from the data stack to the search order.
                ldy #search_order_offset
_loop:
                ; Move one wordlist id over into the search order.
                lda 0,x         ; The search order is a byte array
                sta (up),y      ; so only save the LSB
                iny

                ; Remove it from the data stack.
                inx
                inx

                ; See if that was the last one to process (first in the list).
                dec tmp1
                bne _loop

_done:
z_set_order:    rts



; ## TO_ORDER ( wid -- ) "Add wordlist at beginning of search order"
; ## ">order"  tested  Gforth search
        ; """https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Lists.html"""

xt_to_order:
w_to_order:
                ; Put the wid on the return stack for now.
                jsr w_to_r

                ; Get the current search order.
                jsr w_get_order

                ; Get back the wid and add it to the list.
                jsr w_r_from
                jsr w_swap
                jsr w_one_plus

                ; Set the search order with the new list.
                jsr w_set_order

z_to_order:     rts



; ## WORDLIST ( -- wid ) "Create new wordlist (from pool of 8)"
; ## "wordlist" auto ANS search
        ; """https://forth-standard.org/standard/search/WORDLIST
        ; See the tutorial on Wordlists and the Search Order for
        ; more information.
        ; """

xt_wordlist:
w_wordlist:
                ; Get the current number of wordlists
                ldy #num_wordlists_offset
                lda (up),y      ; This is a byte variable, so only
                                ; the LSB needs to be checked.

                ; See if we are already at the max.
                cmp #max_wordlists
                bne _ok

                ; Print an error message if all wordlists used.
                lda #err_wordlist
                jmp error

_ok:
                ina             ; Increment the wordlist#
                sta (up),y      ; Save it into byte variable #wordlists
                dex             ; and put it on the stack.
                dex
                sta 0,x
                stz 1,x         ; 12 is the max, so upper byte is always zero.

z_wordlist:     rts
