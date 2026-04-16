; ## D_MINUS ( d d -- d ) "Subtract two double-celled numbers"
; ## "d-"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DMinus"""
xt_d_minus:
                jsr underflow_4 ; two double numbers
w_d_minus:
                sec

                lda 6,x         ; LSB of lower word
                sbc 2,x
                sta 6,x

                lda 7,x         ; MSB of lower word
                sbc 3,x
                sta 7,x

                lda 4,x         ; LSB of upper word
                sbc 0,x
                sta 4,x

                lda 5,x         ; MSB of upper word
                sbc 1,x
                sta 5,x

                inx
                inx
                inx
                inx

z_d_minus:      rts


; ## D_PLUS ( d d -- d ) "Add two double-celled numbers"
; ## "d+"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DPlus"""
xt_d_plus:
                jsr underflow_4 ; two double numbers
w_d_plus:
                clc
                lda 2,x         ; LSB of lower word
                adc 6,x
                sta 6,x

                lda 3,x         ; MSB of lower word
                adc 7,x
                sta 7,x

                lda 0,x         ; LSB of upper word
                adc 4,x
                sta 4,x

                lda 1,x         ; MSB of upper word
                adc 5,x
                sta 5,x

                inx
                inx
                inx
                inx

z_d_plus:       rts


; ## D_TO_S ( d -- n ) "Convert a double number to single"
; ## "d>s"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DtoS
        ; Though this is basically just DROP, we keep it
        ; separate so we can test for underflow
        ; """

xt_d_to_s:
                jsr underflow_2
w_d_to_s:
                inx
                inx

z_d_to_s:       rts



; ## DABS ( d -- d ) "Return the absolute value of a double"
; ## "dabs"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DABS"""

xt_dabs:
                jsr underflow_2 ; double number
w_dabs:
                lda 1,x         ; MSB of high cell
                bpl _done       ; positive, we get off light

                ; negative, calculate 0 - d
                ldy #0
                sec

                tya
                sbc 2,x         ; LSB of low cell
                sta 2,x

                tya
                sbc 3,x         ; MSB of low cell
                sta 3,x

                tya
                sbc 0,x         ; LSB of high cell
                sta 0,x

                tya
                sbc 1,x         ; MSB of high cell
                sta 1,x
_done:
z_dabs:         rts



; ## DNEGATE ( d -- d ) "Negate double cell number"
; ## "dnegate"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DNEGATE"""
xt_dnegate:
                jsr underflow_2 ; double number
w_dnegate:
     		ldy #0
                sec

                tya
                sbc 2,x         ; LSB of low cell
                sta 2,x

                tya
                sbc 3,x         ; MSB of low cell
                sta 3,x

                tya
                sbc 0,x         ; LSB of high cell
                sta 0,x

                tya
                sbc 1,x         ; MSB of high cell
                sta 1,x

z_dnegate:      rts



; ## D_DOT ( d -- ) "Print double"
; ## "d."  auto  ANS double
        ; """http://forth-standard.org/standard/double/Dd"""
        ;
        ; From the Forth code:
        ; : D. TUCK DABS <# #S ROT SIGN #> TYPE SPACE ;
        ; """

xt_d_dot:
                jsr underflow_2
w_d_dot:
                jsr w_tuck
                jsr w_dabs
                jsr w_less_number_sign
                jsr w_number_sign_s
                jsr w_rot
                jsr w_sign
                jsr w_number_sign_greater
                jsr w_type
                jsr w_space

z_d_dot:        rts


; ## D_DOT_R ( d u -- ) "Print double right-justified u wide"
; ## "d.r"  auto  ANS double
        ; """http://forth-standard.org/standard/double/DDotR"""
        ; Based on the Forth code
        ;  : D.R >R TUCK DABS <# #S ROT SIGN #> R> OVER - SPACES TYPE ;
        ; """

xt_d_dot_r:
                jsr underflow_3
w_d_dot_r:
                ; From the forth code:
                jsr w_to_r
                jsr w_tuck
                jsr w_dabs
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

z_d_dot_r:      rts


; ## M_STAR_SLASH ( d1 n1 n2 -- d2 ) "Multiply d1 by n1 and divide the triple result by n2.  All values are signed."
; ## "m*/"  auto  ANS double
        ; """https://forth-standard.org/standard/double/MTimesDiv"""
        ; From All About FORTH, MVP-Forth, public domain,
        ; from this forth code which is modified slightly for Tali2:
        ; DDUP XOR SWAP ABS >R SWAP ABS >R OVER XOR ROT ROT DABS
        ; SWAP R@ UM* ROT R> UM* ROT 0 D+ R@ UM/MOD ROT ROT R> UM/MOD
        ; SWAP DROP SWAP ROT 0< if dnegate then
xt_m_star_slash:
                jsr underflow_4
w_m_star_slash:
                ; DDUP XOR SWAP ABS >R SWAP ABS >R OVER XOR ROT ROT DABS
                ; this looks like ( n1^n2^dhi |d1| ) (R: |n2| |n1| )
                ; but we'll do something slightly different to avoid R:
                ; we want |d1| |n1| |n2| along with the sign bit from dhi^n1^n2

                lda 1,x
                eor 3,x
                eor 5,x
                pha                     ; stash the sign bit on the return stack

                jsr w_abs               ; ( d1 n1 |n2| )
                jsr w_swap
                jsr w_abs               ; ( d1 |n2| |n1| )
                jsr w_two_swap
                jsr w_dabs              ; ( |n2| |n1| |d1| )

                ; we want something akin to
                ; SWAP R@ UM* ROT R> UM* ROT 0 D+ R@ UM/MOD ROT ROT R> UM/MOD
                ; but we have |n2| and |n1| on the data stack
                jsr w_swap
                lda 4,x                 ; pick |n1|
                ldy 5,x
                jsr push_ya_tos

                jsr w_um_star           ; ( |n2| |n1| |dhi| d|dlo*n1| ) uses tmp1-3
                jsr w_two_swap          ; ( |n2| d|dhlo*n1| |n1| |dhi| )
                jsr w_um_star           ; ( |n2| d|dlo*n1| d|dhi*n1| )
                jsr w_rot

                jsr w_zero
                jsr w_d_plus            ; ( |n2| t|uvw| )

                ; pick |n2| from under the triple result
                lda 6,x
                ldy 7,x
                jsr push_ya_tos         ; ( |n2| |uvw| |n2| )

                ; do the triple division in two double steps (uses tmpdsp)
                jsr w_um_slash_mod      ; ( |n2| |w| r qhi )
                lda 0,x                 ; swap qhi with |n2|
                ldy 6,x
                sty 0,x
                sta 6,x

                lda 1,x                 ; leaving ( qhi |w| r |n2| )
                ldy 7,x
                sty 1,x
                sta 7,x

                jsr w_um_slash_mod      ; ( qhi r' qlo )

                ; finally we want
                ; SWAP DROP SWAP ROT 0< if dnegate then ;
                ; which just ditches the last remainder and gets
                ; the double result in the right order,
                ; something like NIP SWAP sgn if DNEGATE then ;
                jsr w_nip
                jsr w_swap              ; ( |qd| )

                pla                     ; check and apply sign if needed
                bpl +                   ; ... 0< if ...
                jsr w_dnegate
+
z_m_star_slash: rts


; ## TWO_CONSTANT (C: d "name" -- ) ( -- d) "Create a constant for a double word"
; ## "2constant"  auto  ANS double
        ; """https://forth-standard.org/standard/double/TwoCONSTANT
        ;
        ; Body is two word push templates: first pushes NOS, then TOS.
        ; No HC, no NN — fully inlineable.
        ; """
xt_two_constant:
                jsr underflow_2
w_two_constant:
                jsr w_colon
                jsr w_two_literal
                jmp w_semicolon

z_two_constant:


; ## TWO_LITERAL (C: d -- ) ( -- d) "Compile a literal double word"
; ## "2literal"  auto  ANS double
        ; """https://forth-standard.org/standard/double/TwoLITERAL"""
        ; """
xt_two_literal:
                jsr underflow_2         ; double number
w_two_literal:
                ; the four byte double word UNIX is represented on the data stack
                ; as TOS .byte N, U and NOS .byte X, I
                ; ( XI NU )
                ; If we end up inlining using two w_literal sequences
                ; we need the runtime to push the current NOS=XI first.
                ; But if we can't inline we want push_inline_2literal .word NU, XI.
                lda cp                  ; remember LSB of cp so we can rewind
                pha
                jsr w_over
                ; ( XI NU XI)
                jsr w_literal           ; leaves C=0 if we inlined
                pla                     ; recover cp LSB
                bcs _no_inline

                jsr w_literal           ; inline the second word
                jmp w_drop              ; drop the spare copy of XI

_no_inline:
                ; inline failed so rewind and use ( XI NU ) to compile:
                ;       jsr push_inline_2literal
                ;       .word NU, XI
                cmp cp                  ; compare old cp in A with current cp
                bcc +                   ; if A > cp (C=1) we wrapped a page
                dec cp+1
+
                sta cp
                jsr cmpl_call_inline_literal
                .word push_inline_2literal

                jsr w_comma             ; add the two payload words
                jmp w_comma

z_two_literal:


; ## TWO_VARIABLE ( "name" -- ) "Create a variable for a double word"
; ## "2variable"  auto  ANS double
        ; """https://forth-standard.org/standard/double/TwoVARIABLE
        ;
        ; Body is a word push template for the PFA address, followed
        ; by 4 bytes of uninitialized storage.  No HC, no NN.
        ; """
xt_two_variable:
w_two_variable:
                ; push the address of the variable storage (HERE)
                jsr w_here

                ; allocate 4 bytes of storage, initialized to 0
                lda #0
                tay
                jsr cmpl_word_ya
                jsr cmpl_word_ya
                ; ( addr )

                ; Now define a constant that pushes this address
                jsr w_constant
z_two_variable: rts



; ## UD_DOT ( d -- ) "Print double as unsigned"
; ## "ud."  auto  Tali double
        ;
        ; """Based on the Forth code  : UD. <# #S #> TYPE SPACE ;
        ; """
xt_ud_dot:
                jsr underflow_2 ; double number
w_ud_dot:
                jsr w_less_number_sign
                jsr w_number_sign_s
                jsr w_number_sign_greater
                jsr w_type
                jsr w_space

z_ud_dot:       rts



; ## UD_DOT_R ( d u -- ) "Print unsigned double right-justified u wide"
; ## "ud.r"  auto  Tali double
        ;
        ; """Based on the Forth code : UD.R  >R <# #S #> R> OVER - SPACES TYPE ;
        ; """
xt_ud_dot_r:
                jsr underflow_3
w_ud_dot_r:
                jsr w_to_r
                jsr w_less_number_sign
                jsr w_number_sign_s
                jsr w_number_sign_greater
                jsr w_r_from
                jsr w_over
                jsr w_minus
                jsr w_spaces
                jsr w_type

z_ud_dot_r:      rts


; TODO
; D=
; DMAX
; DMIN
; D2/
; D2*
; DU<
; D0=
; D0<
; D<
; M+
; 2ROT
; 2VALUE

; ## D_EQUALS ( d d -- f ) "flag is true if two doubles are equal"
; ## "d="  auto  ANS double
        ; """https://forth-standard.org/standard/double/DEqual"""
xt_d_equals:
                jsr underflow_4 ; two double numbers
w_d_equals:
                clc
                lda 2,x         ; LSB of lower word
                cmp 6,x
                bne _d_equals_false

                lda 3,x         ; MSB of lower word
                cmp 7,x
                bne _d_equals_false

                lda 0,x         ; LSB of upper word
                cmp 4,x
                bne _d_equals_false

                lda 1,x         ; MSB of upper word
                cmp 5,x
                bne _d_equals_false

_d_equals_true:
                ; Put flag in A
                lda #$FF
                jmp _d_equals_done
_d_equals_false:
                lda #$00
_d_equals_done:
                inx             ; Remove a double
                inx
                inx             
                inx
                inx             ; Remove half a double
                inx
                sta 0,x         ; Save the flag on TOS
                sta 1,x

z_d_equals:       rts
