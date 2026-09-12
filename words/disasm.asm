; Disassembler for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Updated by Sam Colwell and Patrick Surry
; First version: 28. Apr 2018
; This version: 06. Apr 2024

; This is the default disassembler for Tali Forth 2. Use by passing
; the address and length of the block of memory to be disassembled:
;
;       disasm ( addr x -- )

; The underflow checking is handled by the word's stub in words/*.asm, see
; there for more information.

; The code is disassembled in Simpler Assembler Notation (SAN), because that
; is, uh, simpler. See the documentation and https://github.com/scotws/SAN for
; more information. Because disassemblers are used interactively with slow
; humans, we don't care that much about speed and put the emphasis at being
; small.

; Uses: tmp3, tmp2, tmp1 (xt_u_dot_r uses xt_type which uses tmp1)
;       scratch (used for handling literals and JSRs)


; ## DISASM ( addr u -- ) "Disassemble a block of memory"
; ## "disasm"  auto  Tali Forth
        ; """Convert a segment of memory to assembler output. This
        ; word is vectored so people can add their own disassembler.
        ; Natively, this produces Simpler Assembly Notation (SAN)
        ; code, see the section on The Disassembler in the manual and
        ; the file disassembler.asm for more details.
        ; """
xt_disasm:
                jsr underflow_2
w_disasm:
                jsr disassembler

z_disasm:       rts



disassembler:
                jsr w_cr            ; ( addr u )
_byte_loop:
                ; Print address at start of the line. Note we use whatever
                ; number base the user has
                jsr w_over          ; ( addr u addr )
                jsr w_u_dot         ; ( addr u )
                jsr w_space

                lda 2,x
                sta tmp2
                lda 3,x
                sta tmp2+1

                lda (tmp2)          ; get opcode that addr points to
                sta scratch         ; save a copy
                jsr op_find_nt      ; get NT or 0 in tmp1
                lda tmp1+1
                bne _found          ; MSB 0 means we failed

                ; If we don't recognize the opcode as an assembler word
                ; use nt_question which will show "?" as the name
                ; and assume it has length 1

                tay                 ; Y=0
                lda #>nt_question
                pha
                lda #<nt_question
                pha
                phy                 ; flag length as 1
                bra _no_operand

_found:
                ; Since this is Simpler Assembler Notation (SAN) in a Forth
                ; system, we want to print any operand before we print the
                ; mnemonic ('1000 sta' instead of 'sta 1000'). This allows us
                ; to copy and paste directly from the disassembler to the
                ; assembler.

                pha                 ; stash MSB of NT for later
                lda tmp1
                pha                 ; LSB of NT

                lda scratch         ; fetch the opcode
                jsr op_length       ; get length Y = 1,2,3
                dey
                phy                 ; save length
                beq _no_operand

                ; copy Y=1 or 2 operand bytes to scratch+1/2
                ; and advance our string
                tya
                stz scratch+2       ; set MSB to zero in case there isn't one
_copy_operand:
                lda (tmp2),y
                sta scratch,y
                jsr slash_string_1
                dey
                bne _copy_operand

                ; ( addr+n u-n )
                lda scratch+1       ; add operand to stack
                ldy scratch+2
                jsr push_ya_tos
                ; ( addr+n u-n operand )

_no_operand:
                ; We arrive here with either C=0 and no operand ( addr n )
                ; or C=1 and ( addr+n u-n opr )
                ; We want the output to be nicely formatted in columns.
                ; The maximal width of a 16-bit operand in decimal
                ; is five characters so we either use U.R to format it
                ; or simply indent the mnemonic by five spaces.

                jsr push_inline_bliteral      ; ( addr+n u-n [opr] 5 )
                .byte 5

                ply                 ; recall length
                bne _print_operand

                jsr w_spaces        ; no operand, just indent
                bra _print_mnemonic

_print_operand:
                jsr w_u_dot_r       ; right-justify the operand
_print_mnemonic:
                ; ( addr+n u-n )
                jsr w_r_from        ; put NT on stack

                ; ( addr+n u-n nt )
                jsr w_space
                jsr w_name_to_string
                jsr w_type

                ; ( addr+n u-n )

                ; Handle JSR by printing name of function, if available.
                ; scratch has opcode ($20 for JSR)
                ; scratch+1 and scratch+2 have address if it's a JSR.
                lda scratch
                cmp #OpJSR
                bne _not_jsr

                ; It's a JSR.  Print 5 spaces as an offset.
                jsr push_inline_bliteral
                .byte 5
                jsr w_spaces

                jsr disasm_special
                bcs _printing_done

                ; Try the generic JSR handler, which will use the target of the
                ; JSR as an XT and print the name if it exists.
                jsr disasm_jsr
                bcs _printing_done

_not_jsr:
                ; is it a native branch instruction with one byte relative addressing?
                ; opcodes are bra: $80 and bxx: %xxx1 0000
                ; if so we'll display the branch target address

                ; destructive test on opcode in A
                cmp #OpBRA            ; is it bra?
                beq _is_rel
                and #$1F
                eor #$10            ; do bottom five bits match xxx10000 ?
                bne _printing_done
_is_rel:
                ; treat opr as signed byte and add to addr following operand: (addr+1) + 1
                ; scratch+1 contains the operand (offset), stack has (addr+1 u-1)
                ldy #'v'            ; we'll indicate branch forward or back with v or ^
                lda scratch+1       ; Put offset on stack
                jsr push_a_tos
                lda scratch+1       ; Check for negative
                bpl +
                dec 1,x             ; for negative offsets extend the sign bit so add works out
                ldy #'^'            ; it's a backward branch
+               sec                 ; start counting from address after opcode
                adc 4,x
                sta 0,x
                lda 1,x
                adc 5,x
                sta 1,x

                phy                 ; save the direction indicator

                jsr push_inline_bliteral
                .byte 9
                jsr w_u_dot_r       ; print the destination with 5 leading spaces

                lda #AscSP          ; print space and branch direction indicator
                jsr emit_a
                pla
                jsr emit_a

_printing_done:
                jsr w_cr

                ; Housekeeping: Next byte
                jsr slash_string_1      ; ( addr u -- addr+1 u-1 )

                lda 0,x                 ; All done?
                ora 1,x
                beq _done

                lda 1,x                 ; Catch mid-instruction ranges
                bmi _done

                jmp _byte_loop          ; out of range for BRA
_done:
                ; Clean up and leave
                jmp w_two_drop         ; JSR/RTS


; Handlers for various special disassembled instructions:

; JSR handler
disasm_jsr:
                ; The address of the JSR is in scratch+1 and scratch+2.
                ; The current stack is already ( addr u ) where addr is the address of the last byte of
                ; the JSR target address, and we want to leave it like that so moving on to the next byte
                ; works properly.
                ; Put the target address on the stack and see if it's an XT.
                lda scratch+1
                ldy scratch+2
                jsr push_ya_tos
                ; ( xt )
                jsr w_int_to_name
                ; int>name returns zero if we just don't know.
                lda 0,x
                ora 1,x
                bne _found_nt

                ; So we didn't find the JSR target in the dictionary.
                ; Check again at address-3 in case this is a JSR that
                ; skipped underflow checking during compiling by adding
                ; 3 to the JSR address.
                lda scratch+1
                sec
                sbc #3         ; Subtract 3 this time.
                sta 0,x
                lda scratch+2
                sbc #0         ; Subtract the carry if needed.
                sta 1,x
                ; ( xt )
                ; double-check that xt points to JSR underflow_N
                ; see discussion at https://github.com/SamCoVT/TaliForth2/pull/99#discussion_r1636394433
                jsr w_dup
                jsr has_uf_check
                bcc _no_nt

                jsr w_int_to_name     ; Try looking again
                ; int>name returns zero if we just don't know.
                lda 0,x
                ora 1,x
                beq _no_nt

_found_nt:
                ; We now have a name token ( nt ) on the stack.
                ; Change it into the name and print it.
                jsr w_name_to_string
                jsr w_type
                sec
                rts

_no_nt:
                ; Drop the TOS as there is no usable nt
                inx
                inx
                clc
                rts


disasm_special:
                ldy #(_end_handlers - _special_handlers - 4)
_check:         lda _special_handlers,y
                cmp scratch+1
                bne _next
                lda _special_handlers+1,y
                cmp scratch+2
                beq _found_handler
_next:          dey
                dey
                dey
                dey
                bpl _check

                clc
                rts

_found_handler:
                lda _special_handlers+3,y   ; payload + prefix
                phy
                pha                         ; save a copy
                cmp #4                      ; non-zero prefix in top six bits?
                bcc _no_prefix

                lsr                         ; extract the prefix char stored as (ch - 32) << 2
                lsr
                clc
                adc #32
                jsr emit_a                  ; show the prefix

_no_prefix:
                lda _special_handlers+2,y   ; get the handler's label index
                jsr print_string_n          ; show the label

                pla
                ply
                cpy #_payload_handlers-_special_handlers
                bcc _done                   ; is there a payload?

                and #3                      ; extract payload index 0-3
                tay                         ; save in Y
                phy                         ; and keep a copy

                ; ( addr u )
                lda 2,x                     ; set payload addr for pictured literal
                sta tmp1                    ; just like a return address it points one
                lda 3,x                     ; byte before the actual payload
                sta tmp1+1

                lda _pictured_literals,y    ; fetch the literal picture
                ldy #%1000_0000             ; request return here, not past payload
                jsr push_inline_pictured_y  ; fetch the payload to TOS based on A, Y

                ; depending on the payload we'll now have:
                ; ( addr u n ) for a byte or word literal
                ; ( addr u nd ) for a double literal
                ; ( addr u saddr n ) for a string literal

                ply                         ; recover the payload type

                lda tmp1+1                  ; save updated addr' with >R so we can
                pha                         ; deal with it after we've displayed the payload
                lda tmp1
                pha

                cpy #0                      ; is it a double?
                bne +
                jsr w_d_dot
                bra _skip_payload
+
                cpy #3                      ; is it a string?
                php
                bne +
                jsr w_dup                   ; for string show both the length and then a snippet
+
                jsr w_dot                   ; print TOS which is a word payload or string length
                plp                         ; was it a string?
                bne _skip_payload

                ; for a string we'll show up to 15 characters, with ellipses if it's too long

                ; ( addr u saddr n )

                lda 0,x
                and #$f0            ; check high 12 bits
                ora 1,x
                php                 ; non-zero if >15 char
                beq +

                stz 1,x
                lda #12             ; long label shows first 12 chars plus ...
                sta 0,x
+
                jsr w_type          ; print up to 15 characters
                plp
                beq _skip_payload   ; did we truncate?

                ldy #3
-
                lda #'.'            ; add "..."
                jsr emit_a
                dey
                bne -

_skip_payload:
                ; finally update ( addr u ) to skip past the payload
                ; we saved the new addr' from tmp1 on the RS
                ; but updating u is a bit fiddly
                ; we want u' = u - (addr' - addr) = u + addr - addr'
                jsr w_plus                  ; ( addr+u )
                jsr w_r_from                ; ( addr+u addr' )
                jsr w_tuck                  ; ( addr' addr+u addr')
                jsr w_minus                 ; ( addr' u' )

_done:
                ; ( addr' u' )
                sec
                rts


; Table of special handlers with symbol address, label index (with optional prefix character), and payload size
; The payload is the number of inlined bytes following the jsr; 0, 1, 2 or 4.  Note 4 is actually stored as 3
disasm_handler .macro sym, label, payload, prefix=32
    .word \sym
    .byte \label, ((\prefix-32)<<2) | (\payload & 3)
.endmacro

_special_handlers:
    #disasm_handler underflow_1, str_disasm_sdc, 0, '1'
    #disasm_handler underflow_2, str_disasm_sdc, 0, '2'
    #disasm_handler underflow_3, str_disasm_sdc, 0, '3'
    #disasm_handler underflow_4, str_disasm_sdc, 0, '4'
    #disasm_handler do_runtime, str_disasm_do, 0
_payload_handlers:
    ; handlers beyond this point have a payload indexed in _pictured_literals below
    #disasm_handler question_do_runtime, str_disasm_do, 2, '?'
    #disasm_handler zero_branch_runtime, str_disasm_0bra, 2
    #disasm_handler loop_runtime, str_disasm_loop, 2
    #disasm_handler plus_loop_runtime, str_disasm_loop, 2, '+'
    #disasm_handler of_runtime, str_disasm_of, 2
    #disasm_handler push_inline_bliteral, str_disasm_lit, 1, 'B'
    #disasm_handler push_inline_literal, str_disasm_lit, 2
    #disasm_handler push_inline_sliteral, str_disasm_lit, 3, 'S'
    #disasm_handler push_inline_2literal, str_disasm_lit, 4, '2'    ; payload 4 wraps to index zero
_end_handlers:

_pictured_literals:
    ; templates for push_pictured_common, mirroring push_inline_[2literal, bliteral, literal, sliteral]
        .byte %1111_00_10     ; 0: 2literal
        .byte %10_0000_01     ; 1: bliteral
        .byte %11_0000_01     ; 2: literal
        .byte %1100_00_10     ; 3: sliteral

; used to calculate size of assembled disassembler code
disassembler_end:
