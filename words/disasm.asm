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

                lda #>nt_question
                pha
                lda #<nt_question
                pha
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
                dex
                dex
                lda scratch+1       ; add operand to stack
                sta 0,x
                lda scratch+2
                sta 1,x
                ; ( addr+n u-n operand )

                sec                 ; flag operand
                .byte OpBITzp       ; mask the clc
_no_operand:
                clc                 ; flag no operand

                ; We arrive here with either C=0 and no operand ( addr n )
                ; or C=1 and ( addr+n u-n opr )
                ; We want the output to be nicely formatted in columns.
                ; The maximal width of a 16-bit operand in decimal
                ; is five characters so we either use U.R to format it
                ; or simply indent the mnemonic by five spaces.

                lda #5
                jsr push_a_tos      ; ( addr+n u-n [opr] 5 )
                bcs _print_operand

                jsr w_spaces        ; no operand, just indent
                bra _print_mnemonic

_print_operand:
                jsr w_u_dot_r       ; right-justify the operand
_print_mnemonic:
                ; ( addr+n u-n )
                dex
                dex
                pla                 ; put NT on stack
                sta 0,x
                pla
                sta 1,x

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
                lda #5
                jsr push_a_tos
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
                lda scratch+1
                jsr push_a_tos
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

                lda #9
                jsr push_a_tos
                jsr w_u_dot_r      ; print the destination with 5 leading spaces

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
                dex
                dex
                lda scratch+1
                sta 0,x
                lda scratch+2
                sta 1,x
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
                sty scratch+5               ; store the offset for later
                lda _special_handlers+3,y   ; payload + prefix
                pha                         ; stash a copy for payload later
                lsr
                lsr
                beq _no_prefix
                clc
                adc #32
                jsr emit_a                  ; print the char stored as (ch - 32) << 2
_no_prefix:
                lda _special_handlers+2,y   ; string index
                jsr print_string_no_lf
                pla
                and #3                      ; payload is 0, 1 or 2 words
                beq _done
                cmp #3                      ; where 3 means a double-word
                bne _show_payload
                jsr _print_2literal
                bra _done

_show_payload:
                pha
                jsr _print_literal
                pla
                dea
                bne _show_payload

                lda scratch+5
                cmp #_sliteral_handler - _special_handlers
                bne _done

                ; for sliteral we want to skip past the string data
                ; we have ( addr n ) on the stack where addr points
                ; to the last byte of the string length u.
                ; we want to finish with ( addr+u n-u )
                ; and print at least a snippet of the string
                ; which is at addr+1

                jsr w_over
                jsr w_one_minus
                jsr w_fetch         ; ( addr n u )

                ; detour to show snippet of string up to 16 chr
                lda 1,x
                bne _truncate
                lda 0,x
                cmp #16
                bcc +               ; length < 16?
_truncate:
                lda #18             ; extra chars for ellipses
+
                sta tmpdsp

                lda 4,x             ; tmp1 points 1 before string
                sta tmp1
                lda 5,x
                sta tmp1+1

                ldy #1
_snippet:
                lda (tmp1),y
                cpy #16
                bcc +
                lda #'.'
+
                jsr emit_a
                iny
                dec tmpdsp
                bne _snippet

                ; ( addr n u -- addr+u n-u )
                jsr w_slash_string

_done:          sec
                rts

_print_literal:
                ; ( addr u ) address of last byte of JSR and bytes left on the stack.
                ; We need to print the value just after the address and move along two bytes.
                jsr w_over
                jsr w_one_plus              ; ( addr u addr+1 )
                jsr w_question              ; Print the value at the address
                jsr slash_string_1
                jmp slash_string_1          ; leaving (addr+2 u-2)

_print_2literal:
                jsr w_over                  ; ( addr u addr+1 )
                jsr w_one_plus
                jsr w_two_fetch
                jsr w_d_dot                 ; fetch and print double word
                lda #4
                jsr push_a_tos
                jmp w_slash_string          ; ( addr+4 u-4 )


; Table of special handlers with address, strings index, payload in words + prefix
; payload is stored as 0, 1 or 2 words with 3 meaning a double-word (i.e. $1234 vs $34, $12)
_special_handlers:
    .word underflow_1
        .byte str_disasm_sdc, 0 + ('1'-32)*4
    .word underflow_2
        .byte str_disasm_sdc, 0 + ('2'-32)*4
    .word underflow_3
        .byte str_disasm_sdc, 0 + ('3'-32)*4
    .word underflow_4
        .byte str_disasm_sdc, 0 + ('4'-32)*4

    .word literal_runtime
        .byte str_disasm_lit, 1
_sliteral_handler:
    .word sliteral_runtime
        .byte str_disasm_lit, 1 + ('S'-32)*4
    .word two_literal_runtime
        .byte str_disasm_lit, 3 + ('2'-32)*4
    .word zero_branch_runtime
        .byte str_disasm_0bra, 1
    .word loop_runtime
        .byte str_disasm_loop, 1
    .word plus_loop_runtime
        .byte str_disasm_loop, 1 + ('+'-32)*4
    .word do_runtime
        .byte str_disasm_do, 0
    .word question_do_runtime
        .byte str_disasm_do, 1 + ('?'-32)*4
_end_handlers:


; used to calculate size of assembled disassembler code
disassembler_end:
