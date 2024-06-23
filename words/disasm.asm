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
; ## "disasm"  tested  Tali Forth
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
                stz scratch+5   ; flag indicating whether we're arriving at sliteral (vs 2literal)
                jsr w_cr       ; ( addr u )
_byte_loop:
                ; Print address at start of the line. Note we use whatever
                ; number base the user has
                jsr w_over     ; ( addr u addr )
                jsr w_u_dot    ; ( addr u )
                jsr w_space

                ; We use the opcode value as the offset in the oc_index_table.
                ; We have 256 entries, each two bytes long, so we can't just
                ; use an index with Y. We use tmp2 for this.
                lda #<oc_index_table
                sta tmp2
                lda #>oc_index_table
                sta tmp2+1

                lda (2,x)       ; get opcode that addr points to
                sta scratch     ; Save opcode

                asl             ; multiply by two for offset
                bcc +
                inc tmp2+1      ; we're on second page
+
                tay             ; use Y as the index

                ; Get address of the entry in the opcode table. We put it
                ; in tmp3 and push a copy of it to the stack to be able to
                ; print the opcode later
                lda (tmp2),y    ; LSB
                sta tmp3
                pha

                iny

                lda (tmp2),y    ; MSB
                sta tmp3+1
                pha

                ; The first byte is the "lengths byte" which is coded so
                ; that bits 7 to 6 are the length of the instruction (1 to
                ; 3 bytes) and 2 to 0 are the length of the mnemonic.
                lda (tmp3)
                tay                     ; save copy of lengths byte

                ; Since this is Simpler Assembler Notation (SAN) in a Forth
                ; system, we want to print any operand before we print the
                ; mnemonic ('1000 sta' instead of 'sta 1000'). This allows us
                ; to copy and paste directly from the disassembler to the
                ; assembler.

                ; What happens next depends on the length of the instruction in
                ; bytes:

                ;   1 byte:  OPC          -->          OPC  bit sequence: %01
                ;   2 bytes: OPC LSB      -->    0 LSB OPC  bit sequence: %10
                ;   3 bytes: OPC LSB MSB  -->  MSB LSB OPC  bit sequence: %11

                ; We can distinguish between the first case, where there is
                ; only the mnemonic, and the second and third cases, where we
                ; have an operand. We do this by use of the bit sequence in
                ; bits 7 and 6.
                bpl _no_operand         ; bit 7 clear, single-byte instruction

                ; We have an operand. Prepare the Data Stack
                jsr w_zero             ; ( addr u 0 ) ZERO does not use Y

                ; Because of the glory of a little endian CPU, we can start
                ; with the next byte regardless if this is a one or two byte
                ; operand, because we'll need the LSB one way or the other.
                ; We have a copy of the opcode on the stack, so we can now move
                ; to the next byte
                inc 4,x
                bne +
                inc 5,x                 ; ( addr+1 u 0 )
+
                lda 2,x
                bne +
                dec 3,x
+
                dec 2,x                 ; ( addr+1 u-1 0 )

                lda (4,x)
                sta 0,x                 ; LSB of operand ( addr+1 u-1 LSB )
                sta scratch+1           ; Save a copy in the scratch buffer

                ; We still have a copy of the lengths byte in Y, which we use
                ; to see if we have a one-byte operand (and are done already)
                ; or a two-byte operand
                tya                     ; retrieve copy of lengths byte
                rol                     ; shift bit 6 to bit 7
                bpl _print_operand

                ; We have a three-byte instruction, so we need to get the MSB
                ; of the operand. Move to the next byte
                inc 4,x
                bne +
                inc 5,x                 ; ( addr+2 u-1 LSB )
+
                lda 2,x
                bne +
                dec 3,x
+
                dec 2,x                 ; ( addr+2 u-2 LSB )

                lda (4,x)
                sta 1,x                 ; MSB of operand ( addr+2 u-2 opr )
                sta scratch+2           ; Save a copy in the scratch buffer

                ; fall through to _print_operand

_print_operand:

                ; We arrive here with the lengths byte in Y, the address of the
                ; opcode table entry for the instruction on the stack ( addr+n
                ; u-n opr). We want the output to be nicely formatted in
                ; columns, so we use U.R. The maximal width of the number in
                ; decimal on an 16-bit addressed machine is five characters
                dex
                dex
                lda #5
                sta 0,x
                stz 1,x                 ; ( addr+n u-n opr 5 )

                jsr w_u_dot_r          ; U.R ( addr+n u-n )

                bra _print_mnemonic

_no_operand:
                ; We arrive here with the opcode table address on the stack,
                ; the lengths byte in Y and ( addr u ). Since we want to have
                ; a nicely formatted output, we need to indent the mnemonic by
                ; five spaces.
                dex
                dex
                lda #5
                sta 0,x
                stz 1,x                 ; ( addr u 5 )

                jsr w_spaces           ; ( addr u )

                ; fall through to _print_mnemonic

_print_mnemonic:
                ; We arrive here with the opcode table address on the stack and
                ; ( addr u | addr+n u-n ). Time to print the mnemonic.
                jsr w_space

                dex
                dex                     ; ( addr u ? )
                pla                     ; MSB
                sta 1,x                 ; ( addr u MSB )
                pla                     ; LSB
                sta 0,x                 ; ( addr u addr-o )

                jsr w_count            ; ( addr u addr-o u-o )

                ; The length of the mnemnonic string is in bits 2 to 0
                stz 1,x                 ; paranoid
                lda 0,x
                and #%00000111          ; ( addr u addr-o u-o )
                sta 0,x

                jsr w_type             ; ( addr u )

                ; Handle JSR by printing name of function, if available.
                ; scratch has opcode ($20 for JSR)
                ; scratch+1 and scratch+2 have address if it's a JSR.
                lda scratch
                cmp #OpJSR
                bne _not_jsr

                ; It's a JSR.  Print 5 spaces as an offset.
                dex
                dex
                lda #5
                sta 0,x
                stz 1,x
                jsr w_spaces

                jsr disasm_special
                bcs _printing_done

                ; Try the generic JSR handler, which will use the target of the
                ; JSR as an XT and print the name if it exists.
                jsr disasm_jsr
                bcs _printing_done

_not_jsr:
                ; See if the instruction is a jump (instruction still in A)
                ; (Strings start with a jump over the data.)
                cmp #OpJMP
                bne _not_jmp

                ; We have a branch.  See if it's a string by looking for
                ; a JSR sliteral_runtime at the jump target address.
                ; The target address is in scratch+1 and scratch+2
                ; Use scratch+3 and scratch+4 here as we need to move
                ; the pointer.
                lda scratch+1   ; Copy the pointer.
                sta scratch+3
                lda scratch+2
                sta scratch+4

                ; Get the first byte at the jmp target address.
                lda (scratch+3)

                cmp #OpJSR          ; check for JSR
                bne _printing_done
                ; Next byte
                inc scratch+3
                bne +
                inc scratch+4
+
                ; Check for string literal runtime
                lda (scratch+3)

                cmp #<sliteral_runtime
                bne _printing_done
                ; Next byte
                inc scratch+3
                bne +
                inc scratch+4
+
                lda (scratch+3)

                cmp #>sliteral_runtime
                bne _printing_done

                ; It's a string literal jump.
                dec scratch+5                   ; flag for next go round
                jsr disasm_sliteral_jump
                bra _printing_done

_not_jmp:
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
                dex
                dex
                stz 1,x
                lda scratch+1
                sta 0,x
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

                dex
                dex
                lda #9
                sta 0,x
                stz 1,x
                jsr w_u_dot_r      ; print the destination with 5 leading spaces

                lda #AscSP          ; print space and branch direction indicator
                jsr emit_a
                pla
                jsr emit_a

_printing_done:
                jsr w_cr

                ; Housekeeping: Next byte
                inc 2,x
                bne +
                inc 3,x                 ; ( addr+1 u )
+
                jsr w_one_minus        ; ( addr+1 u-1 )

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
; String literal handler (both for inline strings and sliteral)
disasm_sliteral_jump:
                ; If we get here, we are at the jump for a constant string.
                ; Strings are compiled into the dictionary like so:
                ;           jmp a
                ;           <string data bytes>
                ;  a -->    jsr sliteral_runtime
                ;           <string address>
                ;           <string length>
                ;
                ; We have ( addr n ) on the stack where addr is the last
                ; byte of the address a in the above jmp instruction.
                ; Address a is in scratch+1 scratch+2.

                ; Determine the distance of the jump so we end on the byte
                ; just before the JSR (sets us up for SLITERAL on next loop)
                jsr w_swap
                dex
                dex
                lda scratch+1
                sta 0,x
                lda scratch+2
                sta 1,x
                jsr w_swap
                jsr w_minus
                jsr w_one_minus
                ; ( n jump_distance )
                ; Subtract the jump distance from the bytes left.
                jsr w_minus
                ; ( new_n )
                ; Move to one byte before the target address
                dex
                dex
                lda scratch+1
                sta 0,x
                lda scratch+2
                sta 1,x
                jsr w_one_minus
                jsr w_swap ; ( new_addr new_n )
                rts


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
                jsr w_int_to_name    ; Try looking again
                ; int>name returns zero if we just don't know.
                lda 0,x
                ora 1,x
                beq _no_nt

                ; We got an nt this time.  Double check that it has underflow
                ; checking (that could be skipped).
                ; Put the address of the status byte on the stack.
                jsr w_dup
                jsr w_one_plus
                ; Grab the status into A
                lda (0,x)
                ; Get rid of the extra stack usage before doing the check
                inx
                inx
                ;Check the UF flag
                and #UF
                beq _no_nt      ; The word doesn't have underflow checking

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
                lda scratch+5               ; are we expecting sliteral?
                beq +
                stz scratch+5               ; yes, skip 2literal and match again
                bra _next
+
                lda _special_handlers+3,y   ; payload + prefix
                pha                         ; stash a copy for payload later
                lsr
                lsr
                beq _no_prefix
                clc
                adc #32
                jsr emit_a
_no_prefix:
                lda _special_handlers+2,y   ; string index
                jsr print_string_no_lf
                pla
                and #3                      ; payload is 0, 1 or 2 words
                beq _done
                cmp #3                      ; but 3 means a double-word
                bne _show_payload
                jmp _print_2literal

_show_payload:
                pha
                jsr _print_literal
                pla
                dea
                bne _show_payload

_done:          sec
                rts

_print_literal:
                ; ( addr u ) address of last byte of JSR and bytes left on the stack.
                ; We need to print the value just after the address and move along two bytes.
                jsr w_swap ; switch to (u addr)
                jsr w_one_plus

                jsr w_dup
                jsr w_question ; Print the value at the address
                ; Advance one more byte to skip over the constant
                jsr w_one_plus
                jsr w_swap ; (addr+2 u)
                jsr w_one_minus
                jmp w_one_minus ; (addr+2 u-2)

_print_2literal:
                jsr w_swap
                jsr w_one_plus
                jsr w_dup
                jsr w_two_fetch
                jsr w_swap             ; 2! / 2@ put MSW first; but 2literal writes LSW first
                jsr w_d_dot
                clc
                lda 0,x
                adc #3
                sta 0,x
                bcc +
                inc 1,x
+
                jsr w_swap ; ( addr+4 u )
                sec
                lda 0,x
                sbc #4
                sta 0,x
                bcs +
                dec 1,x
+
                rts

; Table of special handlers with address, strings index, payload in words + prefix
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
    .word sliteral_runtime
        .byte str_disasm_lit, 2 + ('S'-32)*4
    .word sliteral_runtime                      ; 2literal and sliteral use the same runtime
        .byte str_disasm_lit, 3 + ('2'-32)*4    ; list is searched in reverse, put 2literal first
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
