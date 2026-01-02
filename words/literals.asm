; Support for inline data payloads.
;
; We often need to put constant (literal) data values onto TaliForth's data stack.
; When small is more important than fast these inline words let us write code like:
;
;               jsr push_inline_literal         ; puts 1234 on the data stack ...
;               .word 1234
;               jsr w_plus                      ; ... continues past the payload
;
; These are used extensively in implementing forth words when we care more about
; saving space than saving time (e.g. user interaction, compilation).
; They're also used to compile Forth literals into compact non-native assembly.
;
; All of the entrypoints share the flexible push_inline_pictured routine.
; The accumulator contains a bit pattern that pictures how to map payload byte(s)
; following the original JSR onto new data stack entries.  The six most significant bits
; are read left to right with a one bit copying a payload byte and a zero byte
; inserting a zero byte into the corresponding stack slot.  The two least
; significant bits count how many stack entries are mapped in the high bits.
; For example a pattern like 1011xx10 would add two stack entries, and
; read the four high bits, consuming three payload bytes.  The first byte becomes
; the LSB of TOS (with MSB 0), the next two bytes form NOS.
; Spare bits should be set to zero.
;
;                    (msb) 7   6   5   4   3   2   1   0 (lsb)
;                    Acc:  p   p   p   p   p   p   n   n
;                        |  TOS  |  NOS  |  3OS  |  0-3  |
;                        | lo hi | lo hi | lo hi |
;               JSR pushp  .word [ .word [ .word ]]   ; call with inline parameters
;               LDA ...                               ; continues after parameters
;
; Normally the routine returns to the instruction following the payload bytes,
; but can be called indirectly where the caller is responsible for providing the
; payload address, and the routine then returns to the caller.
; Postprocessing can also be performed after the stack entries are created.
; These are controlled by the Y register.  Bit 7 indicates indirect return.
; Bit 6 indicates post-processing is required with bits 1-4 giving an
; an even offset into the postprocessor table, with 0 meaning none.  Bit 5 should be 0.

cmpl_call_inline_literal:
        ; Generate code that calls the literal
        ;
        ;       jsr cmpl_call_inline_literal    ; compile "JSR target"
        ;       .word target

                lda #%11_0000_01
                ldy #%0100_0000                 ; first post-processor, offset 0
                bra push_inline_pictured_y

create_inline:
                lda #%1011_00_10
                ldy #%0100_0010                 ; second post-processor entry, offset 2
                bra push_inline_pictured_y

push_inline_literal:
        ; Put a word literal on the stack.  Supported by DISASM
        ;
        ;       jsr push_inline_literal
        ;       .word TOS

                lda #%11_0000_01
                bra push_inline_pictured

push_inline_sliteral:
        ; Put a string literal on the stack.  Supported by DISASM
        ; Note the length and actual text follow the JSR.
        ; The values ( addr length ) are pushed to the data stack
        ; with addr pointing to the literal "<text>"
        ;
        ;       jsr push_inline_sliteral
        ;       .word <length>
        ;       .text "<text>"

                lda #%1100_00_10
                bra push_inline_pictured

push_inline_2literal:
        ; Put a double-word literal on the stack.  Supported by DISASM
        ; The in-memory byte order matches the stack order, i.e. bytes N,U,X,I
        ; following the JSR become the Forth double word UNIX with UN TOS and IX NOS
        ; each stored in little endian order, i.e. N,U, I,X
        ;
        ;       jsr push_inline_2literal
        ;       .word TOS
        ;       .word NOS

                lda #%1111_00_10
                bra push_inline_pictured

push_inline_3literal:
        ; Only used internally, add three words to the stack
        ;
        ;       jsr push_inline_2literal
        ;       .word TOS
        ;       .word NOS
        ;       .word 3OS

                lda #%111111_11
                bra push_inline_pictured

push_inline_addru_literal:
        ; In word implementations we often need to stack (addr u) with u < 256
        ; This saves a byte vs push_inline_2literal.
        ; Not used in generated code so not supported by DISASM
        ;
        ;       jsr push_inline_addru_literal   ; stack (addr u)
        ;       .byte u         ; TOS
        ;       .word addr      ; NOS

                lda #%1011_00_10
                bra push_inline_pictured

push_inline_bliteral:
        ; Put a byte literal on the stack with MSB=0.  Supported by DISASM
        ;       jsr push_inline_bliteral
        ;       .byte TOS

                lda #%10_0000_01

push_inline_pictured:
        ; Note The alternate push_inline_pictured_y entry point supports
        ; various payload post-processing actions as well as indirect payloads.
        ; This is controlled by the Y register as explained above.
                ldy #0
push_inline_pictured_y:
                sta tmptos      ; save the masked picture
                sty tmptos+1    ; save the indirect flag and postprocessing info
                iny
                bmi +           ; if indirect, caller already set up tmp1/+1

                ply             ; LSB of return address
                sty tmp1
                ply             ; MSB
                sty tmp1+1
+
                cmp #%1100_00_10 ; is it sliteral?
                php             ; we'll care later
                and #%11        ; mask length bits
                asl             ; times two gives picture length
                tay             ; save picture length in Y
                sty zpage+$ff,x       ; save as temp to subtract from DS
                txa
                sec
                sbc zpage+$ff,x       ; extend data stack by a byte for each picture bit
                tax             ; update data stack pointer

                phx             ; save final stack pointer
                dex             ; pre-decrement so we can pre-increment
_loop:
                dey             ; Y counts picture bits
                bmi _done
                inx

                asl tmptos      ; fetch next picture bit to carry
                bcs _copy       ; a one bit means copy a value
                stz zpage+0,x         ; otherwise add a zero byte
                bra _loop
_copy:
                inc tmp1        ; inc data pointer to next param byte
                bne +
                inc tmp1+1
+
                lda (tmp1)      ; copy a payload byte to the stack
                sta zpage+0,x
                bra _loop

_done:
                ; after the loop tmp1 points to the last parameter byte
                plx             ; restore final stack pointer

                ; sliteral is special since NOS needs to point at string
                ; and then we need to advance past the string itself
                plp             ; check earlier string? result
                bne _not_string

                ; currently tmp1 points one byte before the string
                ; so put tmp1+1 into NOS
                ldy tmp1+1
                lda tmp1
                inc a
                bne +
                iny
        +
                sta zpage+2,x
                sty zpage+3,x

                ; advance past string data by adding string length in TOS
                clc
                lda tmp1
                adc zpage+0,x
                sta tmp1
                lda tmp1+1
                adc zpage+1,x
                sta tmp1+1

_not_string:
                bit tmptos+1    ; N? means indirect, V? means postprocessing
                bmi +           ; indirect version just returns to caller

                ldy tmp1+1      ; normally we'll return past the payload
                phy
                ldy tmp1
                phy
+
                bvc +           ; any post-processing?
                lda tmptos+1
                and #%0011_1110
                tay
                lda _postprocessors+1,y
                pha
                lda _postprocessors,y
                pha
+
                rts

_postprocessors:
        .word cmpl_call_tos-1
        .word create_common-1
