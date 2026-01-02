; Core Forth compilation routines
; Tali Forth 2 for the 65c02
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; Patrick Surry
; First version: 1. Jan 2014
; This version: 11. May 2024

; The user-visible word COMPILE, is defined here along with various
; supporting cmpl_xxx routines.  These generate 65c02 assembler
; instructions from Forth code.  This is a good place to start
; reading if you want to understand how that process works.
;
; We can compile words inline or as JSR calls based on the nc-limit
; variable:  COMPILE, has internal entry points cmpl_by_limit,
; cmpl_inline[_drop] and cmpl_call_[nos|tos].
; Inline compilation copies the source code for a word between
; xt_<word> and <z_word> with nuances based on UF and ST flags.
;
; A great way to understand what's going on is to write simple
; Forth words and use SEE to disassemble them.  Try different
; values for nc-limit threshold and the strip-underflow flag.
; For example:
;
;       32 nc-limit !
;       true strip-underflow !
;       : fib 0 1 rot 0 ?do over + swap loop drop ;
;       see fib
;
; Forth uses only two branching constructs, an unconditional jump
; and a conditional 0branch.  TaliForth doesn't expose 0BRANCH as
; a user word but see cmpl_jump_ya, cmpl_jump_later, cmpl_jump_tos,
; cmpl_0branch_tos and cmpl_0branch_later.  The xxx_later variants
; let us compile forward references where we need to come back
; and fill in the branch address after we've reached the target.
;
; The conditional zero_branch_runtime is also here.  It has some
; subtlety to support both inline and subroutine forms.
; It's factored into two parts, ztest_runtime and zbranch_runtime.
; Several looping conditional (LOOP, +LOOP, ?DO) implement custom
; tests followed by zbranch_runtime when compiled as subroutines.
; The inline forms are typically much simpler since they can use
; 65c02 jmp and bxx branch opcodes directly.

; this could be exposed as a forth word but currently isn't
compile_nt_comma:  ; ( nt -- )
        ; cf. COMPILE, which finds the nt from an xt (very slow)
        ; This entrypoint is handy if we already have an nt

        jsr w_dup                       ; ( nt nt )
        jsr w_name_to_int               ; ( nt xt )
        jsr w_swap                      ; ( xt nt )
        bra compile_comma_common


; ## COMPILE_COMMA ( xt -- ) "Compile xt"
; ## "compile,"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/COMPILEComma
        ; Compile the given xt in the current word definition. It is an
        ; error if we are not in the compile state. Because we are using
        ; subroutine threading, we can't use , (COMMA) to compile new words
        ; the traditional way. By default, native compiled is allowed, unless
        ; there is a NN (Never Native) flag associated. If not, we use the
        ; value NC_LIMIT (from definitions.asm) to decide if the code
        ; is too large to be natively coded: If the size is larger than
        ; NC_LIMIT, we silently use subroutine coding. If the AN (Always
        ; Native) flag is set, the word is always natively compiled.
        ; """
xt_compile_comma:
                jsr underflow_1
w_compile_comma:
                ; See if this is an Always Native (AN) word by checking the
                ; AN flag. We need nt for this.
                jsr w_dup               ; copy of xt to convert to nt

                ; Nb. this reverse lookup from xt => nt is expensive so it's
                ; better to use the compile_comma_common entrypoint below
                ; if the nt is already available, e.g. while interpreting
                jsr w_int_to_name
                ; ( xt nt|0 )

                ; Does this xt even have a valid (non-zero) nt?
                lda zpage+0,x
                ora zpage+1,x

                ; Without an NT we don't know flags or size so must compile as a JSR
                beq cmpl_call_nos       ; No nt so unknown size; must compile as a JSR

compile_comma_common:
                ; Otherwise investigate the NT to decide how to proceed
                lda (zpage+0,x)               ; stash status flags byte
                sta tmp3

                ; The target word we're compiling looks like this:
                ;
                ; xt_word:
                ;       ST? - optional 5 byte stack prequel
                ; inline_st_word:
                ;       UF? - optional 3 byte jsr underflow_<N>
                ; w_word:
                ;       ... source code ...
                ; z_word:
                ;       rts
                ;
                ; We need to decide both whether to call or inline
                ; the word, and which entrypoint to use.
                ;
                ; The entrypoint is typically xt_word or w_word
                ; depending on whether we're stripping the UF check.
                ; But for ST words, the call entrypoint xt_word
                ; differs from the inline entrypoint st_word or w_word.
                ;
                ; Once we have the inline entrypoint and length
                ; we can decide whether to inline or call.
                ; If the word is always- or never-native (AN/NN) we're
                ; done, otherwise we compare the word length to nc-limit.

                jsr w_wordsize
                jsr w_over
                jsr w_swap

                ; We'll start from ( xt xt u ) and adjust
                ; until we have ( call-addr inline-addr inline-len )

                ; --- SPECIAL CASE 1: STACK THRASHING WORDS (ST) ---

                lda tmp3
                and #ST                 ; Check the Stack Thrash flag (ST=NN+AN)
                cmp #ST                 ; C=1 if equal, C=0 otherwise
                php                     ; We'll reuse this check shortly
                bcc _no_st

                jsr push_inline_bliteral
                .byte 5
                jsr w_slash_string      ; skip 5 byte ST prequel

                ; ( xt xt+5 u-5 )
_no_st:
                ; --- SPECIAL CASE 2: REMOVE UNDERFLOW CHECKING ---

                ; Does the user want to strip underflow checks?
                ldy #uf_strip_offset
                lda (up),y
                iny
                ora (up),y
                beq _no_uf

                jsr w_over
                jsr has_uf_check        ; is there an UF check?
                bcc _no_uf

                jsr push_inline_bliteral
                .byte 3
                jsr w_slash_string
_no_uf:
                plp
                bcs _has_st

                ; for ST words we keep the original XT as call-addr
                ; but for others we can call the inline entrypoint
                lda 2,x         ; ( xt addr u -- addr addr u )
                sta 4,x
                lda 3,x
                sta 5,x

_has_st:
                ; --- END OF SPECIAL CASES ---
                ; ( call-addr inline-addr inline-len )

_check_limit:
                lda tmp3
                and #AN+NN              ; check for AN and NN
                cmp #AN                 ; AN=1, NN=0?  (i.e. not ST=AN+NN)
                beq cmpl_inline_drop    ; always natively compile
                cmp #NN
                beq cmpl_call_3os       ; always compile as call
                bra cmpl_by_limit2      ; else check word length

cmpl_by_limit:
                ; external entrypoint for ( xt u -- xt xt u )
                jsr w_over
                jsr w_swap

cmpl_by_limit2:
                ; ( call-addr inline-addr u )
                ; Compile call or inline based on size vs nc-limit
                ; Eventually returns C=0 if inline, C=1 if call

                ldy #nc_limit_offset+1
                lda zpage+1,x                 ; MSB of word size
                cmp (up),y              ; user-defined limit MSB
                bcc cmpl_inline_drop    ; borrow (C=0) means size < limit
                bne cmpl_call_3os       ; else non-zero means size > limit

                dey                     ; MSB equal so check LSB
                lda (up),y              ; user-defined limit LSB
                cmp zpage+0,x
                bcs cmpl_inline_drop    ; not bigger, we can inline!
+
                ; else fall through and compile as call
cmpl_call_3os:
                ; compile call from ( xt ? ? -- ), return C=1
                jsr w_drop
cmpl_call_nos:
                ; compile call from ( xt ? -- ), return C=1
                jsr w_drop
cmpl_call_tos:
                ; ( target -- )
                lda #OpJSR
                jsr cmpl_a
                jsr w_comma
                sec                     ; return C=1 for call
                rts

cmpl_inline_drop:
        ; compile inline, with extraneous arg to drop, returning C=0
                ; ( call-addr inline-addr u -- )
                jsr w_rot
                jsr w_drop              ; drop call-addr
cmpl_inline:
                ; ( inline-addr u -- )
                jsr w_here
                jsr w_swap
                ; ( xt' cp u -- )
                jsr w_dup
                jsr w_allot             ; allocate space for the word
                jsr w_move              ; let's move those bytes already!
                clc                     ; return C=0 for inline
z_compile_comma:
                rts


has_uf_check:
                ; Check if TOS points to an underflow check,
                ; returning C=1 (true) or C=0 (false)
                ; ( addr -- )

                ; Does addr point at a JSR?
                lda (zpage+0,x)               ; fetch byte @ addr
                cmp #OpJSR
                bne _not_uf             ; not a JSR

                ; Is address between underflow_1 ... underflow_4 ?
                ; We can check 0 <= addr - underflow_1 <= underflow_4 - underflow_1 < 256
                jsr w_one_plus
                jsr w_fetch             ; get JSR address to TOS
                lda zpage+0,x                 ; LSB of jsr address
                sec
                sbc #<underflow_1
                tay                     ; stash LSB of result and finish subtraction
                lda zpage+1,x                 ; MSB of jsr address
                sbc #>underflow_1
                bne _not_uf             ; MSB of result must be zero

                cpy #(underflow_4-underflow_1+1)
                bcs _not_uf             ; LSB is too big

                sec                     ; C=1 means it is an UF check
                .byte OpBITzp           ; mask the clc, with no effect on carry
_not_uf:        clc                     ; C=0 means it isn't a UF check
                inx                     ; clean up stack
                inx
                rts


; =====================================================================
; COMPILE WORDS, JUMPS and SUBROUTINE JUMPS INTO CODE

; These routines compile instructions such as "jsr w_words" into a word
; at compile time so they are available at run time. Words that use this
; routine may not be natively compiled. We use "cmpl" as not to confuse these
; routines with the COMPILE, word.  Always call this with a subroutine jump.
; This means combining JSR/RTS to JMP in those cases is not going to work. To
; use, load the LSB of the address in A and the MSB in Y. You can remember
; which comes first by thinking of the song "Young Americans" ("YA") by David
; Bowie.
;
;               ldy #>addr      ; MSB   ; "Young"
;               lda #<addr      ; LSB   ; "Americans"
;               jsr cmpl_word_ya
;
; We have have various utility routines here for compiling a word in Y/A
; and a single byte in A.

;TODO for all of these we could potentially avoid jmp (and NN) and
; use BRA instead.  jump_later is a bit harder since we need to remember NN state
; in case something else changed it

cmpl_jump_later:
    ; compile a jump to be filled in later with dummy address <MSB=Y/LSB=??>
    ; leaving address of the JMP target TOS
                dex
                dex
                lda cp+1
                sta zpage+1,x
                lda cp
                inc a
                sta zpage+0,x
                bne cmpl_jump_ya
                inc zpage+1,x
                bra cmpl_jump_ya

xt_again:
                jsr underflow_1
w_again:
cmpl_jump_tos:
                ; compile a jump to the address at TOS, consuming it
                lda zpage+0,x         ; set up for cmpl_jump_ya
                ldy zpage+1,x
                inx
                inx
cmpl_jump_ya:
                ; This is the entry point to compile JMP <ADDR=Y/A>
                pha             ; save LSB of address
                lda #%00010000  ; unset bit 4 to flag as never-native (NN)
                trb status
                lda #OpJMP      ; load opcode for JMP
cmpl_op_ya:
                jsr cmpl_a      ; compile opcode
                pla             ; retrieve address LSB; fall thru to cmpl_word
                ; fall through
cmpl_word_ya:
cmpl_op_y:
                ; This is the entry point to compile a word in Y/A (little-endian)
                ; or equivalent an opcode in A and operand in Y
                jsr cmpl_a      ; compile LSB of address
                tya             ; fall thru for MSB
cmpl_a:
                ; This is the entry point to compile a single byte which
                ; is passed in A. The built-in assembler assumes that this
                ; routine does not modify Y.
                sta (cp)
                inc cp
                bne +
                inc cp+1
+
z_again:
                rts



xt_if:
w_if:
cmpl_0branch_later:                     ; ( -- target )
        ; compile a 0BRANCH where we don't know the destination yet
        ; leaving a pointer to the placeholder destination (target) on TOS
                clc
                bra cmpl_0branch_setup         ; now generate native or subroutine branch code

xt_until:
                jsr underflow_1
w_until:
cmpl_0branch_tos:                       ; ( dest -- )
                ; The (known) address to branch back to is TOS.
                sec
cmpl_0branch_setup:
                stz tmpdsp                      ; set up tmpdsp as 0 if branch dest unknown, 1 if known
                rol tmpdsp

                ; compare A > 0 to nc-limit, setting C=0 if A <= nc-limit (should native compile)

                ; First decide whether to inline or call the runtime.
                ; Both start with the zero test
                jsr push_inline_addru_literal
                ;TODO strictly speaking we should also include the appended branch size
                .byte ztest_runtime_size        ; TOS with NUXI order
                .word zero_branch_runtime       ; NOS
                jsr cmpl_by_limit               ; leaves C=1 if inline

cmpl_zbranch_common:                            ; entrypoint for w_of

                lda tmpdsp                      ; sets Z=1 if branch dest unknown, preserving carry
                bcc _inline

                bne +                           ; not inline, destination known?
                jsr w_here                      ; no, save address of placeholder dest
                jsr w_zero                      ; and just compile a zero for now
                ; ( here 0 )
+
                ; we're adding an absolute address, so flag this word as never-native (NN)
                lda #%00010000                  ; unset bit 4 to for NN
                trb status                      ; we're adding an absolute address, so flag this word as never-native (NN)
                ; either ( known -- ) or ( target 0 -- target )
                jmp w_comma                     ; add the payload and return

_inline:
                ; we inlined the test, so compile the branch to test the zero flag
                ; first check if we can use a short relative branch or need a long jmp
                ; the short form 'beq target' will work if addr - (here + 2) fits in a signed byte

                beq _long               ; always use long form for unknown dest

                ; ( dest )
                jsr w_dup
                jsr w_here

                jsr push_inline_bliteral
                .byte 2
                jsr w_plus
                jsr w_minus

                ; ( dest offset )
                ; offset is a signed byte if LSB bit 7 is 0 and MSB is 0 or bit 7 is 1 and MSB is #ff
                inx             ; pre-drop offset and use wraparound indexing to preserve flags
                inx
                lda zpage+$ff,x
                tay             ; Y=MSB of offset
                lda $fe,x       ; A=LSB, setting N flag to bit 7
                bmi _minus
                cpy #0          ; if LSB is positive we need MSB = 0
                bra +
_minus:         cpy #$ff        ; if LSB is negative we need MSB = ff
+               bne _long

                ; short relative branch will work!  all we need is code like:
                ;
                ;       beq target      ; relative branch if nearby target
                ;
                lda #OpBEQ
                jsr cmpl_a
                lda $fe,x       ; single byte offeset
                inx             ; drop the original address we used to calc offset
                inx
                jmp cmpl_a

_long:
                ; too far (or unknown) so emit code like:
                ;
                ;       bne +3
                ;       jmp target


                lda #OpBNE
                ldy #3
                jsr cmpl_word_ya
                lda tmpdsp              ; destination known?
                bne +
                jsr w_here              ; if dest unknown, keep a pointer to the jmp target
                jsr w_one_plus
                jsr w_zero              ; and use a dummy placeholder for now
                ; ( here+1 0 )
+
                jmp cmpl_jump_tos
z_if:
z_until:

; =====================================================================
; 0BRANCH runtime
;
; TaliForth doesn't expose 0BRANCH directly as a word, but implements
; all conditional branching with this runtime.  It's broken into
; two parts: ztest_runtime checks if TOS is zero,
; and zbranch_runtime then conditionally branches to a target address.
; This allows the looping constructs LOOP, +LOOP and ?DO to implement
; their own custom tests and reuse the zbranch_runtime for branching.
;
; Native compilation is very straightforward: we inline a few bytes
; from ztest_runtime and tack on a BEQ <target> or BNE +3/JMP <target>
; to implement the branch.  Non-native compilation generates
; JSR zero_branch_runtime / .word <target> so the runtime can
; use its own return address to read <target> and either return to
; that address or simply continue beyond the <target> word.
; This is obviously much slower but sometimes space is more
; important than speed.

zero_branch_runtime:
        ; Drop TOS of stack setting Z flag, for optimizing short branches (see xt_then)
                inx
                inx
                lda $FE,x           ; wraparound so inx doesn't wreck Z status
                ora $FF,x
        ; The inline form ends here and is followed by a native beq or bne / jmp
ztest_runtime_size = * - zero_branch_runtime

zbranch_runtime:
        ; The subroutine continues here, and is also used as an alternate entry point
        ; by various conditional looping constructs
        ; If A=0 we branch to the address following the jsr that landed here
        ; otherwise skip past that address and continue
                ply
                sty tmp1
                ply
                sty tmp1+1

                tay             ; test if A = 0 which tells us whether to branch
                bne _nobranch   ; the usual case is to repeat, so fall thru

                ; Flag is FALSE (0) so we take the jump to the address given in
                ; the next two bytes. However, the address points to the last
                ; byte of the JSR instruction, not to the next byte afterwards
                ldy #1
                lda (tmp1),y
                pha                     ; stash the LSB until we've read the MSB too
                iny
                lda (tmp1),y
                sta tmp1+1              ; update tmp1 with our branch target
                pla
                sta tmp1
_jmp:
                ; However we got here, tmp1 has the address to jump to.
                jmp (tmp1)

_nobranch:
                ; no branch, continue past the address bytes
                clc
                lda tmp1        ; LSB
                adc #3          ; skip two bytes plus the extra for jsr/rts behavior
                sta tmp1
                bcc _jmp

                inc tmp1+1
                bra _jmp
