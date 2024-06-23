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
; cmpl_inline and cmpl_as_call.  Inline compilation copies the
; source code for a word between xt_<word> and <z_word>,
; optionally removing the initial stack depth check.
; For some words we also discard leading and trailing stack
; juggling based on the ST flag.
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
; a user word but see cmpl_jump, cmpl_jump_later, cmpl_jump_tos,
; cmpl_0branch_tos and cmpl_0branch_tos.  The xxx_later variants
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
                jsr w_dup              ; keep an unadjusted copy of xt
                jsr w_dup              ; plus one to convert to nt
                jsr w_int_to_name
                ; ( xt xt nt )

                ; Does this xt even have a valid (non-zero) nt?
                lda 0,x
                ora 1,x
                beq cmpl_as_call        ; No nt so unknown size; must compile as a JSR

                ; Otherwise investigate the nt
                jsr w_dup
                jsr w_one_plus         ; status is at nt+1
                ; ( xt xt nt nt+1 )
                lda (0,x)               ; get status byte
                inx                     ; drop pointer
                inx
                ; ( xt xt nt )
                sta tmp3                ; keep copy of status byte
                and #NN
                bne cmpl_as_call        ; never native

                ; ( xt xt nt )          ; maybe native, let's check
                jsr w_wordsize
                ; ( xt xt u )

                ; --- SPECIAL CASE 1: PREVENT RETURN STACK THRASHING ---

                lda tmp3
                and #ST                 ; Check the Stack Thrash flag in status
                beq _check_uf

_strip_sz = 10  ; skip the standard 10 byte header which saves return address + 1 to tmp1

                ; Start later: xt += sz
                clc
                lda 2,x
                adc #_strip_sz
                sta 2,x
                bcc +
                inc 3,x                 ; we just care about the carry
+
                ; Quit earlier: u -= sz
                sec
                lda 0,x
                sbc #_strip_sz
                sta 0,x
                bcs +
                dec 1,x                 ; we just care about the borrow
+
                ; ( xt xt+sz u-sz )

                ; --- SPECIAL CASE 2: REMOVE UNDERFLOW CHECKING ---
_check_uf:
                ; The user can choose to remove the unterflow testing in those
                ; words that have the UF flag. This shortens the word by
                ; 3 bytes if there is no underflow.

                ; See if this word even contains underflow checking
                lda tmp3
                and #UF
                beq _check_limit

                ; See if the user wants underflow stripping turned on
                ldy #uf_strip_offset
                lda (up),y
                iny
                ora (up),y
                beq _check_limit

                ; Remove the 3 byte underflow check.

                ; Start later: xt += 3
                clc
                lda 2,x
                adc #3
                sta 2,x
                bcc +
                inc 3,x                  ; we just care about the carry
+
                ; End earlier: u -= 3
                sec
                lda 0,x
                sbc #3
                sta 0,x
                bcs +
                dec 1,x                  ; we just care about the borrow
+
_check_limit:
                ; --- END OF SPECIAL CASES ---
                ; ( xt xt' u )

                lda tmp3
                and #AN                 ; check Always Native (AN) bit
                bne cmpl_inline         ; always natively compile

cmpl_by_limit:
                ; Compile either inline or as subroutine depending on
                ; whether native code size <= user limit
                ; Returns C=0 if native, C=1 if subroutine
                ; ( xt xt' u )
                ldy #nc_limit_offset+1
                lda 1,x                 ; MSB of word size
                cmp (up),y              ; user-defined limit MSB
                bcc cmpl_inline         ; borrow (C=0) means size < limit
                bne cmpl_as_call        ; else non-zero means size > limit

                ; Check the wordsize LSB against the user-defined limit.
                dey
                lda (up),y              ; user-defined limit LSB
                cmp 0,x
                bcs cmpl_inline         ; not bigger, so good to go

cmpl_as_call:
        ; Compile xt as a subroutine call, return with C=0
        ; Stack is either ( xt xt nt ) or ( xt xt' u )
        ; If the word has stack juggling, we need to use original xt
        ; otherwise use the middle value to respect strip-underflow.
                lda tmp3
                and #ST
                bne +
                jsr w_drop     ; no stack juggling, use middle (xt or xt')
                jsr w_nip
                bra _cmpl
+
                jsr w_two_drop ; stack juggling, must use first (xt)
_cmpl:
                ; ( jsr_address -- )
                lda #OpJSR
                jsr cmpl_a
                jsr w_comma
                sec
                rts

cmpl_inline:
        ; compile inline, returning C=1
                ; ( xt xt' u -- )
                jsr w_here
                jsr w_swap
                ; ( xt xt' cp u -- )
                jsr w_dup
                jsr w_allot            ; allocate space for the word
                ; Enough of this, let's move those bytes already!
                ; ( xt xt' cp u ) on the stack at this point
                jsr w_move
                jsr w_drop             ; drop original xt
                clc
z_compile_comma:
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
;               jsr cmpl_subroutine
;
; We have have various utility routines here for compiling a word in Y/A
; and a single byte in A.

cmpl_subroutine:
                ; This is the entry point to compile JSR <ADDR=Y/A>
                pha             ; save LSB of address
                lda #OpJSR      ; load opcode for JSR
                bra +
cmpl_jump:
                ; This is the entry point to compile JMP <ADDR=Y/A>
                pha             ; save LSB of address
                lda #OpJMP      ; load opcode for JMP, fall thru
+
                ; At this point, A contains the opcode to be compiled,
                ; the LSB of the address is on the 65c02 stack, and the MSB of
                ; the address is in Y
                jsr cmpl_a      ; compile opcode
                pla             ; retrieve address LSB; fall thru to cmpl_word
cmpl_word:
                ; This is the entry point to compile a word in Y/A (little-endian)
                jsr cmpl_a      ; compile LSB of address
                tya             ; fall thru for MSB
cmpl_a:
                ; This is the entry point to compile a single byte which
                ; is passed in A. The built-in assembler assumes that this
                ; routine does not modify Y.
                sta (cp)
                inc cp
                bne _done
                inc cp+1
_done:
                rts


cmpl_jump_tos:
    ; compile a jump to the address at TOS, consuming it
                lda #OpJMP
                jsr cmpl_a
                jmp w_comma


cmpl_jump_later:
    ; compile a jump to be filled in later. Populates the dummy address
    ; MSB with Y, LSB indeterminate, leaving address of the JMP target TOS
                lda #OpJMP
                jsr cmpl_a
                jsr w_here
                bra cmpl_word


check_nc_limit:
        ; compare A > 0 to nc-limit, setting C=0 if A <= nc-limit (native compile ok)
                pha
                sec
                ldy #nc_limit_offset+1
                lda (up),y              ; if MSB non zero we're good, leave with C=0
                beq +
                clc
+
                pla
                bcc _done
                dea                     ; simplify test to A-1 < nc-limit
                dey
                cmp (up),y              ; A-1 < LSB leaves C=0, else C=1
                ina                     ; restore A, preserves carry
_done:
                rts


cmpl_0branch_later:
        ; compile a 0BRANCH where we don't know the target yet
        ; leaves pointer to the target on TOS
                jsr w_zero             ; dummy placeholder, which forces long jmp in native version
                jsr cmpl_0branch_tos    ; generate native or subroutine branch code
                jsr w_here             ; either way the target address is two bytes before here
                sec
                lda 0,x
                sbc #2
                sta 0,x
                bcs +
                dec 1,x
+
                rts


cmpl_0branch_tos:
                ; compare A > 0 to nc-limit, setting C=0 if A <= nc-limit (should native compile)

                lda #ztest_runtime_size+5       ; typical size of inline form
                jsr check_nc_limit              ; returns C=0 if we should native compile
                bcc _inline

                ; non-native, just generate a call with two-byte address payload

                ldy #>zero_branch_runtime
                lda #<zero_branch_runtime
                jsr cmpl_subroutine             ; call the 0branch runtime

                jmp w_comma                    ; add the payload and return

_inline:
                ; inline the test code
                ldy #0
-
                lda ztest_runtime,y
                jsr cmpl_a
                iny
                cpy #ztest_runtime_size
                bne -

                ; now we'll compile the branch to test the zero flag
                ; first check if we can use a short relative branch or need a long jmp
                ; the short form 'beq target' will work if addr - (here + 2) fits in a signed byte

                lda 0,x
                ora 1,x
                beq _long               ; always use the long form if target is 0

                ; ( addr )
                jsr w_dup
                jsr w_here
                clc
                lda #2
                adc 0,x
                sta 0,x
                bcc +
                inc 1,x
+
                jsr w_minus
                ; ( addr offset )
                ; offset is a signed byte if LSB bit 7 is 0 and MSB is 0 or bit 7 is 1 and MSB is #ff
                inx             ; pre-drop offset and use wraparound indexing to preserve flags
                inx
                lda $ff,x
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
                lda $fe,x
                inx             ; drop the original address we used to calc offset
                inx
                jmp cmpl_a

_long:
                ; too far (or unknown) so emit code like:
                ;
                ;       bne +3
                ;       jmp target

                lda #OpBNE
                jsr cmpl_a
                lda #3
                jsr cmpl_a
                jmp cmpl_jump_tos


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

ztest_runtime:
        ; Drop TOS of stack setting Z flag, for optimizing short branches (see xt_then)
                inx
                inx
                lda $FE,x           ; wraparound so inx doesn't wreck Z status
                ora $FF,x
        ; The inline form ends here and is follwed by a native beq or bne / jmp
ztest_runtime_size = * - ztest_runtime

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
                beq _branch

                ; no branch, just skip the address bytes and erturn
                clc
                lda tmp1        ; LSB
                adc #3          ; skip two bytes plus the extra for jsr/rts behavior
                sta tmp1
                bcc _jmp

                inc tmp1+1
                bra _jmp

_branch:
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
