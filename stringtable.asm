; List of Strings for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Modified by Sam Colwell and Patrick Surry
; First version: 01. Apr 2016 (for Liara Forth)
; This version: 23. Mar 2024

; This file is included by taliforth.asm

; ## GENERAL STRINGS

; the base36 alphabet for printing values in current base

alpha36:  .text "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

; All general strings are bit-7 terminated (.shift), names start with "s_",
; aliases with "str_"

; The assembler variable ix is used to number these sequentially,
; even when some are missing because they were removed in the
; platform file.
ix := 0
str_ok             = ix         ; referenced by QUIT via state=0
ix += 1
str_compile        = ix         ; referenced by QUIT via state=1
ix += 1
str_redefined      = ix
ix += 1
.if "wordlist" in TALI_OPTIONAL_WORDS
str_wid_forth      = ix
ix += 1
str_wid_editor     = ix
ix += 1
str_wid_assembler  = ix
ix += 1
str_wid_root       = ix
ix += 1
.endif
str_see_nt         = ix
ix += 1
str_see_xt         = ix
ix += 1
str_see_header     = ix
ix += 1
str_see_size       = ix
ix += 1
str_see_cfapfa     = ix
ix += 1
.if "disassembler" in TALI_OPTIONAL_WORDS
str_disasm_sdc     = ix
ix += 1
str_disasm_lit     = ix
ix += 1
str_disasm_0bra    = ix
ix += 1
str_disasm_loop    = ix
ix += 1
str_disasm_do      = ix
ix += 1
.endif

; Since we can't fit a 16-bit address in a register, we use indexes as offsets
; to tables as error and string numbers.
string_table:
        .word s_ok, s_compiled, s_redefined                     ; 0-2
.if "wordlist" in TALI_OPTIONAL_WORDS
        .word s_wid_forth, s_wid_editor, s_wid_asm, s_wid_root  ; 3-6
.endif
        .word s_see_nt, s_see_xt, s_see_header, s_see_size, s_see_cfapfa            ; 7-11
.if "disassembler" in TALI_OPTIONAL_WORDS
        .word s_disasm_sdc, s_disasm_lit, s_disasm_0bra, s_disasm_loop, s_disasm_do ; 12-15
.endif

; note .shift is like .text but terminates the string by setting bit 7 of the last character
; print_shift_string in taliforth.asm shows how we use these

s_ok:         .shift " ok"              ; note space at beginning
s_compiled:   .shift " compiled"        ; note space at beginning
s_redefined:  .shift "redefined "       ; note space at end

.if "wordlist" in TALI_OPTIONAL_WORDS
s_wid_asm:    .shift "Assembler "       ; Wordlist ID 2, note space at end
s_wid_editor: .shift "Editor "     ; Wordlist ID 1, note space at end
s_wid_forth:  .shift "Forth "      ; Wordlist ID 0, note space at end
s_wid_root:   .shift "Root "       ; Wordlist ID 3, note space at end
.endif

s_see_nt:     .shift "nt: "
s_see_xt:     .shift "xt: "
s_see_header: .shift "header: "
s_see_size:   .shift "size (decimal): "
s_see_cfapfa: .shift "CFA 3  PFA "

; this string is referenced directly, not via string table
; must match DICTIONARY FLAGS in definitions.asm and calculated flag order in xt_see
; hi-bit (shift) characters indicate flag insertion points, terminated by (shifted) NUL
see_flags_template:
        .text "flags: HC", s"N","N", s"A","N", s"I","M", s"C","O", s"D","C", s"L","C", s"F","P", s"|"," UF", s"S","T", $80

.if "disassembler" in TALI_OPTIONAL_WORDS
s_disasm_sdc: .shift " STACK DEPTH CHECK"
s_disasm_lit: .shift "LITERAL "
s_disasm_0bra: .shift "0BRANCH "
s_disasm_loop: .shift "LOOP "
s_disasm_do: .shift "DO "
.endif

; ## ERROR STRINGS

; All error strings must be zero-terminated, all names start with "es_",
; aliases with "err_". If the string texts are changed, the test suite must be
; as well

err_allot        = 0
err_badsource    = 1
err_compileonly  = 2
err_defer        = 3
err_divzero      = 4
err_noname       = 5
err_refill       = 6
err_state        = 7
err_syntax       = 8
err_underflow    = 9
err_negallot     = 10
err_wordlist     = 11
err_blockwords   = 12
err_returnstack  = 13
err_toolong      = 14
err_usersigint   = 15

error_table:
        .word es_allot, es_badsource, es_compileonly, es_defer          ;  0-3
        .word es_divzero, es_noname, es_refill, es_state                ;  4-7
        .word es_syntax, es_underflow, es_negallot, es_wordlist         ;  8-11
        .word es_blockwords, es_returnstack, es_toolong, es_usersigint  ; 12-15

.if ! TALI_OPTION_TERSE
es_allot:       .shift "ALLOT using all available memory"
es_badsource:   .shift "Illegal SOURCE-ID during REFILL"
es_compileonly: .shift "Interpreting a compile-only word"
es_defer:       .shift "DEFERed word not defined yet"
es_divzero:     .shift "Division by zero"
es_noname:      .shift "Parsing failure"
es_refill:      .shift "QUIT could not get input (REFILL returned -1)"
es_state:       .shift "Already in compile mode"
es_syntax:      .shift "Undefined word or invalid number"
es_underflow:   .shift "Data stack underflow"
es_negallot:    .shift "Max memory freed with ALLOT"
es_wordlist:    .shift "No wordlists available"
es_blockwords:  .shift "Please assign vectors BLOCK-READ-VECTOR and BLOCK-WRITE-VECTOR"
es_returnstack: .shift "Return stack:"
es_toolong:     .shift "Name too long (max 31)"
es_usersigint:  .shift "User interrupt"
.else
es_allot:       .shift "EALLT"
es_badsource:   .shift "EBSRC"
es_compileonly: .shift "ECMPL"
es_defer:       .shift "EDEFR"
es_divzero:     .shift "EDIV0"
es_noname:      .shift "ENAME"
es_refill:      .shift "EREFL"
es_state:       .shift "ESTAT"
es_syntax:      .shift "ESNTX"
es_underflow:   .shift "EUNDR"
es_negallot:    .shift "ENALT"
es_wordlist:    .shift "EWLST"
es_blockwords:  .shift "EBLKW"
es_returnstack: .shift "RS"
es_toolong:     .shift "E2LNG"
es_usersigint:  .shift "EINTR"
.endif


.if "environment?" in TALI_OPTIONAL_WORDS
; ## ENVIRONMENT STRINGS

; These are used by the ENVIRONMENT? word and stored in the old string format:
; Length byte first, then the string itself that is not rpt. not
; zero-terminated. Note these are uppercase by ANS defintion. All start with
; "envs_".

; These return a single-cell number
envs_cs:        .text "/COUNTED-STRING"
envs_hold:      .text "/HOLD"
envs_pad:       .text "/PAD"
envs_aub:       .text "ADDRESS-UNIT-BITS"
envs_floored:   .text "FLOORED"
envs_max_char:  .text "MAX-CHAR"
envs_max_n:     .text "MAX-N"
envs_max_u:     .text "MAX-U"
envs_rsc:       .text "RETURN-STACK-CELLS"
envs_sc:        .text "STACK-CELLS"
envs_wl:        .text "WORDLISTS"

; These return a double-cell number
envs_max_d:     .text "MAX-D"
envs_max_ud:    .text "MAX-UD"
envs_eot:
.endif

; END
