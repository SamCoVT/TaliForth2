; Dictionary Headers for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Updated by Sam Colwell
; First version: 05. Dec 2016 (Liara Forth)
; This version: 01. Jan 2023

; Dictionary headers are kept separately from the code, which allows various
; tricks in the code. We roughly follow the Gforth terminology: The Execution
; Token (xt) is the address of the first byte of a word's code that can be, uh,
; executed; the Name Token (nt) is a pointer to the beginning of the word's
; header in the Dictionary. There the link to the next word in the Dictionary
; is always one cell down from the current word's own nt. In the code itself,
; we use "nt_<WORD>" for the nt and "xt_<WORD>" for the xt.

; This gives us the following header structure:

;              8 bit     8 bit
;               LSB       MSB
; nt_word ->  +--------+--------+
;          +0 | Length | Status |
;             +--------+--------+
;          +2 | Next Header     | -> nt_next_word
;             +-----------------+
;          +4 | Start of Code   | -> xt_word
;             +-----------------+
;          +6 | End of Code     | -> z_word
;             +--------+--------+
;          +8 | Name   |        |
;             +--------+--------+
;             |        |        |
;             +--------+--------+
;             |        |  ...   | (name string does not end with a zero)
;          +n +--------+--------+

; The Status Byte is created by adding the flags defined in definitions.asm,
; which are:

;       CO - Compile Only
;       IM - Immediate Word
;       NN - Never Native Compile (must always be called by JSR)
;       AN - Always Native Compile (may not be called by JSR)
;       UF - Contains underflow check
;       HC - Has CFA (words created by CREATE and DOES> only)
;       ST - Uses stack manipulation stripped for native compile (R>, >R etc)

; Note there is currently one bit unused.

; By default, all existing words can be natively compiled (compiled inline) or
; as a subroutine jump target; the system decides which variant to use based on
; a threshold the user can set. By default, all user-created words are flagged
; never-native. The user can override this by using the always-native word
; just after defining their new word.  The NN flag forbids native compiling,
; the AN flag forces it.

; The last word (top word in code) is always BYE. It is marked as the last word
; by its value of 0000 in its Next Header field. The words are sorted with the
; more common ones first (further down in code) so they are found earlier.
; Anything to do with output comes later (further up) because things will
; always be slow if there is a human involved.

; The initial skeleton of this list was automatically generated by a script
; in the tools folder and then sorted by hand.

; The dhdr macro is used to generate the chain of dictionary entries.
; Most words use the convention that word NAME is implemented by code between
; xt_name and z_name. These dictionary entries are simply `#dhdr name`,
; adding optional flags like `#dhdr name, UF`.  Special names like M*/ can be
; defined using the optional third argument `#dhdr name, flags, "m*/"`.
; The final dictionary entry in each wordlist should specify last=true.

dhdr .macro label, flags=0, name="", last=false
    _s := \name ? \name : str(.\label)

    .byte len(_s), \flags
    .word \last ? 0 : +, xt_\label, z_\label
    .text _s
.if !\last
+
.endif
.endmacro


; FORTH-WORDLIST

dictionary_start:

#dhdr drop,             UF              ; DROP is always the first native word in the Dictionary
#dhdr dup,              UF
#dhdr swap,             UF
#dhdr store,            UF, "!"
#dhdr fetch,            UF, "@"
#dhdr over,             UF
#dhdr to_r,             CO+UF+ST, ">r"
#dhdr r_from,           CO+ST, "r>"
#dhdr r_fetch,          CO+ST, "r@"
#dhdr nip,              UF
#dhdr rot,              UF
#dhdr not_rote,         UF, "-rot"
#dhdr tuck,             UF
#dhdr comma,            UF, ","
#dhdr c_fetch,          UF, "c@"
#dhdr c_store,          UF, "c!"
#dhdr plus_store,       UF, "+!"
#dhdr zero,             , "0"
#dhdr one,              , "1"
#dhdr two,              , "2"
#dhdr execute,          UF
#dhdr emit,             NN+UF
#dhdr type,             UF
#dhdr dot,              UF, "."
#dhdr u_dot,            UF, "u."
#dhdr u_dot_r,          UF, "u.r"
#dhdr dot_r,            UF, ".r"
#dhdr d_dot,            UF, "d."
#dhdr d_dot_r,          UF, "d.r"
#dhdr ud_dot,           UF, "ud."
#dhdr ud_dot_r,         UF, "ud.r"
#dhdr question,         , "?"
#dhdr false,            , "false"
#dhdr true,             , "true"
#dhdr space
#dhdr two_dup,          UF, "2dup"
#dhdr question_dup,     UF, "?dup"
#dhdr plus,             UF, "+"
#dhdr minus,            UF, "-"
#dhdr one_minus,        UF, "1-"
#dhdr one_plus,         UF, "1+"
#dhdr two_star,         UF, "2*"
#dhdr two_slash,        UF, "2/"
#dhdr abs,              UF
#dhdr dabs,             UF
#dhdr and,              UF
#dhdr or,               UF
#dhdr xor,              UF
#dhdr rshift,           UF
#dhdr lshift,           UF
#dhdr pick                              ; underflow check is complicated, leave off here
#dhdr char
#dhdr bracket_char,     CO+IM, "[char]"
#dhdr char_plus,        , "char+"
#dhdr chars,            UF              ; becomes no-op when underflow stripped
#dhdr cells                             ; same as 2*
#dhdr cell_plus,        UF, "cell+"
#dhdr here
#dhdr equal,            UF, "="
#dhdr not_equals,       UF, "<>"
#dhdr less_than,        UF, "<"
#dhdr u_less_than,      UF, "u<"
#dhdr u_greater_than,   UF, "u>"
#dhdr greater_than,     UF, ">"
#dhdr zero_equal,       UF, "0="
#dhdr zero_unequal,     UF, "0<>"
#dhdr zero_greater,     UF, "0>"
#dhdr zero_less,        UF, "0<"
#dhdr min,              UF
#dhdr max,              UF
#dhdr two_drop,         UF, "2drop"
#dhdr two_swap,         UF, "2swap"
#dhdr two_over,         UF, "2over"
#dhdr two_store,        UF, "2!"
#dhdr two_fetch,        UF, "2@"
#dhdr two_variable,     , "2variable"
#dhdr two_constant,     UF, "2constant"
#dhdr two_literal,      UF+IM, "2literal"
#dhdr two_r_fetch,      CO+NN+ST, "2r@"
#dhdr two_r_from,       CO+ST, "2r>"
#dhdr two_to_r,         CO+UF+ST, "2>r"
#dhdr invert,           UF
#dhdr negate,           UF
#dhdr dnegate,          UF
#dhdr c_comma,          UF, "c,"
#dhdr bounds,           UF
#dhdr spaces,           UF
#dhdr bl
#dhdr minus_trailing,   UF, "-trailing"
#dhdr minus_leading,    UF, "-leading"
#dhdr slash_string,     UF, "/string"
#dhdr refill
#dhdr accept,           UF+NN
#dhdr input_to_r,       NN, "input>r"
#dhdr r_to_input,       NN, "r>input"
#dhdr unused
#dhdr depth
#dhdr key
#dhdr allot,            UF
#dhdr create
#dhdr does,             CO+IM, "does>"
#dhdr variable
#dhdr constant,         UF
#dhdr value,            UF              ; same code as CONSTANT
#dhdr to,               NN+IM
#dhdr s_to_d,           UF, "s>d"
#dhdr d_to_s,           UF, "d>s"
#dhdr d_minus,          UF, "d-"
#dhdr d_plus,           UF, "d+"
#dhdr erase                             ; underflow checked by FILL - TODO
#dhdr blank                             ; underflow checked by FILL - TODO
#dhdr fill,             UF
#dhdr find_name,        UF, "find-name"
#dhdr tick,             , "'"
#dhdr bracket_tick,     CO+IM, "[']"
#dhdr name_to_int,      UF, "name>int"
#dhdr int_to_name,      UF, "int>name"
#dhdr name_to_string,   UF, "name>string"
#dhdr to_body,          UF, ">body"
#dhdr defer
#dhdr latestxt
#dhdr latestnt
#dhdr parse_name,       NN, "parse-name"
#dhdr parse,            UF
#dhdr execute_parsing,  UF, "execute-parsing"
#dhdr source
#dhdr source_id,        , "source-id"
#dhdr colon,            , ":"
#dhdr semicolon,        CO+IM, ";"
#dhdr colon_noname,     , ":noname"
#dhdr compile_comma,    UF+NN, "compile,"
#dhdr left_bracket,     IM+CO, "["
#dhdr right_bracket,    IM, "]"
#dhdr literal,          IM+CO+UF, "literal"
#dhdr sliteral,         CO+IM+UF, "sliteral"
#dhdr dot_quote,        CO+IM, '."'
#dhdr s_quote,          IM+NN, 's"'
#dhdr s_backslash_quote, IM, 's\"'
#dhdr postpone,         IM+CO, "postpone"
#dhdr immediate
#dhdr compile_only,     , "compile-only"
#dhdr never_native,     , "never-native"
#dhdr always_native,    , "always-native"
#dhdr allow_native,     , "allow-native"
#dhdr nc_limit,         NN, "nc-limit"
#dhdr strip_underflow,  NN, "strip-underflow"
#dhdr abort
#dhdr abort_quote,      CO+IM+NN, 'abort"'
#dhdr do,               CO+IM+NN
#dhdr question_do,      CO+IM+NN, "?do"
#dhdr i,                CO
#dhdr j,                CO
#dhdr loop,             CO+IM
#dhdr plus_loop,        CO+IM, "+loop"
#dhdr exit,             AN+CO
#dhdr unloop,           CO
#dhdr leave,            CO+IM
#dhdr recurse,          CO+IM+NN
#dhdr quit
#dhdr begin,            CO+IM
#dhdr again,            CO+IM+UF
#dhdr state
#dhdr evaluate,         UF
#dhdr base
#dhdr digit_question,   UF, "digit?"
#dhdr number,           UF
#dhdr to_number,        UF, ">number"
#dhdr hex
#dhdr decimal
#dhdr count,            UF
#dhdr m_star,           UF, "m*"
#dhdr um_star,          UF, "um*"
#dhdr star,             UF, "*"
#dhdr um_slash_mod,     UF, "um/mod"
#dhdr sm_slash_rem,     UF, "sm/rem"
#dhdr fm_slash_mod,     UF, "fm/mod"
#dhdr slash,            UF, "/"
#dhdr slash_mod,        UF, "/mod"
#dhdr mod,              UF
#dhdr star_slash_mod,   UF, "*/mod"
#dhdr star_slash,       UF, "*/"
#dhdr backslash,        IM, "\"
#dhdr move,             NN+UF
#dhdr cmove_up,         UF, "cmove>"
#dhdr cmove,            UF
#dhdr pad
#dhdr cleave,           UF
#dhdr hexstore,         UF
#dhdr within,           UF
#dhdr to_in,            , ">in"
#dhdr less_number_sign, , "<#"
#dhdr number_sign,      UF, "#"
#dhdr number_sign_s,    UF, "#s"
#dhdr number_sign_greater, UF, "#>"
#dhdr hold,             UF
#dhdr sign,             UF
#dhdr output
#dhdr input
#dhdr cr
#dhdr page
#dhdr at_xy,            UF, "at-xy"
#dhdr marker,           IM
#dhdr words
#dhdr wordsize,         UF
#dhdr aligned                           ; same code as ALIGN
#dhdr align
#dhdr bell
#dhdr dump, UF
#dhdr dot_s,            , ".s"

.if "disassembler" in TALI_OPTIONAL_WORDS
#dhdr disasm,           UF
.endif

#dhdr compare,          UF
#dhdr search,           UF+NN

.if "environment?" in TALI_OPTIONAL_WORDS
#dhdr environment_q,    UF, "environment?"
.endif

#dhdr find,             UF
#dhdr word,             UF
#dhdr paren,            IM, "("
#dhdr dot_paren,        IM, ".("
#dhdr if,               IM+CO+NN
#dhdr then,             IM+CO+NN
#dhdr else,             IM+CO+NN
#dhdr repeat,           IM+CO+NN
#dhdr until,            IM+CO+NN
#dhdr while,            IM+CO+NN
#dhdr case,             IM+CO+NN        ; shares code with ZERO
#dhdr of,               IM+CO+NN
#dhdr endof,            IM+CO+NN
#dhdr endcase,          IM+CO+NN
#dhdr defer_fetch,      , "defer@"
#dhdr defer_store,      , "defer!"
#dhdr is,               IM, "is"
#dhdr action_of,        IM, "action-of"
#dhdr useraddr,         , "useraddr"
#dhdr buffer_colon,     , "buffer:"

.if "block" in TALI_OPTIONAL_WORDS
#dhdr buffstatus
#dhdr buffblocknum
#dhdr blkbuffer
#dhdr scr,              NN
#dhdr blk,              NN
;TODO these mention HC but don't have the flag?
#dhdr block_write,      NN, "block-write"       ; Deferred words need the HC (Code Field) flag.
#dhdr block_write_vector, NN, "block-write-vector" ; Deferred words need the HC (Code Field) flag.
#dhdr block_read,       HC+NN, "block-read"     ; Deferred words need the HC (Code Field) flag.
#dhdr block_read_vector, HC+NN, "block-read-vector" ; Deferred words need the HC (Code Field) flag.
#dhdr save_buffers,     , "save-buffers"
#dhdr block
#dhdr update
#dhdr buffer
#dhdr empty_buffers,    , "empty-buffers"
#dhdr flush
#dhdr load,             UF
#dhdr thru,             UF

.if "editor" in TALI_OPTIONAL_WORDS
#dhdr list,             UF
#dhdr block_c65_init,   , "block-c65-init"
.endif

.endif

.if "block" in TALI_OPTIONAL_WORDS && "ramdrive" in TALI_OPTIONAL_WORDS
#dhdr block_ramdrive_init, UF, "block-ramdrive-init"
.endif

.if "wordlist" in TALI_OPTIONAL_WORDS
#dhdr definitions
#dhdr wordlist
#dhdr search_wordlist,  UF, "search-wordlist"
#dhdr set_current,      UF, "set-current"
#dhdr get_current,      , "get-current"
#dhdr set_order,        , "set-order"
#dhdr get_order,        , "get-order"
#dhdr root_wordlist,    , "root-wordlist"
.endif

.if "assembler" in TALI_OPTIONAL_WORDS && "wordlist" in TALI_OPTIONAL_WORDS
#dhdr assembler_wordlist, , "assembler-wordlist"
.endif

.if "editor" in TALI_OPTIONAL_WORDS && "wordlist" in TALI_OPTIONAL_WORDS
#dhdr editor_wordlist,  , "editor-wordlist"
.endif

.if "wordlist" in TALI_OPTIONAL_WORDS
#dhdr forth_wordlist,   , "forth-wordlist"
#dhdr only
#dhdr also
#dhdr previous
#dhdr to_order,         , ">order"
#dhdr order
#dhdr forth
.endif

#dhdr see,              NN

.if "ed" in TALI_OPTIONAL_WORDS
#dhdr ed,               NN, "ed:"       ; ed6502
.endif

#dhdr cold
#dhdr bye,              , , true        ; true flags last word in the wordlist

; END of FORTH-WORDLIST


; ROOT-WORDLIST
        ; This is a short wordlist that has just the words needed to
        ; set the wordlists. These words are also included in the
        ; FORTH-WORDLIST as well.

root_dictionary_start:
.if "wordlist" in TALI_OPTIONAL_WORDS
#dhdr set_order,        , "set-order"
#dhdr forth
#dhdr forth_wordlist,   , "forth-wordlist"   ; shares code with ZERO
#dhdr words,            , , true
.endif
; END of ROOT-WORDLIST


; EDITOR-WORDLIST

editor_dictionary_start:
.if "editor" in TALI_OPTIONAL_WORDS && "block" in TALI_OPTIONAL_WORDS
#dhdr editor_o,         , "o"
#dhdr editor_line,      UF, "line"
#dhdr editor_l,         , "l"
#dhdr editor_el,        , "el"
#dhdr editor_erase_screen, , "erase-screen"
#dhdr editor_enter_screen, , "enter-screen", true
.endif
; END of EDITOR-WORDLIST


; ASSEMBLER-WORDLIST

; Labels for the opcodes have the format "nt_asm_<OPC>" where a futher
; underscore replaces any dot present in the SAN mnemonic. The hash sign for
; immediate addressing is replaced by an "h" (for example, the label code for
; "lda.#" is "xt_adm_lda_h"). All opcodes are immediate.
assembler_dictionary_start:
.if "assembler" in TALI_OPTIONAL_WORDS
#dhdr asm_adc_h,        IM+NN, "adc.#"
#dhdr asm_adc_x,        IM+NN, "adc.x"
#dhdr asm_adc_y,        IM+NN, "adc.y"
#dhdr asm_adc_z,        IM+NN, "adc.z"
#dhdr asm_adc_zi,       IM+NN, "adc.zi"
#dhdr asm_adc_ziy,      IM+NN, "adc.ziy"
#dhdr asm_adc_zx,       IM+NN, "adc.zx"
#dhdr asm_adc_zxi,      IM+NN, "adc.zxi"
#dhdr asm_and,          IM+NN, "and."   ; not "and" because of conflicts with Forth word
#dhdr asm_and_h,        IM+NN, "and.#"
#dhdr asm_and_x,        IM+NN, "and.x"
#dhdr asm_and_y,        IM+NN, "and.y"
#dhdr asm_and_z,        IM+NN, "and.z"
#dhdr asm_and_zi,       IM+NN, "and.zi"
#dhdr asm_and_ziy,      IM+NN, "and.ziy"
#dhdr asm_and_zx,       IM+NN, "and.zx"
#dhdr asm_and_zxi,      IM+NN, "and.zxi"
#dhdr asm_asl,          IM+NN, "asl"
#dhdr asm_asl_a,        IM+NN, "asl.a"
#dhdr asm_asl_x,        IM+NN, "asl.x"
#dhdr asm_asl_z,        IM+NN, "asl.z"
#dhdr asm_asl_zx,       IM+NN, "asl.zx"
#dhdr asm_bcc,          IM+NN, "bcc"
#dhdr asm_bcs,          IM+NN, "bcs"
#dhdr asm_beq,          IM+NN, "beq"
#dhdr asm_bit,          IM+NN, "bit"
#dhdr asm_bit_h,        IM+NN, "bit.#"
#dhdr asm_bit_x,        IM+NN, "bit.x"
#dhdr asm_bit_z,        IM+NN, "bit.z"
#dhdr asm_bit_zx,       IM+NN, "bit.zx"
#dhdr asm_bmi,          IM+NN, "bmi"
#dhdr asm_bne,          IM+NN, "bne"
#dhdr asm_bpl,          IM+NN, "bpl"
#dhdr asm_bra,          IM+NN, "bra"
#dhdr asm_brk,          IM+NN, "brk"
#dhdr asm_bvc,          IM+NN, "bvc"
#dhdr asm_bvs,          IM+NN, "bvs"
#dhdr asm_clc,          IM+NN, "clc"
#dhdr asm_cld,          IM+NN, "cld"
#dhdr asm_cli,          IM+NN, "cli"
#dhdr asm_clv,          IM+NN, "clv"
#dhdr asm_cmp,          IM+NN, "cmp"
#dhdr asm_cmp_h,        IM+NN, "cmp.#"
#dhdr asm_cmp_x,        IM+NN, "cmp.x"
#dhdr asm_cmp_y,        IM+NN, "cmp.y"
#dhdr asm_cmp_z,        IM+NN, "cmp.z"
#dhdr asm_cmp_zi,       IM+NN, "cmp.zi"
#dhdr asm_cmp_ziy,      IM+NN, "cmp.ziy"
#dhdr asm_cmp_zx,       IM+NN, "cmp.zx"
#dhdr asm_cmp_zxi,      IM+NN, "cmp.zxi"
#dhdr asm_cpx,          IM+NN, "cpx"
#dhdr asm_cpx_h,        IM+NN, "cpx.#"
#dhdr asm_cpx_z,        IM+NN, "cpx.z"
#dhdr asm_cpy,          IM+NN, "cpy"
#dhdr asm_cpy_h,        IM+NN, "cpy.#"
#dhdr asm_cpy_z,        IM+NN, "cpy.z"
#dhdr asm_dec,          IM+NN, "dec"
#dhdr asm_dec_a,        IM+NN, "dec.a"
#dhdr asm_dec_x,        IM+NN, "dec.x"
#dhdr asm_dec_z,        IM+NN, "dec.z"
#dhdr asm_dec_zx,       IM+NN, "dec.zx"
#dhdr asm_dex,          IM+NN, "dex"
#dhdr asm_dey,          IM+NN, "dey"
#dhdr asm_eor,          IM+NN, "eor"
#dhdr asm_eor_h,        IM+NN, "eor.#"
#dhdr asm_eor_x,        IM+NN, "eor.x"
#dhdr asm_eor_y,        IM+NN, "eor.y"
#dhdr asm_eor_z,        IM+NN, "eor.z"
#dhdr asm_eor_zi,       IM+NN, "eor.zi"
#dhdr asm_eor_ziy,      IM+NN, "eor.ziy"
#dhdr asm_eor_zx,       IM+NN, "eor.zx"
#dhdr asm_eor_zxi,      IM+NN, "eor.zxi"
#dhdr asm_inc,          IM+NN, "inc"
#dhdr asm_inc_a,        IM+NN, "inc.a"
#dhdr asm_inc_x,        IM+NN, "inc.x"
#dhdr asm_inc_z,        IM+NN, "inc.z"
#dhdr asm_inc_zx,       IM+NN, "inc.zx"
#dhdr asm_inx,          IM+NN, "inx"
#dhdr asm_iny,          IM+NN, "iny"
#dhdr asm_jmp,          IM+NN, "jmp"
#dhdr asm_jmp_i,        IM+NN, "jmp.i"
#dhdr asm_jmp_xi,       IM+NN, "jmp.xi"
#dhdr asm_jsr,          IM+NN, "jsr"
#dhdr asm_lda,          IM+NN, "lda"
#dhdr asm_lda_h,        IM+NN, "lda.#"
#dhdr asm_lda_x,        IM+NN, "lda.x"
#dhdr asm_lda_y,        IM+NN, "lda.y"
#dhdr asm_lda_z,        IM+NN, "lda.z"
#dhdr asm_lda_zi,       IM+NN, "lda.zi"
#dhdr asm_lda_ziy,      IM+NN, "lda.ziy"
#dhdr asm_lda_zx,       IM+NN, "lda.zx"
#dhdr asm_lda_zxi,      IM+NN, "lda.zxi"
#dhdr asm_ldx,          IM+NN, "ldx"
#dhdr asm_ldx_h,        IM+NN, "ldx.#"
#dhdr asm_ldx_y,        IM+NN, "ldx.y"
#dhdr asm_ldx_z,        IM+NN, "ldx.z"
#dhdr asm_ldx_zy,       IM+NN, "ldx.zy"
#dhdr asm_ldy,          IM+NN, "ldy"
#dhdr asm_ldy_h,        IM+NN, "ldy.#"
#dhdr asm_ldy_x,        IM+NN, "ldy.x"
#dhdr asm_ldy_z,        IM+NN, "ldy.z"
#dhdr asm_ldy_zx,       IM+NN, "ldy.zx"
#dhdr asm_lsr,          IM+NN, "lsr"
#dhdr asm_lsr_a,        IM+NN, "lsr.a"
#dhdr asm_lsr_x,        IM+NN, "lsr.x"
#dhdr asm_lsr_z,        IM+NN, "lsr.z"
#dhdr asm_lsr_zx,       IM+NN, "lsr.zx"
#dhdr asm_nop,          IM+NN, "nop"
#dhdr asm_ora,          IM+NN, "ora"
#dhdr asm_ora_h,        IM+NN, "ora.#"
#dhdr asm_ora_x,        IM+NN, "ora.x"
#dhdr asm_ora_y,        IM+NN, "ora.y"
#dhdr asm_ora_z,        IM+NN, "ora.z"
#dhdr asm_ora_zi,       IM+NN, "ora.zi"
#dhdr asm_ora_ziy,      IM+NN, "ora.ziy"
#dhdr asm_ora_zx,       IM+NN, "ora.zx"
#dhdr asm_ora_zxi,      IM+NN, "ora.zxi"
#dhdr asm_pha,          IM+NN, "pha"
#dhdr asm_php,          IM+NN, "php"
#dhdr asm_phx,          IM+NN, "phx"
#dhdr asm_phy,          IM+NN, "phy"
#dhdr asm_pla,          IM+NN, "pla"
#dhdr asm_plp,          IM+NN, "plp"
#dhdr asm_plx,          IM+NN, "plx"
#dhdr asm_ply,          IM+NN, "ply"
#dhdr asm_rol,          IM+NN, "rol"
#dhdr asm_rol_a,        IM+NN, "rol.a"
#dhdr asm_rol_x,        IM+NN, "rol.x"
#dhdr asm_rol_z,        IM+NN, "rol.z"
#dhdr asm_rol_zx,       IM+NN, "rol.zx"
#dhdr asm_ror,          IM+NN, "ror"
#dhdr asm_ror_a,        IM+NN, "ror.a"
#dhdr asm_ror_x,        IM+NN, "ror.x"
#dhdr asm_ror_z,        IM+NN, "ror.z"
#dhdr asm_ror_zx,       IM+NN, "ror.zx"
#dhdr asm_rti,          IM+NN, "rti"
#dhdr asm_rts,          IM+NN, "rts"
#dhdr asm_sbc,          IM+NN, "sbc"
#dhdr asm_sbc_h,        IM+NN, "sbc.#"
#dhdr asm_sbc_x,        IM+NN, "sbc.x"
#dhdr asm_sbc_y,        IM+NN, "sbc.y"
#dhdr asm_sbc_z,        IM+NN, "sbc.z"
#dhdr asm_sbc_zi,       IM+NN, "sbc.zi"
#dhdr asm_sbc_ziy,      IM+NN, "sbc.ziy"
#dhdr asm_sbc_zx,       IM+NN, "sbc.zx"
#dhdr asm_sbc_zxi,      IM+NN, "sbc.zxi"
#dhdr asm_sec,          IM+NN, "sec"
#dhdr asm_sed,          IM+NN, "sed"
#dhdr asm_sei,          IM+NN, "sei"
#dhdr asm_sta,          IM+NN, "sta"
#dhdr asm_sta_x,        IM+NN, "sta.x"
#dhdr asm_sta_y,        IM+NN, "sta.y"
#dhdr asm_sta_z,        IM+NN, "sta.z"
#dhdr asm_sta_zi,       IM+NN, "sta.zi"
#dhdr asm_sta_ziy,      IM+NN, "sta.ziy"
#dhdr asm_sta_zx,       IM+NN, "sta.zx"
#dhdr asm_sta_zxi,      IM+NN, "sta.zxi"
#dhdr asm_stx,          IM+NN, "stx"
#dhdr asm_stx_z,        IM+NN, "stx.z"
#dhdr asm_stx_zy,       IM+NN, "stx.zy"
#dhdr asm_sty,          IM+NN,"sty"
#dhdr asm_sty_z,        IM+NN, "sty.z"
#dhdr asm_sty_zx,       IM+NN, "sty.zx"
#dhdr asm_stz,          IM+NN, "stz"
#dhdr asm_stz_x,        IM+NN, "stz.x"
#dhdr asm_stz_z,        IM+NN, "stz.z"
#dhdr asm_stz_zx,       IM+NN, "stz.zx"
#dhdr asm_tax,          IM+NN, "tax"
#dhdr asm_tay,          IM+NN, "tay"
#dhdr asm_trb,          IM+NN, "trb"
#dhdr asm_trb_z,        IM+NN, "trb.z"
#dhdr asm_tsb,          IM+NN, "tsb"
#dhdr asm_tsb_z,        IM+NN, "tsb.z"
#dhdr asm_tsx,          IM+NN, "tsx"
#dhdr asm_txa,          IM+NN, "txa"
#dhdr asm_txs,          IM+NN, "txs"
#dhdr asm_tya,          IM+NN, "tya"

; Assembler pseudo-instructions, directives and macros

#dhdr asm_arrow,        IM, "-->"       ; uses same code as HERE, but immediate
#dhdr asm_back_jump,    IM, "<j"        ; syntactic sugar, does nothing
#dhdr asm_back_branch,  IM, "<b"
#dhdr asm_push_a,       IM+NN, "push-a", true

.endif
; END of ASSEMBLER-WORDLIST

; END
