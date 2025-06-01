; Dictionary Headers for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Updated by Sam Colwell
; First version: 05. Dec 2016 (Liara Forth)
; This version: 01. Jan 2023

; Dictionary headers are kept separately from the code, which allows various
; tricks in the code. We roughly follow the Gforth terminology: The Execution
; Token (xt) is the address of the first byte of a word's code that can be, uh,
; executed; the Name Token (nt) is a pointer to the beginning of the word's
; header in the Dictionary.   Conceptually the header looks like this:

;   nt_word --> +----------------+
;               | Status flags   |
;               +----------------+
;               | Name length    |
;               +----------------+
;               | Prev header    | -> nt_prev_word
;               +----------------+
;               | Start of code  | -> xt_word
;               +----------------+
;               | Length of code | -> z_word - xt_word
;               +----------------+
;               | Name string    | (name length bytes, not zero terminated)
;               +----------------+
;
; In practice we use variable sizes for several of the fields so that
; each header is between 4-8 bytes plus the word's name, with most headers
; taking only four bytes.  The variable size fields are indicated in the status byte
; along with other flags defined in definitions.asm:
;
; Status flags byte
;
;  (msb)  7    6    5    4    3    2    1    0  (lsb)
;       +----+----+----+----+----+----+----+----+
;       | HC | NN | AN | IM | CO | DC | LC | FP |
;       +----+----+----+----+----+----+----+----+
;
; Flag bits:
;
;       FP - Far previous NT (LSB/MSB not just LSB within previous page)
;       LC - Long code (two byte vs one byte length for native compile)
;       DC - Disjoint code (two byte pointer to xt rather than adjoining header)
;
;       CO - Compile Only
;       IM - Immediate Word
;       AN - Always Native Compile (may not be called by JSR)
;       NN - Never Native Compile (must always be called by JSR)
;       HC - Has CFA (words created by CREATE and DOES> only)
;
;       The NN and AN flags are intrepreted together like this:
;
;            NN  AN
;           +---+---+
;           | 0 | 0 |  -- : Normal word called by JSR (non-native) or inlined (native)
;           | 1 | 0 |  NN : Word can only be called by JSR (never native)
;           | 0 | 1 |  AN : Word can only be inlined (always native)
;           | 1 | 1 |  ST : Normal word with return stack juggling that
;           +---+---+       must be removed when inlining (R>, R@, >R etc)
;
; Note that there are currently no free bits in the status byte.
;
; By default, all words can be natively compiled (compiled inline) or
; as a subroutine jump target; the system decides which variant to use based on
; a threshold the user can set. Tali detects users words that can't be inlined,
; typically because they contain non-relocatable `jmp` instructions, and flags
; them as NN.  The user can override the default flags using the `always-native`
; or `never-native` words, for example when using raw assembly.
;
;
; The actual header implementation looks like this:
;
;   nt_prev --> +----------------+
;               :                :
;               :                :
;                     ...
;
;   nt_word --> +----------------+
;               |  Status flags  |   See description below
;               |    (1 byte)    |
;               +----------------+
;               |  Name length   |   Max name length 31 (5 bits)
;               |   (1 byte)     |   Top three bits are currently reserved
;               +----------------+
;               |    Prev NT     |   FP=0: nt_prev = nt_word - offset
;               |  (1|2 bytes)   |   FP=1: nt_prev = 2 byte pointer
;               +----------------+
;               | Start of code  |   DC=0: xt_word = nt_end
;               |  (0|2 bytes)   |   DC=1: xt_word = 2 byte pointer
;               +----------------+
;               |   Code size    |   LC=0: One byte code length
;               |  (1|2 bytes)   |   LC=1: Two byte code length
;               +----------------+
;               |  Name string   |   Name is 7-bit lower case ascii
;               :                :
;               :                :   Note string is not zero-terminated
;    nt_end --> +----------------+
;                       ...
;   xt_word --> +----------------+
;               |   Word body    |   When DC=0 the code body adjoins the header
;               :  (65c02 code)  :
;               :                :
;    z_word --> +----------------+
;
;
; We can calculate the actual header length from the flag bits FP, LC and DC
; using simple assembly:
;
;       lda flags       ; start with status flags in the accumulator
;       and #DC+LC+FP   ; mask the header length bits
;       lsr             ; shift FP to carry flag, A = 2*DC + LC
;       adc #4          ; header length is 4 bytes + 2*DC + LC + FP
;
; To simplify header creation, reduce errors and allow for future changes
; we use the #nt_header macro to generate the actual header bytes for each word.
; Usually word NAME is implemented by code between `xt_name` and `z_name`.
; These headers are generated with just `#nt_header name`.  When a word's name
; doesn't match its start and end labels simply pass the name string
; as the optional second argument, like `#nt_header m_star_slash, "m*/"`.
; If the word needs any of the CO, IM, AN, NN flags, pass both the name
; and flags like `#nt_header myword, "myword", IM+CO`.
; The macro automatically chains words together in the dictionary using
; the `prev_nt` compiler variable.  When your word list is complete
; you can capture the head of the dictionary (the latest NT header)
; by assigning from it, like `dictionary_start = prev_nt`.
; Then start a new wordlist by resetting `prev_nt := 0`.


; The #nt_header macro is used to generate the physical header representation
nt_header .macro label, name="", flags=0
        ; It might be nicer to pass the name string instead of the bare label
        ; but it seems hard to generate a symbol from a string in 64tass
        ; see https://sourceforge.net/p/tass64/feature-requests/23/

    ; temp string with explicit name or stringified label
    _s := \name ? \name : str(.\label)

_nt:                    ; remember start of header to update prev_nt pointer
    _fp := _nt < prev_nt || _nt - prev_nt > 256 ? FP : 0
    _sz := z_\label - xt_\label
    _lc := _sz > 255 ? LC : 0
    _dc := _nt_end != xt_\label ? DC : 0

    .byte \flags | _fp | _lc | _dc       ; status flags byte
    .byte len(_s)       ; length of word string, max 31
.if _fp
    .word prev_nt       ; previous Dictionary header, 0000 signals start
.else
    .byte <prev_nt
.endif
.if _dc
    .word xt_\label     ; start of code block (xt of this word)
.endif
.if _lc
    .word _sz          ; code size up to but not including final RTS
.else
    .byte _sz
.endif
    .text _s            ; word name, always lower case, not zero-terminated
_nt_end = *

prev_nt ::= _nt         ; update to this header
.endmacro

; a specialized macro to generate assembler words
nt_asm .macro op, label
_nt:
        .byte IM + NN + DC      ; immediate, never native, and header disjoint from code
        .byte len(\label)
        .byte <prev_nt          ; predefined headers are close together
        .word xt_asm_op(\op)    ; pick jsr from the asm_op_table whose return address gives op code
        .byte 3                 ; the body is always 3 bytes tho never-native anyway
        .text \label
prev_nt ::= _nt
    .endmacro


; prev_nt tracks the previous header, and is reset after each wordlist
prev_nt := 0


; FORTH-WORDLIST

; Each wordlist is organized as a linked list with the head of the list stored
; in the `cold_user_table` table called `wordlists_offset`.  The main Forth
; dictionary is linked from `dictionary_start`.  Whenever we search for a word,
; for example via `find-name`, we begin at the head and walk through the linked
; list until we find the word or find a header whose previous NT pointer is 0.
; This means it's more efficient to have frequently used words towards the head
; of the list (later in memory).  In the main dictionary the first word we search
; (highest in memory) is DROP and the final word we search (lowest in memory) is BYE.
; Other words are sorted with the more common ones higher in memory so they are
; found faster. Anything to do with output is further back in the list because
; speed is less important when a human is involved.

; The initial skeleton of this list was automatically generated by a script
; in the tools folder and then sorted by hand.

#nt_header bye
#nt_header cold

.if "ed" in TALI_OPTIONAL_WORDS
#nt_header ed, "ed:", NN                                ; ed6502
.endif

.if "wordlist" in TALI_OPTIONAL_WORDS
    .if "editor" in TALI_OPTIONAL_WORDS
#nt_header editor_wordlist, "editor-wordlist"           ; shares code with ONE
    .endif

    .if "assembler" in TALI_OPTIONAL_WORDS
#nt_header assembler_wordlist, "assembler-wordlist"     ; shares code with TWO
    .endif

#nt_header forth
#nt_header order
#nt_header to_order, ">order"
#nt_header previous
#nt_header also
#nt_header only
#nt_header forth_wordlist, "forth-wordlist"             ; shares code with ZERO
.endif

.if "wordlist" in TALI_OPTIONAL_WORDS
#nt_header root_wordlist, "root-wordlist"
#nt_header get_order, "get-order"
#nt_header set_order, "set-order"
#nt_header get_current, "get-current"
#nt_header set_current, "set-current"
#nt_header search_wordlist, "search-wordlist"
#nt_header wordlist
#nt_header definitions
.endif

.if "block" in TALI_OPTIONAL_WORDS

    .if "ramdrive" in TALI_OPTIONAL_WORDS
#nt_header block_ramdrive_init, "block-ramdrive-init"
    .endif
   .if TALI_ARCH == "c65"
#nt_header block_c65_init, "block-c65-init"
   .endif
   .if "editor" in TALI_OPTIONAL_WORDS
#nt_header list
   .endif

#nt_header thru
#nt_header load
#nt_header flush
#nt_header empty_buffers, "empty-buffers"
#nt_header buffer
#nt_header update
#nt_header block
#nt_header save_buffers, "save-buffers"
#nt_header block_read_vector, "block-read-vector", NN
#nt_header block_read, "block-read", NN
#nt_header block_write_vector, "block-write-vector", NN
#nt_header block_write, "block-write", NN
#nt_header blk, "blk", NN
#nt_header scr, "scr", NN
#nt_header blkbuffer
#nt_header buffblocknum
#nt_header buffstatus
.endif

.if "environment?" in TALI_OPTIONAL_WORDS
#nt_header environment_q, "environment?"
.endif

.if "disassembler" in TALI_OPTIONAL_WORDS
#nt_header disasm
.endif

#nt_header see, "see", NN
#nt_header buffer_colon, "buffer:"
#nt_header useraddr
#nt_header action_of, "action-of", IM
#nt_header is, "is", IM
#nt_header defer_store, "defer!"
#nt_header defer_fetch, "defer@"
#nt_header endcase, "endcase", IM+CO+NN
#nt_header endof, "endof", IM+CO+NN             ; shares code with ELSE
#nt_header of, "of", IM+CO+NN
#nt_header case, "case", IM+CO+NN               ; shares code with ZERO
#nt_header while, "while", IM+CO+NN
#nt_header until, "until", IM+CO+NN
#nt_header repeat, "repeat", IM+CO+NN
#nt_header else, "else", IM+CO+NN
#nt_header then, "then", IM+CO+NN
#nt_header if, "if", IM+CO+NN
#nt_header dot_paren, ".(", IM
#nt_header paren, "(", IM
#nt_header word
#nt_header find
#nt_header search, "search", NN
#nt_header compare
#nt_header dot_s, ".s"
#nt_header dump
#nt_header bell
#nt_header align
#nt_header aligned
#nt_header wordsize
#nt_header words
#nt_header marker, "marker", IM
#nt_header at_xy, "at-xy"
#nt_header page
#nt_header cr
#nt_header havekey
#nt_header input
#nt_header output
#nt_header sign
#nt_header hold
#nt_header number_sign_greater, "#>"
#nt_header number_sign_s, "#s"
#nt_header number_sign, "#"
#nt_header less_number_sign, "<#"
#nt_header to_in, ">in"
#nt_header within
#nt_header hexstore
#nt_header cleave
#nt_header pad
#nt_header cmove
#nt_header cmove_up, "cmove>"
#nt_header move, "move", NN
#nt_header backslash, "\", IM
#nt_header star_slash, "*/"
#nt_header star_slash_mod, "*/mod"
#nt_header mod
#nt_header slash_mod, "/mod"
#nt_header slash, "/"
#nt_header fm_slash_mod, "fm/mod"
#nt_header sm_slash_rem, "sm/rem"
#nt_header um_slash_mod, "um/mod"
#nt_header star, "*"
#nt_header um_star, "um*"
#nt_header m_star, "m*"
#nt_header count
#nt_header decimal
#nt_header hex
#nt_header to_number, ">number"
#nt_header number
#nt_header digit_question, "digit?"
#nt_header base
#nt_header evaluate
#nt_header state
#nt_header again, "again", CO+IM
#nt_header begin, "begin", CO+IM
#nt_header quit
#nt_header recurse, "recurse", CO+IM
#nt_header leave, "leave", CO+IM
nt_unloop:
#nt_header unloop, "unloop", CO
#nt_header exit, "exit", AN+CO
#nt_header plus_loop, "+loop", CO+IM
#nt_header loop, "loop", CO+IM
#nt_header j, "j", CO
#nt_header i, "i", CO
#nt_header question_do, "?do", CO+IM+NN
#nt_header do, "do", CO+IM+NN
#nt_header abort_quote, 'abort"', CO+IM+NN
#nt_header abort
#nt_header strip_underflow, "strip-underflow", NN
#nt_header nc_limit, "nc-limit", NN
#nt_header allow_native, "allow-native"
#nt_header always_native, "always-native"
#nt_header never_native, "never-native"
#nt_header compile_only, "compile-only"
#nt_header immediate
#nt_header postpone, "postpone", IM+CO
#nt_header s_backslash_quote, 's\"', IM
#nt_header s_quote, 's"', IM+NN
#nt_header dot_quote, '."', CO+IM
#nt_header sliteral, "sliteral", CO+IM
#nt_header literal, "literal", IM+CO
#nt_header right_bracket, "]", IM
#nt_header left_bracket, "[", IM+CO
#nt_header compile_comma, "compile,", NN
#nt_header colon_noname, ":noname"
#nt_header semicolon, ";", CO+IM
#nt_header colon, ":"
#nt_header source_id, "source-id"
#nt_header source
#nt_header execute_parsing, "execute-parsing"
#nt_header parse
#nt_header parse_name, "parse-name", NN
#nt_header latestnt
#nt_header latestxt
#nt_header defer
#nt_header to_body, ">body"
#nt_header name_to_string, "name>string"
#nt_header int_to_name, "int>name"
#nt_header name_to_int, "name>int"
#nt_header bracket_tick, "[']", CO+IM
#nt_header tick, "'"
#nt_header find_name, "find-name"
#nt_header fill
#nt_header blank
#nt_header erase
#nt_header d_plus, "d+"
#nt_header d_minus, "d-"
#nt_header d_to_s, "d>s"
#nt_header s_to_d, "s>d"
#nt_header to, "to", NN+IM
#nt_header value                        ; same code as CONSTANT
#nt_header constant
#nt_header variable
#nt_header does, "does>", CO+IM
#nt_header create
#nt_header allot
#nt_header keyq, "key?"
#nt_header key
#nt_header depth
#nt_header unused
#nt_header r_to_input, "r>input", NN
#nt_header input_to_r, "input>r", NN
#nt_header accept, "accept", NN
#nt_header refill
#nt_header slash_string, "/string"
#nt_header minus_leading, "-leading"
#nt_header minus_trailing, "-trailing"
#nt_header bl
#nt_header spaces
#nt_header bounds
#nt_header c_comma, "c,"
#nt_header dnegate
#nt_header negate
#nt_header invert
#nt_header two_to_r, "2>r", CO+ST       ; native skips stack juggling
#nt_header two_r_from, "2r>", CO+ST     ; native skips stack juggling
#nt_header two_r_fetch, "2r@", CO+ST    ; native skips stack juggling
#nt_header two_literal, "2literal", IM
#nt_header two_constant, "2constant"
#nt_header two_variable, "2variable"
#nt_header two_fetch, "2@"
#nt_header two_store, "2!"
#nt_header two_over, "2over"
#nt_header two_swap, "2swap"
#nt_header two_drop, "2drop"
#nt_header max
#nt_header min
#nt_header zero_less, "0<"
#nt_header zero_greater, "0>"
#nt_header zero_unequal, "0<>"
#nt_header zero_equal, "0="
#nt_header greater_than, ">"
#nt_header u_greater_than, "u>"
#nt_header u_less_than, "u<"
#nt_header less_than, "<"
#nt_header not_equals, "<>"
#nt_header equal, "="
#nt_header here
#nt_header cell_plus, "cell+"
#nt_header cells                ; same as 2*
#nt_header chars                ; no-op (underflow check) during compile
#nt_header char_plus, "char+"
#nt_header bracket_char, "[char]", CO+IM
#nt_header char
#nt_header pick                 ; underflow check is complicated, leave off here
#nt_header lshift
#nt_header rshift
#nt_header xor
#nt_header or
#nt_header and
#nt_header dabs
#nt_header abs
#nt_header two_slash, "2/"
#nt_header two_star, "2*"
#nt_header one_plus, "1+"
#nt_header one_minus, "1-"
#nt_header minus, "-"
#nt_header plus, "+"
#nt_header question_dup, "?dup"
#nt_header two_dup, "2dup"
#nt_header space
#nt_header true
#nt_header false
nt_question:
#nt_header question, "?"
#nt_header ud_dot_r, "ud.r"
#nt_header ud_dot, "ud."
#nt_header m_star_slash, "m*/"
#nt_header d_dot_r, "d.r"
#nt_header d_dot, "d."
#nt_header dot_r, ".r"
#nt_header u_dot_r, "u.r"
#nt_header u_dot, "u."
#nt_header dot, "."
#nt_header type
#nt_header emit, "emit", NN
#nt_header execute, "execute", NN
#nt_header two, "2"
#nt_header one, "1"
#nt_header zero, "0"
#nt_header plus_store, "+!"
#nt_header c_store, "c!"
#nt_header c_fetch, "c@"
#nt_header comma, ","
#nt_header tuck
#nt_header not_rot, "-rot"
#nt_header rot
#nt_header nip
#nt_header r_fetch, "r@", CO+ST ; native skips stack juggling
#nt_header r_from, "r>", CO+ST  ; native skips stack juggling
#nt_header to_r, ">r", CO+ST    ; native skips stack juggling
#nt_header over
#nt_header fetch, "@"
#nt_header store, "!"
#nt_header swap
#nt_header dup
#nt_header drop                 ; DROP is always the first native word in the Dictionary

dictionary_start = prev_nt
prev_nt := 0

; END of FORTH-WORDLIST


; ROOT-WORDLIST
        ; This is a short wordlist that has just the words needed to
        ; set the wordlists. These words are also included in the
        ; FORTH-WORDLIST as well.

.if "wordlist" in TALI_OPTIONAL_WORDS
#nt_header set_order, "set-order"
#nt_header forth
#nt_header forth_wordlist, "forth-wordlist"     ; shares code with ZERO
#nt_header words
.endif

root_dictionary_start = prev_nt
prev_nt := 0

; END of ROOT-WORDLIST


; EDITOR-WORDLIST

.if "editor" in TALI_OPTIONAL_WORDS && "block" in TALI_OPTIONAL_WORDS
#nt_header editor_o, "o"
#nt_header editor_line, "line"
#nt_header editor_l, "l"
#nt_header editor_el, "el"
#nt_header editor_erase_screen, "erase-screen"
#nt_header editor_enter_screen, "enter-screen"
.endif

editor_dictionary_start = prev_nt
prev_nt := 0

; END of EDITOR-WORDLIST


; ASSEMBLER-WORDLIST

.if "assembler" in TALI_OPTIONAL_WORDS || "disassembler" in TALI_OPTIONAL_WORDS

; Assembler pseudo-instructions, directives and macros
; nb. these need to come before the opcode macros since it assumes near previous NT

#nt_header asm_arrow, "-->", IM         ; uses same code as HERE, but immediate
#nt_header asm_back_jump, "<j", IM      ; syntactic sugar, does nothing
#nt_header asm_back_branch, "<b", IM
#nt_header asm_push_a, "push-a", IM+NN

; Labels for the opcodes have the format "nt_asm_<OPC>" where a futher
; underscore replaces any dot present in the SAN mnemonic. The hash sign for
; immediate addressing is replaced by an "h".  For example, the label code for
; "lda.#" is "xt_adm_lda_h"). All opcode words are immediate and never-native.
;
; Each opcode shows the traditional mnemonic as a comment with abbreviations:
;       llhh - a 16bit little endian address
;       zp - an 8bit zero page address
;       #dd - an 8bit data value
;
; Currently the extended opcodes rmbN, smbN, bbrN, and bbsN aren't supported.
;
; The .nt_asm macro ensures that each word's XT points to one of a list
; of 256 repeated JSR insructions so that the return address (XT+2) has
; an LSB equal to the opcode.  This gives a compact way to assemble each
; opcode as well as a simple way to find known opcodes during disassembly.
; See assembler.asm for details.

nt_asm_begin:   ; ... nt_asm_end bracket the asm words for disasm search

nt_asm_adc:     .nt_asm $6d, "adc"      ; ADC llhh
nt_asm_adc_h:   .nt_asm $69, "adc.#"    ; ADC #dd
nt_asm_adc_x:   .nt_asm $7d, "adc.x"    ; ADC llhh,x
nt_asm_adc_y:   .nt_asm $79, "adc.y"    ; ADC llhh,y
nt_asm_adc_z:   .nt_asm $65, "adc.z"    ; ADC zp
nt_asm_adc_zi:  .nt_asm $72, "adc.zi"   ; ADC (zp)
nt_asm_adc_ziy: .nt_asm $71, "adc.ziy"  ; ADC (zp),y
nt_asm_adc_zx:  .nt_asm $75, "adc.zx"   ; ADC zp,x
nt_asm_adc_zxi: .nt_asm $61, "adc.zxi"  ; ADC (zp,x)

nt_asm_and:     .nt_asm $2d, "and."     ; AND llhh - not "and" because of conflicts with Forth word
nt_asm_and_h:   .nt_asm $29, "and.#"    ; AND #dd
nt_asm_and_x:   .nt_asm $3d, "and.x"    ; AND llhh,x
nt_asm_and_y:   .nt_asm $39, "and.y"    ; AND llhh,y
nt_asm_and_z:   .nt_asm $25, "and.z"    ; AND zp
nt_asm_and_zi:  .nt_asm $32, "and.zi"   ; AND (zp)
nt_asm_and_ziy: .nt_asm $31, "and.ziy"  ; AND (zp),y
nt_asm_and_zx:  .nt_asm $35, "and.zx"   ; AND zp,x
nt_asm_and_zxi: .nt_asm $21, "and.zxi"  ; AND (zp,x)

nt_asm_asl:     .nt_asm $0e, "asl"      ; ASL llhh
nt_asm_asl_a:   .nt_asm $0a, "asl.a"    ; ASL A
nt_asm_asl_x:   .nt_asm $1e, "asl.x"    ; ASL llhh,x
nt_asm_asl_z:   .nt_asm $06, "asl.z"    ; ASL zp
nt_asm_asl_zx:  .nt_asm $16, "asl.zx"   ; ASL zp,x

nt_asm_bcc:     .nt_asm $90, "bcc"      ; BCC rr
nt_asm_bcs:     .nt_asm $b0, "bcs"      ; BCS rr
nt_asm_beq:     .nt_asm $f0, "beq"      ; BEQ rr
nt_asm_bmi:     .nt_asm $30, "bmi"      ; BMI rr
nt_asm_bne:     .nt_asm $d0, "bne"      ; BNE rr
nt_asm_bpl:     .nt_asm $10, "bpl"      ; BPL rr
nt_asm_bra:     .nt_asm $80, "bra"      ; BRA rr
nt_asm_bvc:     .nt_asm $50, "bvc"      ; BVC rr
nt_asm_bvs:     .nt_asm $70, "bvs"      ; BVS rr

nt_asm_bit:     .nt_asm $2c, "bit"      ; BIT llhh
nt_asm_bit_h:   .nt_asm $89, "bit.#"    ; BIT #dd
nt_asm_bit_x:   .nt_asm $3c, "bit.x"    ; BIT llhh,x
nt_asm_bit_z:   .nt_asm $24, "bit.z"    ; BIT zp
nt_asm_bit_zx:  .nt_asm $34, "bit.zx"   ; BIT zp,x

nt_asm_brk:     .nt_asm $00, "brk"      ; BRK

nt_asm_clc:     .nt_asm $18, "clc"      ; CLC
nt_asm_cld:     .nt_asm $d8, "cld"      ; CLD
nt_asm_cli:     .nt_asm $58, "cli"      ; CLI
nt_asm_clv:     .nt_asm $b8, "clv"      ; CLV

nt_asm_cmp:     .nt_asm $cd, "cmp"      ; CMP llhh
nt_asm_cmp_h:   .nt_asm $c9, "cmp.#"    ; CMP #dd
nt_asm_cmp_x:   .nt_asm $dd, "cmp.x"    ; CMP llhh,x
nt_asm_cmp_y:   .nt_asm $d9, "cmp.y"    ; CMP llhh,y
nt_asm_cmp_z:   .nt_asm $c5, "cmp.z"    ; CMP zp
nt_asm_cmp_zi:  .nt_asm $d2, "cmp.zi"   ; CMP (zp)
nt_asm_cmp_ziy: .nt_asm $d1, "cmp.ziy"  ; CMP (zp),y
nt_asm_cmp_zx:  .nt_asm $d5, "cmp.zx"   ; CMP zp,x
nt_asm_cmp_zxi: .nt_asm $c1, "cmp.zxi"  ; CMP (zp,x)

nt_asm_cpx:     .nt_asm $ec, "cpx"      ; CPX llhh
nt_asm_cpx_h:   .nt_asm $e0, "cpx.#"    ; CPX #dd
nt_asm_cpx_z:   .nt_asm $e4, "cpx.z"    ; CPX zp

nt_asm_cpy:     .nt_asm $cc, "cpy"      ; CPY llhh
nt_asm_cpy_h:   .nt_asm $c0, "cpy.#"    ; CPY #dd
nt_asm_cpy_z:   .nt_asm $c4, "cpy.z"    ; CPY zp

nt_asm_dec:     .nt_asm $ce, "dec"      ; DEC llhh
nt_asm_dec_a:   .nt_asm $3a, "dec.a"    ; DEC A
nt_asm_dec_x:   .nt_asm $de, "dec.x"    ; DEC llhh,x
nt_asm_dec_z:   .nt_asm $c6, "dec.z"    ; DEC zp
nt_asm_dec_zx:  .nt_asm $d6, "dec.zx"   ; DEC zp,x

nt_asm_dex:     .nt_asm $ca, "dex"      ; DEX
nt_asm_dey:     .nt_asm $88, "dey"      ; DEY

nt_asm_eor:     .nt_asm $4d, "eor"      ; EOR llhh
nt_asm_eor_h:   .nt_asm $49, "eor.#"    ; EOR #dd
nt_asm_eor_x:   .nt_asm $5d, "eor.x"    ; EOR llhh,x
nt_asm_eor_y:   .nt_asm $59, "eor.y"    ; EOR llhh,y
nt_asm_eor_z:   .nt_asm $45, "eor.z"    ; EOR zp
nt_asm_eor_zi:  .nt_asm $52, "eor.zi"   ; EOR (zp)
nt_asm_eor_ziy: .nt_asm $51, "eor.ziy"  ; EOR (zp),y
nt_asm_eor_zx:  .nt_asm $55, "eor.zx"   ; EOR zp,x
nt_asm_eor_zxi: .nt_asm $41, "eor.zxi"  ; EOR (zp,x)

nt_asm_inc:     .nt_asm $ee, "inc"      ; INC llhh
nt_asm_inc_a:   .nt_asm $1a, "inc.a"    ; INC A
nt_asm_inc_x:   .nt_asm $fe, "inc.x"    ; INC llhh,x
nt_asm_inc_z:   .nt_asm $e6, "inc.z"    ; INC zp
nt_asm_inc_zx:  .nt_asm $f6, "inc.zx"   ; INC zp,x

nt_asm_inx:     .nt_asm $e8, "inx"      ; INX
nt_asm_iny:     .nt_asm $c8, "iny"      ; INY

nt_asm_jmp:     .nt_asm $4c, "jmp"      ; JMP llhh - flags containing word as NN
nt_asm_jmp_i:   .nt_asm $6c, "jmp.i"    ; JMP (llhh)
nt_asm_jmp_xi:  .nt_asm $7c, "jmp.xi"   ; JMP (llhh,x)

nt_asm_jsr:     .nt_asm $20, "jsr"      ; JSR llhh

nt_asm_lda:     .nt_asm $ad, "lda"      ; LDA llhh
nt_asm_lda_h:   .nt_asm $a9, "lda.#"    ; LDA #dd
nt_asm_lda_x:   .nt_asm $bd, "lda.x"    ; LDA llhh,x
nt_asm_lda_y:   .nt_asm $b9, "lda.y"    ; LDA llhh,y
nt_asm_lda_z:   .nt_asm $a5, "lda.z"    ; LDA zp
nt_asm_lda_zi:  .nt_asm $b2, "lda.zi"   ; LDA (zp)
nt_asm_lda_ziy: .nt_asm $b1, "lda.ziy"  ; LDA (zp),y
nt_asm_lda_zx:  .nt_asm $b5, "lda.zx"   ; LDA zp,x
nt_asm_lda_zxi: .nt_asm $a1, "lda.zxi"  ; LDA (zp,x)

nt_asm_ldx:     .nt_asm $ae, "ldx"      ; LDX llhh
nt_asm_ldx_h:   .nt_asm $a2, "ldx.#"    ; LDX #dd
nt_asm_ldx_y:   .nt_asm $be, "ldx.y"    ; LDX llhh,x
nt_asm_ldx_z:   .nt_asm $a6, "ldx.z"    ; LDX zp
nt_asm_ldx_zy:  .nt_asm $b6, "ldx.zy"   ; LDX zp,y

nt_asm_ldy:     .nt_asm $ac, "ldy"      ; LDY llhh
nt_asm_ldy_h:   .nt_asm $a0, "ldy.#"    ; LDY #dd
nt_asm_ldy_x:   .nt_asm $bc, "ldy.x"    ; LDY llhh,x
nt_asm_ldy_z:   .nt_asm $a4, "ldy.z"    ; LDY zp
nt_asm_ldy_zx:  .nt_asm $b4, "ldy.zx"   ; LDY zp,x

nt_asm_lsr:     .nt_asm $4e, "lsr"      ; LSR llhh
nt_asm_lsr_a:   .nt_asm $4a, "lsr.a"    ; LSR A
nt_asm_lsr_x:   .nt_asm $5e, "lsr.x"    ; LSR llhh,x
nt_asm_lsr_z:   .nt_asm $46, "lsr.z"    ; LSR zp
nt_asm_lsr_zx:  .nt_asm $56, "lsr.zx"   ; LSR zp,x

nt_asm_nop:     .nt_asm $ea, "nop"      ; NOP

nt_asm_ora:     .nt_asm $0d, "ora"      ; ORA llhh
nt_asm_ora_h:   .nt_asm $09, "ora.#"    ; ORA #dd
nt_asm_ora_x:   .nt_asm $1d, "ora.x"    ; ORA llhh,x
nt_asm_ora_y:   .nt_asm $19, "ora.y"    ; ORA llhh,y
nt_asm_ora_z:   .nt_asm $05, "ora.z"    ; ORA zp
nt_asm_ora_zi:  .nt_asm $12, "ora.zi"   ; ORA (zp)
nt_asm_ora_ziy: .nt_asm $11, "ora.ziy"  ; ORA (zp),y
nt_asm_ora_zx:  .nt_asm $15, "ora.zx"   ; ORA zp,x
nt_asm_ora_zxi: .nt_asm $01, "ora.zxi"  ; ORA (zp,x)

nt_asm_pha:     .nt_asm $48, "pha"      ; PHA
nt_asm_php:     .nt_asm $08, "php"      ; PHP
nt_asm_phx:     .nt_asm $da, "phx"      ; PHX
nt_asm_phy:     .nt_asm $5a, "phy"      ; PHY

nt_asm_pla:     .nt_asm $68, "pla"      ; PLA
nt_asm_plp:     .nt_asm $28, "plp"      ; PLP
nt_asm_plx:     .nt_asm $fa, "plx"      ; PLX
nt_asm_ply:     .nt_asm $7a, "ply"      ; PLY

nt_asm_rol:     .nt_asm $2e, "rol"      ; ROL llhh
nt_asm_rol_a:   .nt_asm $2a, "rol.a"    ; ROL A
nt_asm_rol_x:   .nt_asm $3e, "rol.x"    ; ROL llhh,x
nt_asm_rol_z:   .nt_asm $26, "rol.z"    ; ROL zp
nt_asm_rol_zx:  .nt_asm $36, "rol.zx"   ; ROL zp,x

nt_asm_ror:     .nt_asm $6e, "ror"      ; ROR llhh
nt_asm_ror_a:   .nt_asm $6a, "ror.a"    ; ROR A
nt_asm_ror_x:   .nt_asm $7e, "ror.x"    ; ROR llhh,x
nt_asm_ror_z:   .nt_asm $66, "ror.z"    ; ROR zp
nt_asm_ror_zx:  .nt_asm $76, "ror.zx"   ; ROR zp,x

nt_asm_rti:     .nt_asm $40, "rti"      ; RTI
nt_asm_rts:     .nt_asm $60, "rts"      ; RTS

nt_asm_sbc:     .nt_asm $ed, "sbc"      ; SBC llhh
nt_asm_sbc_h:   .nt_asm $e9, "sbc.#"    ; SBC #dd
nt_asm_sbc_x:   .nt_asm $fd, "sbc.x"    ; SBC llhh,x
nt_asm_sbc_y:   .nt_asm $f9, "sbc.y"    ; SBC llhh,y
nt_asm_sbc_z:   .nt_asm $e5, "sbc.z"    ; SBC zp
nt_asm_sbc_zi:  .nt_asm $f2, "sbc.zi"   ; SBC (zp)
nt_asm_sbc_ziy: .nt_asm $f1, "sbc.ziy"  ; SBC (zp),y
nt_asm_sbc_zx:  .nt_asm $f5, "sbc.zx"   ; SBC zp,x
nt_asm_sbc_zxi: .nt_asm $e1, "sbc.zxi"  ; SBC (zp,x)

nt_asm_sec:     .nt_asm $38, "sec"      ; SEC
nt_asm_sed:     .nt_asm $f8, "sed"      ; SED
nt_asm_sei:     .nt_asm $78, "sei"      ; SEI

nt_asm_sta:     .nt_asm $8d, "sta"      ; STA llhh
nt_asm_sta_x:   .nt_asm $9d, "sta.x"    ; STA llhh,x
nt_asm_sta_y:   .nt_asm $99, "sta.y"    ; STA llhh,y
nt_asm_sta_z:   .nt_asm $85, "sta.z"    ; STA zp
nt_asm_sta_zi:  .nt_asm $92, "sta.zi"   ; STA (zp)
nt_asm_sta_ziy: .nt_asm $91, "sta.ziy"  ; STA (zp),y
nt_asm_sta_zx:  .nt_asm $95, "sta.zx"   ; STA zp,x
nt_asm_sta_zxi: .nt_asm $81, "sta.zxi"  ; STA (zp,x)

nt_asm_stx:     .nt_asm $8e, "stx"      ; STX llhh
nt_asm_stx_z:   .nt_asm $86, "stx.z"    ; STX zp
nt_asm_stx_zy:  .nt_asm $96, "stx.zy"   ; STX zp,y

nt_asm_sty:     .nt_asm $8c, "sty"      ; STY llhh
nt_asm_sty_z:   .nt_asm $84, "sty.z"    ; STY zp
nt_asm_sty_zx:  .nt_asm $94, "sty.zx"   ; STY zp,x

nt_asm_stz:     .nt_asm $9c, "stz"      ; STZ llhh
nt_asm_stz_x:   .nt_asm $9e, "stz.x"    ; STZ llhh,x
nt_asm_stz_z:   .nt_asm $64, "stz.z"    ; STZ zp
nt_asm_stz_zx:  .nt_asm $74, "stz.zx"   ; STZ zp,x

nt_asm_tax:     .nt_asm $aa, "tax"      ; TAX
nt_asm_tay:     .nt_asm $a8, "tay"      ; TAY

nt_asm_trb:     .nt_asm $1c, "trb"      ; TRB llhh
nt_asm_trb_z:   .nt_asm $14, "trb.z"    ; TRB zp

nt_asm_tsb:     .nt_asm $0c, "tsb"      ; TSB llhh
nt_asm_tsb_z:   .nt_asm $04, "tsb.z"    ; TSB zp

nt_asm_tsx:     .nt_asm $ba, "tsx"      ; TSX
nt_asm_txa:     .nt_asm $8a, "txa"      ; TAX
nt_asm_txs:     .nt_asm $9a, "txs"      ; TXS

nt_asm_tya:     .nt_asm $98, "tya"      ; TYA

nt_asm_end = prev_nt                    ; mark the end of the linked list of asm opcodes

; END of ASSEMBLER-WORDLIST
.endif

assembler_dictionary_start = prev_nt
prev_nt := 0

; END
