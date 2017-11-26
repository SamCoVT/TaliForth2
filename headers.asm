; Dictionary Headers for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 05. Dec 2016 (Liara Forth)
; This version: 24. Nov 2017

; Dictionary headers are kept separately from the code, which allows various
; tricks in the code. We roughly follow the Gforth terminology: The Execution
; Token (xt) is the address of the first byte of a word's code that can be,
; uh, executed; the Name Token (nt) is a pointer to the beginning of the
; word's header in the Dictionary. There, the link to the next word in the
; Dictionary is always one cell down from the current word's own nt. In the 
; code ; itself, we use "nt_<WORD>" for the nt and "xt_<WORD>" for the xt. 
;
; This gives us the following header structure:
;
;              8 bit     8 bit
;               LSB       MSB
; nt_word ->  +--------+--------+
;             | Length | Status |
;          +2 +--------+--------+
;             | Next Header     | -> nt_next_word
;          +4 +-----------------+
;             | Start of Code   | -> xt_word 
;          +6 +-----------------+
;             | End of Code     | -> z_word
;          +8 +--------+--------+
;             | Name   |        |
;             +--------+--------+
;             |        |        |
;             +--------+--------+
;             |        |  ...   | (name string does not end with a zero)
;          +n +--------+--------+
;
; The Status Byte is created by adding the flags defined in
; definitions.asm, which are:
;
;       CO - Compile Only
;       IM - Immediate Word
;       NN - Never Native Compile 
;       AN - Always Native Compile (may not be called by JSR)

; Note there are currently four bits unused. By default, all words can be
; natively compiled (compiled inline) or as a subroutine jump target; the system
; decides which variant to use based on a threshold the user can set. The NN
; flag forbids native compiling, the AN flag forces it.  

; The last word (top word in code) is always BYE. It is marked as the last word
; by its value of 0000 in its Next Header field. The words are sorted with the
; more common ones first (further down in code) so they are found earlier.
; Anything to do with output comes later (further up) because things will
; always be slow if there is a human involved.

; The initial skeleton of this list was automatically generated by a script
; in the tools folder and then sorted by hand. To make sure that didn't
; introduce errors, there is also a validation tool.
;
nt_store:
        .byte 1, 0
        .word 0000, xt_store, z_store
        .byte "!"

nt_tick:
        .byte 1, 0
        .word 0000, xt_tick, z_tick
        .byte "'"

nt_paren:
        .byte 1, 0
        .word 0000, xt_paren, z_paren
        .byte "("

nt_paren_plus_loop:
        .byte 7, 0
        .word 0000, xt_paren_plus_loop, z_paren_plus_loop
        .byte "(+loop)"

nt_paren_q_do:
        .byte 5, 0
        .word 0000, xt_paren_q_do, z_paren_q_do
        .byte "(?do)"

nt_paren_do:
        .byte 4, 0
        .word 0000, xt_paren_do, z_paren_do
        .byte "(do)"

nt_right_paren:
        .byte 1, 0
        .word 0000, xt_right_paren, z_right_paren
        .byte ")"

nt_star:
        .byte 1, 0
        .word 0000, xt_star, z_star
        .byte "*"

nt_star_slash:
        .byte 2, 0
        .word 0000, xt_star_slash, z_star_slash
        .byte "*/"

nt_star_slash_mod:
        .byte 5, 0
        .word 0000, xt_star_slash_mod, z_star_slash_mod
        .byte "*/mod"

nt_plus:
        .byte 1, 0
        .word 0000, xt_plus, z_plus
        .byte "+"

nt_plus_store:
        .byte 2, 0
        .word 0000, xt_plus_store, z_plus_store
        .byte "+!"

nt_plus_loop:
        .byte 5, 0
        .word 0000, xt_plus_loop, z_plus_loop
        .byte "+loop"

nt_comma:
        .byte 1, 0
        .word 0000, xt_comma, z_comma
        .byte ","

nt_minus:
        .byte 1, 0
        .word 0000, xt_minus, z_minus
        .byte "-"

nt_not_rote:
        .byte 4, 0
        .word 0000, xt_not_rote, z_not_rote
        .byte "-rot"

nt_minus_trailing:
        .byte 9, 0
        .word 0000, xt_minus_trailing, z_minus_trailing
        .byte "-trailing"

nt_dot:
        .byte 1, 0
        .word 0000, xt_dot, z_dot
        .byte "."

nt_dot_quote:
        .byte 2, 0
        .word 0000, xt_dot_quote, z_dot_quote
        .byte ".", $22

nt_dot_paren:
        .byte 2, 0
        .word 0000, xt_dot_paren, z_dot_paren
        .byte ".("

nt_dot_byte:
        .byte 5, 0
        .word 0000, xt_dot_byte, z_dot_byte
        .byte ".byte"

nt_dot_r:
        .byte 2, 0
        .word 0000, xt_dot_r, z_dot_r
        .byte ".r"

nt_dot_s:
        .byte 2, 0
        .word 0000, xt_dot_s, z_dot_s
        .byte ".s"

nt_slash:
        .byte 1, 0
        .word 0000, xt_slash, z_slash
        .byte "/"

nt_slash_mod:
        .byte 4, 0
        .word 0000, xt_slash_mod, z_slash_mod
        .byte "/mod"

nt_slash_string:
        .byte 7, 0
        .word 0000, xt_slash_string, z_slash_string
        .byte "/string"

nt_zero:
        .byte 1, 0
        .word 0000, xt_zero, z_zero
        .byte "0"

nt_zero_less:
        .byte 2, 0
        .word 0000, xt_zero_less, z_zero_less
        .byte "0<"

nt_zero_unequal:
        .byte 3, 0
        .word 0000, xt_zero_unequal, z_zero_unequal
        .byte "0<>"

nt_zero_equal:
        .byte 2, 0
        .word 0000, xt_zero_equal, z_zero_equal
        .byte "0="

nt_zero_greater:
        .byte 2, 0
        .word 0000, xt_zero_greater, z_zero_greater
        .byte "0>"

nt_zero_branch:
        .byte 7, 0
        .word 0000, xt_zero_branch, z_zero_branch
        .byte "0branch"

nt_one:
        .byte 1, 0
        .word 0000, xt_one, z_one
        .byte "1"

nt_one_plus:
        .byte 2, 0
        .word 0000, xt_one_plus, z_one_plus
        .byte "1+"

nt_one_minus:
        .byte 2, 0
        .word 0000, xt_one_minus, z_one_minus
        .byte "1-"

nt_two:
        .byte 1, 0
        .word 0000, xt_two, z_two
        .byte "2"

nt_two_star:
        .byte 2, 0
        .word 0000, xt_two_star, z_two_star
        .byte "2*"

nt_two_to_r:
        .byte 3, 0
        .word 0000, xt_two_to_r, z_two_to_r
        .byte "2>r"

nt_two_drop:
        .byte 5, 0
        .word 0000, xt_two_drop, z_two_drop
        .byte "2drop"

nt_two_dup:
        .byte 4, 0
        .word 0000, xt_two_dup, z_two_dup
        .byte "2dup"

nt_two_over:
        .byte 5, 0
        .word 0000, xt_two_over, z_two_over
        .byte "2over"

nt_two_r_from:
        .byte 3, 0
        .word 0000, xt_two_r_from, z_two_r_from
        .byte "2r>"

nt_two_r_fetch:
        .byte 3, 0
        .word 0000, xt_two_r_fetch, z_two_r_fetch
        .byte "2r@"

nt_two_swap:
        .byte 5, 0
        .word 0000, xt_two_swap, z_two_swap
        .byte "2swap"

nt_two_variable:
        .byte 9, 0
        .word 0000, xt_two_variable, z_two_variable
        .byte "2variable"

nt_colon:
        .byte 1, 0
        .word 0000, xt_colon, z_colon
        .byte ":"

nt_semicolon:
        .byte 1, 0
        .word 0000, xt_semicolon, z_semicolon
        .byte ";"

nt_less_than:
        .byte 1, 0
        .word 0000, xt_less_than, z_less_than
        .byte "<"

nt_less_number_sign:
        .byte 2, 0
        .word 0000, xt_less_number_sign, z_less_number_sign
        .byte "<#"

nt_not_equals:
        .byte 2, 0
        .word 0000, xt_not_equals, z_not_equals
        .byte "<>"

nt_equals:
        .byte 1, 0
        .word 0000, xt_equals, z_equals
        .byte "="

nt_greater_than:
        .byte 1, 0
        .word 0000, xt_greater_than, z_greater_than
        .byte ">"

nt_to_body:
        .byte 5, 0
        .word 0000, xt_to_body, z_to_body
        .byte ">body"

nt_to_in:
        .byte 3, 0
        .word 0000, xt_to_in, z_to_in
        .byte ">in"

nt_to_number:
        .byte 7, 0
        .word 0000, xt_to_number, z_to_number
        .byte ">number"

nt_to_r:
        .byte 2, 0
        .word 0000, xt_to_r, z_to_r
        .byte ">r"

nt_question:
        .byte 1, 0
        .word 0000, xt_question, z_question
        .byte "?"

nt_question_do:
        .byte 3, 0
        .word 0000, xt_question_do, z_question_do
        .byte "?do"

nt_question_dup:
        .byte 4, 0
        .word 0000, xt_question_dup, z_question_dup
        .byte "?dup"

nt_fetch:
        .byte 1, 0
        .word 0000, xt_fetch, z_fetch
        .byte "@"

nt_left_bracket:
        .byte 1, 0
        .word 0000, xt_left_bracket, z_left_bracket
        .byte "["

nt_bracket_tick:
        .byte 3, 0
        .word 0000, xt_bracket_tick, z_bracket_tick
        .byte "[']"

nt_bracket_char:
        .byte 6, 0
        .word 0000, xt_bracket_char, z_bracket_char
        .byte "[char]"

nt_backslash:
        .byte 1, 0
        .word 0000, xt_backslash, z_backslash
        .byte $5c

nt_right_bracket:
        .byte 1, 0
        .word 0000, xt_right_bracket, z_right_bracket
        .byte "]"

nt_abort:
        .byte 5, 0
        .word 0000, xt_abort, z_abort
        .byte "abort"

nt_abort_quote:
        .byte 6, 0
        .word 0000, xt_abort_quote, z_abort_quote
        .byte "abort", $22

nt_abs:
        .byte 3, 0
        .word 0000, xt_abs, z_abs
        .byte "abs"

nt_accept:
        .byte 6, 0
        .word 0000, xt_accept, z_accept
        .byte "accept"

nt_again:
        .byte 5, 0
        .word 0000, xt_again, z_again
        .byte "again"

nt_align:
        .byte 5, 0
        .word 0000, xt_align, z_align
        .byte "align"

nt_aligned:
        .byte 7, 0
        .word 0000, xt_aligned, z_aligned
        .byte "aligned"

nt_allot:
        .byte 5, 0
        .word 0000, xt_allot, z_allot
        .byte "allot"

nt_and:
        .byte 3, 0
        .word 0000, xt_and, z_and
        .byte "and"

nt_at_xy:
        .byte 5, 0
        .word 0000, xt_at_xy, z_at_xy
        .byte "at-xy"

nt_base:
        .byte 4, 0
        .word 0000, xt_base, z_base
        .byte "base"

nt_begin:
        .byte 5, 0
        .word 0000, xt_begin, z_begin
        .byte "begin"

nt_bell:
        .byte 4, 0
        .word 0000, xt_bell, z_bell
        .byte "bell"

nt_bl:
        .byte 2, 0
        .word 0000, xt_bl, z_bl
        .byte "bl"

nt_bounds:
        .byte 6, 0
        .word 0000, xt_bounds, z_bounds
        .byte "bounds"

nt_branch:
        .byte 6, 0
        .word 0000, xt_branch, z_branch
        .byte "branch"

nt_bye:
        .byte 3, 0
        .word 0000, xt_bye, z_bye
        .byte "bye"

nt_c_store:
        .byte 2, 0
        .word 0000, xt_c_store, z_c_store
        .byte "c!"

nt_c_comma:
        .byte 2, 0
        .word 0000, xt_c_comma, z_c_comma
        .byte "c,"

nt_c_fetch:
        .byte 2, 0
        .word 0000, xt_c_fetch, z_c_fetch
        .byte "c@"

nt_cell_plus:
        .byte 5, 0
        .word 0000, xt_cell_plus, z_cell_plus
        .byte "cell+"

nt_cells:
        .byte 5, 0
        .word 0000, xt_cells, z_cells
        .byte "cells"

nt_char:
        .byte 4, 0
        .word 0000, xt_char, z_char
        .byte "char"

nt_char_plus:
        .byte 5, 0
        .word 0000, xt_char_plus, z_char_plus
        .byte "char+"

nt_chars:
        .byte 5, 0
        .word 0000, xt_chars, z_chars
        .byte "chars"

nt_cmove:
        .byte 5, 0
        .word 0000, xt_cmove, z_cmove
        .byte "cmove"

nt_cmove_up:
        .byte 6, 0
        .word 0000, xt_cmove_up, z_cmove_up
        .byte "cmove>"

nt_cold:
        .byte 4, 0
        .word 0000, xt_cold, z_cold
        .byte "cold"

nt_compile_comma:
        .byte 8, 0
        .word 0000, xt_compile_comma, z_compile_comma
        .byte "compile,"

nt_compile_only:
        .byte 12, 0
        .word 0000, xt_compile_only, z_compile_only
        .byte "compile-only"

nt_constant:
        .byte 8, 0
        .word 0000, xt_constant, z_constant
        .byte "constant"

nt_count:
        .byte 5, 0
        .word 0000, xt_count, z_count
        .byte "count"

nt_cr:
        .byte 2, 0
        .word 0000, xt_cr, z_cr
        .byte "cr"

nt_create:
        .byte 6, 0
        .word 0000, xt_create, z_create
        .byte "create"

nt_d_plus:
        .byte 2, 0
        .word 0000, xt_d_plus, z_d_plus
        .byte "d+"

nt_d_minus:
        .byte 2, 0
        .word 0000, xt_d_minus, z_d_minus
        .byte "d-"

nt_d_dot:
        .byte 2, 0
        .word 0000, xt_d_dot, z_d_dot
        .byte "d."

nt_d_r:
        .byte 3, 0
        .word 0000, xt_d_r, z_d_r
        .byte "d.r"

nt_d_to_s:
        .byte 3, 0
        .word 0000, xt_d_to_s, z_d_to_s
        .byte "d>s"

nt_dabs:
        .byte 4, 0
        .word 0000, xt_dabs, z_dabs
        .byte "dabs"

nt_decimal:
        .byte 7, 0
        .word 0000, xt_decimal, z_decimal
        .byte "decimal"

nt_defer:
        .byte 5, 0
        .word 0000, xt_defer, z_defer
        .byte "defer"

nt_depth:
        .byte 5, 0
        .word 0000, xt_depth, z_depth
        .byte "depth"

nt_digit_question:
        .byte 6, 0
        .word 0000, xt_digit_question, z_digit_question
        .byte "digit?"

nt_dnegate:
        .byte 7, 0
        .word 0000, xt_dnegate, z_dnegate
        .byte "dnegate"

nt_do:
        .byte 2, 0
        .word 0000, xt_do, z_do
        .byte "do"

nt_does:
        .byte 5, 0
        .word 0000, xt_does, z_does
        .byte "does>"

nt_drop:
        .byte 4, 0
        .word 0000, xt_drop, z_drop
        .byte "drop"

nt_dump:
        .byte 4, 0
        .word 0000, xt_dump, z_dump
        .byte "dump"

nt_dup:
        .byte 3, 0
        .word 0000, xt_dup, z_dup
        .byte "dup"

nt_else:
        .byte 4, 0
        .word 0000, xt_else, z_else
        .byte "else"

nt_emit:
        .byte 4, 0
        .word 0000, xt_emit, z_emit
        .byte "emit"

nt_erase:
        .byte 5, 0
        .word 0000, xt_erase, z_erase
        .byte "erase"

nt_evaluate:
        .byte 8, 0
        .word 0000, xt_evaluate, z_evaluate
        .byte "evaluate"

nt_execute:
        .byte 7, 0
        .word 0000, xt_execute, z_execute
        .byte "execute"

nt_exit:
        .byte 4, 0
        .word 0000, xt_exit, z_exit
        .byte "exit"

nt_false:
        .byte 5, 0
        .word 0000, xt_false, z_false
        .byte "false"

nt_fill:
        .byte 4, 0
        .word 0000, xt_fill, z_fill
        .byte "fill"

nt_find:
        .byte 4, 0
        .word 0000, xt_find, z_find
        .byte "find"

nt_find_name:
        .byte 9, 0
        .word 0000, xt_find_name, z_find_name
        .byte "find-name"

nt_fm_slash_mod:
        .byte 6, 0
        .word 0000, xt_fm_slash_mod, z_fm_slash_mod
        .byte "fm/mod"

nt_here:
        .byte 4, 0
        .word 0000, xt_here, z_here
        .byte "here"

nt_hex:
        .byte 3, 0
        .word 0000, xt_hex, z_hex
        .byte "hex"

nt_hold:
        .byte 4, 0
        .word 0000, xt_hold, z_hold
        .byte "hold"

nt_i:
        .byte 1, 0
        .word 0000, xt_i, z_i
        .byte "i"

nt_if:
        .byte 2, 0
        .word 0000, xt_if, z_if
        .byte "if"

nt_immediate:
        .byte 9, 0
        .word 0000, xt_immediate, z_immediate
        .byte "immediate"

nt_input:
        .byte 5, 0
        .word 0000, xt_input, z_input
        .byte "input"

nt_int_to_name:
        .byte 8, 0
        .word 0000, xt_int_to_name, z_int_to_name
        .byte "int>name"

nt_invert:
        .byte 6, 0
        .word 0000, xt_invert, z_invert
        .byte "invert"

nt_j:
        .byte 1, 0
        .word 0000, xt_j, z_j
        .byte "j"

nt_key:
        .byte 3, 0
        .word 0000, xt_key, z_key
        .byte "key"

nt_key_question:
        .byte 4, 0
        .word 0000, xt_key_question, z_key_question
        .byte "key?"

nt_latestnt:
        .byte 8, 0
        .word 0000, xt_latestnt, z_latestnt
        .byte "latestnt"

nt_latestxt:
        .byte 8, 0
        .word 0000, xt_latestxt, z_latestxt
        .byte "latestxt"

nt_leave:
        .byte 5, 0
        .word 0000, xt_leave, z_leave
        .byte "leave"

nt_literal:
        .byte 7, 0
        .word 0000, xt_literal, z_literal
        .byte "literal"

nt_loop:
        .byte 4, 0
        .word 0000, xt_loop, z_loop
        .byte "loop"

nt_lshift:
        .byte 6, 0
        .word 0000, xt_lshift, z_lshift
        .byte "lshift"

nt_m_star:
        .byte 2, 0
        .word 0000, xt_m_star, z_m_star
        .byte "m*"

nt_marker:
        .byte 6, 0
        .word 0000, xt_marker, z_marker
        .byte "marker"

nt_max:
        .byte 3, 0
        .word 0000, xt_max, z_max
        .byte "max"

nt_min:
        .byte 3, 0
        .word 0000, xt_min, z_min
        .byte "min"

nt_mod:
        .byte 3, 0
        .word 0000, xt_mod, z_mod
        .byte "mod"

nt_move:
        .byte 4, 0
        .word 0000, xt_move, z_move
        .byte "move"

nt_name_to_int:
        .byte 8, 0
        .word 0000, xt_name_to_int, z_name_to_int
        .byte "name>int"

nt_name_to_string:
        .byte 11, 0
        .word 0000, xt_name_to_string, z_name_to_string
        .byte "name>string"

nt_nc_limit:
        .byte 8, 0
        .word 0000, xt_nc_limit, z_nc_limit
        .byte "nc-limit"

nt_negate:
        .byte 6, 0
        .word 0000, xt_negate, z_negate
        .byte "negate"

nt_never_compile:
        .byte 13, 0
        .word 0000, xt_never_compile, z_never_compile
        .byte "never-compile"

nt_nip:
        .byte 3, 0
        .word 0000, xt_nip, z_nip
        .byte "nip"

nt_number:
        .byte 6, 0
        .word 0000, xt_number, z_number
        .byte "number"

nt_or:
        .byte 2, 0
        .word 0000, xt_or, z_or
        .byte "or"

nt_output:
        .byte 6, 0
        .word 0000, xt_output, z_output
        .byte "output"

nt_over:
        .byte 4, 0
        .word 0000, xt_over, z_over
        .byte "over"

nt_pad:
        .byte 3, 0
        .word 0000, xt_pad, z_pad
        .byte "pad"

nt_page:
        .byte 4, 0
        .word 0000, xt_page, z_page
        .byte "page"

nt_parse:
        .byte 5, 0
        .word 0000, xt_parse, z_parse
        .byte "parse"

nt_parse_name:
        .byte 10, 0
        .word 0000, xt_parse_name, z_parse_name
        .byte "parse-name"

nt_pick:
        .byte 4, 0
        .word 0000, xt_pick, z_pick
        .byte "pick"

nt_postpone:
        .byte 8, 0
        .word 0000, xt_postpone, z_postpone
        .byte "postpone"

nt_quit:
        .byte 4, 0
        .word 0000, xt_quit, z_quit
        .byte "quit"

nt_r_from:
        .byte 2, 0
        .word 0000, xt_r_from, z_r_from
        .byte "r>"

nt_r_fetch:
        .byte 2, 0
        .word 0000, xt_r_fetch, z_r_fetch
        .byte "r@"

nt_recurse:
        .byte 7, 0
        .word 0000, xt_recurse, z_recurse
        .byte "recurse"

nt_refill:
        .byte 6, 0
        .word 0000, xt_refill, z_refill
        .byte "refill"

nt_repeat:
        .byte 6, 0
        .word 0000, xt_repeat, z_repeat
        .byte "repeat"

nt_rote:
        .byte 3, 0
        .word 0000, xt_rote, z_rote
        .byte "rot"

nt_rshift:
        .byte 6, 0
        .word 0000, xt_rshift, z_rshift
        .byte "rshift"

nt_s_quote:
        .byte 2, 0
        .word 0000, xt_s_quote, z_s_quote
        .byte "s", $22

nt_s_to_d:
        .byte 3, 0
        .word 0000, xt_s_to_d, z_s_to_d
        .byte "s>d"

nt_sign:
        .byte 4, 0
        .word 0000, xt_sign, z_sign
        .byte "sign"

nt_sliteral:
        .byte 8, 0
        .word 0000, xt_sliteral, z_sliteral
        .byte "sliteral"

nt_sm_slash_rem:
        .byte 6, 0
        .word 0000, xt_sm_slash_rem, z_sm_slash_rem
        .byte "sm/rem"

nt_source:
        .byte 6, 0
        .word 0000, xt_source, z_source
        .byte "source"

nt_source_id:
        .byte 9, 0
        .word 0000, xt_source_id, z_source_id
        .byte "source-id"

nt_space:
        .byte 5, 0
        .word 0000, xt_space, z_space
        .byte "space"

nt_spaces:
        .byte 6, 0
        .word 0000, xt_spaces, z_spaces
        .byte "spaces"

nt_state:
        .byte 5, 0
        .word 0000, xt_state, z_state
        .byte "state"

nt_swap:
        .byte 4, 0
        .word 0000, xt_swap, z_swap
        .byte "swap"

nt_then:
        .byte 4, 0
        .word 0000, xt_then, z_then
        .byte "then"

nt_to:
        .byte 2, 0
        .word 0000, xt_to, z_to
        .byte "to"

nt_true:
        .byte 4, 0
        .word 0000, xt_true, z_true
        .byte "true"

nt_tuck:
        .byte 4, 0
        .word 0000, xt_tuck, z_tuck
        .byte "tuck"

nt_type:
        .byte 4, 0
        .word 0000, xt_type, z_type
        .byte "type"

nt_u_dot:
        .byte 2, 0
        .word 0000, xt_u_dot, z_u_dot
        .byte "u."

nt_u_dot_r:
        .byte 3, 0
        .word 0000, xt_u_dot_r, z_u_dot_r
        .byte "u.r"

nt_ud_dot:
        .byte 3, 0
        .word 0000, xt_ud_dot, z_ud_dot
        .byte "ud."

nt_ud_dot_r:
        .byte 4, 0
        .word 0000, xt_ud_dot_r, z_ud_dot_r
        .byte "ud.r"

nt_ud_slash_mod:
        .byte 6, 0
        .word 0000, xt_ud_slash_mod, z_ud_slash_mod
        .byte "ud/mod"

nt_um_star:
        .byte 3, 0
        .word 0000, xt_um_star, z_um_star
        .byte "um*"

nt_um_slash_mod:
        .byte 6, 0
        .word 0000, xt_um_slash_mod, z_um_slash_mod
        .byte "um/mod"

nt_unloop:
        .byte 6, 0
        .word 0000, xt_unloop, z_unloop
        .byte "unloop"

nt_unused:
        .byte 6, 0
        .word 0000, xt_unused, z_unused
        .byte "unused"

nt_value:
        .byte 5, 0
        .word 0000, xt_value, z_value
        .byte "value"

nt_variable:
        .byte 8, 0
        .word 0000, xt_variable, z_variable
        .byte "variable"

nt_within:
        .byte 6, 0
        .word 0000, xt_within, z_within
        .byte "within"

nt_word:
        .byte 4, 0
        .word 0000, xt_word, z_word
        .byte "word"

nt_words:
        .byte 5, 0
        .word 0000, xt_words, z_words
        .byte "words"

nt_words_and_sizes:
        .byte 11, 0
        .word 0000, xt_words_and_sizes, z_words_and_sizes
        .byte "words&sizes"

nt_wordsize:
        .byte 8, 0
        .word 0000, xt_wordsize, z_wordsize
        .byte "wordsize"

nt_xor:
        .byte 3, 0
        .word 0000, xt_xor, z_xor
        .byte "xor"

