\ ------------------------------------------------------------------------
testing tools words: .s ? dump name>string see state words

\ Test for BYE not implemented

\ TYPE tests
T{ s" five by five" 2dup capture-output type restore-output compare -> 0 }T

T{
  \ longer than 256 chars
  s" Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  2dup capture-output type restore-output compare -> 0
}T

T{
  \ exactly 256 chars
  s" Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in"
  dup 256 <> -rot 2dup capture-output type restore-output compare or -> 0
}T

\ .S tests
T{ capture-output .s restore-output s" <0> " compare -> 0 }T
T{ 1 2 3 capture-output .s restore-output s" <3> 1 2 3 " compare -> 1 2 3 0 }T
T{ hex $12345678. capture-output .s restore-output s" <2> 5678 1234 " compare decimal -> $12345678. 0 }T

\ ? tests
: clinging-to-life ;    \ ensure following variable has a near pointer
variable life  42 life !
T{ capture-output life ? restore-output s" 42 " compare -> 0 }T

\ WORDS should print a bunch of output including a common sequence
T{ capture-output words restore-output nip 1024 2048 within -> true }T
T{ capture-output words restore-output s" drop dup swap" search -rot 2drop -> true }T

\ DUMP tests

:noname blkbuffer 32 bounds do i i c! loop ; execute
T{ blkbuffer 0 capture-output dump restore-output s\" \n" compare -> 0 }T
T{ blkbuffer 3 capture-output dump restore-output s\" \n
0400  00 01 02                                          ...\n"
compare -> 0 }T
T{ blkbuffer 11 capture-output dump restore-output s\" \n
0400  00 01 02 03 04 05 06 07  08 09 0A                 ........ ...\n"
compare -> 0 }T
T{ blkbuffer 32 capture-output dump restore-output s\" \n
0400  00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ........ ........\n
0410  10 11 12 13 14 15 16 17  18 19 1A 1B 1C 1D 1E 1F  ........ ........\n
" compare -> 0 }T



\ Split a string containing the glob characters * and ?
\ One * or multiple ? at the start of the string are discarded
\ and flagged, with the remainder of the string split at the next
\ glob character, leaving an initial glob-free prefix and the remainder

: cleave-glob ( addr n -- addr1 n1 addr2 n2 f )
  \ f = 0 if string doesn't start with * or ?
  \   = -1 if string starts with *
  \   = n if string starts with n ?s
  \ addr2 n2 is the prefix discarding initial glob chars, splitting before the next glob char
  \ addr1 n1 is the remaining string

  \ empty string?
  dup 0= if
    2dup 0 exit
  then
  0 >r
  over c@ case
    [char] * of 1 /string r> 1- >r endof
    \ TODO bug if string ends with ?
    [char] ? of begin r> 1+ >r 1 /string ( ?0 ) over c@ [char] ? = while repeat endof
  endcase
  ( addr n ) ( R: f )
  2dup 0 -rot bounds ?do
    i c@ dup [char] ? = swap [char] * = or if
      leave
    then
    1+
  loop
  ( addr n k ) ( R: f )
  swap over - ( addr k n-k )
  >r 2dup + r> ( addr k addr+k n-k )
  2swap r>
  ( addr+k n-k addr k f )
;

: starts-with ( 2tgt 2pfx -- 2tgt true|false )
  \ is target too short?
  2 pick over < if
    2drop false exit
  then
  2over drop over ( 2tgt 2pfx 2tgt[:#pfx] )
  compare 0=
;


\ compare a target string with a simplistic glob pattern to aid unit testsing
\ the glob character ? matches exactly one character in the target string
\ the glob character * is the minimal match of 0+ characters before the next pattern character
\ for example "banana" matches "b??ana" and "banan?" and "*nana" but not "b*na" (* is always minimal)
\ as a special case, * will also match the remainder of a string, so "b*" will also match

: compare-glob ( 2tgt 2pat -- 0 | <>0 )
  \ stash the target and get the next chunk of pattern to match
  2swap 2>r cleave-glob ( 2pat 2pfx f ) ( R: 2tgt )

  dup 0> if
    \ f > 0 means pattern started with ?s to skip
    2r> rot /string dup 0< if
      \ string ran out ?
      2drop 2drop 2drop -1 exit
    then
    \ remainder of pattern needs to match start of target
    0 >r
    ( 2pat 2pfx 2tgt[k:] ) ( R: 0 )
  else
    2r> rot >r
    ( 2pat 2pfx 2tgt ) ( R: 0|-1 )
  then

  \ fetch the flag and stash the prefix length
  2swap r> over >r
  ( 2pat 2tgt 2pfx 0|-1 ) ( R: #pfx )
  if
    \ flag -1 means '*' so we can match anywhere
    \ special case: trailing '*' matches rest of string
    4 pick over or 0= if  \ #pfx and #pat both zero?
      \ drop 2pfx and replace #pfx with #tgt on RS, returning true
      2drop r> drop dup >r true
    else
      search
    then
  else
    \ flag 0 means we must match at the start
    starts-with
  then

  ( 2pat 2match ?matched ) ( R: #pfx )
  r> swap
  0= if
    \ failed
    drop 2drop 2drop -2 exit
  then

  \ drop the length of the matched prefix from the remaining target
  /string 2swap
  ( 2tgt' 2pat' )

  \ if pattern is empty, we're done one way or another
  ?dup 0= if
    \ just return tgt length, so 0 means success
    drop nip exit
  then

  \ otherwise keep going with the reduced target and pattern
  recurse
;


: see-/mod-output s\" \n
nt: *  xt: *  header: 04 04 ?? ?? ?? 1E \n
flags: HC 0 NN 0 AN 0 IM 0 CO 0 DC 1 LC 0 FP 0 | UF 1 ST 0 \n
size (decimal): 30 \n
\n
*  20 ?? ?? A9 FF 48 B5 00  95 FE B5 01 95 FF E8 E8   ??..H.. ........
*  20 ?? ?? CA CA 20 ?? ??  68 D0 03 20 ?? ??         ??.. ?? h.. ??
\n
*  ????? jsr     2 STACK DEPTH CHECK\n
*     FF lda.#\n
*        pha\n
*      0 lda.zx\n
*     FE sta.zx\n
*      1 lda.zx\n
*     FF sta.zx\n
*        inx\n
*        inx\n
*   ???? jsr     s>d\n
*        dex\n
*        dex\n
*   ???? jsr     sm/rem\n
*        pla\n
*      3 bne     ???? v\n
*   ???? jsr     nip\n"
;

\ these tests are a little fiddly since the width of some fields can vary based on the base
\ and the starting offset.  HEX is better, but with DECIMAL in disasm it can be sensitive.
\ our simple compare-glob does a minimal match on * but is enough to identify a non-blank
\ number field delimited by whitespace.  For fixed width fields like hex bytes, it's better to
\ wildcard individual characters, e.g. ?? matches exactly two arbitrary characters.
\ To debug, add 2DUP DUMP after both restore-output and see-/mod and compare in results.txt.
T{ capture-output see /mod restore-output see-/mod-output compare-glob -> 0 }T

\ CASE has CO+IM+NN flags

: see-case-output s\" \n
nt: *  xt: *  header: 5C 04 ?? ?? ?? 06 \n
flags: HC 0 NN 1 AN 0 IM 1 CO 1 DC 1 LC 0 FP 0 | UF 0 ST 0 \n
size (decimal): 6 \n
\n
*  CA CA 74 00 74 01                                 ..t.t.\n
\n
*        dex\n
*        dex\n
*      0 stz.zx\n
*      1 stz.zx\n"
;
T{ capture-output see case restore-output see-case-output compare-glob -> 0 }T

\ EXIT has AN flag

: see-exit-output s\" \n
nt: *  xt: *  header: 2C 04 ?? ?? ?? 01 \n
flags: HC 0 NN 0 AN 1 IM 0 CO 1 DC 1 LC 0 FP 0 | UF 0 ST 0 \n
size (decimal): 1 \n
\n
*  60                                                `\n
\n
*        rts\n"
;
T{ capture-output see exit restore-output see-exit-output compare-glob -> 0 }T

\ check for HC flag

create saw 2 allot -1 saw !
: see-saw-output s\" \n
nt: *  xt: *  header: C0 03 ?? 05 \n
flags: HC 1 NN 1 AN 0 IM 0 CO 0 DC 0 LC 0 FP 0 | UF 0 ST 0 \n
size (decimal): CFA 3  PFA 2 \n
\n
*  20 ?? ?? FF FF                                     ??..\n
\n
*   ???? jsr     \n"
;

T{ capture-output see saw restore-output 2dup type see-saw-output compare-glob -> 0 }T

\ this used to check HC flag when variables still had that...

: see-life-output s\" \n
nt: *  xt: *  header: 00 04 ?? 0A \n
flags: HC 0 NN 0 AN 0 IM 0 CO 0 DC 0 LC 0 FP 0 | UF 0 ST 0 \n
size (decimal): 10 \n
\n
*  A0 ?? A9 ?? CA CA 95 00  94 01                    .?.?.... ..\n
\n
*     ?? ldy.#\n
*     ?? lda.#\n
*        dex\n
*        dex\n
*      0 sta.zx\n
*      1 sty.zx\n"
;
T{ capture-output see life restore-output see-life-output compare-glob -> 0 }T

nc-limit @
0 nc-limit !
: disasm-test
    10 0 do $12345678. 2drop s" banana" 2drop i 1 and if leave then loop
    0 0 ?do -1 +loop
;
nc-limit !

: disasm-test-output s\" \n
*  ????? jsr     BLITERAL 10 \n
*  ????? jsr     0\n
*  ????? jsr     DO \n
*  ????? jsr     2LITERAL 305419896 \n
*  ????? jsr     2drop\n
*  ????? jsr     SLITERAL 6 banana\n
*  ????? jsr     2drop\n
*  ????? jsr     i\n
*  ????? jsr     1\n
*  ????? jsr     and\n
*  ????? jsr     0BRANCH * \n
*  ????? jmp\n
*  ????? jsr     LOOP * \n
*  ????? jsr     unloop\n
*  ????? jsr     0\n
*  ????? jsr     0\n
*  ????? jsr     ?DO * \n
*  ????? jsr     DO \n
*  ????? jsr     LITERAL -1 \n
*  ????? jsr     +LOOP * \n
*  ????? jsr     unloop\n"
;

T{
    ' disasm-test dup int>name wordsize
    capture-output disasm restore-output
    disasm-test-output  compare-glob -> 0 }T
