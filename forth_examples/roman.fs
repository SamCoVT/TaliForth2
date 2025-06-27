\ The word `roman>` converts a roman numeral string into an integer.
\ This was written to exercise the case/of construct.

variable prior

: roman> ( "MCMXIX" -- u )
    \ Example usage: roman> MCMXIX .
    \ This routine handles valid roman numbers and will abort" on invalid digits
    \ but will give unexpected results for invalid arrangements of valid digits like IIIX
    \ This could be fixed by asserting that non-subtractive digits are always in non-decreasing
    \ order reading from right to left.  An exercise for the reader?
    0 prior !
    parse-name
    ( addr n )
    \ we'll walk through digits from right to left
    0 -rot over + 1-
    ( sum addr addr+n-1 )
    do
        i c@ $20 or
        ( sum char-lc )
        case
            \ convert a roman digit to its value and whether it could start
            \ a subtractive pair i.e. one of: iv, ix, xl, xc, cd, cm
            \ we'll test later whether the following digit is 5x or 10x
            [char] i of 1 true endof
            [char] v of 5 false endof
            [char] x of 10 true endof
            [char] l of 50 false endof
            [char] c of 100 true endof
            [char] d of 500 false endof
            [char] m of 1000 false endof
            dup emit abort" : unrecognized roman numeral"
        endcase
        ( sum digit subtractive? )
        \ is 5*digit <= prior < 25*digit
        if
            prior @ over 5 * dup 5 * within
        else
            false
        then
        ( sum digit negate? )
        if negate 0 else dup then
        ( sum -digit 0 | sum +digit digit )
        prior !
        +
        ( sum )
    -1 +loop
;

\ The inverse word `roman` converts an integer into a roman numeral string.
\ Ported from Leo Brodie's Thinking Forth, e.g. https://www.forth.com/wp-content/uploads/2018/11/thinking-forth-color.pdf
create romans
        ( ones ) char I c, char V c,
        ( tens ) char X c, char L c,
    ( hundreds ) char C c, char D c,
   ( thousands ) char M c,

variable column# ( current_offset )
: ones      0 column# ! ;
: tens      2 column# ! ;
: hundreds  4 column# ! ;
: thousands 6 column# ! ;

: column ( -- address-of-column ) romans column# @ + ;
: .symbol ( offset -- ) column + c@ emit ;
: oner 0 .symbol ;
: fiver 1 .symbol ;
: tenner 2 .symbol ;

: oners ( #-of-oners -- )
    ?dup if 0 do oner loop then ;

: almost ( quotient-of-5/ -- )
    oner if tenner else fiver then ;

: digit ( digit -- )
    5 /mod over 4 = if
        almost drop
    else
        if fiver then
        oners
    then
;

: roman ( u -- )
    1000 /mod thousands digit
     100 /mod hundreds digit
      10 /mod tens digit
      ones digit
;