\ platform_forth.fs

\ Put any forth you would like Tali to run at startup in this file.
\ All comments will be removed and all whitespace reduced to a single space
\ between words before it's imported (as a single string) into the binary.
\ Tali evaluates this string as part of a COLD startup.  If there is an error
\ then Tali will abort evaluating this code.









\ Splash strings. We leave these as high-level words because they are
\ generated at the end of the boot process and signal that the other
\ high-level definitions worked (or at least didn't crash)

        cr .( Tali Forth 2 for the 65c02)
        cr .( Version 1.1  27. Jun 2025 )
        cr .( Copyright 2014-2025 Scot W. Stevenson, Sam Colwell, Patrick Surry)
        cr .( Tali Forth 2 comes with absolutely NO WARRANTY)
        cr .( Type 'bye' to exit) cr
