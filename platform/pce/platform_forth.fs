\ platform_forth.fs

\ Put any Forth code that Tali should execute at startup in this file.
\ If this file isn't provided TaliForth will insert some Forth code to
\ display some splash strings as a basic sanity check on the boot process.

\ If you add a `platform_forth.fs` or other `.fs` file,
\ make sure you include it in your `platform.asm` file using code like this:
\
\       user_words_start:
\       .binary "platform_forth.asc"
\       user_words_end:
\
\ The `Makefile` automatically converts to `.asc` using `tools/forth_pack.py`.
\ This removes all comments and reduces whitespace between words to a single space
\ so it takes minimal space when imported as a single string into the binary.

\ We'll define a new word and then show an alternative message:

        : ultimate 42 ;
        cr
        .( I've studied species Turian, Asari, and Batarian. )
        cr

\ Tali will evaluate this string as part of a COLD startup.
\ If there's an error then Tali will abort evaluating this code.

\ Although system words are normally "assemblerized" in Tali Forth for speed,
\ `platform_forth.fs` can be useful for adding a few custom words.
\ You can find some example words in `examples/forth`.
\ Tali Forth reserves about 2 KB of ROM space for platform words.
\ If you want to import lots of forth code you should probably
\ read from some sort of block device.
