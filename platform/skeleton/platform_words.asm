; platform_words.asm
; Put any words (written in assembly) that you would like to add for your
; platform here.  You may delete the examples here, but leave this file
; in your platform folder.

; For each word, you need to create a header (with the #nt_header macro)
; and you need to mark the entry with an "xt_wordname:" label and the
; exit (just before the rts at the end) with a "z_wordname:" label where
; wordname is replaced with the name of your word.

; You will want to refer to words/headers.asm for a description of how to
; use the #nt_header macro and the Developer Guide section of the manual.

; ==== HEADERS ====

; To define your own words, you need to indicate you are starting a wordlist.
; We do this by assigning the label prev_nt to zero.  The headers are
; created in reverse order, so these words will show up at the END of
; the FORTH-WORDLIST, which is the default wordlist Tali starts up with.
prev_nt := 0

; Here are a couple of example words:
; FIVE - puts the value 5 on the data stack.
; ~    - prints a tilde to the screen, and will be a compile-only
;        word that is always natively compiled.
;
; The first word, FIVE, is very simple.
; The word ~ will show off more advanced usage.

; Create the headers.

; The word FIVE is very simple and the labels can be determined from the word
; name, so only the word name is given (use lowercase in headers!)
#nt_header five

; The word ~ is more complex.  The first issue is that we can't use symbols in
; the label names, so we will spell out the word tilde in the labels.  Because
; the label names are different than the word name, we will need to give both
; the label version and the name of the word in Forth to the #nt_header macro.
; In addition, we want this word to be Compile-Only and Always-Native, so we add
; those flags as a third argument to the #nt_header macro.
#nt_header tilde, "~", CO+AN


; ==== WORDS ====

; The beginning of a word need to be marked with a label xt_wordname
; where wordname needs to match the name used in #nt_header (first argument).
xt_five:
                ; Make room on the Forth data stack for a value.
                dex
                dex
                ; Put a 5 there.
                lda #5
                sta 0,x
                ; Put a 0 in the high byte.
                stz 1,x
; The end of the word (just BEFORE the RTS) needs to be marked with
; a label z_wordname where wordname needs to match the name used in
; #nt_header.
z_five:
                rts



; Here is the tilde word.  Because the header has the CO (Compile-Only) flag,
; Tali will give an error if this word is used in interpreted mode.  It can
; only be compiled into other words.  In addition, the AN (Always-Native) flag
; means that Tali will natively comple (e.g. just copy the assembly) into
; a word that uses this word.  Do note that AN flagged words must not have
; unconditional jumps (relative branches are fine).  Any words with the JMP
; instruction MUST have the NN (Never-Native) flag instead.
xt_tilde:
                ; Put a tilde on the Forth data stack.
                dex
                dex
                lda #'~'
                sta 0,x
                stz 1,x
                ; Use the forth word EMIT to print the tilde
                ; A JSR to the xt of a word will run that word.
                jsr xt_emit
z_tilde:
                rts
