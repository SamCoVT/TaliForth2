\ Test code adapted from TaliForth
\ This applies 16 randomly chosen tests per opcode from
\ https://github.com/SingleStepTests/ProcessorTests/tree/main/wdc65c02
\ excluding tests that have memory clashes in a 4K emulator image.
\ These can be run in batch using a command like:
\     cat examples/forth/emu65c02.fs examples/forth/emu65c02_test.fs | tools/c65/c65 -r taliforth-c65.bin > results.txt

hex

variable actual-depth   \ stack record
create actual-results  20 cells allot


\ Empty stack: handles underflowed stack too
: empty-stack ( ... -- )
   depth ?dup if
      dup 0< if
         negate 0 do 0 loop
      else
         0 do drop loop
      then
   then ;

\ Print the previous test's actual results. Added by SamCo 2018-05
: show-results ( -- )
   cr s"  ACTUAL RESULT: { " type
   actual-depth @ 0 ?do
      actual-results
      actual-depth @ i - 1- \ Print them in reverse order to match test.
      cells + @ .
   loop
   s" }" type ;

\ Display an error message followed by the line that had the error
: error  \ ( C-ADDR U -- )
   type source type \ display line corresponding to error
   empty-stack      \ throw away every thing else
   show-results ;   \ added by SamCo to show what actually happened

\ Syntactic sugar
: T{  ( -- ) ;

\ Record depth and content of stack
: ->  ( ... -- )
   depth dup actual-depth !  \ record depth
   ?dup if                   \ if there is something on stack ...
      0 do
         actual-results i cells + !
      loop                   \ ... save it
   then ;

\ Compare stack (expected) contents with saved (actual) contents
: }T  ( ... -- )
   depth actual-depth @ = if     \ if depths match
      depth ?dup if              \ if there is something on the stack
         0 do                    \ for each stack item
            actual-results i cells + @  \ compare actual with expected
            <> if
               cr s" INCORRECT RESULT: " error leave
            then
         loop
      then
   else                          \ depth mismatch
      cr s" WRONG NUMBER OF RESULTS: " error
   then ;

\ Tests extracted from https://github.com/SingleStepTests/ProcessorTests/tree/main/wdc65c02
\ This is a random sampling of 16 tests per opcode, avoiding tests that access the
\ same emulated 12-bit memory location via distinct 16-bit synonyms

( 00 )
$184b >PC $70 >S $21 >A $78 >X $6c >Y $64 >P $184b $00 >M $184c $cd >M $184d $73 >M $95bb $3a >M $fffe $bb >M $ffff $95 >M
T{ op PC S A X Y P -> $95bb $6d $21 $78 $6c $64 }T T{ $016e M $016f M $0170 M $184b M $184c M $184d M $95bb M $fffe M $ffff M -> $74 $4d $18 $00 $cd $73 $3a $bb $95 }T
$c482 >PC $36 >S $79 >A $e3 >X $fa >Y $a7 >P $6a87 $07 >M $c482 $00 >M $c483 $aa >M $c484 $c8 >M $fffe $87 >M $ffff $6a >M
T{ op PC S A X Y P -> $6a87 $33 $79 $e3 $fa $a7 }T T{ $0134 M $0135 M $0136 M $6a87 M $c482 M $c483 M $c484 M $fffe M $ffff M -> $b7 $84 $c4 $07 $00 $aa $c8 $87 $6a }T
$b67a >PC $d9 >S $4d >A $e4 >X $25 >Y $2f >P $5aab $7f >M $b67a $00 >M $b67b $41 >M $b67c $a6 >M $fffe $ab >M $ffff $5a >M
T{ op PC S A X Y P -> $5aab $d6 $4d $e4 $25 $27 }T T{ $01d7 M $01d8 M $01d9 M $5aab M $b67a M $b67b M $b67c M $fffe M $ffff M -> $3f $7c $b6 $7f $00 $41 $a6 $ab $5a }T
$79b8 >PC $6b >S $ec >A $eb >X $3f >Y $e6 >P $79b8 $00 >M $79b9 $7e >M $79ba $3b >M $9dab $91 >M $fffe $ab >M $ffff $9d >M
T{ op PC S A X Y P -> $9dab $68 $ec $eb $3f $e6 }T T{ $0169 M $016a M $016b M $79b8 M $79b9 M $79ba M $9dab M $fffe M $ffff M -> $f6 $ba $79 $00 $7e $3b $91 $ab $9d }T
$0920 >PC $12 >S $26 >A $72 >X $00 >Y $e2 >P $0920 $00 >M $0921 $cc >M $0922 $e6 >M $4867 $d9 >M $fffe $67 >M $ffff $48 >M
T{ op PC S A X Y P -> $4867 $0f $26 $72 $00 $e6 }T T{ $0110 M $0111 M $0112 M $0920 M $0921 M $0922 M $4867 M $fffe M $ffff M -> $f2 $22 $09 $00 $cc $e6 $d9 $67 $48 }T
$bbd1 >PC $61 >S $e2 >A $05 >X $77 >Y $6a >P $545b $8b >M $bbd1 $00 >M $bbd2 $e6 >M $bbd3 $b5 >M $fffe $5b >M $ffff $54 >M
T{ op PC S A X Y P -> $545b $5e $e2 $05 $77 $66 }T T{ $015f M $0160 M $0161 M $545b M $bbd1 M $bbd2 M $bbd3 M $fffe M $ffff M -> $7a $d3 $bb $8b $00 $e6 $b5 $5b $54 }T
$79c9 >PC $57 >S $69 >A $dd >X $17 >Y $2c >P $79c9 $00 >M $79ca $bf >M $79cb $11 >M $7ea4 $ae >M $fffe $a4 >M $ffff $7e >M
T{ op PC S A X Y P -> $7ea4 $54 $69 $dd $17 $24 }T T{ $0155 M $0156 M $0157 M $79c9 M $79ca M $79cb M $7ea4 M $fffe M $ffff M -> $3c $cb $79 $00 $bf $11 $ae $a4 $7e }T
$3cdc >PC $47 >S $ca >A $e8 >X $8f >Y $a6 >P $34c4 $66 >M $3cdc $00 >M $3cdd $72 >M $3cde $73 >M $fffe $c4 >M $ffff $34 >M
T{ op PC S A X Y P -> $34c4 $44 $ca $e8 $8f $a6 }T T{ $0145 M $0146 M $0147 M $34c4 M $3cdc M $3cdd M $3cde M $fffe M $ffff M -> $b6 $de $3c $66 $00 $72 $73 $c4 $34 }T
$7601 >PC $a1 >S $1e >A $94 >X $c2 >Y $a5 >P $7601 $00 >M $7602 $07 >M $7603 $59 >M $94fb $5c >M $fffe $fb >M $ffff $94 >M
T{ op PC S A X Y P -> $94fb $9e $1e $94 $c2 $a5 }T T{ $019f M $01a0 M $01a1 M $7601 M $7602 M $7603 M $94fb M $fffe M $ffff M -> $b5 $03 $76 $00 $07 $59 $5c $fb $94 }T
$7437 >PC $af >S $8d >A $42 >X $ae >Y $a1 >P $7437 $00 >M $7438 $f6 >M $7439 $1b >M $fdae $50 >M $fffe $ae >M $ffff $fd >M
T{ op PC S A X Y P -> $fdae $ac $8d $42 $ae $a5 }T T{ $01ad M $01ae M $01af M $7437 M $7438 M $7439 M $fdae M $fffe M $ffff M -> $b1 $39 $74 $00 $f6 $1b $50 $ae $fd }T
$eda1 >PC $96 >S $79 >A $dc >X $7b >Y $66 >P $2409 $6f >M $eda1 $00 >M $eda2 $c5 >M $eda3 $6e >M $fffe $09 >M $ffff $24 >M
T{ op PC S A X Y P -> $2409 $93 $79 $dc $7b $66 }T T{ $0194 M $0195 M $0196 M $2409 M $eda1 M $eda2 M $eda3 M $fffe M $ffff M -> $76 $a3 $ed $6f $00 $c5 $6e $09 $24 }T
$ba75 >PC $85 >S $54 >A $3e >X $1f >Y $2d >P $7e4d $8e >M $ba75 $00 >M $ba76 $10 >M $ba77 $9a >M $fffe $4d >M $ffff $7e >M
T{ op PC S A X Y P -> $7e4d $82 $54 $3e $1f $25 }T T{ $0183 M $0184 M $0185 M $7e4d M $ba75 M $ba76 M $ba77 M $fffe M $ffff M -> $3d $77 $ba $8e $00 $10 $9a $4d $7e }T
$2970 >PC $f3 >S $b5 >A $4a >X $85 >Y $e6 >P $2970 $00 >M $2971 $43 >M $2972 $e6 >M $95ae $34 >M $fffe $ae >M $ffff $95 >M
T{ op PC S A X Y P -> $95ae $f0 $b5 $4a $85 $e6 }T T{ $01f1 M $01f2 M $01f3 M $2970 M $2971 M $2972 M $95ae M $fffe M $ffff M -> $f6 $72 $29 $00 $43 $e6 $34 $ae $95 }T
$adfe >PC $a2 >S $69 >A $09 >X $3f >Y $27 >P $533d $09 >M $adfe $00 >M $adff $a9 >M $ae00 $66 >M $fffe $3d >M $ffff $53 >M
T{ op PC S A X Y P -> $533d $9f $69 $09 $3f $27 }T T{ $01a0 M $01a1 M $01a2 M $533d M $adfe M $adff M $ae00 M $fffe M $ffff M -> $37 $00 $ae $09 $00 $a9 $66 $3d $53 }T
$0228 >PC $b4 >S $41 >A $cc >X $57 >Y $aa >P $0228 $00 >M $0229 $10 >M $022a $4f >M $1109 $68 >M $fffe $09 >M $ffff $11 >M
T{ op PC S A X Y P -> $1109 $b1 $41 $cc $57 $a6 }T T{ $01b2 M $01b3 M $01b4 M $0228 M $0229 M $022a M $1109 M $fffe M $ffff M -> $ba $2a $02 $00 $10 $4f $68 $09 $11 }T
$6de4 >PC $cd >S $53 >A $08 >X $29 >Y $e7 >P $6de4 $00 >M $6de5 $f5 >M $6de6 $9f >M $fd86 $e0 >M $fffe $86 >M $ffff $fd >M
T{ op PC S A X Y P -> $fd86 $ca $53 $08 $29 $e7 }T T{ $01cb M $01cc M $01cd M $6de4 M $6de5 M $6de6 M $fd86 M $fffe M $ffff M -> $f7 $e6 $6d $00 $f5 $9f $e0 $86 $fd }T
( 01 )
$5add >PC $28 >S $0b >A $f0 >X $72 >Y $20 >P $0028 $6c >M $0029 $d4 >M $0038 $a0 >M $5add $01 >M $5ade $38 >M $5adf $95 >M $d46c $92 >M
T{ op PC S A X Y P -> $5adf $28 $9b $f0 $72 $a0 }T T{ $0028 M $0029 M $0038 M $5add M $5ade M $5adf M $d46c M -> $6c $d4 $a0 $01 $38 $95 $92 }T
$02c9 >PC $7b >S $a6 >A $a8 >X $8f >Y $eb >P $0023 $c5 >M $00cb $03 >M $00cc $6d >M $02c9 $01 >M $02ca $23 >M $02cb $3d >M $6d03 $98 >M
T{ op PC S A X Y P -> $02cb $7b $be $a8 $8f $e9 }T T{ $0023 M $00cb M $00cc M $02c9 M $02ca M $02cb M $6d03 M -> $c5 $03 $6d $01 $23 $3d $98 }T
$63bf >PC $47 >S $6f >A $1b >X $3c >Y $6e >P $0013 $89 >M $002e $9f >M $002f $bd >M $63bf $01 >M $63c0 $13 >M $63c1 $56 >M $bd9f $62 >M
T{ op PC S A X Y P -> $63c1 $47 $6f $1b $3c $6c }T T{ $0013 M $002e M $002f M $63bf M $63c0 M $63c1 M $bd9f M -> $89 $9f $bd $01 $13 $56 $62 }T
$e956 >PC $78 >S $17 >A $9e >X $6a >Y $68 >P $003e $df >M $003f $0c >M $00a0 $d3 >M $0cdf $77 >M $e956 $01 >M $e957 $a0 >M $e958 $c0 >M
T{ op PC S A X Y P -> $e958 $78 $77 $9e $6a $68 }T T{ $003e M $003f M $00a0 M $0cdf M $e956 M $e957 M $e958 M -> $df $0c $d3 $77 $01 $a0 $c0 }T
$0b00 >PC $68 >S $07 >A $18 >X $4e >Y $23 >P $0007 $6f >M $0008 $a3 >M $00ef $e3 >M $0b00 $01 >M $0b01 $ef >M $0b02 $b7 >M $a36f $32 >M
T{ op PC S A X Y P -> $0b02 $68 $37 $18 $4e $21 }T T{ $0007 M $0008 M $00ef M $0b00 M $0b01 M $0b02 M $a36f M -> $6f $a3 $e3 $01 $ef $b7 $32 }T
$5360 >PC $8c >S $d2 >A $e5 >X $81 >Y $ec >P $0099 $95 >M $009a $4a >M $00b4 $92 >M $4a95 $83 >M $5360 $01 >M $5361 $b4 >M $5362 $51 >M
T{ op PC S A X Y P -> $5362 $8c $d3 $e5 $81 $ec }T T{ $0099 M $009a M $00b4 M $4a95 M $5360 M $5361 M $5362 M -> $95 $4a $92 $83 $01 $b4 $51 }T
$1014 >PC $af >S $c2 >A $d0 >X $d6 >Y $e8 >P $009b $6e >M $009c $ae >M $00cb $aa >M $1014 $01 >M $1015 $cb >M $1016 $47 >M $ae6e $cb >M
T{ op PC S A X Y P -> $1016 $af $cb $d0 $d6 $e8 }T T{ $009b M $009c M $00cb M $1014 M $1015 M $1016 M $ae6e M -> $6e $ae $aa $01 $cb $47 $cb }T
$0e71 >PC $9e >S $1c >A $64 >X $37 >Y $e0 >P $007d $95 >M $00e1 $00 >M $00e2 $25 >M $0e71 $01 >M $0e72 $7d >M $0e73 $86 >M $2500 $83 >M
T{ op PC S A X Y P -> $0e73 $9e $9f $64 $37 $e0 }T T{ $007d M $00e1 M $00e2 M $0e71 M $0e72 M $0e73 M $2500 M -> $95 $00 $25 $01 $7d $86 $83 }T
$cc4a >PC $90 >S $82 >A $8f >X $f0 >Y $a8 >P $005a $46 >M $005b $4f >M $00cb $25 >M $4f46 $bf >M $cc4a $01 >M $cc4b $cb >M $cc4c $6f >M
T{ op PC S A X Y P -> $cc4c $90 $bf $8f $f0 $a8 }T T{ $005a M $005b M $00cb M $4f46 M $cc4a M $cc4b M $cc4c M -> $46 $4f $25 $bf $01 $cb $6f }T
$8b67 >PC $d6 >S $10 >A $34 >X $54 >Y $23 >P $0032 $98 >M $0066 $47 >M $0067 $b6 >M $8b67 $01 >M $8b68 $32 >M $8b69 $fa >M $b647 $d9 >M
T{ op PC S A X Y P -> $8b69 $d6 $d9 $34 $54 $a1 }T T{ $0032 M $0066 M $0067 M $8b67 M $8b68 M $8b69 M $b647 M -> $98 $47 $b6 $01 $32 $fa $d9 }T
$b86e >PC $c5 >S $7a >A $63 >X $68 >Y $2d >P $005a $8d >M $00bd $1e >M $00be $70 >M $701e $ae >M $b86e $01 >M $b86f $5a >M $b870 $71 >M
T{ op PC S A X Y P -> $b870 $c5 $fe $63 $68 $ad }T T{ $005a M $00bd M $00be M $701e M $b86e M $b86f M $b870 M -> $8d $1e $70 $ae $01 $5a $71 }T
$e3f0 >PC $40 >S $36 >A $52 >X $1c >Y $a5 >P $002a $25 >M $007c $69 >M $007d $b4 >M $b469 $63 >M $e3f0 $01 >M $e3f1 $2a >M $e3f2 $6b >M
T{ op PC S A X Y P -> $e3f2 $40 $77 $52 $1c $25 }T T{ $002a M $007c M $007d M $b469 M $e3f0 M $e3f1 M $e3f2 M -> $25 $69 $b4 $63 $01 $2a $6b }T
$23b3 >PC $81 >S $05 >A $af >X $6e >Y $68 >P $001d $8e >M $001e $f1 >M $006e $fe >M $23b3 $01 >M $23b4 $6e >M $23b5 $ba >M $f18e $38 >M
T{ op PC S A X Y P -> $23b5 $81 $3d $af $6e $68 }T T{ $001d M $001e M $006e M $23b3 M $23b4 M $23b5 M $f18e M -> $8e $f1 $fe $01 $6e $ba $38 }T
$4fa2 >PC $7c >S $64 >A $e6 >X $88 >Y $68 >P $00be $33 >M $00bf $98 >M $00d8 $99 >M $4fa2 $01 >M $4fa3 $d8 >M $4fa4 $a5 >M $9833 $6b >M
T{ op PC S A X Y P -> $4fa4 $7c $6f $e6 $88 $68 }T T{ $00be M $00bf M $00d8 M $4fa2 M $4fa3 M $4fa4 M $9833 M -> $33 $98 $99 $01 $d8 $a5 $6b }T
$4544 >PC $a1 >S $04 >A $de >X $85 >Y $e7 >P $001c $e7 >M $00fa $2e >M $00fb $cc >M $4544 $01 >M $4545 $1c >M $4546 $ce >M $cc2e $a2 >M
T{ op PC S A X Y P -> $4546 $a1 $a6 $de $85 $e5 }T T{ $001c M $00fa M $00fb M $4544 M $4545 M $4546 M $cc2e M -> $e7 $2e $cc $01 $1c $ce $a2 }T
$8804 >PC $e7 >S $33 >A $a2 >X $7e >Y $a9 >P $0027 $3f >M $005a $0e >M $00fc $27 >M $00fd $00 >M $8804 $01 >M $8805 $5a >M $8806 $3e >M
T{ op PC S A X Y P -> $8806 $e7 $3f $a2 $7e $29 }T T{ $0027 M $005a M $00fc M $00fd M $8804 M $8805 M $8806 M -> $3f $0e $27 $00 $01 $5a $3e }T
( 02 )
$69b9 >PC $74 >S $bd >A $8f >X $4e >Y $65 >P $69b9 $02 >M $69ba $c5 >M $69bb $6d >M
T{ op PC S A X Y P -> $69bb $74 $bd $8f $4e $65 }T T{ $69b9 M $69ba M $69bb M -> $02 $c5 $6d }T
$bbef >PC $37 >S $cc >A $49 >X $b0 >Y $27 >P $bbef $02 >M $bbf0 $0f >M $bbf1 $1b >M
T{ op PC S A X Y P -> $bbf1 $37 $cc $49 $b0 $27 }T T{ $bbef M $bbf0 M $bbf1 M -> $02 $0f $1b }T
$bdb8 >PC $db >S $d4 >A $15 >X $32 >Y $6c >P $bdb8 $02 >M $bdb9 $c8 >M $bdba $5d >M
T{ op PC S A X Y P -> $bdba $db $d4 $15 $32 $6c }T T{ $bdb8 M $bdb9 M $bdba M -> $02 $c8 $5d }T
$4d6b >PC $30 >S $af >A $eb >X $42 >Y $60 >P $4d6b $02 >M $4d6c $3c >M $4d6d $b4 >M
T{ op PC S A X Y P -> $4d6d $30 $af $eb $42 $60 }T T{ $4d6b M $4d6c M $4d6d M -> $02 $3c $b4 }T
$5461 >PC $79 >S $5f >A $66 >X $a7 >Y $a6 >P $5461 $02 >M $5462 $66 >M $5463 $a7 >M
T{ op PC S A X Y P -> $5463 $79 $5f $66 $a7 $a6 }T T{ $5461 M $5462 M $5463 M -> $02 $66 $a7 }T
$bb2b >PC $7c >S $0b >A $09 >X $4a >Y $e5 >P $bb2b $02 >M $bb2c $7d >M $bb2d $f1 >M
T{ op PC S A X Y P -> $bb2d $7c $0b $09 $4a $e5 }T T{ $bb2b M $bb2c M $bb2d M -> $02 $7d $f1 }T
$a1b1 >PC $61 >S $47 >A $1e >X $29 >Y $6d >P $a1b1 $02 >M $a1b2 $4b >M $a1b3 $64 >M
T{ op PC S A X Y P -> $a1b3 $61 $47 $1e $29 $6d }T T{ $a1b1 M $a1b2 M $a1b3 M -> $02 $4b $64 }T
$0bd0 >PC $47 >S $66 >A $2e >X $16 >Y $60 >P $0bd0 $02 >M $0bd1 $18 >M $0bd2 $94 >M
T{ op PC S A X Y P -> $0bd2 $47 $66 $2e $16 $60 }T T{ $0bd0 M $0bd1 M $0bd2 M -> $02 $18 $94 }T
$54db >PC $da >S $9a >A $98 >X $d2 >Y $a1 >P $54db $02 >M $54dc $18 >M $54dd $5e >M
T{ op PC S A X Y P -> $54dd $da $9a $98 $d2 $a1 }T T{ $54db M $54dc M $54dd M -> $02 $18 $5e }T
$fc40 >PC $a5 >S $73 >A $0b >X $32 >Y $ab >P $fc40 $02 >M $fc41 $ec >M $fc42 $51 >M
T{ op PC S A X Y P -> $fc42 $a5 $73 $0b $32 $ab }T T{ $fc40 M $fc41 M $fc42 M -> $02 $ec $51 }T
$66a0 >PC $6d >S $a5 >A $e1 >X $57 >Y $aa >P $66a0 $02 >M $66a1 $bc >M $66a2 $13 >M
T{ op PC S A X Y P -> $66a2 $6d $a5 $e1 $57 $aa }T T{ $66a0 M $66a1 M $66a2 M -> $02 $bc $13 }T
$f0e9 >PC $8b >S $72 >A $33 >X $c9 >Y $a4 >P $f0e9 $02 >M $f0ea $92 >M $f0eb $38 >M
T{ op PC S A X Y P -> $f0eb $8b $72 $33 $c9 $a4 }T T{ $f0e9 M $f0ea M $f0eb M -> $02 $92 $38 }T
$3080 >PC $50 >S $33 >A $87 >X $41 >Y $e3 >P $3080 $02 >M $3081 $aa >M $3082 $41 >M
T{ op PC S A X Y P -> $3082 $50 $33 $87 $41 $e3 }T T{ $3080 M $3081 M $3082 M -> $02 $aa $41 }T
$3c84 >PC $28 >S $b2 >A $45 >X $65 >Y $a1 >P $3c84 $02 >M $3c85 $e8 >M $3c86 $b0 >M
T{ op PC S A X Y P -> $3c86 $28 $b2 $45 $65 $a1 }T T{ $3c84 M $3c85 M $3c86 M -> $02 $e8 $b0 }T
$9c4f >PC $95 >S $9f >A $9b >X $d1 >Y $63 >P $9c4f $02 >M $9c50 $4e >M $9c51 $58 >M
T{ op PC S A X Y P -> $9c51 $95 $9f $9b $d1 $63 }T T{ $9c4f M $9c50 M $9c51 M -> $02 $4e $58 }T
$d064 >PC $c7 >S $1b >A $d0 >X $b6 >Y $a5 >P $d064 $02 >M $d065 $4e >M $d066 $5b >M
T{ op PC S A X Y P -> $d066 $c7 $1b $d0 $b6 $a5 }T T{ $d064 M $d065 M $d066 M -> $02 $4e $5b }T
( 03 )
$abbb >PC $17 >S $8e >A $c7 >X $1c >Y $61 >P $abbb $03 >M $abbc $f6 >M $abbd $fb >M
T{ op PC S A X Y P -> $abbc $17 $8e $c7 $1c $61 }T T{ $abbb M $abbc M $abbd M -> $03 $f6 $fb }T
$3ea6 >PC $d9 >S $64 >A $5b >X $ad >Y $29 >P $3ea6 $03 >M $3ea7 $ad >M $3ea8 $82 >M
T{ op PC S A X Y P -> $3ea7 $d9 $64 $5b $ad $29 }T T{ $3ea6 M $3ea7 M $3ea8 M -> $03 $ad $82 }T
$c7aa >PC $b2 >S $09 >A $32 >X $f8 >Y $29 >P $c7aa $03 >M $c7ab $46 >M $c7ac $fe >M
T{ op PC S A X Y P -> $c7ab $b2 $09 $32 $f8 $29 }T T{ $c7aa M $c7ab M $c7ac M -> $03 $46 $fe }T
$bffe >PC $a6 >S $fb >A $50 >X $2b >Y $2c >P $bffe $03 >M $bfff $1b >M $c000 $ff >M
T{ op PC S A X Y P -> $bfff $a6 $fb $50 $2b $2c }T T{ $bffe M $bfff M $c000 M -> $03 $1b $ff }T
$8ef7 >PC $46 >S $bc >A $2f >X $c2 >Y $e9 >P $8ef7 $03 >M $8ef8 $25 >M $8ef9 $4c >M
T{ op PC S A X Y P -> $8ef8 $46 $bc $2f $c2 $e9 }T T{ $8ef7 M $8ef8 M $8ef9 M -> $03 $25 $4c }T
$6441 >PC $9f >S $1b >A $cb >X $9c >Y $a5 >P $6441 $03 >M $6442 $06 >M $6443 $10 >M
T{ op PC S A X Y P -> $6442 $9f $1b $cb $9c $a5 }T T{ $6441 M $6442 M $6443 M -> $03 $06 $10 }T
$516d >PC $b4 >S $b4 >A $d5 >X $cd >Y $25 >P $516d $03 >M $516e $f1 >M $516f $90 >M
T{ op PC S A X Y P -> $516e $b4 $b4 $d5 $cd $25 }T T{ $516d M $516e M $516f M -> $03 $f1 $90 }T
$42aa >PC $0d >S $a6 >A $0b >X $cc >Y $a3 >P $42aa $03 >M $42ab $da >M $42ac $ba >M
T{ op PC S A X Y P -> $42ab $0d $a6 $0b $cc $a3 }T T{ $42aa M $42ab M $42ac M -> $03 $da $ba }T
$8e98 >PC $14 >S $c4 >A $81 >X $52 >Y $e0 >P $8e98 $03 >M $8e99 $d0 >M $8e9a $e8 >M
T{ op PC S A X Y P -> $8e99 $14 $c4 $81 $52 $e0 }T T{ $8e98 M $8e99 M $8e9a M -> $03 $d0 $e8 }T
$3c5d >PC $24 >S $c3 >A $fb >X $b9 >Y $a7 >P $3c5d $03 >M $3c5e $a4 >M $3c5f $9b >M
T{ op PC S A X Y P -> $3c5e $24 $c3 $fb $b9 $a7 }T T{ $3c5d M $3c5e M $3c5f M -> $03 $a4 $9b }T
$3150 >PC $b3 >S $c0 >A $43 >X $8b >Y $60 >P $3150 $03 >M $3151 $08 >M $3152 $f3 >M
T{ op PC S A X Y P -> $3151 $b3 $c0 $43 $8b $60 }T T{ $3150 M $3151 M $3152 M -> $03 $08 $f3 }T
$6cb2 >PC $93 >S $7c >A $64 >X $f8 >Y $ee >P $6cb2 $03 >M $6cb3 $87 >M $6cb4 $b7 >M
T{ op PC S A X Y P -> $6cb3 $93 $7c $64 $f8 $ee }T T{ $6cb2 M $6cb3 M $6cb4 M -> $03 $87 $b7 }T
$4f6d >PC $ae >S $d9 >A $6e >X $00 >Y $a3 >P $4f6d $03 >M $4f6e $54 >M $4f6f $cc >M
T{ op PC S A X Y P -> $4f6e $ae $d9 $6e $00 $a3 }T T{ $4f6d M $4f6e M $4f6f M -> $03 $54 $cc }T
$b308 >PC $f0 >S $db >A $a8 >X $56 >Y $e5 >P $b308 $03 >M $b309 $8f >M $b30a $aa >M
T{ op PC S A X Y P -> $b309 $f0 $db $a8 $56 $e5 }T T{ $b308 M $b309 M $b30a M -> $03 $8f $aa }T
$7f46 >PC $22 >S $3f >A $ec >X $4a >Y $af >P $7f46 $03 >M $7f47 $88 >M $7f48 $f1 >M
T{ op PC S A X Y P -> $7f47 $22 $3f $ec $4a $af }T T{ $7f46 M $7f47 M $7f48 M -> $03 $88 $f1 }T
$7bf9 >PC $15 >S $2e >A $34 >X $7c >Y $a6 >P $7bf9 $03 >M $7bfa $e5 >M $7bfb $df >M
T{ op PC S A X Y P -> $7bfa $15 $2e $34 $7c $a6 }T T{ $7bf9 M $7bfa M $7bfb M -> $03 $e5 $df }T
( 04 )
$f96c >PC $7b >S $2f >A $2c >X $93 >Y $6a >P $000b $7c >M $f96c $04 >M $f96d $0b >M $f96e $82 >M
T{ op PC S A X Y P -> $f96e $7b $2f $2c $93 $68 }T T{ $000b M $f96c M $f96d M $f96e M -> $7f $04 $0b $82 }T
$96e8 >PC $04 >S $88 >A $39 >X $87 >Y $27 >P $0060 $93 >M $96e8 $04 >M $96e9 $60 >M $96ea $e6 >M
T{ op PC S A X Y P -> $96ea $04 $88 $39 $87 $25 }T T{ $0060 M $96e8 M $96e9 M $96ea M -> $9b $04 $60 $e6 }T
$c7ee >PC $dc >S $e7 >A $34 >X $42 >Y $e4 >P $006e $9f >M $c7ee $04 >M $c7ef $6e >M $c7f0 $96 >M
T{ op PC S A X Y P -> $c7f0 $dc $e7 $34 $42 $e4 }T T{ $006e M $c7ee M $c7ef M $c7f0 M -> $ff $04 $6e $96 }T
$9c98 >PC $71 >S $a5 >A $25 >X $ca >Y $ad >P $00e3 $3e >M $9c98 $04 >M $9c99 $e3 >M $9c9a $84 >M
T{ op PC S A X Y P -> $9c9a $71 $a5 $25 $ca $ad }T T{ $00e3 M $9c98 M $9c99 M $9c9a M -> $bf $04 $e3 $84 }T
$b05e >PC $b9 >S $4f >A $81 >X $b9 >Y $6b >P $00cb $c3 >M $b05e $04 >M $b05f $cb >M $b060 $4b >M
T{ op PC S A X Y P -> $b060 $b9 $4f $81 $b9 $69 }T T{ $00cb M $b05e M $b05f M $b060 M -> $cf $04 $cb $4b }T
$f7ec >PC $d9 >S $3b >A $14 >X $ed >Y $64 >P $003d $7e >M $f7ec $04 >M $f7ed $3d >M $f7ee $aa >M
T{ op PC S A X Y P -> $f7ee $d9 $3b $14 $ed $64 }T T{ $003d M $f7ec M $f7ed M $f7ee M -> $7f $04 $3d $aa }T
$f3c9 >PC $67 >S $02 >A $54 >X $c5 >Y $ef >P $004e $d3 >M $f3c9 $04 >M $f3ca $4e >M $f3cb $1f >M
T{ op PC S A X Y P -> $f3cb $67 $02 $54 $c5 $ed }T T{ $004e M $f3c9 M $f3ca M $f3cb M -> $d3 $04 $4e $1f }T
$7236 >PC $ac >S $bc >A $42 >X $63 >Y $22 >P $005f $43 >M $7236 $04 >M $7237 $5f >M $7238 $15 >M
T{ op PC S A X Y P -> $7238 $ac $bc $42 $63 $22 }T T{ $005f M $7236 M $7237 M $7238 M -> $ff $04 $5f $15 }T
$a3cf >PC $b1 >S $33 >A $2d >X $0f >Y $e1 >P $008c $57 >M $a3cf $04 >M $a3d0 $8c >M $a3d1 $96 >M
T{ op PC S A X Y P -> $a3d1 $b1 $33 $2d $0f $e1 }T T{ $008c M $a3cf M $a3d0 M $a3d1 M -> $77 $04 $8c $96 }T
$2bc2 >PC $ac >S $76 >A $84 >X $86 >Y $ad >P $00ec $f6 >M $2bc2 $04 >M $2bc3 $ec >M $2bc4 $52 >M
T{ op PC S A X Y P -> $2bc4 $ac $76 $84 $86 $ad }T T{ $00ec M $2bc2 M $2bc3 M $2bc4 M -> $f6 $04 $ec $52 }T
$a921 >PC $6e >S $dc >A $90 >X $db >Y $26 >P $007c $03 >M $a921 $04 >M $a922 $7c >M $a923 $f2 >M
T{ op PC S A X Y P -> $a923 $6e $dc $90 $db $26 }T T{ $007c M $a921 M $a922 M $a923 M -> $df $04 $7c $f2 }T
$d918 >PC $4a >S $09 >A $f1 >X $b8 >Y $2c >P $00ab $fb >M $d918 $04 >M $d919 $ab >M $d91a $12 >M
T{ op PC S A X Y P -> $d91a $4a $09 $f1 $b8 $2c }T T{ $00ab M $d918 M $d919 M $d91a M -> $fb $04 $ab $12 }T
$2712 >PC $25 >S $88 >A $f8 >X $ca >Y $29 >P $00a5 $b0 >M $2712 $04 >M $2713 $a5 >M $2714 $84 >M
T{ op PC S A X Y P -> $2714 $25 $88 $f8 $ca $29 }T T{ $00a5 M $2712 M $2713 M $2714 M -> $b8 $04 $a5 $84 }T
$170a >PC $83 >S $62 >A $4b >X $ed >Y $2a >P $0010 $85 >M $170a $04 >M $170b $10 >M $170c $6a >M
T{ op PC S A X Y P -> $170c $83 $62 $4b $ed $2a }T T{ $0010 M $170a M $170b M $170c M -> $e7 $04 $10 $6a }T
$5051 >PC $a7 >S $44 >A $60 >X $77 >Y $65 >P $00cf $3c >M $5051 $04 >M $5052 $cf >M $5053 $6a >M
T{ op PC S A X Y P -> $5053 $a7 $44 $60 $77 $65 }T T{ $00cf M $5051 M $5052 M $5053 M -> $7c $04 $cf $6a }T
$9eb4 >PC $7f >S $e4 >A $0e >X $07 >Y $22 >P $0004 $f5 >M $9eb4 $04 >M $9eb5 $04 >M $9eb6 $70 >M
T{ op PC S A X Y P -> $9eb6 $7f $e4 $0e $07 $20 }T T{ $0004 M $9eb4 M $9eb5 M $9eb6 M -> $f5 $04 $04 $70 }T
( 05 )
$5535 >PC $a1 >S $a6 >A $34 >X $41 >Y $ed >P $0079 $7b >M $5535 $05 >M $5536 $79 >M $5537 $b4 >M
T{ op PC S A X Y P -> $5537 $a1 $ff $34 $41 $ed }T T{ $0079 M $5535 M $5536 M $5537 M -> $7b $05 $79 $b4 }T
$b414 >PC $b6 >S $00 >A $b7 >X $ef >Y $6e >P $005e $df >M $b414 $05 >M $b415 $5e >M $b416 $e0 >M
T{ op PC S A X Y P -> $b416 $b6 $df $b7 $ef $ec }T T{ $005e M $b414 M $b415 M $b416 M -> $df $05 $5e $e0 }T
$10de >PC $4d >S $cd >A $86 >X $7b >Y $20 >P $0052 $5d >M $10de $05 >M $10df $52 >M $10e0 $0a >M
T{ op PC S A X Y P -> $10e0 $4d $dd $86 $7b $a0 }T T{ $0052 M $10de M $10df M $10e0 M -> $5d $05 $52 $0a }T
$4f7e >PC $f7 >S $96 >A $88 >X $58 >Y $e4 >P $009c $a4 >M $4f7e $05 >M $4f7f $9c >M $4f80 $c8 >M
T{ op PC S A X Y P -> $4f80 $f7 $b6 $88 $58 $e4 }T T{ $009c M $4f7e M $4f7f M $4f80 M -> $a4 $05 $9c $c8 }T
$3090 >PC $1d >S $14 >A $f2 >X $20 >Y $e9 >P $00b4 $95 >M $3090 $05 >M $3091 $b4 >M $3092 $f9 >M
T{ op PC S A X Y P -> $3092 $1d $95 $f2 $20 $e9 }T T{ $00b4 M $3090 M $3091 M $3092 M -> $95 $05 $b4 $f9 }T
$c0d2 >PC $d2 >S $26 >A $83 >X $e5 >Y $ab >P $002b $fa >M $c0d2 $05 >M $c0d3 $2b >M $c0d4 $4c >M
T{ op PC S A X Y P -> $c0d4 $d2 $fe $83 $e5 $a9 }T T{ $002b M $c0d2 M $c0d3 M $c0d4 M -> $fa $05 $2b $4c }T
$626a >PC $cc >S $16 >A $51 >X $cc >Y $a8 >P $008d $9d >M $626a $05 >M $626b $8d >M $626c $c9 >M
T{ op PC S A X Y P -> $626c $cc $9f $51 $cc $a8 }T T{ $008d M $626a M $626b M $626c M -> $9d $05 $8d $c9 }T
$0574 >PC $0e >S $00 >A $43 >X $e4 >Y $2c >P $007b $a2 >M $0574 $05 >M $0575 $7b >M $0576 $c9 >M
T{ op PC S A X Y P -> $0576 $0e $a2 $43 $e4 $ac }T T{ $007b M $0574 M $0575 M $0576 M -> $a2 $05 $7b $c9 }T
$64aa >PC $f7 >S $30 >A $15 >X $c4 >Y $e0 >P $00e4 $6f >M $64aa $05 >M $64ab $e4 >M $64ac $dd >M
T{ op PC S A X Y P -> $64ac $f7 $7f $15 $c4 $60 }T T{ $00e4 M $64aa M $64ab M $64ac M -> $6f $05 $e4 $dd }T
$e07a >PC $94 >S $ce >A $ec >X $f3 >Y $23 >P $0032 $ed >M $e07a $05 >M $e07b $32 >M $e07c $d4 >M
T{ op PC S A X Y P -> $e07c $94 $ef $ec $f3 $a1 }T T{ $0032 M $e07a M $e07b M $e07c M -> $ed $05 $32 $d4 }T
$bbb8 >PC $ef >S $82 >A $15 >X $e7 >Y $67 >P $00d2 $3b >M $bbb8 $05 >M $bbb9 $d2 >M $bbba $cc >M
T{ op PC S A X Y P -> $bbba $ef $bb $15 $e7 $e5 }T T{ $00d2 M $bbb8 M $bbb9 M $bbba M -> $3b $05 $d2 $cc }T
$6242 >PC $e3 >S $4e >A $56 >X $72 >Y $63 >P $002a $1f >M $6242 $05 >M $6243 $2a >M $6244 $79 >M
T{ op PC S A X Y P -> $6244 $e3 $5f $56 $72 $61 }T T{ $002a M $6242 M $6243 M $6244 M -> $1f $05 $2a $79 }T
$9c3a >PC $f1 >S $6e >A $89 >X $95 >Y $ab >P $00cb $0e >M $9c3a $05 >M $9c3b $cb >M $9c3c $ff >M
T{ op PC S A X Y P -> $9c3c $f1 $6e $89 $95 $29 }T T{ $00cb M $9c3a M $9c3b M $9c3c M -> $0e $05 $cb $ff }T
$6659 >PC $7b >S $08 >A $56 >X $ee >Y $e7 >P $007d $b4 >M $6659 $05 >M $665a $7d >M $665b $fe >M
T{ op PC S A X Y P -> $665b $7b $bc $56 $ee $e5 }T T{ $007d M $6659 M $665a M $665b M -> $b4 $05 $7d $fe }T
$fbfe >PC $d9 >S $6b >A $88 >X $05 >Y $64 >P $006e $48 >M $fbfe $05 >M $fbff $6e >M $fc00 $27 >M
T{ op PC S A X Y P -> $fc00 $d9 $6b $88 $05 $64 }T T{ $006e M $fbfe M $fbff M $fc00 M -> $48 $05 $6e $27 }T
$cf33 >PC $d4 >S $fc >A $ea >X $58 >Y $2e >P $00ed $da >M $cf33 $05 >M $cf34 $ed >M $cf35 $80 >M
T{ op PC S A X Y P -> $cf35 $d4 $fe $ea $58 $ac }T T{ $00ed M $cf33 M $cf34 M $cf35 M -> $da $05 $ed $80 }T
( 06 )
$6acf >PC $df >S $c7 >A $ca >X $45 >Y $a4 >P $00a3 $47 >M $6acf $06 >M $6ad0 $a3 >M $6ad1 $59 >M
T{ op PC S A X Y P -> $6ad1 $df $c7 $ca $45 $a4 }T T{ $00a3 M $6acf M $6ad0 M $6ad1 M -> $8e $06 $a3 $59 }T
$7991 >PC $12 >S $bb >A $72 >X $da >Y $ae >P $00ef $25 >M $7991 $06 >M $7992 $ef >M $7993 $23 >M
T{ op PC S A X Y P -> $7993 $12 $bb $72 $da $2c }T T{ $00ef M $7991 M $7992 M $7993 M -> $4a $06 $ef $23 }T
$76c8 >PC $28 >S $e2 >A $68 >X $0e >Y $6a >P $003c $30 >M $76c8 $06 >M $76c9 $3c >M $76ca $dd >M
T{ op PC S A X Y P -> $76ca $28 $e2 $68 $0e $68 }T T{ $003c M $76c8 M $76c9 M $76ca M -> $60 $06 $3c $dd }T
$7564 >PC $a8 >S $9a >A $ee >X $4d >Y $63 >P $00e3 $d2 >M $7564 $06 >M $7565 $e3 >M $7566 $f9 >M
T{ op PC S A X Y P -> $7566 $a8 $9a $ee $4d $e1 }T T{ $00e3 M $7564 M $7565 M $7566 M -> $a4 $06 $e3 $f9 }T
$0fd8 >PC $b0 >S $c2 >A $20 >X $5c >Y $2a >P $008b $46 >M $0fd8 $06 >M $0fd9 $8b >M $0fda $11 >M
T{ op PC S A X Y P -> $0fda $b0 $c2 $20 $5c $a8 }T T{ $008b M $0fd8 M $0fd9 M $0fda M -> $8c $06 $8b $11 }T
$7384 >PC $ae >S $ba >A $13 >X $0b >Y $2a >P $003b $43 >M $7384 $06 >M $7385 $3b >M $7386 $e8 >M
T{ op PC S A X Y P -> $7386 $ae $ba $13 $0b $a8 }T T{ $003b M $7384 M $7385 M $7386 M -> $86 $06 $3b $e8 }T
$e6f2 >PC $ed >S $a0 >A $9a >X $6d >Y $a7 >P $0091 $39 >M $e6f2 $06 >M $e6f3 $91 >M $e6f4 $42 >M
T{ op PC S A X Y P -> $e6f4 $ed $a0 $9a $6d $24 }T T{ $0091 M $e6f2 M $e6f3 M $e6f4 M -> $72 $06 $91 $42 }T
$d9d3 >PC $64 >S $b1 >A $34 >X $c5 >Y $ad >P $0084 $bf >M $d9d3 $06 >M $d9d4 $84 >M $d9d5 $25 >M
T{ op PC S A X Y P -> $d9d5 $64 $b1 $34 $c5 $2d }T T{ $0084 M $d9d3 M $d9d4 M $d9d5 M -> $7e $06 $84 $25 }T
$c141 >PC $0f >S $5c >A $78 >X $76 >Y $a1 >P $0099 $d4 >M $c141 $06 >M $c142 $99 >M $c143 $e3 >M
T{ op PC S A X Y P -> $c143 $0f $5c $78 $76 $a1 }T T{ $0099 M $c141 M $c142 M $c143 M -> $a8 $06 $99 $e3 }T
$2eb5 >PC $f6 >S $d3 >A $99 >X $77 >Y $a5 >P $0007 $1d >M $2eb5 $06 >M $2eb6 $07 >M $2eb7 $c1 >M
T{ op PC S A X Y P -> $2eb7 $f6 $d3 $99 $77 $24 }T T{ $0007 M $2eb5 M $2eb6 M $2eb7 M -> $3a $06 $07 $c1 }T
$50d2 >PC $d5 >S $03 >A $89 >X $dd >Y $a5 >P $0054 $b7 >M $50d2 $06 >M $50d3 $54 >M $50d4 $36 >M
T{ op PC S A X Y P -> $50d4 $d5 $03 $89 $dd $25 }T T{ $0054 M $50d2 M $50d3 M $50d4 M -> $6e $06 $54 $36 }T
$417f >PC $a4 >S $67 >A $b8 >X $56 >Y $e1 >P $0080 $9e >M $417f $06 >M $4180 $80 >M $4181 $09 >M
T{ op PC S A X Y P -> $4181 $a4 $67 $b8 $56 $61 }T T{ $0080 M $417f M $4180 M $4181 M -> $3c $06 $80 $09 }T
$d6cf >PC $78 >S $0a >A $0a >X $8b >Y $65 >P $008b $3f >M $d6cf $06 >M $d6d0 $8b >M $d6d1 $72 >M
T{ op PC S A X Y P -> $d6d1 $78 $0a $0a $8b $64 }T T{ $008b M $d6cf M $d6d0 M $d6d1 M -> $7e $06 $8b $72 }T
$391d >PC $58 >S $40 >A $be >X $47 >Y $e1 >P $00f7 $49 >M $391d $06 >M $391e $f7 >M $391f $45 >M
T{ op PC S A X Y P -> $391f $58 $40 $be $47 $e0 }T T{ $00f7 M $391d M $391e M $391f M -> $92 $06 $f7 $45 }T
$f79a >PC $ad >S $fe >A $42 >X $bc >Y $6f >P $00a4 $91 >M $f79a $06 >M $f79b $a4 >M $f79c $44 >M
T{ op PC S A X Y P -> $f79c $ad $fe $42 $bc $6d }T T{ $00a4 M $f79a M $f79b M $f79c M -> $22 $06 $a4 $44 }T
$2d75 >PC $80 >S $fa >A $76 >X $fb >Y $ec >P $0029 $18 >M $2d75 $06 >M $2d76 $29 >M $2d77 $05 >M
T{ op PC S A X Y P -> $2d77 $80 $fa $76 $fb $6c }T T{ $0029 M $2d75 M $2d76 M $2d77 M -> $30 $06 $29 $05 }T
( 07 )
$6c88 >PC $b8 >S $d9 >A $61 >X $5d >Y $e2 >P $00db $18 >M $6c88 $07 >M $6c89 $db >M $6c8a $c2 >M
T{ op PC S A X Y P -> $6c8a $b8 $d9 $61 $5d $e2 }T T{ $00db M $6c88 M $6c89 M $6c8a M -> $18 $07 $db $c2 }T
$9bde >PC $b9 >S $93 >A $ab >X $c2 >Y $a0 >P $00d2 $ae >M $9bde $07 >M $9bdf $d2 >M $9be0 $9d >M
T{ op PC S A X Y P -> $9be0 $b9 $93 $ab $c2 $a0 }T T{ $00d2 M $9bde M $9bdf M $9be0 M -> $ae $07 $d2 $9d }T
$995f >PC $d6 >S $4d >A $61 >X $fd >Y $6f >P $001b $3c >M $995f $07 >M $9960 $1b >M $9961 $c2 >M
T{ op PC S A X Y P -> $9961 $d6 $4d $61 $fd $6f }T T{ $001b M $995f M $9960 M $9961 M -> $3c $07 $1b $c2 }T
$5115 >PC $82 >S $51 >A $9f >X $2c >Y $a5 >P $00e5 $ec >M $5115 $07 >M $5116 $e5 >M $5117 $42 >M
T{ op PC S A X Y P -> $5117 $82 $51 $9f $2c $a5 }T T{ $00e5 M $5115 M $5116 M $5117 M -> $ec $07 $e5 $42 }T
$b092 >PC $c2 >S $10 >A $b5 >X $af >Y $a7 >P $0035 $27 >M $b092 $07 >M $b093 $35 >M $b094 $54 >M
T{ op PC S A X Y P -> $b094 $c2 $10 $b5 $af $a7 }T T{ $0035 M $b092 M $b093 M $b094 M -> $26 $07 $35 $54 }T
$359e >PC $31 >S $aa >A $2d >X $68 >Y $64 >P $005b $d4 >M $359e $07 >M $359f $5b >M $35a0 $f2 >M
T{ op PC S A X Y P -> $35a0 $31 $aa $2d $68 $64 }T T{ $005b M $359e M $359f M $35a0 M -> $d4 $07 $5b $f2 }T
$9b1d >PC $d8 >S $be >A $2c >X $07 >Y $a5 >P $0066 $1c >M $9b1d $07 >M $9b1e $66 >M $9b1f $96 >M
T{ op PC S A X Y P -> $9b1f $d8 $be $2c $07 $a5 }T T{ $0066 M $9b1d M $9b1e M $9b1f M -> $1c $07 $66 $96 }T
$0c8c >PC $3e >S $8e >A $ea >X $e6 >Y $ec >P $0032 $5c >M $0c8c $07 >M $0c8d $32 >M $0c8e $92 >M
T{ op PC S A X Y P -> $0c8e $3e $8e $ea $e6 $ec }T T{ $0032 M $0c8c M $0c8d M $0c8e M -> $5c $07 $32 $92 }T
$73b2 >PC $a4 >S $29 >A $4e >X $4d >Y $e2 >P $0052 $bd >M $73b2 $07 >M $73b3 $52 >M $73b4 $28 >M
T{ op PC S A X Y P -> $73b4 $a4 $29 $4e $4d $e2 }T T{ $0052 M $73b2 M $73b3 M $73b4 M -> $bc $07 $52 $28 }T
$8ae8 >PC $cd >S $7e >A $00 >X $7d >Y $ef >P $0034 $2c >M $8ae8 $07 >M $8ae9 $34 >M $8aea $8a >M
T{ op PC S A X Y P -> $8aea $cd $7e $00 $7d $ef }T T{ $0034 M $8ae8 M $8ae9 M $8aea M -> $2c $07 $34 $8a }T
$046f >PC $d5 >S $cb >A $9e >X $0a >Y $6f >P $00aa $94 >M $046f $07 >M $0470 $aa >M $0471 $ff >M
T{ op PC S A X Y P -> $0471 $d5 $cb $9e $0a $6f }T T{ $00aa M $046f M $0470 M $0471 M -> $94 $07 $aa $ff }T
$1c8a >PC $7b >S $71 >A $ca >X $dd >Y $e3 >P $008d $77 >M $1c8a $07 >M $1c8b $8d >M $1c8c $be >M
T{ op PC S A X Y P -> $1c8c $7b $71 $ca $dd $e3 }T T{ $008d M $1c8a M $1c8b M $1c8c M -> $76 $07 $8d $be }T
$012b >PC $3b >S $af >A $5b >X $ff >Y $66 >P $0026 $ca >M $012b $07 >M $012c $26 >M $012d $82 >M
T{ op PC S A X Y P -> $012d $3b $af $5b $ff $66 }T T{ $0026 M $012b M $012c M $012d M -> $ca $07 $26 $82 }T
$0125 >PC $3f >S $e5 >A $4a >X $58 >Y $e0 >P $0025 $94 >M $0125 $07 >M $0126 $25 >M $0127 $be >M
T{ op PC S A X Y P -> $0127 $3f $e5 $4a $58 $e0 }T T{ $0025 M $0125 M $0126 M $0127 M -> $94 $07 $25 $be }T
$c729 >PC $13 >S $84 >A $44 >X $df >Y $eb >P $00e0 $36 >M $c729 $07 >M $c72a $e0 >M $c72b $11 >M
T{ op PC S A X Y P -> $c72b $13 $84 $44 $df $eb }T T{ $00e0 M $c729 M $c72a M $c72b M -> $36 $07 $e0 $11 }T
$fef9 >PC $50 >S $cc >A $17 >X $26 >Y $a7 >P $0061 $eb >M $fef9 $07 >M $fefa $61 >M $fefb $f6 >M
T{ op PC S A X Y P -> $fefb $50 $cc $17 $26 $a7 }T T{ $0061 M $fef9 M $fefa M $fefb M -> $ea $07 $61 $f6 }T
( 08 )
$f97c >PC $1a >S $92 >A $b9 >X $df >Y $23 >P $f97c $08 >M $f97d $b2 >M $f97e $28 >M
T{ op PC S A X Y P -> $f97d $19 $92 $b9 $df $23 }T T{ $011a M $f97c M $f97d M $f97e M -> $33 $08 $b2 $28 }T
$f198 >PC $78 >S $20 >A $a6 >X $48 >Y $e4 >P $f198 $08 >M $f199 $6b >M $f19a $da >M
T{ op PC S A X Y P -> $f199 $77 $20 $a6 $48 $e4 }T T{ $0178 M $f198 M $f199 M $f19a M -> $f4 $08 $6b $da }T
$2dd6 >PC $44 >S $3a >A $64 >X $73 >Y $ad >P $2dd6 $08 >M $2dd7 $31 >M $2dd8 $12 >M
T{ op PC S A X Y P -> $2dd7 $43 $3a $64 $73 $ad }T T{ $0144 M $2dd6 M $2dd7 M $2dd8 M -> $bd $08 $31 $12 }T
$3875 >PC $3a >S $7c >A $b7 >X $2d >Y $ec >P $3875 $08 >M $3876 $7a >M $3877 $a6 >M
T{ op PC S A X Y P -> $3876 $39 $7c $b7 $2d $ec }T T{ $013a M $3875 M $3876 M $3877 M -> $fc $08 $7a $a6 }T
$d79f >PC $27 >S $11 >A $79 >X $42 >Y $6c >P $d79f $08 >M $d7a0 $e4 >M $d7a1 $c8 >M
T{ op PC S A X Y P -> $d7a0 $26 $11 $79 $42 $6c }T T{ $0127 M $d79f M $d7a0 M $d7a1 M -> $7c $08 $e4 $c8 }T
$2fe0 >PC $2f >S $e4 >A $9d >X $82 >Y $6b >P $2fe0 $08 >M $2fe1 $0e >M $2fe2 $79 >M
T{ op PC S A X Y P -> $2fe1 $2e $e4 $9d $82 $6b }T T{ $012f M $2fe0 M $2fe1 M $2fe2 M -> $7b $08 $0e $79 }T
$07eb >PC $c4 >S $fc >A $b2 >X $d2 >Y $2a >P $07eb $08 >M $07ec $47 >M $07ed $bc >M
T{ op PC S A X Y P -> $07ec $c3 $fc $b2 $d2 $2a }T T{ $01c4 M $07eb M $07ec M $07ed M -> $3a $08 $47 $bc }T
$96a1 >PC $ae >S $7b >A $76 >X $65 >Y $6f >P $96a1 $08 >M $96a2 $87 >M $96a3 $14 >M
T{ op PC S A X Y P -> $96a2 $ad $7b $76 $65 $6f }T T{ $01ae M $96a1 M $96a2 M $96a3 M -> $7f $08 $87 $14 }T
$e7ce >PC $48 >S $0a >A $76 >X $93 >Y $e0 >P $e7ce $08 >M $e7cf $63 >M $e7d0 $e2 >M
T{ op PC S A X Y P -> $e7cf $47 $0a $76 $93 $e0 }T T{ $0148 M $e7ce M $e7cf M $e7d0 M -> $f0 $08 $63 $e2 }T
$0fdd >PC $89 >S $bc >A $68 >X $c4 >Y $2b >P $0fdd $08 >M $0fde $8c >M $0fdf $3a >M
T{ op PC S A X Y P -> $0fde $88 $bc $68 $c4 $2b }T T{ $0189 M $0fdd M $0fde M $0fdf M -> $3b $08 $8c $3a }T
$66d5 >PC $a4 >S $cc >A $5c >X $62 >Y $29 >P $66d5 $08 >M $66d6 $3d >M $66d7 $55 >M
T{ op PC S A X Y P -> $66d6 $a3 $cc $5c $62 $29 }T T{ $01a4 M $66d5 M $66d6 M $66d7 M -> $39 $08 $3d $55 }T
$8d3d >PC $04 >S $71 >A $1e >X $84 >Y $a1 >P $8d3d $08 >M $8d3e $b0 >M $8d3f $86 >M
T{ op PC S A X Y P -> $8d3e $03 $71 $1e $84 $a1 }T T{ $0104 M $8d3d M $8d3e M $8d3f M -> $b1 $08 $b0 $86 }T
$4221 >PC $ff >S $ea >A $12 >X $cd >Y $21 >P $4221 $08 >M $4222 $1c >M $4223 $8b >M
T{ op PC S A X Y P -> $4222 $fe $ea $12 $cd $21 }T T{ $01ff M $4221 M $4222 M $4223 M -> $31 $08 $1c $8b }T
$6135 >PC $2c >S $cc >A $33 >X $15 >Y $21 >P $6135 $08 >M $6136 $a9 >M $6137 $6e >M
T{ op PC S A X Y P -> $6136 $2b $cc $33 $15 $21 }T T{ $012c M $6135 M $6136 M $6137 M -> $31 $08 $a9 $6e }T
$30c0 >PC $16 >S $e6 >A $e0 >X $ac >Y $2a >P $30c0 $08 >M $30c1 $2e >M $30c2 $ad >M
T{ op PC S A X Y P -> $30c1 $15 $e6 $e0 $ac $2a }T T{ $0116 M $30c0 M $30c1 M $30c2 M -> $3a $08 $2e $ad }T
$1f75 >PC $52 >S $7d >A $3a >X $93 >Y $ad >P $1f75 $08 >M $1f76 $c0 >M $1f77 $26 >M
T{ op PC S A X Y P -> $1f76 $51 $7d $3a $93 $ad }T T{ $0152 M $1f75 M $1f76 M $1f77 M -> $bd $08 $c0 $26 }T
( 09 )
$1cd9 >PC $9f >S $9a >A $1c >X $6d >Y $2d >P $1cd9 $09 >M $1cda $e9 >M $1cdb $7e >M
T{ op PC S A X Y P -> $1cdb $9f $fb $1c $6d $ad }T T{ $1cd9 M $1cda M $1cdb M -> $09 $e9 $7e }T
$51ea >PC $0f >S $1c >A $b3 >X $31 >Y $68 >P $51ea $09 >M $51eb $52 >M $51ec $1e >M
T{ op PC S A X Y P -> $51ec $0f $5e $b3 $31 $68 }T T{ $51ea M $51eb M $51ec M -> $09 $52 $1e }T
$79e4 >PC $54 >S $59 >A $3c >X $46 >Y $2e >P $79e4 $09 >M $79e5 $30 >M $79e6 $41 >M
T{ op PC S A X Y P -> $79e6 $54 $79 $3c $46 $2c }T T{ $79e4 M $79e5 M $79e6 M -> $09 $30 $41 }T
$3438 >PC $e7 >S $28 >A $21 >X $d4 >Y $ac >P $3438 $09 >M $3439 $10 >M $343a $c4 >M
T{ op PC S A X Y P -> $343a $e7 $38 $21 $d4 $2c }T T{ $3438 M $3439 M $343a M -> $09 $10 $c4 }T
$6417 >PC $32 >S $58 >A $b3 >X $16 >Y $2f >P $6417 $09 >M $6418 $dc >M $6419 $12 >M
T{ op PC S A X Y P -> $6419 $32 $dc $b3 $16 $ad }T T{ $6417 M $6418 M $6419 M -> $09 $dc $12 }T
$bb68 >PC $dd >S $2e >A $f3 >X $09 >Y $69 >P $bb68 $09 >M $bb69 $22 >M $bb6a $f1 >M
T{ op PC S A X Y P -> $bb6a $dd $2e $f3 $09 $69 }T T{ $bb68 M $bb69 M $bb6a M -> $09 $22 $f1 }T
$9f71 >PC $12 >S $86 >A $4e >X $26 >Y $2f >P $9f71 $09 >M $9f72 $59 >M $9f73 $97 >M
T{ op PC S A X Y P -> $9f73 $12 $df $4e $26 $ad }T T{ $9f71 M $9f72 M $9f73 M -> $09 $59 $97 }T
$155e >PC $a6 >S $12 >A $96 >X $c0 >Y $e0 >P $155e $09 >M $155f $9b >M $1560 $e3 >M
T{ op PC S A X Y P -> $1560 $a6 $9b $96 $c0 $e0 }T T{ $155e M $155f M $1560 M -> $09 $9b $e3 }T
$557b >PC $3a >S $1e >A $e8 >X $5d >Y $a8 >P $557b $09 >M $557c $0b >M $557d $80 >M
T{ op PC S A X Y P -> $557d $3a $1f $e8 $5d $28 }T T{ $557b M $557c M $557d M -> $09 $0b $80 }T
$f93f >PC $40 >S $d1 >A $50 >X $d6 >Y $ad >P $f93f $09 >M $f940 $c2 >M $f941 $77 >M
T{ op PC S A X Y P -> $f941 $40 $d3 $50 $d6 $ad }T T{ $f93f M $f940 M $f941 M -> $09 $c2 $77 }T
$a86a >PC $3a >S $94 >A $5a >X $0a >Y $a6 >P $a86a $09 >M $a86b $aa >M $a86c $e2 >M
T{ op PC S A X Y P -> $a86c $3a $be $5a $0a $a4 }T T{ $a86a M $a86b M $a86c M -> $09 $aa $e2 }T
$473c >PC $9c >S $d2 >A $50 >X $13 >Y $24 >P $473c $09 >M $473d $a9 >M $473e $42 >M
T{ op PC S A X Y P -> $473e $9c $fb $50 $13 $a4 }T T{ $473c M $473d M $473e M -> $09 $a9 $42 }T
$e334 >PC $5a >S $e2 >A $88 >X $17 >Y $66 >P $e334 $09 >M $e335 $76 >M $e336 $c8 >M
T{ op PC S A X Y P -> $e336 $5a $f6 $88 $17 $e4 }T T{ $e334 M $e335 M $e336 M -> $09 $76 $c8 }T
$c359 >PC $4b >S $c0 >A $5b >X $05 >Y $65 >P $c359 $09 >M $c35a $66 >M $c35b $1c >M
T{ op PC S A X Y P -> $c35b $4b $e6 $5b $05 $e5 }T T{ $c359 M $c35a M $c35b M -> $09 $66 $1c }T
$1e2d >PC $dc >S $4b >A $63 >X $c5 >Y $27 >P $1e2d $09 >M $1e2e $a5 >M $1e2f $0b >M
T{ op PC S A X Y P -> $1e2f $dc $ef $63 $c5 $a5 }T T{ $1e2d M $1e2e M $1e2f M -> $09 $a5 $0b }T
$6c96 >PC $d4 >S $a4 >A $27 >X $08 >Y $64 >P $6c96 $09 >M $6c97 $de >M $6c98 $1e >M
T{ op PC S A X Y P -> $6c98 $d4 $fe $27 $08 $e4 }T T{ $6c96 M $6c97 M $6c98 M -> $09 $de $1e }T
( 0a )
$43b3 >PC $f8 >S $9b >A $41 >X $1b >Y $aa >P $43b3 $0a >M $43b4 $7b >M $43b5 $be >M
T{ op PC S A X Y P -> $43b4 $f8 $36 $41 $1b $29 }T T{ $43b3 M $43b4 M $43b5 M -> $0a $7b $be }T
$01f7 >PC $34 >S $9f >A $e8 >X $d8 >Y $a0 >P $01f7 $0a >M $01f8 $37 >M $01f9 $d1 >M
T{ op PC S A X Y P -> $01f8 $34 $3e $e8 $d8 $21 }T T{ $01f7 M $01f8 M $01f9 M -> $0a $37 $d1 }T
$1d6f >PC $cc >S $a4 >A $72 >X $b9 >Y $6e >P $1d6f $0a >M $1d70 $44 >M $1d71 $3d >M
T{ op PC S A X Y P -> $1d70 $cc $48 $72 $b9 $6d }T T{ $1d6f M $1d70 M $1d71 M -> $0a $44 $3d }T
$dbf4 >PC $56 >S $cc >A $45 >X $29 >Y $6c >P $dbf4 $0a >M $dbf5 $ed >M $dbf6 $a5 >M
T{ op PC S A X Y P -> $dbf5 $56 $98 $45 $29 $ed }T T{ $dbf4 M $dbf5 M $dbf6 M -> $0a $ed $a5 }T
$4c13 >PC $7d >S $93 >A $95 >X $68 >Y $27 >P $4c13 $0a >M $4c14 $79 >M $4c15 $de >M
T{ op PC S A X Y P -> $4c14 $7d $26 $95 $68 $25 }T T{ $4c13 M $4c14 M $4c15 M -> $0a $79 $de }T
$1b9d >PC $c5 >S $21 >A $48 >X $84 >Y $29 >P $1b9d $0a >M $1b9e $51 >M $1b9f $16 >M
T{ op PC S A X Y P -> $1b9e $c5 $42 $48 $84 $28 }T T{ $1b9d M $1b9e M $1b9f M -> $0a $51 $16 }T
$d81d >PC $9f >S $a3 >A $5a >X $d8 >Y $a2 >P $d81d $0a >M $d81e $3d >M $d81f $20 >M
T{ op PC S A X Y P -> $d81e $9f $46 $5a $d8 $21 }T T{ $d81d M $d81e M $d81f M -> $0a $3d $20 }T
$d0e0 >PC $3b >S $a6 >A $a6 >X $50 >Y $2f >P $d0e0 $0a >M $d0e1 $9d >M $d0e2 $ce >M
T{ op PC S A X Y P -> $d0e1 $3b $4c $a6 $50 $2d }T T{ $d0e0 M $d0e1 M $d0e2 M -> $0a $9d $ce }T
$289c >PC $d8 >S $e2 >A $e0 >X $b6 >Y $64 >P $289c $0a >M $289d $04 >M $289e $cc >M
T{ op PC S A X Y P -> $289d $d8 $c4 $e0 $b6 $e5 }T T{ $289c M $289d M $289e M -> $0a $04 $cc }T
$8bdc >PC $37 >S $30 >A $95 >X $63 >Y $6e >P $8bdc $0a >M $8bdd $0f >M $8bde $80 >M
T{ op PC S A X Y P -> $8bdd $37 $60 $95 $63 $6c }T T{ $8bdc M $8bdd M $8bde M -> $0a $0f $80 }T
$9eac >PC $c0 >S $6c >A $ec >X $56 >Y $2b >P $9eac $0a >M $9ead $ae >M $9eae $26 >M
T{ op PC S A X Y P -> $9ead $c0 $d8 $ec $56 $a8 }T T{ $9eac M $9ead M $9eae M -> $0a $ae $26 }T
$24b4 >PC $20 >S $97 >A $a5 >X $54 >Y $e5 >P $24b4 $0a >M $24b5 $c0 >M $24b6 $4c >M
T{ op PC S A X Y P -> $24b5 $20 $2e $a5 $54 $65 }T T{ $24b4 M $24b5 M $24b6 M -> $0a $c0 $4c }T
$5b28 >PC $39 >S $66 >A $fc >X $d0 >Y $a8 >P $5b28 $0a >M $5b29 $01 >M $5b2a $2e >M
T{ op PC S A X Y P -> $5b29 $39 $cc $fc $d0 $a8 }T T{ $5b28 M $5b29 M $5b2a M -> $0a $01 $2e }T
$ee22 >PC $76 >S $42 >A $7b >X $0c >Y $e2 >P $ee22 $0a >M $ee23 $ac >M $ee24 $8a >M
T{ op PC S A X Y P -> $ee23 $76 $84 $7b $0c $e0 }T T{ $ee22 M $ee23 M $ee24 M -> $0a $ac $8a }T
$d4af >PC $6b >S $6e >A $5c >X $c2 >Y $e0 >P $d4af $0a >M $d4b0 $05 >M $d4b1 $b8 >M
T{ op PC S A X Y P -> $d4b0 $6b $dc $5c $c2 $e0 }T T{ $d4af M $d4b0 M $d4b1 M -> $0a $05 $b8 }T
$5e34 >PC $f3 >S $b2 >A $fa >X $45 >Y $6b >P $5e34 $0a >M $5e35 $98 >M $5e36 $b9 >M
T{ op PC S A X Y P -> $5e35 $f3 $64 $fa $45 $69 }T T{ $5e34 M $5e35 M $5e36 M -> $0a $98 $b9 }T
( 0b )
$6f09 >PC $99 >S $75 >A $77 >X $30 >Y $26 >P $6f09 $0b >M $6f0a $be >M $6f0b $6d >M
T{ op PC S A X Y P -> $6f0a $99 $75 $77 $30 $26 }T T{ $6f09 M $6f0a M $6f0b M -> $0b $be $6d }T
$3db7 >PC $46 >S $40 >A $87 >X $c6 >Y $6c >P $3db7 $0b >M $3db8 $19 >M $3db9 $b2 >M
T{ op PC S A X Y P -> $3db8 $46 $40 $87 $c6 $6c }T T{ $3db7 M $3db8 M $3db9 M -> $0b $19 $b2 }T
$e67c >PC $30 >S $f5 >A $07 >X $21 >Y $6a >P $e67c $0b >M $e67d $6a >M $e67e $60 >M
T{ op PC S A X Y P -> $e67d $30 $f5 $07 $21 $6a }T T{ $e67c M $e67d M $e67e M -> $0b $6a $60 }T
$f323 >PC $7a >S $4b >A $fd >X $09 >Y $a2 >P $f323 $0b >M $f324 $76 >M $f325 $d8 >M
T{ op PC S A X Y P -> $f324 $7a $4b $fd $09 $a2 }T T{ $f323 M $f324 M $f325 M -> $0b $76 $d8 }T
$3156 >PC $a5 >S $17 >A $cd >X $df >Y $60 >P $3156 $0b >M $3157 $8f >M $3158 $e3 >M
T{ op PC S A X Y P -> $3157 $a5 $17 $cd $df $60 }T T{ $3156 M $3157 M $3158 M -> $0b $8f $e3 }T
$c87b >PC $cf >S $ce >A $31 >X $57 >Y $60 >P $c87b $0b >M $c87c $61 >M $c87d $a3 >M
T{ op PC S A X Y P -> $c87c $cf $ce $31 $57 $60 }T T{ $c87b M $c87c M $c87d M -> $0b $61 $a3 }T
$85ce >PC $5b >S $c4 >A $6f >X $e3 >Y $63 >P $85ce $0b >M $85cf $0f >M $85d0 $c7 >M
T{ op PC S A X Y P -> $85cf $5b $c4 $6f $e3 $63 }T T{ $85ce M $85cf M $85d0 M -> $0b $0f $c7 }T
$fab8 >PC $47 >S $94 >A $f6 >X $76 >Y $aa >P $fab8 $0b >M $fab9 $c0 >M $faba $b0 >M
T{ op PC S A X Y P -> $fab9 $47 $94 $f6 $76 $aa }T T{ $fab8 M $fab9 M $faba M -> $0b $c0 $b0 }T
$0baa >PC $00 >S $58 >A $8a >X $ba >Y $2c >P $0baa $0b >M $0bab $1f >M $0bac $09 >M
T{ op PC S A X Y P -> $0bab $00 $58 $8a $ba $2c }T T{ $0baa M $0bab M $0bac M -> $0b $1f $09 }T
$f314 >PC $c0 >S $4f >A $89 >X $9a >Y $22 >P $f314 $0b >M $f315 $2d >M $f316 $78 >M
T{ op PC S A X Y P -> $f315 $c0 $4f $89 $9a $22 }T T{ $f314 M $f315 M $f316 M -> $0b $2d $78 }T
$d67f >PC $6d >S $da >A $ec >X $c0 >Y $e1 >P $d67f $0b >M $d680 $f3 >M $d681 $05 >M
T{ op PC S A X Y P -> $d680 $6d $da $ec $c0 $e1 }T T{ $d67f M $d680 M $d681 M -> $0b $f3 $05 }T
$800e >PC $f1 >S $33 >A $f9 >X $e5 >Y $6b >P $800e $0b >M $800f $7f >M $8010 $17 >M
T{ op PC S A X Y P -> $800f $f1 $33 $f9 $e5 $6b }T T{ $800e M $800f M $8010 M -> $0b $7f $17 }T
$2a8c >PC $bc >S $c2 >A $9e >X $2f >Y $2a >P $2a8c $0b >M $2a8d $2d >M $2a8e $fe >M
T{ op PC S A X Y P -> $2a8d $bc $c2 $9e $2f $2a }T T{ $2a8c M $2a8d M $2a8e M -> $0b $2d $fe }T
$984a >PC $ee >S $2f >A $04 >X $fc >Y $ad >P $984a $0b >M $984b $a1 >M $984c $28 >M
T{ op PC S A X Y P -> $984b $ee $2f $04 $fc $ad }T T{ $984a M $984b M $984c M -> $0b $a1 $28 }T
$64a9 >PC $88 >S $76 >A $a8 >X $88 >Y $ac >P $64a9 $0b >M $64aa $16 >M $64ab $12 >M
T{ op PC S A X Y P -> $64aa $88 $76 $a8 $88 $ac }T T{ $64a9 M $64aa M $64ab M -> $0b $16 $12 }T
$902d >PC $b0 >S $f8 >A $92 >X $3f >Y $ac >P $902d $0b >M $902e $a3 >M $902f $e8 >M
T{ op PC S A X Y P -> $902e $b0 $f8 $92 $3f $ac }T T{ $902d M $902e M $902f M -> $0b $a3 $e8 }T
( 0c )
$7bbd >PC $53 >S $2f >A $d0 >X $b5 >Y $6b >P $7bbd $0c >M $7bbe $0a >M $7bbf $ce >M $7bc0 $40 >M $ce0a $35 >M
T{ op PC S A X Y P -> $7bc0 $53 $2f $d0 $b5 $69 }T T{ $7bbd M $7bbe M $7bbf M $7bc0 M $ce0a M -> $0c $0a $ce $40 $3f }T
$2ebe >PC $5d >S $ec >A $a0 >X $41 >Y $20 >P $2ebe $0c >M $2ebf $54 >M $2ec0 $95 >M $2ec1 $b6 >M $9554 $e5 >M
T{ op PC S A X Y P -> $2ec1 $5d $ec $a0 $41 $20 }T T{ $2ebe M $2ebf M $2ec0 M $2ec1 M $9554 M -> $0c $54 $95 $b6 $ed }T
$42d9 >PC $55 >S $ad >A $e2 >X $96 >Y $ae >P $42d9 $0c >M $42da $a6 >M $42db $af >M $42dc $74 >M $afa6 $50 >M
T{ op PC S A X Y P -> $42dc $55 $ad $e2 $96 $ae }T T{ $42d9 M $42da M $42db M $42dc M $afa6 M -> $0c $a6 $af $74 $fd }T
$c815 >PC $82 >S $fa >A $2a >X $d1 >Y $a0 >P $3b61 $e4 >M $c815 $0c >M $c816 $61 >M $c817 $3b >M $c818 $6c >M
T{ op PC S A X Y P -> $c818 $82 $fa $2a $d1 $a0 }T T{ $3b61 M $c815 M $c816 M $c817 M $c818 M -> $fe $0c $61 $3b $6c }T
$0518 >PC $1d >S $1c >A $2a >X $dd >Y $6e >P $0518 $0c >M $0519 $96 >M $051a $14 >M $051b $c4 >M $1496 $e3 >M
T{ op PC S A X Y P -> $051b $1d $1c $2a $dd $6e }T T{ $0518 M $0519 M $051a M $051b M $1496 M -> $0c $96 $14 $c4 $ff }T
$4e02 >PC $d0 >S $84 >A $fd >X $0b >Y $20 >P $2ef8 $17 >M $4e02 $0c >M $4e03 $f8 >M $4e04 $2e >M $4e05 $ec >M
T{ op PC S A X Y P -> $4e05 $d0 $84 $fd $0b $20 }T T{ $2ef8 M $4e02 M $4e03 M $4e04 M $4e05 M -> $97 $0c $f8 $2e $ec }T
$05d5 >PC $5e >S $74 >A $fa >X $05 >Y $63 >P $05d5 $0c >M $05d6 $b0 >M $05d7 $15 >M $05d8 $f0 >M $15b0 $46 >M
T{ op PC S A X Y P -> $05d8 $5e $74 $fa $05 $61 }T T{ $05d5 M $05d6 M $05d7 M $05d8 M $15b0 M -> $0c $b0 $15 $f0 $76 }T
$fc47 >PC $c7 >S $c0 >A $40 >X $de >Y $ec >P $44ae $7c >M $fc47 $0c >M $fc48 $ae >M $fc49 $44 >M $fc4a $09 >M
T{ op PC S A X Y P -> $fc4a $c7 $c0 $40 $de $ec }T T{ $44ae M $fc47 M $fc48 M $fc49 M $fc4a M -> $fc $0c $ae $44 $09 }T
$37fb >PC $6c >S $8a >A $66 >X $00 >Y $66 >P $297f $d4 >M $37fb $0c >M $37fc $7f >M $37fd $29 >M $37fe $69 >M
T{ op PC S A X Y P -> $37fe $6c $8a $66 $00 $64 }T T{ $297f M $37fb M $37fc M $37fd M $37fe M -> $de $0c $7f $29 $69 }T
$87ef >PC $9e >S $09 >A $79 >X $59 >Y $ed >P $40b7 $20 >M $87ef $0c >M $87f0 $b7 >M $87f1 $40 >M $87f2 $33 >M
T{ op PC S A X Y P -> $87f2 $9e $09 $79 $59 $ef }T T{ $40b7 M $87ef M $87f0 M $87f1 M $87f2 M -> $29 $0c $b7 $40 $33 }T
$37f4 >PC $39 >S $42 >A $c4 >X $70 >Y $e6 >P $37f4 $0c >M $37f5 $4b >M $37f6 $99 >M $37f7 $b1 >M $994b $46 >M
T{ op PC S A X Y P -> $37f7 $39 $42 $c4 $70 $e4 }T T{ $37f4 M $37f5 M $37f6 M $37f7 M $994b M -> $0c $4b $99 $b1 $46 }T
$4b3c >PC $2d >S $30 >A $c5 >X $08 >Y $e9 >P $4b3c $0c >M $4b3d $58 >M $4b3e $c9 >M $4b3f $25 >M $c958 $5c >M
T{ op PC S A X Y P -> $4b3f $2d $30 $c5 $08 $e9 }T T{ $4b3c M $4b3d M $4b3e M $4b3f M $c958 M -> $0c $58 $c9 $25 $7c }T
$797a >PC $0b >S $c7 >A $94 >X $de >Y $27 >P $797a $0c >M $797b $d0 >M $797c $9f >M $797d $9b >M $9fd0 $d4 >M
T{ op PC S A X Y P -> $797d $0b $c7 $94 $de $25 }T T{ $797a M $797b M $797c M $797d M $9fd0 M -> $0c $d0 $9f $9b $d7 }T
$3ec7 >PC $15 >S $59 >A $dc >X $86 >Y $e1 >P $193c $06 >M $3ec7 $0c >M $3ec8 $3c >M $3ec9 $19 >M $3eca $94 >M
T{ op PC S A X Y P -> $3eca $15 $59 $dc $86 $e3 }T T{ $193c M $3ec7 M $3ec8 M $3ec9 M $3eca M -> $5f $0c $3c $19 $94 }T
$5caf >PC $f0 >S $62 >A $f0 >X $59 >Y $ab >P $5caf $0c >M $5cb0 $26 >M $5cb1 $6b >M $5cb2 $95 >M $6b26 $eb >M
T{ op PC S A X Y P -> $5cb2 $f0 $62 $f0 $59 $a9 }T T{ $5caf M $5cb0 M $5cb1 M $5cb2 M $6b26 M -> $0c $26 $6b $95 $eb }T
$9cdb >PC $64 >S $a9 >A $ae >X $b2 >Y $aa >P $092c $53 >M $9cdb $0c >M $9cdc $2c >M $9cdd $09 >M $9cde $56 >M
T{ op PC S A X Y P -> $9cde $64 $a9 $ae $b2 $a8 }T T{ $092c M $9cdb M $9cdc M $9cdd M $9cde M -> $fb $0c $2c $09 $56 }T
( 0d )
$46db >PC $8f >S $8e >A $8d >X $0d >Y $a9 >P $46db $0d >M $46dc $09 >M $46dd $f8 >M $46de $d4 >M $f809 $c6 >M
T{ op PC S A X Y P -> $46de $8f $ce $8d $0d $a9 }T T{ $46db M $46dc M $46dd M $46de M $f809 M -> $0d $09 $f8 $d4 $c6 }T
$98a0 >PC $23 >S $81 >A $53 >X $2c >Y $a1 >P $13db $e3 >M $98a0 $0d >M $98a1 $db >M $98a2 $13 >M $98a3 $85 >M
T{ op PC S A X Y P -> $98a3 $23 $e3 $53 $2c $a1 }T T{ $13db M $98a0 M $98a1 M $98a2 M $98a3 M -> $e3 $0d $db $13 $85 }T
$1828 >PC $59 >S $22 >A $30 >X $dd >Y $6f >P $1828 $0d >M $1829 $48 >M $182a $30 >M $182b $70 >M $3048 $19 >M
T{ op PC S A X Y P -> $182b $59 $3b $30 $dd $6d }T T{ $1828 M $1829 M $182a M $182b M $3048 M -> $0d $48 $30 $70 $19 }T
$62ba >PC $2c >S $80 >A $af >X $5a >Y $e3 >P $2d71 $8a >M $62ba $0d >M $62bb $71 >M $62bc $2d >M $62bd $e9 >M
T{ op PC S A X Y P -> $62bd $2c $8a $af $5a $e1 }T T{ $2d71 M $62ba M $62bb M $62bc M $62bd M -> $8a $0d $71 $2d $e9 }T
$dffe >PC $4b >S $28 >A $b3 >X $fe >Y $6c >P $dffe $0d >M $dfff $12 >M $e000 $f1 >M $e001 $05 >M $f112 $fd >M
T{ op PC S A X Y P -> $e001 $4b $fd $b3 $fe $ec }T T{ $dffe M $dfff M $e000 M $e001 M $f112 M -> $0d $12 $f1 $05 $fd }T
$7ac0 >PC $5b >S $46 >A $75 >X $2a >Y $2d >P $7ac0 $0d >M $7ac1 $e2 >M $7ac2 $ca >M $7ac3 $6c >M $cae2 $08 >M
T{ op PC S A X Y P -> $7ac3 $5b $4e $75 $2a $2d }T T{ $7ac0 M $7ac1 M $7ac2 M $7ac3 M $cae2 M -> $0d $e2 $ca $6c $08 }T
$e76b >PC $09 >S $84 >A $a1 >X $f5 >Y $2c >P $1368 $96 >M $e76b $0d >M $e76c $68 >M $e76d $13 >M $e76e $16 >M
T{ op PC S A X Y P -> $e76e $09 $96 $a1 $f5 $ac }T T{ $1368 M $e76b M $e76c M $e76d M $e76e M -> $96 $0d $68 $13 $16 }T
$7149 >PC $26 >S $ba >A $ce >X $68 >Y $ae >P $7149 $0d >M $714a $1b >M $714b $f8 >M $714c $bf >M $f81b $c9 >M
T{ op PC S A X Y P -> $714c $26 $fb $ce $68 $ac }T T{ $7149 M $714a M $714b M $714c M $f81b M -> $0d $1b $f8 $bf $c9 }T
$d9d9 >PC $97 >S $78 >A $3e >X $2c >Y $a8 >P $14a6 $ee >M $d9d9 $0d >M $d9da $a6 >M $d9db $14 >M $d9dc $4c >M
T{ op PC S A X Y P -> $d9dc $97 $fe $3e $2c $a8 }T T{ $14a6 M $d9d9 M $d9da M $d9db M $d9dc M -> $ee $0d $a6 $14 $4c }T
$9673 >PC $ab >S $9b >A $05 >X $96 >Y $a8 >P $9673 $0d >M $9674 $3f >M $9675 $f8 >M $9676 $3d >M $f83f $f3 >M
T{ op PC S A X Y P -> $9676 $ab $fb $05 $96 $a8 }T T{ $9673 M $9674 M $9675 M $9676 M $f83f M -> $0d $3f $f8 $3d $f3 }T
$3bd9 >PC $af >S $26 >A $21 >X $3f >Y $a0 >P $3bd9 $0d >M $3bda $83 >M $3bdb $71 >M $3bdc $d7 >M $7183 $49 >M
T{ op PC S A X Y P -> $3bdc $af $6f $21 $3f $20 }T T{ $3bd9 M $3bda M $3bdb M $3bdc M $7183 M -> $0d $83 $71 $d7 $49 }T
$5cf6 >PC $06 >S $74 >A $92 >X $03 >Y $a0 >P $5cf6 $0d >M $5cf7 $65 >M $5cf8 $90 >M $5cf9 $43 >M $9065 $e3 >M
T{ op PC S A X Y P -> $5cf9 $06 $f7 $92 $03 $a0 }T T{ $5cf6 M $5cf7 M $5cf8 M $5cf9 M $9065 M -> $0d $65 $90 $43 $e3 }T
$0197 >PC $38 >S $7f >A $43 >X $76 >Y $ac >P $0197 $0d >M $0198 $a1 >M $0199 $6d >M $019a $f0 >M $6da1 $c6 >M
T{ op PC S A X Y P -> $019a $38 $ff $43 $76 $ac }T T{ $0197 M $0198 M $0199 M $019a M $6da1 M -> $0d $a1 $6d $f0 $c6 }T
$ac1f >PC $c9 >S $e3 >A $59 >X $88 >Y $29 >P $a30d $7d >M $ac1f $0d >M $ac20 $0d >M $ac21 $a3 >M $ac22 $7a >M
T{ op PC S A X Y P -> $ac22 $c9 $ff $59 $88 $a9 }T T{ $a30d M $ac1f M $ac20 M $ac21 M $ac22 M -> $7d $0d $0d $a3 $7a }T
$65e8 >PC $ff >S $7b >A $49 >X $b0 >Y $aa >P $65e8 $0d >M $65e9 $67 >M $65ea $d2 >M $65eb $27 >M $d267 $7e >M
T{ op PC S A X Y P -> $65eb $ff $7f $49 $b0 $28 }T T{ $65e8 M $65e9 M $65ea M $65eb M $d267 M -> $0d $67 $d2 $27 $7e }T
$3035 >PC $58 >S $c7 >A $72 >X $6c >Y $63 >P $3035 $0d >M $3036 $78 >M $3037 $69 >M $3038 $30 >M $6978 $f3 >M
T{ op PC S A X Y P -> $3038 $58 $f7 $72 $6c $e1 }T T{ $3035 M $3036 M $3037 M $3038 M $6978 M -> $0d $78 $69 $30 $f3 }T
( 0e )
$2340 >PC $76 >S $21 >A $c7 >X $83 >Y $26 >P $2340 $0e >M $2341 $24 >M $2342 $8e >M $2343 $b9 >M $8e24 $d2 >M
T{ op PC S A X Y P -> $2343 $76 $21 $c7 $83 $a5 }T T{ $2340 M $2341 M $2342 M $2343 M $8e24 M -> $0e $24 $8e $b9 $a4 }T
$1d43 >PC $e2 >S $5c >A $7c >X $a3 >Y $ed >P $1d43 $0e >M $1d44 $5f >M $1d45 $d8 >M $1d46 $c3 >M $d85f $f3 >M
T{ op PC S A X Y P -> $1d46 $e2 $5c $7c $a3 $ed }T T{ $1d43 M $1d44 M $1d45 M $1d46 M $d85f M -> $0e $5f $d8 $c3 $e6 }T
$883e >PC $26 >S $e8 >A $09 >X $e3 >Y $6c >P $0cde $46 >M $883e $0e >M $883f $de >M $8840 $0c >M $8841 $b0 >M
T{ op PC S A X Y P -> $8841 $26 $e8 $09 $e3 $ec }T T{ $0cde M $883e M $883f M $8840 M $8841 M -> $8c $0e $de $0c $b0 }T
$d489 >PC $e2 >S $a4 >A $0e >X $04 >Y $ed >P $8e86 $59 >M $d489 $0e >M $d48a $86 >M $d48b $8e >M $d48c $a4 >M
T{ op PC S A X Y P -> $d48c $e2 $a4 $0e $04 $ec }T T{ $8e86 M $d489 M $d48a M $d48b M $d48c M -> $b2 $0e $86 $8e $a4 }T
$f133 >PC $3f >S $ee >A $f0 >X $9b >Y $29 >P $c215 $c8 >M $f133 $0e >M $f134 $15 >M $f135 $c2 >M $f136 $28 >M
T{ op PC S A X Y P -> $f136 $3f $ee $f0 $9b $a9 }T T{ $c215 M $f133 M $f134 M $f135 M $f136 M -> $90 $0e $15 $c2 $28 }T
$da8d >PC $38 >S $61 >A $f1 >X $9b >Y $22 >P $8886 $43 >M $da8d $0e >M $da8e $86 >M $da8f $88 >M $da90 $bd >M
T{ op PC S A X Y P -> $da90 $38 $61 $f1 $9b $a0 }T T{ $8886 M $da8d M $da8e M $da8f M $da90 M -> $86 $0e $86 $88 $bd }T
$0142 >PC $9f >S $56 >A $fa >X $e5 >Y $e2 >P $0142 $0e >M $0143 $3b >M $0144 $20 >M $0145 $b9 >M $203b $f9 >M
T{ op PC S A X Y P -> $0145 $9f $56 $fa $e5 $e1 }T T{ $0142 M $0143 M $0144 M $0145 M $203b M -> $0e $3b $20 $b9 $f2 }T
$b123 >PC $0b >S $d0 >A $f9 >X $d1 >Y $a8 >P $8d34 $70 >M $b123 $0e >M $b124 $34 >M $b125 $8d >M $b126 $87 >M
T{ op PC S A X Y P -> $b126 $0b $d0 $f9 $d1 $a8 }T T{ $8d34 M $b123 M $b124 M $b125 M $b126 M -> $e0 $0e $34 $8d $87 }T
$18ce >PC $20 >S $d7 >A $e3 >X $ca >Y $68 >P $18ce $0e >M $18cf $3b >M $18d0 $a7 >M $18d1 $57 >M $a73b $c0 >M
T{ op PC S A X Y P -> $18d1 $20 $d7 $e3 $ca $e9 }T T{ $18ce M $18cf M $18d0 M $18d1 M $a73b M -> $0e $3b $a7 $57 $80 }T
$7472 >PC $94 >S $9f >A $91 >X $e8 >Y $af >P $2c4c $34 >M $7472 $0e >M $7473 $4c >M $7474 $2c >M $7475 $ac >M
T{ op PC S A X Y P -> $7475 $94 $9f $91 $e8 $2c }T T{ $2c4c M $7472 M $7473 M $7474 M $7475 M -> $68 $0e $4c $2c $ac }T
$0f0c >PC $ff >S $5e >A $07 >X $61 >Y $a5 >P $0f0c $0e >M $0f0d $76 >M $0f0e $58 >M $0f0f $5e >M $5876 $09 >M
T{ op PC S A X Y P -> $0f0f $ff $5e $07 $61 $24 }T T{ $0f0c M $0f0d M $0f0e M $0f0f M $5876 M -> $0e $76 $58 $5e $12 }T
$42de >PC $88 >S $ed >A $30 >X $dc >Y $e5 >P $42de $0e >M $42df $61 >M $42e0 $e4 >M $42e1 $1a >M $e461 $d1 >M
T{ op PC S A X Y P -> $42e1 $88 $ed $30 $dc $e5 }T T{ $42de M $42df M $42e0 M $42e1 M $e461 M -> $0e $61 $e4 $1a $a2 }T
$8def >PC $d8 >S $11 >A $9e >X $c9 >Y $2d >P $8def $0e >M $8df0 $ed >M $8df1 $93 >M $8df2 $d6 >M $93ed $80 >M
T{ op PC S A X Y P -> $8df2 $d8 $11 $9e $c9 $2f }T T{ $8def M $8df0 M $8df1 M $8df2 M $93ed M -> $0e $ed $93 $d6 $00 }T
$3b4c >PC $6a >S $fc >A $39 >X $e7 >Y $29 >P $2dc5 $b0 >M $3b4c $0e >M $3b4d $c5 >M $3b4e $2d >M $3b4f $a2 >M
T{ op PC S A X Y P -> $3b4f $6a $fc $39 $e7 $29 }T T{ $2dc5 M $3b4c M $3b4d M $3b4e M $3b4f M -> $60 $0e $c5 $2d $a2 }T
$e537 >PC $4e >S $c0 >A $3a >X $0e >Y $ec >P $1a5c $fa >M $e537 $0e >M $e538 $5c >M $e539 $1a >M $e53a $b0 >M
T{ op PC S A X Y P -> $e53a $4e $c0 $3a $0e $ed }T T{ $1a5c M $e537 M $e538 M $e539 M $e53a M -> $f4 $0e $5c $1a $b0 }T
$72a2 >PC $3c >S $b7 >A $02 >X $6b >Y $60 >P $543b $50 >M $72a2 $0e >M $72a3 $3b >M $72a4 $54 >M $72a5 $82 >M
T{ op PC S A X Y P -> $72a5 $3c $b7 $02 $6b $e0 }T T{ $543b M $72a2 M $72a3 M $72a4 M $72a5 M -> $a0 $0e $3b $54 $82 }T
( 0f )
$9a2f >PC $d5 >S $4d >A $f1 >X $e8 >Y $e5 >P $00c7 $c5 >M $9a13 $6b >M $9a2f $0f >M $9a30 $c7 >M $9a31 $e1 >M $9a32 $e3 >M
T{ op PC S A X Y P -> $9a32 $d5 $4d $f1 $e8 $e5 }T T{ $00c7 M $9a13 M $9a2f M $9a30 M $9a31 M $9a32 M -> $c5 $6b $0f $c7 $e1 $e3 }T
$9bce >PC $fa >S $9c >A $5b >X $f9 >Y $2e >P $0042 $ad >M $9b62 $a5 >M $9bce $0f >M $9bcf $42 >M $9bd0 $91 >M $9bd1 $5e >M
T{ op PC S A X Y P -> $9bd1 $fa $9c $5b $f9 $2e }T T{ $0042 M $9b62 M $9bce M $9bcf M $9bd0 M $9bd1 M -> $ad $a5 $0f $42 $91 $5e }T
$279c >PC $11 >S $46 >A $ba >X $c8 >Y $e0 >P $0025 $ac >M $279c $0f >M $279d $25 >M $279e $0f >M $27ae $a5 >M
T{ op PC S A X Y P -> $27ae $11 $46 $ba $c8 $e0 }T T{ $0025 M $279c M $279d M $279e M $27ae M -> $ac $0f $25 $0f $a5 }T
$a076 >PC $3a >S $6d >A $37 >X $44 >Y $27 >P $00da $fe >M $a038 $d8 >M $a076 $0f >M $a077 $da >M $a078 $bf >M
T{ op PC S A X Y P -> $a038 $3a $6d $37 $44 $27 }T T{ $00da M $a038 M $a076 M $a077 M $a078 M -> $fe $d8 $0f $da $bf }T
$6b11 >PC $e5 >S $37 >A $7b >X $0b >Y $60 >P $0055 $52 >M $6b11 $0f >M $6b12 $55 >M $6b13 $67 >M $6b7b $0b >M
T{ op PC S A X Y P -> $6b7b $e5 $37 $7b $0b $60 }T T{ $0055 M $6b11 M $6b12 M $6b13 M $6b7b M -> $52 $0f $55 $67 $0b }T
$108a >PC $90 >S $57 >A $c7 >X $b0 >Y $20 >P $003c $b6 >M $108a $0f >M $108b $3c >M $108c $1c >M $10a9 $10 >M
T{ op PC S A X Y P -> $10a9 $90 $57 $c7 $b0 $20 }T T{ $003c M $108a M $108b M $108c M $10a9 M -> $b6 $0f $3c $1c $10 }T
$0833 >PC $db >S $f9 >A $97 >X $59 >Y $67 >P $001e $a2 >M $0812 $cc >M $0833 $0f >M $0834 $1e >M $0835 $dc >M
T{ op PC S A X Y P -> $0812 $db $f9 $97 $59 $67 }T T{ $001e M $0812 M $0833 M $0834 M $0835 M -> $a2 $cc $0f $1e $dc }T
$55cf >PC $17 >S $b2 >A $e9 >X $a5 >Y $a4 >P $0062 $91 >M $55cf $0f >M $55d0 $62 >M $55d1 $20 >M $55d2 $ea >M $55f2 $5b >M
T{ op PC S A X Y P -> $55d2 $17 $b2 $e9 $a5 $a4 }T T{ $0062 M $55cf M $55d0 M $55d1 M $55d2 M $55f2 M -> $91 $0f $62 $20 $ea $5b }T
$f690 >PC $ec >S $5a >A $3d >X $48 >Y $23 >P $000a $26 >M $f690 $0f >M $f691 $0a >M $f692 $32 >M $f6c5 $96 >M
T{ op PC S A X Y P -> $f6c5 $ec $5a $3d $48 $23 }T T{ $000a M $f690 M $f691 M $f692 M $f6c5 M -> $26 $0f $0a $32 $96 }T
$e8c3 >PC $7d >S $95 >A $c8 >X $79 >Y $6f >P $0031 $e4 >M $e8c3 $0f >M $e8c4 $31 >M $e8c5 $30 >M $e8f6 $92 >M
T{ op PC S A X Y P -> $e8f6 $7d $95 $c8 $79 $6f }T T{ $0031 M $e8c3 M $e8c4 M $e8c5 M $e8f6 M -> $e4 $0f $31 $30 $92 }T
$29ec >PC $23 >S $91 >A $ec >X $7f >Y $e3 >P $007c $84 >M $29d7 $8b >M $29ec $0f >M $29ed $7c >M $29ee $e8 >M
T{ op PC S A X Y P -> $29d7 $23 $91 $ec $7f $e3 }T T{ $007c M $29d7 M $29ec M $29ed M $29ee M -> $84 $8b $0f $7c $e8 }T
$8454 >PC $11 >S $46 >A $bf >X $fb >Y $a0 >P $00d7 $da >M $8435 $4b >M $8454 $0f >M $8455 $d7 >M $8456 $de >M
T{ op PC S A X Y P -> $8435 $11 $46 $bf $fb $a0 }T T{ $00d7 M $8435 M $8454 M $8455 M $8456 M -> $da $4b $0f $d7 $de }T
$2cea >PC $15 >S $9b >A $4f >X $c5 >Y $ee >P $0072 $97 >M $2ca5 $20 >M $2cea $0f >M $2ceb $72 >M $2cec $b8 >M $2ced $d6 >M
T{ op PC S A X Y P -> $2ced $15 $9b $4f $c5 $ee }T T{ $0072 M $2ca5 M $2cea M $2ceb M $2cec M $2ced M -> $97 $20 $0f $72 $b8 $d6 }T
$333b >PC $01 >S $35 >A $37 >X $fb >Y $e1 >P $0031 $97 >M $333b $0f >M $333c $31 >M $333d $af >M $333e $f6 >M $33ed $0e >M
T{ op PC S A X Y P -> $333e $01 $35 $37 $fb $e1 }T T{ $0031 M $333b M $333c M $333d M $333e M $33ed M -> $97 $0f $31 $af $f6 $0e }T
$d329 >PC $4c >S $15 >A $03 >X $0c >Y $6f >P $0069 $7f >M $d329 $0f >M $d32a $69 >M $d32b $98 >M $d32c $90 >M $d3c4 $50 >M
T{ op PC S A X Y P -> $d32c $4c $15 $03 $0c $6f }T T{ $0069 M $d329 M $d32a M $d32b M $d32c M $d3c4 M -> $7f $0f $69 $98 $90 $50 }T
$be80 >PC $3a >S $62 >A $20 >X $56 >Y $2b >P $00ae $65 >M $be80 $0f >M $be81 $ae >M $be82 $73 >M $be83 $3e >M $bef6 $d4 >M
T{ op PC S A X Y P -> $be83 $3a $62 $20 $56 $2b }T T{ $00ae M $be80 M $be81 M $be82 M $be83 M $bef6 M -> $65 $0f $ae $73 $3e $d4 }T
( 10 )
$599f >PC $cc >S $dd >A $73 >X $9d >Y $22 >P $5987 $6d >M $599f $10 >M $59a0 $e6 >M $59a1 $78 >M
T{ op PC S A X Y P -> $5987 $cc $dd $73 $9d $22 }T T{ $5987 M $599f M $59a0 M $59a1 M -> $6d $10 $e6 $78 }T
$c269 >PC $5b >S $6d >A $30 >X $12 >Y $63 >P $c212 $1d >M $c269 $10 >M $c26a $a7 >M $c26b $41 >M
T{ op PC S A X Y P -> $c212 $5b $6d $30 $12 $63 }T T{ $c212 M $c269 M $c26a M $c26b M -> $1d $10 $a7 $41 }T
$dd67 >PC $d5 >S $37 >A $3a >X $8b >Y $29 >P $dcfe $4a >M $dd67 $10 >M $dd68 $95 >M $dd69 $fd >M $ddfe $bf >M
T{ op PC S A X Y P -> $dcfe $d5 $37 $3a $8b $29 }T T{ $dcfe M $dd67 M $dd68 M $dd69 M $ddfe M -> $4a $10 $95 $fd $bf }T
$7090 >PC $ac >S $08 >A $40 >X $06 >Y $2e >P $7021 $54 >M $7090 $10 >M $7091 $8f >M $7092 $a4 >M
T{ op PC S A X Y P -> $7021 $ac $08 $40 $06 $2e }T T{ $7021 M $7090 M $7091 M $7092 M -> $54 $10 $8f $a4 }T
$d929 >PC $e1 >S $99 >A $59 >X $68 >Y $e5 >P $d929 $10 >M $d92a $8e >M $d92b $94 >M
T{ op PC S A X Y P -> $d92b $e1 $99 $59 $68 $e5 }T T{ $d929 M $d92a M $d92b M -> $10 $8e $94 }T
$4bbb >PC $f5 >S $ae >A $18 >X $89 >Y $e3 >P $4bbb $10 >M $4bbc $91 >M $4bbd $94 >M
T{ op PC S A X Y P -> $4bbd $f5 $ae $18 $89 $e3 }T T{ $4bbb M $4bbc M $4bbd M -> $10 $91 $94 }T
$ddd2 >PC $fc >S $32 >A $11 >X $42 >Y $2e >P $dd53 $48 >M $ddd2 $10 >M $ddd3 $7f >M $ddd4 $74 >M $de53 $1b >M
T{ op PC S A X Y P -> $de53 $fc $32 $11 $42 $2e }T T{ $dd53 M $ddd2 M $ddd3 M $ddd4 M $de53 M -> $48 $10 $7f $74 $1b }T
$082f >PC $57 >S $24 >A $29 >X $9d >Y $ac >P $082f $10 >M $0830 $4e >M $0831 $3f >M
T{ op PC S A X Y P -> $0831 $57 $24 $29 $9d $ac }T T{ $082f M $0830 M $0831 M -> $10 $4e $3f }T
$c517 >PC $49 >S $b0 >A $a8 >X $25 >Y $a3 >P $c517 $10 >M $c518 $87 >M $c519 $1c >M
T{ op PC S A X Y P -> $c519 $49 $b0 $a8 $25 $a3 }T T{ $c517 M $c518 M $c519 M -> $10 $87 $1c }T
$abcd >PC $d3 >S $eb >A $9b >X $6f >Y $e9 >P $abcd $10 >M $abce $eb >M $abcf $d9 >M
T{ op PC S A X Y P -> $abcf $d3 $eb $9b $6f $e9 }T T{ $abcd M $abce M $abcf M -> $10 $eb $d9 }T
$9a9b >PC $67 >S $78 >A $c2 >X $38 >Y $ed >P $9a9b $10 >M $9a9c $ce >M $9a9d $e8 >M
T{ op PC S A X Y P -> $9a9d $67 $78 $c2 $38 $ed }T T{ $9a9b M $9a9c M $9a9d M -> $10 $ce $e8 }T
$8046 >PC $66 >S $07 >A $82 >X $f1 >Y $eb >P $8046 $10 >M $8047 $1f >M $8048 $af >M
T{ op PC S A X Y P -> $8048 $66 $07 $82 $f1 $eb }T T{ $8046 M $8047 M $8048 M -> $10 $1f $af }T
$9fd5 >PC $0f >S $0d >A $60 >X $bd >Y $ad >P $9fd5 $10 >M $9fd6 $02 >M $9fd7 $55 >M
T{ op PC S A X Y P -> $9fd7 $0f $0d $60 $bd $ad }T T{ $9fd5 M $9fd6 M $9fd7 M -> $10 $02 $55 }T
$0e3e >PC $fb >S $1b >A $f1 >X $c8 >Y $25 >P $0e31 $8b >M $0e3e $10 >M $0e3f $f1 >M $0e40 $f3 >M
T{ op PC S A X Y P -> $0e31 $fb $1b $f1 $c8 $25 }T T{ $0e31 M $0e3e M $0e3f M $0e40 M -> $8b $10 $f1 $f3 }T
$3b43 >PC $a8 >S $3d >A $32 >X $38 >Y $a3 >P $3b43 $10 >M $3b44 $10 >M $3b45 $37 >M
T{ op PC S A X Y P -> $3b45 $a8 $3d $32 $38 $a3 }T T{ $3b43 M $3b44 M $3b45 M -> $10 $10 $37 }T
$a502 >PC $68 >S $0f >A $6b >X $4a >Y $65 >P $a502 $10 >M $a503 $fe >M $a504 $65 >M
T{ op PC S A X Y P -> $a502 $68 $0f $6b $4a $65 }T T{ $a502 M $a503 M $a504 M -> $10 $fe $65 }T
( 11 )
$298b >PC $41 >S $78 >A $77 >X $dc >Y $a0 >P $00e4 $4b >M $00e5 $10 >M $1127 $7b >M $298b $11 >M $298c $e4 >M $298d $48 >M
T{ op PC S A X Y P -> $298d $41 $7b $77 $dc $20 }T T{ $00e4 M $00e5 M $1127 M $298b M $298c M $298d M -> $4b $10 $7b $11 $e4 $48 }T
$c910 >PC $8f >S $52 >A $a0 >X $4f >Y $6e >P $0046 $63 >M $0047 $07 >M $07b2 $3a >M $c910 $11 >M $c911 $46 >M $c912 $bd >M
T{ op PC S A X Y P -> $c912 $8f $7a $a0 $4f $6c }T T{ $0046 M $0047 M $07b2 M $c910 M $c911 M $c912 M -> $63 $07 $3a $11 $46 $bd }T
$b48c >PC $b1 >S $cd >A $94 >X $d2 >Y $6d >P $00c6 $0b >M $00c7 $31 >M $31dd $86 >M $b48c $11 >M $b48d $c6 >M $b48e $03 >M
T{ op PC S A X Y P -> $b48e $b1 $cf $94 $d2 $ed }T T{ $00c6 M $00c7 M $31dd M $b48c M $b48d M $b48e M -> $0b $31 $86 $11 $c6 $03 }T
$2287 >PC $51 >S $bc >A $21 >X $3b >Y $2b >P $0010 $00 >M $0011 $85 >M $2287 $11 >M $2288 $10 >M $2289 $42 >M $853b $a8 >M
T{ op PC S A X Y P -> $2289 $51 $bc $21 $3b $a9 }T T{ $0010 M $0011 M $2287 M $2288 M $2289 M $853b M -> $00 $85 $11 $10 $42 $a8 }T
$c3dc >PC $53 >S $53 >A $43 >X $82 >Y $e4 >P $00d4 $68 >M $00d5 $98 >M $98ea $11 >M $c3dc $11 >M $c3dd $d4 >M $c3de $67 >M
T{ op PC S A X Y P -> $c3de $53 $53 $43 $82 $64 }T T{ $00d4 M $00d5 M $98ea M $c3dc M $c3dd M $c3de M -> $68 $98 $11 $11 $d4 $67 }T
$890b >PC $45 >S $9c >A $21 >X $8d >Y $63 >P $00e2 $c8 >M $00e3 $e6 >M $890b $11 >M $890c $e2 >M $890d $7b >M $e755 $3c >M
T{ op PC S A X Y P -> $890d $45 $bc $21 $8d $e1 }T T{ $00e2 M $00e3 M $890b M $890c M $890d M $e755 M -> $c8 $e6 $11 $e2 $7b $3c }T
$f858 >PC $ee >S $40 >A $8b >X $ef >Y $e8 >P $008a $f4 >M $008b $08 >M $09e3 $38 >M $f858 $11 >M $f859 $8a >M $f85a $16 >M
T{ op PC S A X Y P -> $f85a $ee $78 $8b $ef $68 }T T{ $008a M $008b M $09e3 M $f858 M $f859 M $f85a M -> $f4 $08 $38 $11 $8a $16 }T
$329f >PC $0c >S $55 >A $b2 >X $21 >Y $e7 >P $0096 $da >M $0097 $52 >M $329f $11 >M $32a0 $96 >M $32a1 $51 >M $52fb $74 >M
T{ op PC S A X Y P -> $32a1 $0c $75 $b2 $21 $65 }T T{ $0096 M $0097 M $329f M $32a0 M $32a1 M $52fb M -> $da $52 $11 $96 $51 $74 }T
$c9d2 >PC $16 >S $87 >A $a2 >X $50 >Y $a9 >P $003a $31 >M $003b $88 >M $8881 $c9 >M $c9d2 $11 >M $c9d3 $3a >M $c9d4 $db >M
T{ op PC S A X Y P -> $c9d4 $16 $cf $a2 $50 $a9 }T T{ $003a M $003b M $8881 M $c9d2 M $c9d3 M $c9d4 M -> $31 $88 $c9 $11 $3a $db }T
$1837 >PC $0a >S $99 >A $1c >X $6f >Y $6b >P $007d $92 >M $007e $72 >M $1837 $11 >M $1838 $7d >M $1839 $77 >M $7301 $a0 >M
T{ op PC S A X Y P -> $1839 $0a $b9 $1c $6f $e9 }T T{ $007d M $007e M $1837 M $1838 M $1839 M $7301 M -> $92 $72 $11 $7d $77 $a0 }T
$34f2 >PC $d2 >S $f4 >A $cd >X $32 >Y $ec >P $00c3 $7c >M $00c4 $cf >M $34f2 $11 >M $34f3 $c3 >M $34f4 $62 >M $cfae $8a >M
T{ op PC S A X Y P -> $34f4 $d2 $fe $cd $32 $ec }T T{ $00c3 M $00c4 M $34f2 M $34f3 M $34f4 M $cfae M -> $7c $cf $11 $c3 $62 $8a }T
$21c3 >PC $b2 >S $00 >A $6e >X $93 >Y $a5 >P $00c7 $9e >M $00c8 $05 >M $0631 $e2 >M $21c3 $11 >M $21c4 $c7 >M $21c5 $b7 >M
T{ op PC S A X Y P -> $21c5 $b2 $e2 $6e $93 $a5 }T T{ $00c7 M $00c8 M $0631 M $21c3 M $21c4 M $21c5 M -> $9e $05 $e2 $11 $c7 $b7 }T
$f122 >PC $4d >S $07 >A $b7 >X $56 >Y $a4 >P $007c $f8 >M $007d $11 >M $124e $39 >M $f122 $11 >M $f123 $7c >M $f124 $3b >M
T{ op PC S A X Y P -> $f124 $4d $3f $b7 $56 $24 }T T{ $007c M $007d M $124e M $f122 M $f123 M $f124 M -> $f8 $11 $39 $11 $7c $3b }T
$dcbe >PC $43 >S $7a >A $4a >X $16 >Y $ea >P $0041 $22 >M $0042 $b9 >M $b938 $ee >M $dcbe $11 >M $dcbf $41 >M $dcc0 $7f >M
T{ op PC S A X Y P -> $dcc0 $43 $fe $4a $16 $e8 }T T{ $0041 M $0042 M $b938 M $dcbe M $dcbf M $dcc0 M -> $22 $b9 $ee $11 $41 $7f }T
$c289 >PC $33 >S $ca >A $61 >X $d7 >Y $ee >P $0021 $e2 >M $0022 $79 >M $7ab9 $bd >M $c289 $11 >M $c28a $21 >M $c28b $9d >M
T{ op PC S A X Y P -> $c28b $33 $ff $61 $d7 $ec }T T{ $0021 M $0022 M $7ab9 M $c289 M $c28a M $c28b M -> $e2 $79 $bd $11 $21 $9d }T
$71f4 >PC $7b >S $fd >A $54 >X $6f >Y $20 >P $0027 $bc >M $0028 $c3 >M $71f4 $11 >M $71f5 $27 >M $71f6 $c0 >M $c42b $e1 >M
T{ op PC S A X Y P -> $71f6 $7b $fd $54 $6f $a0 }T T{ $0027 M $0028 M $71f4 M $71f5 M $71f6 M $c42b M -> $bc $c3 $11 $27 $c0 $e1 }T
( 12 )
$009e >PC $10 >S $0e >A $e3 >X $4e >Y $22 >P $0060 $02 >M $0061 $fb >M $009e $12 >M $009f $60 >M $00a0 $bd >M $fb02 $9b >M
T{ op PC S A X Y P -> $00a0 $10 $9f $e3 $4e $a0 }T T{ $0060 M $0061 M $009e M $009f M $00a0 M $fb02 M -> $02 $fb $12 $60 $bd $9b }T
$8e4f >PC $c5 >S $27 >A $94 >X $d3 >Y $e8 >P $0031 $d5 >M $0032 $bf >M $8e4f $12 >M $8e50 $31 >M $8e51 $39 >M $bfd5 $5e >M
T{ op PC S A X Y P -> $8e51 $c5 $7f $94 $d3 $68 }T T{ $0031 M $0032 M $8e4f M $8e50 M $8e51 M $bfd5 M -> $d5 $bf $12 $31 $39 $5e }T
$1fda >PC $3b >S $5f >A $e9 >X $0d >Y $e2 >P $00e3 $cd >M $00e4 $7b >M $1fda $12 >M $1fdb $e3 >M $1fdc $52 >M $7bcd $cc >M
T{ op PC S A X Y P -> $1fdc $3b $df $e9 $0d $e0 }T T{ $00e3 M $00e4 M $1fda M $1fdb M $1fdc M $7bcd M -> $cd $7b $12 $e3 $52 $cc }T
$f1d1 >PC $e7 >S $0c >A $60 >X $01 >Y $aa >P $00ec $82 >M $00ed $c2 >M $c282 $f1 >M $f1d1 $12 >M $f1d2 $ec >M $f1d3 $bb >M
T{ op PC S A X Y P -> $f1d3 $e7 $fd $60 $01 $a8 }T T{ $00ec M $00ed M $c282 M $f1d1 M $f1d2 M $f1d3 M -> $82 $c2 $f1 $12 $ec $bb }T
$f8a2 >PC $08 >S $66 >A $b7 >X $64 >Y $27 >P $0075 $fb >M $0076 $cb >M $cbfb $04 >M $f8a2 $12 >M $f8a3 $75 >M $f8a4 $27 >M
T{ op PC S A X Y P -> $f8a4 $08 $66 $b7 $64 $25 }T T{ $0075 M $0076 M $cbfb M $f8a2 M $f8a3 M $f8a4 M -> $fb $cb $04 $12 $75 $27 }T
$2358 >PC $10 >S $74 >A $62 >X $14 >Y $e5 >P $004e $9d >M $004f $cf >M $2358 $12 >M $2359 $4e >M $235a $e6 >M $cf9d $a9 >M
T{ op PC S A X Y P -> $235a $10 $fd $62 $14 $e5 }T T{ $004e M $004f M $2358 M $2359 M $235a M $cf9d M -> $9d $cf $12 $4e $e6 $a9 }T
$c6be >PC $cf >S $10 >A $72 >X $93 >Y $a7 >P $0056 $ab >M $0057 $4c >M $4cab $1b >M $c6be $12 >M $c6bf $56 >M $c6c0 $09 >M
T{ op PC S A X Y P -> $c6c0 $cf $1b $72 $93 $25 }T T{ $0056 M $0057 M $4cab M $c6be M $c6bf M $c6c0 M -> $ab $4c $1b $12 $56 $09 }T
$ca76 >PC $a5 >S $1a >A $80 >X $2d >Y $ef >P $00d5 $c4 >M $00d6 $8a >M $8ac4 $3b >M $ca76 $12 >M $ca77 $d5 >M $ca78 $c4 >M
T{ op PC S A X Y P -> $ca78 $a5 $3b $80 $2d $6d }T T{ $00d5 M $00d6 M $8ac4 M $ca76 M $ca77 M $ca78 M -> $c4 $8a $3b $12 $d5 $c4 }T
$d7eb >PC $68 >S $4a >A $cf >X $c3 >Y $29 >P $000e $61 >M $000f $03 >M $0361 $5b >M $d7eb $12 >M $d7ec $0e >M $d7ed $47 >M
T{ op PC S A X Y P -> $d7ed $68 $5b $cf $c3 $29 }T T{ $000e M $000f M $0361 M $d7eb M $d7ec M $d7ed M -> $61 $03 $5b $12 $0e $47 }T
$ca56 >PC $a8 >S $2e >A $54 >X $b3 >Y $2c >P $0011 $d2 >M $0012 $0e >M $0ed2 $5d >M $ca56 $12 >M $ca57 $11 >M $ca58 $fc >M
T{ op PC S A X Y P -> $ca58 $a8 $7f $54 $b3 $2c }T T{ $0011 M $0012 M $0ed2 M $ca56 M $ca57 M $ca58 M -> $d2 $0e $5d $12 $11 $fc }T
$1540 >PC $4b >S $05 >A $f4 >X $b5 >Y $6c >P $0049 $f6 >M $004a $54 >M $1540 $12 >M $1541 $49 >M $1542 $33 >M $54f6 $7b >M
T{ op PC S A X Y P -> $1542 $4b $7f $f4 $b5 $6c }T T{ $0049 M $004a M $1540 M $1541 M $1542 M $54f6 M -> $f6 $54 $12 $49 $33 $7b }T
$2b07 >PC $50 >S $9c >A $c1 >X $fe >Y $a5 >P $00b9 $ad >M $00ba $03 >M $03ad $c2 >M $2b07 $12 >M $2b08 $b9 >M $2b09 $ba >M
T{ op PC S A X Y P -> $2b09 $50 $de $c1 $fe $a5 }T T{ $00b9 M $00ba M $03ad M $2b07 M $2b08 M $2b09 M -> $ad $03 $c2 $12 $b9 $ba }T
$66df >PC $fd >S $67 >A $87 >X $84 >Y $e5 >P $006c $43 >M $006d $94 >M $66df $12 >M $66e0 $6c >M $66e1 $e9 >M $9443 $70 >M
T{ op PC S A X Y P -> $66e1 $fd $77 $87 $84 $65 }T T{ $006c M $006d M $66df M $66e0 M $66e1 M $9443 M -> $43 $94 $12 $6c $e9 $70 }T
$8666 >PC $83 >S $f4 >A $f7 >X $cb >Y $a8 >P $0068 $32 >M $0069 $a8 >M $8666 $12 >M $8667 $68 >M $8668 $74 >M $a832 $87 >M
T{ op PC S A X Y P -> $8668 $83 $f7 $f7 $cb $a8 }T T{ $0068 M $0069 M $8666 M $8667 M $8668 M $a832 M -> $32 $a8 $12 $68 $74 $87 }T
$bee3 >PC $54 >S $a6 >A $de >X $83 >Y $ed >P $0059 $6d >M $005a $61 >M $616d $49 >M $bee3 $12 >M $bee4 $59 >M $bee5 $bd >M
T{ op PC S A X Y P -> $bee5 $54 $ef $de $83 $ed }T T{ $0059 M $005a M $616d M $bee3 M $bee4 M $bee5 M -> $6d $61 $49 $12 $59 $bd }T
$7fda >PC $a6 >S $ae >A $4a >X $36 >Y $60 >P $00fc $a4 >M $00fd $2b >M $2ba4 $de >M $7fda $12 >M $7fdb $fc >M $7fdc $2b >M
T{ op PC S A X Y P -> $7fdc $a6 $fe $4a $36 $e0 }T T{ $00fc M $00fd M $2ba4 M $7fda M $7fdb M $7fdc M -> $a4 $2b $de $12 $fc $2b }T
( 13 )
$c58a >PC $4f >S $b0 >A $82 >X $e4 >Y $6a >P $c58a $13 >M $c58b $bb >M $c58c $ea >M
T{ op PC S A X Y P -> $c58b $4f $b0 $82 $e4 $6a }T T{ $c58a M $c58b M $c58c M -> $13 $bb $ea }T
$491b >PC $e6 >S $88 >A $d6 >X $79 >Y $25 >P $491b $13 >M $491c $2e >M $491d $0f >M
T{ op PC S A X Y P -> $491c $e6 $88 $d6 $79 $25 }T T{ $491b M $491c M $491d M -> $13 $2e $0f }T
$e911 >PC $11 >S $c9 >A $c8 >X $fd >Y $2b >P $e911 $13 >M $e912 $32 >M $e913 $6b >M
T{ op PC S A X Y P -> $e912 $11 $c9 $c8 $fd $2b }T T{ $e911 M $e912 M $e913 M -> $13 $32 $6b }T
$710a >PC $78 >S $ab >A $25 >X $62 >Y $20 >P $710a $13 >M $710b $e3 >M $710c $db >M
T{ op PC S A X Y P -> $710b $78 $ab $25 $62 $20 }T T{ $710a M $710b M $710c M -> $13 $e3 $db }T
$dffe >PC $63 >S $50 >A $8a >X $68 >Y $65 >P $dffe $13 >M $dfff $40 >M $e000 $b1 >M
T{ op PC S A X Y P -> $dfff $63 $50 $8a $68 $65 }T T{ $dffe M $dfff M $e000 M -> $13 $40 $b1 }T
$e04f >PC $40 >S $c3 >A $fc >X $3b >Y $26 >P $e04f $13 >M $e050 $59 >M $e051 $ee >M
T{ op PC S A X Y P -> $e050 $40 $c3 $fc $3b $26 }T T{ $e04f M $e050 M $e051 M -> $13 $59 $ee }T
$8e87 >PC $55 >S $3c >A $12 >X $21 >Y $ae >P $8e87 $13 >M $8e88 $18 >M $8e89 $dd >M
T{ op PC S A X Y P -> $8e88 $55 $3c $12 $21 $ae }T T{ $8e87 M $8e88 M $8e89 M -> $13 $18 $dd }T
$d62a >PC $b8 >S $d6 >A $2a >X $9b >Y $65 >P $d62a $13 >M $d62b $1f >M $d62c $40 >M
T{ op PC S A X Y P -> $d62b $b8 $d6 $2a $9b $65 }T T{ $d62a M $d62b M $d62c M -> $13 $1f $40 }T
$65af >PC $56 >S $c0 >A $4f >X $8a >Y $26 >P $65af $13 >M $65b0 $14 >M $65b1 $cb >M
T{ op PC S A X Y P -> $65b0 $56 $c0 $4f $8a $26 }T T{ $65af M $65b0 M $65b1 M -> $13 $14 $cb }T
$c787 >PC $3f >S $d4 >A $1d >X $c2 >Y $a4 >P $c787 $13 >M $c788 $46 >M $c789 $af >M
T{ op PC S A X Y P -> $c788 $3f $d4 $1d $c2 $a4 }T T{ $c787 M $c788 M $c789 M -> $13 $46 $af }T
$6350 >PC $95 >S $eb >A $40 >X $c9 >Y $a3 >P $6350 $13 >M $6351 $32 >M $6352 $31 >M
T{ op PC S A X Y P -> $6351 $95 $eb $40 $c9 $a3 }T T{ $6350 M $6351 M $6352 M -> $13 $32 $31 }T
$e859 >PC $80 >S $18 >A $2c >X $d9 >Y $a3 >P $e859 $13 >M $e85a $c8 >M $e85b $e2 >M
T{ op PC S A X Y P -> $e85a $80 $18 $2c $d9 $a3 }T T{ $e859 M $e85a M $e85b M -> $13 $c8 $e2 }T
$051d >PC $cb >S $05 >A $f9 >X $09 >Y $e5 >P $051d $13 >M $051e $e3 >M $051f $92 >M
T{ op PC S A X Y P -> $051e $cb $05 $f9 $09 $e5 }T T{ $051d M $051e M $051f M -> $13 $e3 $92 }T
$aead >PC $24 >S $7b >A $ed >X $eb >Y $67 >P $aead $13 >M $aeae $f2 >M $aeaf $2c >M
T{ op PC S A X Y P -> $aeae $24 $7b $ed $eb $67 }T T{ $aead M $aeae M $aeaf M -> $13 $f2 $2c }T
$0a92 >PC $dd >S $52 >A $e1 >X $db >Y $a3 >P $0a92 $13 >M $0a93 $be >M $0a94 $2c >M
T{ op PC S A X Y P -> $0a93 $dd $52 $e1 $db $a3 }T T{ $0a92 M $0a93 M $0a94 M -> $13 $be $2c }T
$5e8a >PC $0b >S $f3 >A $7d >X $e2 >Y $e3 >P $5e8a $13 >M $5e8b $b8 >M $5e8c $04 >M
T{ op PC S A X Y P -> $5e8b $0b $f3 $7d $e2 $e3 }T T{ $5e8a M $5e8b M $5e8c M -> $13 $b8 $04 }T
( 14 )
$ae45 >PC $1d >S $ef >A $c9 >X $ce >Y $a3 >P $0087 $7f >M $ae45 $14 >M $ae46 $87 >M $ae47 $c9 >M
T{ op PC S A X Y P -> $ae47 $1d $ef $c9 $ce $a1 }T T{ $0087 M $ae45 M $ae46 M $ae47 M -> $10 $14 $87 $c9 }T
$bd3c >PC $20 >S $ca >A $27 >X $74 >Y $67 >P $0006 $9c >M $bd3c $14 >M $bd3d $06 >M $bd3e $07 >M
T{ op PC S A X Y P -> $bd3e $20 $ca $27 $74 $65 }T T{ $0006 M $bd3c M $bd3d M $bd3e M -> $14 $14 $06 $07 }T
$35f1 >PC $b4 >S $2f >A $65 >X $55 >Y $ea >P $0020 $57 >M $35f1 $14 >M $35f2 $20 >M $35f3 $c5 >M
T{ op PC S A X Y P -> $35f3 $b4 $2f $65 $55 $e8 }T T{ $0020 M $35f1 M $35f2 M $35f3 M -> $50 $14 $20 $c5 }T
$770c >PC $66 >S $84 >A $3a >X $ce >Y $a3 >P $00a0 $f7 >M $770c $14 >M $770d $a0 >M $770e $34 >M
T{ op PC S A X Y P -> $770e $66 $84 $3a $ce $a1 }T T{ $00a0 M $770c M $770d M $770e M -> $73 $14 $a0 $34 }T
$b26a >PC $0a >S $ed >A $2b >X $58 >Y $e5 >P $005a $3a >M $b26a $14 >M $b26b $5a >M $b26c $85 >M
T{ op PC S A X Y P -> $b26c $0a $ed $2b $58 $e5 }T T{ $005a M $b26a M $b26b M $b26c M -> $12 $14 $5a $85 }T
$8c1f >PC $22 >S $62 >A $85 >X $c2 >Y $e5 >P $0017 $4b >M $8c1f $14 >M $8c20 $17 >M $8c21 $39 >M
T{ op PC S A X Y P -> $8c21 $22 $62 $85 $c2 $e5 }T T{ $0017 M $8c1f M $8c20 M $8c21 M -> $09 $14 $17 $39 }T
$af57 >PC $84 >S $fb >A $eb >X $f2 >Y $60 >P $0094 $6e >M $af57 $14 >M $af58 $94 >M $af59 $76 >M
T{ op PC S A X Y P -> $af59 $84 $fb $eb $f2 $60 }T T{ $0094 M $af57 M $af58 M $af59 M -> $04 $14 $94 $76 }T
$de20 >PC $02 >S $18 >A $be >X $25 >Y $a4 >P $00c0 $ef >M $de20 $14 >M $de21 $c0 >M $de22 $75 >M
T{ op PC S A X Y P -> $de22 $02 $18 $be $25 $a4 }T T{ $00c0 M $de20 M $de21 M $de22 M -> $e7 $14 $c0 $75 }T
$366d >PC $61 >S $cb >A $bb >X $60 >Y $68 >P $0078 $62 >M $366d $14 >M $366e $78 >M $366f $db >M
T{ op PC S A X Y P -> $366f $61 $cb $bb $60 $68 }T T{ $0078 M $366d M $366e M $366f M -> $20 $14 $78 $db }T
$8df8 >PC $1b >S $18 >A $f4 >X $9d >Y $a8 >P $003c $3c >M $8df8 $14 >M $8df9 $3c >M $8dfa $b0 >M
T{ op PC S A X Y P -> $8dfa $1b $18 $f4 $9d $a8 }T T{ $003c M $8df8 M $8df9 M $8dfa M -> $24 $14 $3c $b0 }T
$720e >PC $82 >S $84 >A $83 >X $52 >Y $e7 >P $009a $95 >M $720e $14 >M $720f $9a >M $7210 $ca >M
T{ op PC S A X Y P -> $7210 $82 $84 $83 $52 $e5 }T T{ $009a M $720e M $720f M $7210 M -> $11 $14 $9a $ca }T
$ec44 >PC $73 >S $4d >A $2c >X $49 >Y $2a >P $0031 $05 >M $ec44 $14 >M $ec45 $31 >M $ec46 $24 >M
T{ op PC S A X Y P -> $ec46 $73 $4d $2c $49 $28 }T T{ $0031 M $ec44 M $ec45 M $ec46 M -> $00 $14 $31 $24 }T
$f552 >PC $ff >S $c4 >A $c6 >X $d8 >Y $62 >P $00d8 $da >M $f552 $14 >M $f553 $d8 >M $f554 $aa >M
T{ op PC S A X Y P -> $f554 $ff $c4 $c6 $d8 $60 }T T{ $00d8 M $f552 M $f553 M $f554 M -> $1a $14 $d8 $aa }T
$5ead >PC $00 >S $1d >A $5d >X $c0 >Y $2c >P $0059 $e2 >M $5ead $14 >M $5eae $59 >M $5eaf $56 >M
T{ op PC S A X Y P -> $5eaf $00 $1d $5d $c0 $2e }T T{ $0059 M $5ead M $5eae M $5eaf M -> $e2 $14 $59 $56 }T
$2ded >PC $f9 >S $66 >A $0f >X $ed >Y $6b >P $009d $09 >M $2ded $14 >M $2dee $9d >M $2def $2c >M
T{ op PC S A X Y P -> $2def $f9 $66 $0f $ed $6b }T T{ $009d M $2ded M $2dee M $2def M -> $09 $14 $9d $2c }T
$f50e >PC $87 >S $38 >A $b3 >X $de >Y $e4 >P $004f $1c >M $f50e $14 >M $f50f $4f >M $f510 $ed >M
T{ op PC S A X Y P -> $f510 $87 $38 $b3 $de $e4 }T T{ $004f M $f50e M $f50f M $f510 M -> $04 $14 $4f $ed }T
( 15 )
$7162 >PC $76 >S $c6 >A $b4 >X $df >Y $20 >P $0004 $85 >M $0050 $e8 >M $7162 $15 >M $7163 $50 >M $7164 $d1 >M
T{ op PC S A X Y P -> $7164 $76 $c7 $b4 $df $a0 }T T{ $0004 M $0050 M $7162 M $7163 M $7164 M -> $85 $e8 $15 $50 $d1 }T
$3c7c >PC $32 >S $bc >A $fc >X $ab >Y $e8 >P $00f8 $28 >M $00fc $6b >M $3c7c $15 >M $3c7d $fc >M $3c7e $3f >M
T{ op PC S A X Y P -> $3c7e $32 $bc $fc $ab $e8 }T T{ $00f8 M $00fc M $3c7c M $3c7d M $3c7e M -> $28 $6b $15 $fc $3f }T
$b9a6 >PC $80 >S $fe >A $7f >X $ab >Y $e0 >P $002b $52 >M $00aa $5d >M $b9a6 $15 >M $b9a7 $2b >M $b9a8 $bc >M
T{ op PC S A X Y P -> $b9a8 $80 $ff $7f $ab $e0 }T T{ $002b M $00aa M $b9a6 M $b9a7 M $b9a8 M -> $52 $5d $15 $2b $bc }T
$b885 >PC $07 >S $20 >A $ac >X $e9 >Y $61 >P $0058 $8f >M $00ac $6f >M $b885 $15 >M $b886 $ac >M $b887 $43 >M
T{ op PC S A X Y P -> $b887 $07 $af $ac $e9 $e1 }T T{ $0058 M $00ac M $b885 M $b886 M $b887 M -> $8f $6f $15 $ac $43 }T
$d4b7 >PC $83 >S $87 >A $db >X $63 >Y $e4 >P $0055 $0f >M $007a $00 >M $d4b7 $15 >M $d4b8 $7a >M $d4b9 $29 >M
T{ op PC S A X Y P -> $d4b9 $83 $8f $db $63 $e4 }T T{ $0055 M $007a M $d4b7 M $d4b8 M $d4b9 M -> $0f $00 $15 $7a $29 }T
$0153 >PC $23 >S $f5 >A $eb >X $02 >Y $23 >P $0016 $97 >M $002b $82 >M $0153 $15 >M $0154 $2b >M $0155 $3f >M
T{ op PC S A X Y P -> $0155 $23 $f7 $eb $02 $a1 }T T{ $0016 M $002b M $0153 M $0154 M $0155 M -> $97 $82 $15 $2b $3f }T
$2d31 >PC $90 >S $24 >A $99 >X $30 >Y $a6 >P $004a $66 >M $00b1 $bf >M $2d31 $15 >M $2d32 $b1 >M $2d33 $9a >M
T{ op PC S A X Y P -> $2d33 $90 $66 $99 $30 $24 }T T{ $004a M $00b1 M $2d31 M $2d32 M $2d33 M -> $66 $bf $15 $b1 $9a }T
$899a >PC $d0 >S $d4 >A $87 >X $a7 >Y $2e >P $001a $82 >M $0093 $ef >M $899a $15 >M $899b $93 >M $899c $9f >M
T{ op PC S A X Y P -> $899c $d0 $d6 $87 $a7 $ac }T T{ $001a M $0093 M $899a M $899b M $899c M -> $82 $ef $15 $93 $9f }T
$b5d4 >PC $26 >S $0e >A $9c >X $54 >Y $6b >P $0053 $2e >M $00b7 $ff >M $b5d4 $15 >M $b5d5 $b7 >M $b5d6 $ff >M
T{ op PC S A X Y P -> $b5d6 $26 $2e $9c $54 $69 }T T{ $0053 M $00b7 M $b5d4 M $b5d5 M $b5d6 M -> $2e $ff $15 $b7 $ff }T
$229a >PC $0e >S $d9 >A $d2 >X $22 >Y $6c >P $0007 $c4 >M $0035 $12 >M $229a $15 >M $229b $35 >M $229c $70 >M
T{ op PC S A X Y P -> $229c $0e $dd $d2 $22 $ec }T T{ $0007 M $0035 M $229a M $229b M $229c M -> $c4 $12 $15 $35 $70 }T
$4e1b >PC $52 >S $5a >A $ae >X $b7 >Y $6b >P $0002 $1a >M $00b0 $09 >M $4e1b $15 >M $4e1c $02 >M $4e1d $79 >M
T{ op PC S A X Y P -> $4e1d $52 $5b $ae $b7 $69 }T T{ $0002 M $00b0 M $4e1b M $4e1c M $4e1d M -> $1a $09 $15 $02 $79 }T
$d3a0 >PC $92 >S $65 >A $c6 >X $69 >Y $67 >P $0084 $41 >M $00be $e0 >M $d3a0 $15 >M $d3a1 $be >M $d3a2 $04 >M
T{ op PC S A X Y P -> $d3a2 $92 $65 $c6 $69 $65 }T T{ $0084 M $00be M $d3a0 M $d3a1 M $d3a2 M -> $41 $e0 $15 $be $04 }T
$647a >PC $b2 >S $d4 >A $d0 >X $5e >Y $6b >P $0080 $bd >M $00b0 $e2 >M $647a $15 >M $647b $b0 >M $647c $01 >M
T{ op PC S A X Y P -> $647c $b2 $fd $d0 $5e $e9 }T T{ $0080 M $00b0 M $647a M $647b M $647c M -> $bd $e2 $15 $b0 $01 }T
$8380 >PC $2a >S $d4 >A $b7 >X $56 >Y $24 >P $0054 $c9 >M $009d $2c >M $8380 $15 >M $8381 $9d >M $8382 $26 >M
T{ op PC S A X Y P -> $8382 $2a $dd $b7 $56 $a4 }T T{ $0054 M $009d M $8380 M $8381 M $8382 M -> $c9 $2c $15 $9d $26 }T
$0532 >PC $ef >S $2f >A $06 >X $19 >Y $a7 >P $009c $cb >M $00a2 $b3 >M $0532 $15 >M $0533 $9c >M $0534 $ce >M
T{ op PC S A X Y P -> $0534 $ef $bf $06 $19 $a5 }T T{ $009c M $00a2 M $0532 M $0533 M $0534 M -> $cb $b3 $15 $9c $ce }T
$672b >PC $de >S $88 >A $29 >X $5b >Y $e8 >P $00c2 $4c >M $00eb $95 >M $672b $15 >M $672c $c2 >M $672d $89 >M
T{ op PC S A X Y P -> $672d $de $9d $29 $5b $e8 }T T{ $00c2 M $00eb M $672b M $672c M $672d M -> $4c $95 $15 $c2 $89 }T
( 16 )
$d98e >PC $9d >S $95 >A $ae >X $8b >Y $26 >P $003d $9e >M $00eb $bc >M $d98e $16 >M $d98f $3d >M $d990 $1f >M
T{ op PC S A X Y P -> $d990 $9d $95 $ae $8b $25 }T T{ $003d M $00eb M $d98e M $d98f M $d990 M -> $9e $78 $16 $3d $1f }T
$566d >PC $90 >S $25 >A $04 >X $06 >Y $28 >P $0029 $33 >M $002d $79 >M $566d $16 >M $566e $29 >M $566f $e1 >M
T{ op PC S A X Y P -> $566f $90 $25 $04 $06 $a8 }T T{ $0029 M $002d M $566d M $566e M $566f M -> $33 $f2 $16 $29 $e1 }T
$63ee >PC $08 >S $6c >A $e6 >X $9d >Y $e1 >P $0026 $33 >M $0040 $b3 >M $63ee $16 >M $63ef $40 >M $63f0 $da >M
T{ op PC S A X Y P -> $63f0 $08 $6c $e6 $9d $60 }T T{ $0026 M $0040 M $63ee M $63ef M $63f0 M -> $66 $b3 $16 $40 $da }T
$52b5 >PC $19 >S $ee >A $4a >X $d9 >Y $e2 >P $0018 $35 >M $00ce $36 >M $52b5 $16 >M $52b6 $ce >M $52b7 $e0 >M
T{ op PC S A X Y P -> $52b7 $19 $ee $4a $d9 $60 }T T{ $0018 M $00ce M $52b5 M $52b6 M $52b7 M -> $6a $36 $16 $ce $e0 }T
$99a4 >PC $d9 >S $40 >A $8a >X $e7 >Y $e5 >P $0020 $e9 >M $0096 $b5 >M $99a4 $16 >M $99a5 $96 >M $99a6 $a7 >M
T{ op PC S A X Y P -> $99a6 $d9 $40 $8a $e7 $e5 }T T{ $0020 M $0096 M $99a4 M $99a5 M $99a6 M -> $d2 $b5 $16 $96 $a7 }T
$7904 >PC $ca >S $b4 >A $38 >X $fc >Y $6a >P $0075 $ba >M $00ad $b7 >M $7904 $16 >M $7905 $75 >M $7906 $fa >M
T{ op PC S A X Y P -> $7906 $ca $b4 $38 $fc $69 }T T{ $0075 M $00ad M $7904 M $7905 M $7906 M -> $ba $6e $16 $75 $fa }T
$b5cd >PC $ba >S $c5 >A $ad >X $f2 >Y $64 >P $0029 $fa >M $007c $bd >M $b5cd $16 >M $b5ce $7c >M $b5cf $df >M
T{ op PC S A X Y P -> $b5cf $ba $c5 $ad $f2 $e5 }T T{ $0029 M $007c M $b5cd M $b5ce M $b5cf M -> $f4 $bd $16 $7c $df }T
$4b89 >PC $7f >S $8b >A $c7 >X $44 >Y $a0 >P $00bf $59 >M $00f8 $97 >M $4b89 $16 >M $4b8a $f8 >M $4b8b $f2 >M
T{ op PC S A X Y P -> $4b8b $7f $8b $c7 $44 $a0 }T T{ $00bf M $00f8 M $4b89 M $4b8a M $4b8b M -> $b2 $97 $16 $f8 $f2 }T
$2450 >PC $d7 >S $11 >A $5d >X $56 >Y $63 >P $0002 $bc >M $00a5 $c4 >M $2450 $16 >M $2451 $a5 >M $2452 $00 >M
T{ op PC S A X Y P -> $2452 $d7 $11 $5d $56 $61 }T T{ $0002 M $00a5 M $2450 M $2451 M $2452 M -> $78 $c4 $16 $a5 $00 }T
$2526 >PC $bd >S $1b >A $95 >X $07 >Y $67 >P $0036 $f1 >M $00a1 $03 >M $2526 $16 >M $2527 $a1 >M $2528 $1a >M
T{ op PC S A X Y P -> $2528 $bd $1b $95 $07 $e5 }T T{ $0036 M $00a1 M $2526 M $2527 M $2528 M -> $e2 $03 $16 $a1 $1a }T
$5a33 >PC $16 >S $53 >A $ca >X $1d >Y $63 >P $0094 $6c >M $00ca $af >M $5a33 $16 >M $5a34 $ca >M $5a35 $8d >M
T{ op PC S A X Y P -> $5a35 $16 $53 $ca $1d $e0 }T T{ $0094 M $00ca M $5a33 M $5a34 M $5a35 M -> $d8 $af $16 $ca $8d }T
$5ecb >PC $4d >S $16 >A $1a >X $b0 >Y $e8 >P $00b9 $9c >M $00d3 $5d >M $5ecb $16 >M $5ecc $b9 >M $5ecd $0c >M
T{ op PC S A X Y P -> $5ecd $4d $16 $1a $b0 $e8 }T T{ $00b9 M $00d3 M $5ecb M $5ecc M $5ecd M -> $9c $ba $16 $b9 $0c }T
$0e26 >PC $cc >S $b2 >A $72 >X $6f >Y $a3 >P $006a $7c >M $00dc $db >M $0e26 $16 >M $0e27 $6a >M $0e28 $de >M
T{ op PC S A X Y P -> $0e28 $cc $b2 $72 $6f $a1 }T T{ $006a M $00dc M $0e26 M $0e27 M $0e28 M -> $7c $b6 $16 $6a $de }T
$a0c8 >PC $fb >S $9f >A $5c >X $64 >Y $e0 >P $0028 $5a >M $00cc $90 >M $a0c8 $16 >M $a0c9 $cc >M $a0ca $91 >M
T{ op PC S A X Y P -> $a0ca $fb $9f $5c $64 $e0 }T T{ $0028 M $00cc M $a0c8 M $a0c9 M $a0ca M -> $b4 $90 $16 $cc $91 }T
$7443 >PC $8e >S $14 >A $e8 >X $e1 >Y $2a >P $0012 $0a >M $002a $3b >M $7443 $16 >M $7444 $2a >M $7445 $e7 >M
T{ op PC S A X Y P -> $7445 $8e $14 $e8 $e1 $28 }T T{ $0012 M $002a M $7443 M $7444 M $7445 M -> $14 $3b $16 $2a $e7 }T
$d22f >PC $cd >S $71 >A $8b >X $a2 >Y $2c >P $003b $20 >M $00b0 $f8 >M $d22f $16 >M $d230 $b0 >M $d231 $19 >M
T{ op PC S A X Y P -> $d231 $cd $71 $8b $a2 $2c }T T{ $003b M $00b0 M $d22f M $d230 M $d231 M -> $40 $f8 $16 $b0 $19 }T
( 17 )
$c6b5 >PC $09 >S $bd >A $bd >X $83 >Y $ed >P $00c6 $e5 >M $c6b5 $17 >M $c6b6 $c6 >M $c6b7 $a3 >M
T{ op PC S A X Y P -> $c6b7 $09 $bd $bd $83 $ed }T T{ $00c6 M $c6b5 M $c6b6 M $c6b7 M -> $e5 $17 $c6 $a3 }T
$c373 >PC $59 >S $58 >A $0e >X $a9 >Y $68 >P $00f8 $e3 >M $c373 $17 >M $c374 $f8 >M $c375 $a4 >M
T{ op PC S A X Y P -> $c375 $59 $58 $0e $a9 $68 }T T{ $00f8 M $c373 M $c374 M $c375 M -> $e1 $17 $f8 $a4 }T
$66ef >PC $8b >S $c0 >A $90 >X $e8 >Y $ea >P $009d $34 >M $66ef $17 >M $66f0 $9d >M $66f1 $7b >M
T{ op PC S A X Y P -> $66f1 $8b $c0 $90 $e8 $ea }T T{ $009d M $66ef M $66f0 M $66f1 M -> $34 $17 $9d $7b }T
$4dab >PC $d0 >S $1d >A $59 >X $52 >Y $2a >P $00a7 $c7 >M $4dab $17 >M $4dac $a7 >M $4dad $60 >M
T{ op PC S A X Y P -> $4dad $d0 $1d $59 $52 $2a }T T{ $00a7 M $4dab M $4dac M $4dad M -> $c5 $17 $a7 $60 }T
$ab39 >PC $54 >S $7d >A $ad >X $7c >Y $23 >P $0010 $15 >M $ab39 $17 >M $ab3a $10 >M $ab3b $3f >M
T{ op PC S A X Y P -> $ab3b $54 $7d $ad $7c $23 }T T{ $0010 M $ab39 M $ab3a M $ab3b M -> $15 $17 $10 $3f }T
$287a >PC $e0 >S $fe >A $6d >X $10 >Y $e1 >P $00db $3d >M $287a $17 >M $287b $db >M $287c $29 >M
T{ op PC S A X Y P -> $287c $e0 $fe $6d $10 $e1 }T T{ $00db M $287a M $287b M $287c M -> $3d $17 $db $29 }T
$5505 >PC $79 >S $8e >A $bf >X $dc >Y $6a >P $00b8 $48 >M $5505 $17 >M $5506 $b8 >M $5507 $b3 >M
T{ op PC S A X Y P -> $5507 $79 $8e $bf $dc $6a }T T{ $00b8 M $5505 M $5506 M $5507 M -> $48 $17 $b8 $b3 }T
$9fcf >PC $08 >S $7b >A $31 >X $23 >Y $2e >P $00d6 $e3 >M $9fcf $17 >M $9fd0 $d6 >M $9fd1 $60 >M
T{ op PC S A X Y P -> $9fd1 $08 $7b $31 $23 $2e }T T{ $00d6 M $9fcf M $9fd0 M $9fd1 M -> $e1 $17 $d6 $60 }T
$b578 >PC $b4 >S $0a >A $14 >X $88 >Y $27 >P $00ba $c6 >M $b578 $17 >M $b579 $ba >M $b57a $c7 >M
T{ op PC S A X Y P -> $b57a $b4 $0a $14 $88 $27 }T T{ $00ba M $b578 M $b579 M $b57a M -> $c4 $17 $ba $c7 }T
$29f5 >PC $ab >S $b5 >A $09 >X $7f >Y $63 >P $00df $1c >M $29f5 $17 >M $29f6 $df >M $29f7 $75 >M
T{ op PC S A X Y P -> $29f7 $ab $b5 $09 $7f $63 }T T{ $00df M $29f5 M $29f6 M $29f7 M -> $1c $17 $df $75 }T
$0cba >PC $8f >S $a6 >A $2e >X $f9 >Y $6f >P $00ea $4e >M $0cba $17 >M $0cbb $ea >M $0cbc $44 >M
T{ op PC S A X Y P -> $0cbc $8f $a6 $2e $f9 $6f }T T{ $00ea M $0cba M $0cbb M $0cbc M -> $4c $17 $ea $44 }T
$039d >PC $d6 >S $f7 >A $66 >X $62 >Y $a4 >P $0066 $ad >M $039d $17 >M $039e $66 >M $039f $77 >M
T{ op PC S A X Y P -> $039f $d6 $f7 $66 $62 $a4 }T T{ $0066 M $039d M $039e M $039f M -> $ad $17 $66 $77 }T
$83bb >PC $5d >S $4c >A $14 >X $4c >Y $63 >P $00de $b0 >M $83bb $17 >M $83bc $de >M $83bd $f5 >M
T{ op PC S A X Y P -> $83bd $5d $4c $14 $4c $63 }T T{ $00de M $83bb M $83bc M $83bd M -> $b0 $17 $de $f5 }T
$adc6 >PC $30 >S $28 >A $f6 >X $4f >Y $69 >P $0034 $2c >M $adc6 $17 >M $adc7 $34 >M $adc8 $68 >M
T{ op PC S A X Y P -> $adc8 $30 $28 $f6 $4f $69 }T T{ $0034 M $adc6 M $adc7 M $adc8 M -> $2c $17 $34 $68 }T
$d747 >PC $36 >S $18 >A $e4 >X $dc >Y $6a >P $004c $a7 >M $d747 $17 >M $d748 $4c >M $d749 $8f >M
T{ op PC S A X Y P -> $d749 $36 $18 $e4 $dc $6a }T T{ $004c M $d747 M $d748 M $d749 M -> $a5 $17 $4c $8f }T
$0568 >PC $8b >S $65 >A $55 >X $f3 >Y $e7 >P $0040 $92 >M $0568 $17 >M $0569 $40 >M $056a $b4 >M
T{ op PC S A X Y P -> $056a $8b $65 $55 $f3 $e7 }T T{ $0040 M $0568 M $0569 M $056a M -> $90 $17 $40 $b4 }T
( 18 )
$1187 >PC $b6 >S $fb >A $46 >X $c8 >Y $a0 >P $1187 $18 >M $1188 $91 >M $1189 $d2 >M
T{ op PC S A X Y P -> $1188 $b6 $fb $46 $c8 $a0 }T T{ $1187 M $1188 M $1189 M -> $18 $91 $d2 }T
$e1df >PC $89 >S $3b >A $94 >X $95 >Y $e2 >P $e1df $18 >M $e1e0 $1e >M $e1e1 $58 >M
T{ op PC S A X Y P -> $e1e0 $89 $3b $94 $95 $e2 }T T{ $e1df M $e1e0 M $e1e1 M -> $18 $1e $58 }T
$ecf4 >PC $0b >S $85 >A $a2 >X $c6 >Y $26 >P $ecf4 $18 >M $ecf5 $58 >M $ecf6 $cb >M
T{ op PC S A X Y P -> $ecf5 $0b $85 $a2 $c6 $26 }T T{ $ecf4 M $ecf5 M $ecf6 M -> $18 $58 $cb }T
$0947 >PC $5b >S $cd >A $00 >X $48 >Y $af >P $0947 $18 >M $0948 $83 >M $0949 $de >M
T{ op PC S A X Y P -> $0948 $5b $cd $00 $48 $ae }T T{ $0947 M $0948 M $0949 M -> $18 $83 $de }T
$940e >PC $04 >S $a6 >A $44 >X $be >Y $e9 >P $940e $18 >M $940f $d3 >M $9410 $3c >M
T{ op PC S A X Y P -> $940f $04 $a6 $44 $be $e8 }T T{ $940e M $940f M $9410 M -> $18 $d3 $3c }T
$8703 >PC $35 >S $eb >A $ff >X $c4 >Y $20 >P $8703 $18 >M $8704 $cb >M $8705 $71 >M
T{ op PC S A X Y P -> $8704 $35 $eb $ff $c4 $20 }T T{ $8703 M $8704 M $8705 M -> $18 $cb $71 }T
$a8a6 >PC $c2 >S $0e >A $76 >X $05 >Y $aa >P $a8a6 $18 >M $a8a7 $a7 >M $a8a8 $4e >M
T{ op PC S A X Y P -> $a8a7 $c2 $0e $76 $05 $aa }T T{ $a8a6 M $a8a7 M $a8a8 M -> $18 $a7 $4e }T
$2887 >PC $0c >S $ee >A $62 >X $64 >Y $ed >P $2887 $18 >M $2888 $ac >M $2889 $36 >M
T{ op PC S A X Y P -> $2888 $0c $ee $62 $64 $ec }T T{ $2887 M $2888 M $2889 M -> $18 $ac $36 }T
$e4e2 >PC $6d >S $43 >A $f3 >X $3f >Y $a0 >P $e4e2 $18 >M $e4e3 $d7 >M $e4e4 $97 >M
T{ op PC S A X Y P -> $e4e3 $6d $43 $f3 $3f $a0 }T T{ $e4e2 M $e4e3 M $e4e4 M -> $18 $d7 $97 }T
$cbd2 >PC $52 >S $5b >A $81 >X $db >Y $28 >P $cbd2 $18 >M $cbd3 $c3 >M $cbd4 $a5 >M
T{ op PC S A X Y P -> $cbd3 $52 $5b $81 $db $28 }T T{ $cbd2 M $cbd3 M $cbd4 M -> $18 $c3 $a5 }T
$d7de >PC $59 >S $c3 >A $ce >X $0b >Y $20 >P $d7de $18 >M $d7df $e1 >M $d7e0 $c9 >M
T{ op PC S A X Y P -> $d7df $59 $c3 $ce $0b $20 }T T{ $d7de M $d7df M $d7e0 M -> $18 $e1 $c9 }T
$4bc6 >PC $d5 >S $06 >A $d7 >X $1d >Y $2e >P $4bc6 $18 >M $4bc7 $e6 >M $4bc8 $0b >M
T{ op PC S A X Y P -> $4bc7 $d5 $06 $d7 $1d $2e }T T{ $4bc6 M $4bc7 M $4bc8 M -> $18 $e6 $0b }T
$1039 >PC $e8 >S $db >A $07 >X $99 >Y $6e >P $1039 $18 >M $103a $50 >M $103b $ea >M
T{ op PC S A X Y P -> $103a $e8 $db $07 $99 $6e }T T{ $1039 M $103a M $103b M -> $18 $50 $ea }T
$9bb8 >PC $b5 >S $cc >A $63 >X $ae >Y $a7 >P $9bb8 $18 >M $9bb9 $91 >M $9bba $99 >M
T{ op PC S A X Y P -> $9bb9 $b5 $cc $63 $ae $a6 }T T{ $9bb8 M $9bb9 M $9bba M -> $18 $91 $99 }T
$2521 >PC $20 >S $7c >A $d9 >X $31 >Y $eb >P $2521 $18 >M $2522 $56 >M $2523 $f1 >M
T{ op PC S A X Y P -> $2522 $20 $7c $d9 $31 $ea }T T{ $2521 M $2522 M $2523 M -> $18 $56 $f1 }T
$ab49 >PC $44 >S $10 >A $97 >X $f1 >Y $af >P $ab49 $18 >M $ab4a $62 >M $ab4b $1b >M
T{ op PC S A X Y P -> $ab4a $44 $10 $97 $f1 $ae }T T{ $ab49 M $ab4a M $ab4b M -> $18 $62 $1b }T
( 19 )
$7d3e >PC $51 >S $24 >A $e7 >X $ac >Y $63 >P $7d3e $19 >M $7d3f $be >M $7d40 $d4 >M $7d41 $c7 >M $d56a $cc >M
T{ op PC S A X Y P -> $7d41 $51 $ec $e7 $ac $e1 }T T{ $7d3e M $7d3f M $7d40 M $7d41 M $d56a M -> $19 $be $d4 $c7 $cc }T
$3b19 >PC $60 >S $ac >A $96 >X $bb >Y $e3 >P $3b19 $19 >M $3b1a $0f >M $3b1b $ce >M $3b1c $8a >M $ceca $e8 >M
T{ op PC S A X Y P -> $3b1c $60 $ec $96 $bb $e1 }T T{ $3b19 M $3b1a M $3b1b M $3b1c M $ceca M -> $19 $0f $ce $8a $e8 }T
$0227 >PC $ed >S $4a >A $56 >X $2f >Y $a2 >P $0227 $19 >M $0228 $96 >M $0229 $92 >M $022a $a3 >M $92c5 $0c >M
T{ op PC S A X Y P -> $022a $ed $4e $56 $2f $20 }T T{ $0227 M $0228 M $0229 M $022a M $92c5 M -> $19 $96 $92 $a3 $0c }T
$6f10 >PC $4e >S $b5 >A $4a >X $fd >Y $2b >P $6f10 $19 >M $6f11 $bb >M $6f12 $d5 >M $6f13 $8e >M $d6b8 $7d >M
T{ op PC S A X Y P -> $6f13 $4e $fd $4a $fd $a9 }T T{ $6f10 M $6f11 M $6f12 M $6f13 M $d6b8 M -> $19 $bb $d5 $8e $7d }T
$87e6 >PC $77 >S $f3 >A $78 >X $15 >Y $25 >P $4870 $15 >M $87e6 $19 >M $87e7 $5b >M $87e8 $48 >M $87e9 $04 >M
T{ op PC S A X Y P -> $87e9 $77 $f7 $78 $15 $a5 }T T{ $4870 M $87e6 M $87e7 M $87e8 M $87e9 M -> $15 $19 $5b $48 $04 }T
$2321 >PC $3e >S $a5 >A $6b >X $09 >Y $61 >P $2321 $19 >M $2322 $99 >M $2323 $e5 >M $2324 $8f >M $e5a2 $5e >M
T{ op PC S A X Y P -> $2324 $3e $ff $6b $09 $e1 }T T{ $2321 M $2322 M $2323 M $2324 M $e5a2 M -> $19 $99 $e5 $8f $5e }T
$70c2 >PC $d9 >S $07 >A $69 >X $70 >Y $aa >P $70c2 $19 >M $70c3 $31 >M $70c4 $a0 >M $70c5 $25 >M $a0a1 $4d >M
T{ op PC S A X Y P -> $70c5 $d9 $4f $69 $70 $28 }T T{ $70c2 M $70c3 M $70c4 M $70c5 M $a0a1 M -> $19 $31 $a0 $25 $4d }T
$e5d1 >PC $6e >S $c4 >A $46 >X $b5 >Y $e9 >P $de3e $5d >M $e5d1 $19 >M $e5d2 $89 >M $e5d3 $dd >M $e5d4 $bf >M
T{ op PC S A X Y P -> $e5d4 $6e $dd $46 $b5 $e9 }T T{ $de3e M $e5d1 M $e5d2 M $e5d3 M $e5d4 M -> $5d $19 $89 $dd $bf }T
$0f14 >PC $a8 >S $4b >A $ff >X $23 >Y $e6 >P $0f14 $19 >M $0f15 $d7 >M $0f16 $57 >M $0f17 $f4 >M $57fa $eb >M
T{ op PC S A X Y P -> $0f17 $a8 $eb $ff $23 $e4 }T T{ $0f14 M $0f15 M $0f16 M $0f17 M $57fa M -> $19 $d7 $57 $f4 $eb }T
$3af8 >PC $d6 >S $90 >A $7f >X $9c >Y $a0 >P $3af8 $19 >M $3af9 $d1 >M $3afa $b5 >M $3afb $7a >M $b66d $c6 >M
T{ op PC S A X Y P -> $3afb $d6 $d6 $7f $9c $a0 }T T{ $3af8 M $3af9 M $3afa M $3afb M $b66d M -> $19 $d1 $b5 $7a $c6 }T
$13c0 >PC $c6 >S $46 >A $5f >X $f8 >Y $24 >P $13c0 $19 >M $13c1 $4d >M $13c2 $89 >M $13c3 $c6 >M $8a45 $9c >M
T{ op PC S A X Y P -> $13c3 $c6 $de $5f $f8 $a4 }T T{ $13c0 M $13c1 M $13c2 M $13c3 M $8a45 M -> $19 $4d $89 $c6 $9c }T
$deef >PC $04 >S $6e >A $0c >X $56 >Y $ef >P $deef $19 >M $def0 $10 >M $def1 $e0 >M $def2 $3f >M $e066 $47 >M
T{ op PC S A X Y P -> $def2 $04 $6f $0c $56 $6d }T T{ $deef M $def0 M $def1 M $def2 M $e066 M -> $19 $10 $e0 $3f $47 }T
$d37d >PC $65 >S $28 >A $1a >X $86 >Y $e8 >P $3d1d $29 >M $d37d $19 >M $d37e $97 >M $d37f $3c >M $d380 $b9 >M
T{ op PC S A X Y P -> $d380 $65 $29 $1a $86 $68 }T T{ $3d1d M $d37d M $d37e M $d37f M $d380 M -> $29 $19 $97 $3c $b9 }T
$3f03 >PC $d0 >S $7d >A $33 >X $6a >Y $ef >P $3f03 $19 >M $3f04 $ca >M $3f05 $ad >M $3f06 $d0 >M $ae34 $2c >M
T{ op PC S A X Y P -> $3f06 $d0 $7d $33 $6a $6d }T T{ $3f03 M $3f04 M $3f05 M $3f06 M $ae34 M -> $19 $ca $ad $d0 $2c }T
$d39c >PC $d9 >S $45 >A $10 >X $14 >Y $a1 >P $2d35 $61 >M $d39c $19 >M $d39d $21 >M $d39e $2d >M $d39f $3a >M
T{ op PC S A X Y P -> $d39f $d9 $65 $10 $14 $21 }T T{ $2d35 M $d39c M $d39d M $d39e M $d39f M -> $61 $19 $21 $2d $3a }T
$58b8 >PC $9b >S $30 >A $14 >X $e6 >Y $2a >P $588a $e3 >M $58b8 $19 >M $58b9 $a4 >M $58ba $57 >M $58bb $f2 >M
T{ op PC S A X Y P -> $58bb $9b $f3 $14 $e6 $a8 }T T{ $588a M $58b8 M $58b9 M $58ba M $58bb M -> $e3 $19 $a4 $57 $f2 }T
( 1a )
$b648 >PC $ad >S $b7 >A $b1 >X $ca >Y $27 >P $b648 $1a >M $b649 $28 >M $b64a $cc >M
T{ op PC S A X Y P -> $b649 $ad $b8 $b1 $ca $a5 }T T{ $b648 M $b649 M $b64a M -> $1a $28 $cc }T
$e50f >PC $e0 >S $eb >A $83 >X $07 >Y $ab >P $e50f $1a >M $e510 $6e >M $e511 $8e >M
T{ op PC S A X Y P -> $e510 $e0 $ec $83 $07 $a9 }T T{ $e50f M $e510 M $e511 M -> $1a $6e $8e }T
$3e74 >PC $42 >S $c0 >A $50 >X $e3 >Y $a8 >P $3e74 $1a >M $3e75 $2f >M $3e76 $5e >M
T{ op PC S A X Y P -> $3e75 $42 $c1 $50 $e3 $a8 }T T{ $3e74 M $3e75 M $3e76 M -> $1a $2f $5e }T
$aa72 >PC $39 >S $55 >A $5f >X $0f >Y $a9 >P $aa72 $1a >M $aa73 $dd >M $aa74 $f4 >M
T{ op PC S A X Y P -> $aa73 $39 $56 $5f $0f $29 }T T{ $aa72 M $aa73 M $aa74 M -> $1a $dd $f4 }T
$b394 >PC $61 >S $f2 >A $83 >X $33 >Y $ab >P $b394 $1a >M $b395 $48 >M $b396 $02 >M
T{ op PC S A X Y P -> $b395 $61 $f3 $83 $33 $a9 }T T{ $b394 M $b395 M $b396 M -> $1a $48 $02 }T
$83f3 >PC $ce >S $73 >A $db >X $52 >Y $ec >P $83f3 $1a >M $83f4 $21 >M $83f5 $b2 >M
T{ op PC S A X Y P -> $83f4 $ce $74 $db $52 $6c }T T{ $83f3 M $83f4 M $83f5 M -> $1a $21 $b2 }T
$b651 >PC $76 >S $2c >A $73 >X $6d >Y $6a >P $b651 $1a >M $b652 $11 >M $b653 $73 >M
T{ op PC S A X Y P -> $b652 $76 $2d $73 $6d $68 }T T{ $b651 M $b652 M $b653 M -> $1a $11 $73 }T
$4c0a >PC $3a >S $b6 >A $0b >X $d8 >Y $24 >P $4c0a $1a >M $4c0b $b7 >M $4c0c $04 >M
T{ op PC S A X Y P -> $4c0b $3a $b7 $0b $d8 $a4 }T T{ $4c0a M $4c0b M $4c0c M -> $1a $b7 $04 }T
$71b0 >PC $6e >S $31 >A $ff >X $43 >Y $6a >P $71b0 $1a >M $71b1 $aa >M $71b2 $b4 >M
T{ op PC S A X Y P -> $71b1 $6e $32 $ff $43 $68 }T T{ $71b0 M $71b1 M $71b2 M -> $1a $aa $b4 }T
$74d6 >PC $9f >S $a2 >A $25 >X $7d >Y $62 >P $74d6 $1a >M $74d7 $f9 >M $74d8 $89 >M
T{ op PC S A X Y P -> $74d7 $9f $a3 $25 $7d $e0 }T T{ $74d6 M $74d7 M $74d8 M -> $1a $f9 $89 }T
$0ecb >PC $00 >S $76 >A $28 >X $36 >Y $eb >P $0ecb $1a >M $0ecc $0b >M $0ecd $37 >M
T{ op PC S A X Y P -> $0ecc $00 $77 $28 $36 $69 }T T{ $0ecb M $0ecc M $0ecd M -> $1a $0b $37 }T
$b207 >PC $1d >S $47 >A $90 >X $c1 >Y $2b >P $b207 $1a >M $b208 $b6 >M $b209 $1b >M
T{ op PC S A X Y P -> $b208 $1d $48 $90 $c1 $29 }T T{ $b207 M $b208 M $b209 M -> $1a $b6 $1b }T
$2df6 >PC $6d >S $9a >A $2b >X $76 >Y $ea >P $2df6 $1a >M $2df7 $7e >M $2df8 $5d >M
T{ op PC S A X Y P -> $2df7 $6d $9b $2b $76 $e8 }T T{ $2df6 M $2df7 M $2df8 M -> $1a $7e $5d }T
$b388 >PC $57 >S $03 >A $fd >X $ff >Y $a0 >P $b388 $1a >M $b389 $cd >M $b38a $37 >M
T{ op PC S A X Y P -> $b389 $57 $04 $fd $ff $20 }T T{ $b388 M $b389 M $b38a M -> $1a $cd $37 }T
$f1c3 >PC $b8 >S $bd >A $b2 >X $b9 >Y $62 >P $f1c3 $1a >M $f1c4 $02 >M $f1c5 $64 >M
T{ op PC S A X Y P -> $f1c4 $b8 $be $b2 $b9 $e0 }T T{ $f1c3 M $f1c4 M $f1c5 M -> $1a $02 $64 }T
$a224 >PC $c6 >S $f9 >A $c9 >X $67 >Y $6d >P $a224 $1a >M $a225 $39 >M $a226 $21 >M
T{ op PC S A X Y P -> $a225 $c6 $fa $c9 $67 $ed }T T{ $a224 M $a225 M $a226 M -> $1a $39 $21 }T
( 1b )
$6e3a >PC $19 >S $d5 >A $c8 >X $d6 >Y $ea >P $6e3a $1b >M $6e3b $f1 >M $6e3c $6c >M
T{ op PC S A X Y P -> $6e3b $19 $d5 $c8 $d6 $ea }T T{ $6e3a M $6e3b M $6e3c M -> $1b $f1 $6c }T
$c586 >PC $95 >S $8d >A $d3 >X $a5 >Y $a9 >P $c586 $1b >M $c587 $b3 >M $c588 $85 >M
T{ op PC S A X Y P -> $c587 $95 $8d $d3 $a5 $a9 }T T{ $c586 M $c587 M $c588 M -> $1b $b3 $85 }T
$1ad6 >PC $39 >S $2f >A $ad >X $b3 >Y $eb >P $1ad6 $1b >M $1ad7 $e9 >M $1ad8 $32 >M
T{ op PC S A X Y P -> $1ad7 $39 $2f $ad $b3 $eb }T T{ $1ad6 M $1ad7 M $1ad8 M -> $1b $e9 $32 }T
$3d76 >PC $7d >S $d0 >A $99 >X $38 >Y $a1 >P $3d76 $1b >M $3d77 $69 >M $3d78 $b6 >M
T{ op PC S A X Y P -> $3d77 $7d $d0 $99 $38 $a1 }T T{ $3d76 M $3d77 M $3d78 M -> $1b $69 $b6 }T
$17bd >PC $b5 >S $3d >A $6a >X $f6 >Y $2b >P $17bd $1b >M $17be $3a >M $17bf $a7 >M
T{ op PC S A X Y P -> $17be $b5 $3d $6a $f6 $2b }T T{ $17bd M $17be M $17bf M -> $1b $3a $a7 }T
$3e5e >PC $e2 >S $51 >A $53 >X $87 >Y $60 >P $3e5e $1b >M $3e5f $c3 >M $3e60 $90 >M
T{ op PC S A X Y P -> $3e5f $e2 $51 $53 $87 $60 }T T{ $3e5e M $3e5f M $3e60 M -> $1b $c3 $90 }T
$2e4f >PC $a9 >S $c6 >A $07 >X $15 >Y $e5 >P $2e4f $1b >M $2e50 $4b >M $2e51 $d3 >M
T{ op PC S A X Y P -> $2e50 $a9 $c6 $07 $15 $e5 }T T{ $2e4f M $2e50 M $2e51 M -> $1b $4b $d3 }T
$75d4 >PC $91 >S $ce >A $31 >X $30 >Y $e3 >P $75d4 $1b >M $75d5 $ce >M $75d6 $37 >M
T{ op PC S A X Y P -> $75d5 $91 $ce $31 $30 $e3 }T T{ $75d4 M $75d5 M $75d6 M -> $1b $ce $37 }T
$4bf6 >PC $0d >S $3b >A $00 >X $68 >Y $a2 >P $4bf6 $1b >M $4bf7 $86 >M $4bf8 $93 >M
T{ op PC S A X Y P -> $4bf7 $0d $3b $00 $68 $a2 }T T{ $4bf6 M $4bf7 M $4bf8 M -> $1b $86 $93 }T
$6ae4 >PC $8b >S $8a >A $24 >X $61 >Y $e2 >P $6ae4 $1b >M $6ae5 $b2 >M $6ae6 $3a >M
T{ op PC S A X Y P -> $6ae5 $8b $8a $24 $61 $e2 }T T{ $6ae4 M $6ae5 M $6ae6 M -> $1b $b2 $3a }T
$c297 >PC $be >S $4d >A $a0 >X $d0 >Y $ed >P $c297 $1b >M $c298 $82 >M $c299 $67 >M
T{ op PC S A X Y P -> $c298 $be $4d $a0 $d0 $ed }T T{ $c297 M $c298 M $c299 M -> $1b $82 $67 }T
$01c5 >PC $4b >S $51 >A $4c >X $34 >Y $60 >P $01c5 $1b >M $01c6 $c5 >M $01c7 $57 >M
T{ op PC S A X Y P -> $01c6 $4b $51 $4c $34 $60 }T T{ $01c5 M $01c6 M $01c7 M -> $1b $c5 $57 }T
$e623 >PC $a5 >S $76 >A $18 >X $fe >Y $a3 >P $e623 $1b >M $e624 $d3 >M $e625 $f5 >M
T{ op PC S A X Y P -> $e624 $a5 $76 $18 $fe $a3 }T T{ $e623 M $e624 M $e625 M -> $1b $d3 $f5 }T
$1566 >PC $6c >S $44 >A $d9 >X $9a >Y $22 >P $1566 $1b >M $1567 $bf >M $1568 $85 >M
T{ op PC S A X Y P -> $1567 $6c $44 $d9 $9a $22 }T T{ $1566 M $1567 M $1568 M -> $1b $bf $85 }T
$a86c >PC $42 >S $ee >A $1d >X $4d >Y $2a >P $a86c $1b >M $a86d $87 >M $a86e $06 >M
T{ op PC S A X Y P -> $a86d $42 $ee $1d $4d $2a }T T{ $a86c M $a86d M $a86e M -> $1b $87 $06 }T
$c1b0 >PC $a6 >S $12 >A $b3 >X $e4 >Y $ed >P $c1b0 $1b >M $c1b1 $27 >M $c1b2 $fb >M
T{ op PC S A X Y P -> $c1b1 $a6 $12 $b3 $e4 $ed }T T{ $c1b0 M $c1b1 M $c1b2 M -> $1b $27 $fb }T
( 1c )
$b265 >PC $10 >S $87 >A $b4 >X $a4 >Y $ed >P $b265 $1c >M $b266 $dd >M $b267 $c9 >M $b268 $22 >M $c9dd $9c >M
T{ op PC S A X Y P -> $b268 $10 $87 $b4 $a4 $ed }T T{ $b265 M $b266 M $b267 M $b268 M $c9dd M -> $1c $dd $c9 $22 $18 }T
$c934 >PC $8b >S $4e >A $4d >X $ee >Y $26 >P $696e $b5 >M $c934 $1c >M $c935 $6e >M $c936 $69 >M $c937 $49 >M
T{ op PC S A X Y P -> $c937 $8b $4e $4d $ee $24 }T T{ $696e M $c934 M $c935 M $c936 M $c937 M -> $b1 $1c $6e $69 $49 }T
$d2ac >PC $3c >S $41 >A $66 >X $5f >Y $ec >P $d2ac $1c >M $d2ad $dc >M $d2ae $d3 >M $d2af $d9 >M $d3dc $fc >M
T{ op PC S A X Y P -> $d2af $3c $41 $66 $5f $ec }T T{ $d2ac M $d2ad M $d2ae M $d2af M $d3dc M -> $1c $dc $d3 $d9 $bc }T
$49c8 >PC $e3 >S $af >A $da >X $09 >Y $ac >P $49c8 $1c >M $49c9 $5c >M $49ca $d9 >M $49cb $c9 >M $d95c $0d >M
T{ op PC S A X Y P -> $49cb $e3 $af $da $09 $ac }T T{ $49c8 M $49c9 M $49ca M $49cb M $d95c M -> $1c $5c $d9 $c9 $00 }T
$803d >PC $76 >S $80 >A $1c >X $eb >Y $26 >P $3dd2 $ed >M $803d $1c >M $803e $d2 >M $803f $3d >M $8040 $f4 >M
T{ op PC S A X Y P -> $8040 $76 $80 $1c $eb $24 }T T{ $3dd2 M $803d M $803e M $803f M $8040 M -> $6d $1c $d2 $3d $f4 }T
$48d1 >PC $2e >S $41 >A $25 >X $0f >Y $24 >P $48d1 $1c >M $48d2 $f1 >M $48d3 $c6 >M $48d4 $92 >M $c6f1 $7f >M
T{ op PC S A X Y P -> $48d4 $2e $41 $25 $0f $24 }T T{ $48d1 M $48d2 M $48d3 M $48d4 M $c6f1 M -> $1c $f1 $c6 $92 $3e }T
$fa06 >PC $93 >S $2e >A $76 >X $9d >Y $e5 >P $efdc $20 >M $fa06 $1c >M $fa07 $dc >M $fa08 $ef >M $fa09 $12 >M
T{ op PC S A X Y P -> $fa09 $93 $2e $76 $9d $e5 }T T{ $efdc M $fa06 M $fa07 M $fa08 M $fa09 M -> $00 $1c $dc $ef $12 }T
$50d2 >PC $9f >S $89 >A $02 >X $80 >Y $ac >P $50d2 $1c >M $50d3 $43 >M $50d4 $a6 >M $50d5 $64 >M $a643 $60 >M
T{ op PC S A X Y P -> $50d5 $9f $89 $02 $80 $ae }T T{ $50d2 M $50d3 M $50d4 M $50d5 M $a643 M -> $1c $43 $a6 $64 $60 }T
$c4a3 >PC $48 >S $f7 >A $7b >X $f8 >Y $ae >P $c4a3 $1c >M $c4a4 $17 >M $c4a5 $f3 >M $c4a6 $4f >M $f317 $d2 >M
T{ op PC S A X Y P -> $c4a6 $48 $f7 $7b $f8 $ac }T T{ $c4a3 M $c4a4 M $c4a5 M $c4a6 M $f317 M -> $1c $17 $f3 $4f $00 }T
$9e62 >PC $4f >S $e7 >A $b3 >X $b5 >Y $e4 >P $9e62 $1c >M $9e63 $31 >M $9e64 $db >M $9e65 $9b >M $db31 $38 >M
T{ op PC S A X Y P -> $9e65 $4f $e7 $b3 $b5 $e4 }T T{ $9e62 M $9e63 M $9e64 M $9e65 M $db31 M -> $1c $31 $db $9b $18 }T
$cd76 >PC $64 >S $c1 >A $53 >X $09 >Y $2d >P $b188 $68 >M $cd76 $1c >M $cd77 $88 >M $cd78 $b1 >M $cd79 $84 >M
T{ op PC S A X Y P -> $cd79 $64 $c1 $53 $09 $2d }T T{ $b188 M $cd76 M $cd77 M $cd78 M $cd79 M -> $28 $1c $88 $b1 $84 }T
$621d >PC $a3 >S $33 >A $10 >X $fe >Y $6e >P $3d6c $fa >M $621d $1c >M $621e $6c >M $621f $3d >M $6220 $14 >M
T{ op PC S A X Y P -> $6220 $a3 $33 $10 $fe $6c }T T{ $3d6c M $621d M $621e M $621f M $6220 M -> $c8 $1c $6c $3d $14 }T
$669e >PC $8b >S $64 >A $46 >X $f5 >Y $28 >P $669e $1c >M $669f $9c >M $66a0 $f2 >M $66a1 $3a >M $f29c $70 >M
T{ op PC S A X Y P -> $66a1 $8b $64 $46 $f5 $28 }T T{ $669e M $669f M $66a0 M $66a1 M $f29c M -> $1c $9c $f2 $3a $10 }T
$a6f4 >PC $be >S $a9 >A $97 >X $36 >Y $ac >P $8067 $a0 >M $a6f4 $1c >M $a6f5 $67 >M $a6f6 $80 >M $a6f7 $29 >M
T{ op PC S A X Y P -> $a6f7 $be $a9 $97 $36 $ac }T T{ $8067 M $a6f4 M $a6f5 M $a6f6 M $a6f7 M -> $00 $1c $67 $80 $29 }T
$58fa >PC $58 >S $58 >A $40 >X $ff >Y $63 >P $58fa $1c >M $58fb $c2 >M $58fc $ae >M $58fd $30 >M $aec2 $e2 >M
T{ op PC S A X Y P -> $58fd $58 $58 $40 $ff $61 }T T{ $58fa M $58fb M $58fc M $58fd M $aec2 M -> $1c $c2 $ae $30 $a2 }T
$26fa >PC $0a >S $4d >A $53 >X $b4 >Y $a0 >P $0926 $fa >M $26fa $1c >M $26fb $26 >M $26fc $09 >M $26fd $08 >M
T{ op PC S A X Y P -> $26fd $0a $4d $53 $b4 $a0 }T T{ $0926 M $26fa M $26fb M $26fc M $26fd M -> $b2 $1c $26 $09 $08 }T
( 1d )
$81a5 >PC $81 >S $bf >A $66 >X $82 >Y $2b >P $81a5 $1d >M $81a6 $f7 >M $81a7 $8c >M $81a8 $01 >M $8d5d $53 >M
T{ op PC S A X Y P -> $81a8 $81 $ff $66 $82 $a9 }T T{ $81a5 M $81a6 M $81a7 M $81a8 M $8d5d M -> $1d $f7 $8c $01 $53 }T
$4ee1 >PC $cb >S $70 >A $25 >X $a2 >Y $66 >P $4ee1 $1d >M $4ee2 $2c >M $4ee3 $7f >M $4ee4 $a4 >M $7f51 $20 >M
T{ op PC S A X Y P -> $4ee4 $cb $70 $25 $a2 $64 }T T{ $4ee1 M $4ee2 M $4ee3 M $4ee4 M $7f51 M -> $1d $2c $7f $a4 $20 }T
$8174 >PC $5c >S $ea >A $6d >X $be >Y $e4 >P $8174 $1d >M $8175 $81 >M $8176 $f0 >M $8177 $ef >M $f0ee $5f >M
T{ op PC S A X Y P -> $8177 $5c $ff $6d $be $e4 }T T{ $8174 M $8175 M $8176 M $8177 M $f0ee M -> $1d $81 $f0 $ef $5f }T
$fb12 >PC $2d >S $73 >A $08 >X $d4 >Y $6d >P $5b5e $cd >M $fb12 $1d >M $fb13 $56 >M $fb14 $5b >M $fb15 $48 >M
T{ op PC S A X Y P -> $fb15 $2d $ff $08 $d4 $ed }T T{ $5b5e M $fb12 M $fb13 M $fb14 M $fb15 M -> $cd $1d $56 $5b $48 }T
$a567 >PC $37 >S $3f >A $82 >X $a6 >Y $a3 >P $a567 $1d >M $a568 $5a >M $a569 $d0 >M $a56a $0c >M $d0dc $c1 >M
T{ op PC S A X Y P -> $a56a $37 $ff $82 $a6 $a1 }T T{ $a567 M $a568 M $a569 M $a56a M $d0dc M -> $1d $5a $d0 $0c $c1 }T
$f6e5 >PC $a3 >S $9e >A $42 >X $b2 >Y $63 >P $5bb0 $99 >M $f6e5 $1d >M $f6e6 $6e >M $f6e7 $5b >M $f6e8 $dd >M
T{ op PC S A X Y P -> $f6e8 $a3 $9f $42 $b2 $e1 }T T{ $5bb0 M $f6e5 M $f6e6 M $f6e7 M $f6e8 M -> $99 $1d $6e $5b $dd }T
$233a >PC $b3 >S $6b >A $34 >X $d7 >Y $27 >P $233a $1d >M $233b $84 >M $233c $2d >M $233d $b9 >M $2db8 $ef >M
T{ op PC S A X Y P -> $233d $b3 $ef $34 $d7 $a5 }T T{ $233a M $233b M $233c M $233d M $2db8 M -> $1d $84 $2d $b9 $ef }T
$e374 >PC $e2 >S $06 >A $2b >X $64 >Y $61 >P $6048 $f4 >M $e374 $1d >M $e375 $1d >M $e376 $60 >M $e377 $f0 >M
T{ op PC S A X Y P -> $e377 $e2 $f6 $2b $64 $e1 }T T{ $6048 M $e374 M $e375 M $e376 M $e377 M -> $f4 $1d $1d $60 $f0 }T
$e427 >PC $76 >S $65 >A $b8 >X $a8 >Y $2e >P $73d4 $a8 >M $e427 $1d >M $e428 $1c >M $e429 $73 >M $e42a $cd >M
T{ op PC S A X Y P -> $e42a $76 $ed $b8 $a8 $ac }T T{ $73d4 M $e427 M $e428 M $e429 M $e42a M -> $a8 $1d $1c $73 $cd }T
$60fc >PC $45 >S $66 >A $c4 >X $e2 >Y $a1 >P $60fc $1d >M $60fd $0f >M $60fe $99 >M $60ff $cf >M $99d3 $cf >M
T{ op PC S A X Y P -> $60ff $45 $ef $c4 $e2 $a1 }T T{ $60fc M $60fd M $60fe M $60ff M $99d3 M -> $1d $0f $99 $cf $cf }T
$4b20 >PC $43 >S $f8 >A $d8 >X $fd >Y $a8 >P $4b20 $1d >M $4b21 $3d >M $4b22 $b3 >M $4b23 $85 >M $b415 $f2 >M
T{ op PC S A X Y P -> $4b23 $43 $fa $d8 $fd $a8 }T T{ $4b20 M $4b21 M $4b22 M $4b23 M $b415 M -> $1d $3d $b3 $85 $f2 }T
$bf61 >PC $12 >S $65 >A $af >X $0c >Y $e4 >P $68bc $9d >M $bf61 $1d >M $bf62 $0d >M $bf63 $68 >M $bf64 $0a >M
T{ op PC S A X Y P -> $bf64 $12 $fd $af $0c $e4 }T T{ $68bc M $bf61 M $bf62 M $bf63 M $bf64 M -> $9d $1d $0d $68 $0a }T
$44e9 >PC $7b >S $d9 >A $49 >X $5f >Y $a5 >P $44e9 $1d >M $44ea $c1 >M $44eb $ae >M $44ec $ea >M $af0a $b2 >M
T{ op PC S A X Y P -> $44ec $7b $fb $49 $5f $a5 }T T{ $44e9 M $44ea M $44eb M $44ec M $af0a M -> $1d $c1 $ae $ea $b2 }T
$217c >PC $ec >S $b2 >A $37 >X $58 >Y $a0 >P $217c $1d >M $217d $bf >M $217e $3d >M $217f $1e >M $3df6 $47 >M
T{ op PC S A X Y P -> $217f $ec $f7 $37 $58 $a0 }T T{ $217c M $217d M $217e M $217f M $3df6 M -> $1d $bf $3d $1e $47 }T
$5d97 >PC $1e >S $ee >A $3a >X $30 >Y $24 >P $0c95 $e5 >M $5d97 $1d >M $5d98 $5b >M $5d99 $0c >M $5d9a $95 >M
T{ op PC S A X Y P -> $5d9a $1e $ef $3a $30 $a4 }T T{ $0c95 M $5d97 M $5d98 M $5d99 M $5d9a M -> $e5 $1d $5b $0c $95 }T
$39e7 >PC $5a >S $39 >A $49 >X $5f >Y $e4 >P $1a77 $dc >M $39e7 $1d >M $39e8 $2e >M $39e9 $1a >M $39ea $fe >M
T{ op PC S A X Y P -> $39ea $5a $fd $49 $5f $e4 }T T{ $1a77 M $39e7 M $39e8 M $39e9 M $39ea M -> $dc $1d $2e $1a $fe }T
( 1e )
$5007 >PC $1a >S $1f >A $48 >X $ac >Y $ac >P $1a37 $38 >M $5007 $1e >M $5008 $ef >M $5009 $19 >M $500a $64 >M
T{ op PC S A X Y P -> $500a $1a $1f $48 $ac $2c }T T{ $1a37 M $5007 M $5008 M $5009 M $500a M -> $70 $1e $ef $19 $64 }T
$a83f >PC $e7 >S $1c >A $4e >X $06 >Y $25 >P $30a0 $3e >M $a83f $1e >M $a840 $52 >M $a841 $30 >M $a842 $ef >M
T{ op PC S A X Y P -> $a842 $e7 $1c $4e $06 $24 }T T{ $30a0 M $a83f M $a840 M $a841 M $a842 M -> $7c $1e $52 $30 $ef }T
$ccd3 >PC $82 >S $77 >A $9e >X $37 >Y $e0 >P $69fb $b2 >M $ccd3 $1e >M $ccd4 $5d >M $ccd5 $69 >M $ccd6 $b7 >M
T{ op PC S A X Y P -> $ccd6 $82 $77 $9e $37 $61 }T T{ $69fb M $ccd3 M $ccd4 M $ccd5 M $ccd6 M -> $64 $1e $5d $69 $b7 }T
$473c >PC $5b >S $bd >A $a8 >X $1c >Y $ea >P $473c $1e >M $473d $e2 >M $473e $aa >M $473f $fd >M $ab8a $b6 >M
T{ op PC S A X Y P -> $473f $5b $bd $a8 $1c $69 }T T{ $473c M $473d M $473e M $473f M $ab8a M -> $1e $e2 $aa $fd $6c }T
$b82a >PC $74 >S $f6 >A $ac >X $7e >Y $a7 >P $b82a $1e >M $b82b $a5 >M $b82c $d7 >M $b82d $c5 >M $d851 $f9 >M
T{ op PC S A X Y P -> $b82d $74 $f6 $ac $7e $a5 }T T{ $b82a M $b82b M $b82c M $b82d M $d851 M -> $1e $a5 $d7 $c5 $f2 }T
$e00f >PC $a0 >S $f8 >A $12 >X $20 >Y $6f >P $e00f $1e >M $e010 $20 >M $e011 $e1 >M $e012 $2f >M $e132 $e1 >M
T{ op PC S A X Y P -> $e012 $a0 $f8 $12 $20 $ed }T T{ $e00f M $e010 M $e011 M $e012 M $e132 M -> $1e $20 $e1 $2f $c2 }T
$a355 >PC $c2 >S $6b >A $36 >X $d3 >Y $27 >P $9c3c $5d >M $a355 $1e >M $a356 $06 >M $a357 $9c >M $a358 $a2 >M
T{ op PC S A X Y P -> $a358 $c2 $6b $36 $d3 $a4 }T T{ $9c3c M $a355 M $a356 M $a357 M $a358 M -> $ba $1e $06 $9c $a2 }T
$056a >PC $a0 >S $9f >A $0f >X $25 >Y $6b >P $056a $1e >M $056b $d8 >M $056c $5c >M $056d $13 >M $5ce7 $49 >M
T{ op PC S A X Y P -> $056d $a0 $9f $0f $25 $e8 }T T{ $056a M $056b M $056c M $056d M $5ce7 M -> $1e $d8 $5c $13 $92 }T
$7fc9 >PC $1a >S $f3 >A $8f >X $62 >Y $eb >P $03ea $4f >M $7fc9 $1e >M $7fca $5b >M $7fcb $03 >M $7fcc $21 >M
T{ op PC S A X Y P -> $7fcc $1a $f3 $8f $62 $e8 }T T{ $03ea M $7fc9 M $7fca M $7fcb M $7fcc M -> $9e $1e $5b $03 $21 }T
$ac0c >PC $d3 >S $ff >A $b5 >X $c8 >Y $a8 >P $00aa $df >M $ac0c $1e >M $ac0d $f5 >M $ac0e $ff >M $ac0f $2b >M
T{ op PC S A X Y P -> $ac0f $d3 $ff $b5 $c8 $a9 }T T{ $00aa M $ac0c M $ac0d M $ac0e M $ac0f M -> $be $1e $f5 $ff $2b }T
$85fc >PC $95 >S $f6 >A $49 >X $7b >Y $2b >P $85fc $1e >M $85fd $e8 >M $85fe $88 >M $85ff $3d >M $8931 $13 >M
T{ op PC S A X Y P -> $85ff $95 $f6 $49 $7b $28 }T T{ $85fc M $85fd M $85fe M $85ff M $8931 M -> $1e $e8 $88 $3d $26 }T
$feff >PC $97 >S $3c >A $2e >X $ae >Y $ed >P $03c2 $34 >M $feff $1e >M $ff00 $94 >M $ff01 $03 >M $ff02 $0b >M
T{ op PC S A X Y P -> $ff02 $97 $3c $2e $ae $6c }T T{ $03c2 M $feff M $ff00 M $ff01 M $ff02 M -> $68 $1e $94 $03 $0b }T
$1747 >PC $e3 >S $4d >A $86 >X $cf >Y $ae >P $1747 $1e >M $1748 $03 >M $1749 $52 >M $174a $2b >M $5289 $74 >M
T{ op PC S A X Y P -> $174a $e3 $4d $86 $cf $ac }T T{ $1747 M $1748 M $1749 M $174a M $5289 M -> $1e $03 $52 $2b $e8 }T
$941f >PC $ab >S $c8 >A $b2 >X $c6 >Y $e2 >P $2284 $ee >M $941f $1e >M $9420 $d2 >M $9421 $21 >M $9422 $51 >M
T{ op PC S A X Y P -> $9422 $ab $c8 $b2 $c6 $e1 }T T{ $2284 M $941f M $9420 M $9421 M $9422 M -> $dc $1e $d2 $21 $51 }T
$9003 >PC $90 >S $83 >A $ee >X $61 >Y $a6 >P $4e5b $7f >M $9003 $1e >M $9004 $6d >M $9005 $4d >M $9006 $61 >M
T{ op PC S A X Y P -> $9006 $90 $83 $ee $61 $a4 }T T{ $4e5b M $9003 M $9004 M $9005 M $9006 M -> $fe $1e $6d $4d $61 }T
$2256 >PC $db >S $84 >A $55 >X $65 >Y $29 >P $2256 $1e >M $2257 $2e >M $2258 $bb >M $2259 $78 >M $bb83 $13 >M
T{ op PC S A X Y P -> $2259 $db $84 $55 $65 $28 }T T{ $2256 M $2257 M $2258 M $2259 M $bb83 M -> $1e $2e $bb $78 $26 }T
( 1f )
$af0e >PC $d7 >S $85 >A $99 >X $86 >Y $e1 >P $0023 $78 >M $af0e $1f >M $af0f $23 >M $af10 $4a >M $af5b $e8 >M
T{ op PC S A X Y P -> $af5b $d7 $85 $99 $86 $e1 }T T{ $0023 M $af0e M $af0f M $af10 M $af5b M -> $78 $1f $23 $4a $e8 }T
$a4b4 >PC $9b >S $c0 >A $38 >X $0b >Y $a2 >P $0057 $1b >M $a420 $ee >M $a4b4 $1f >M $a4b5 $57 >M $a4b6 $69 >M $a4b7 $b0 >M
T{ op PC S A X Y P -> $a4b7 $9b $c0 $38 $0b $a2 }T T{ $0057 M $a420 M $a4b4 M $a4b5 M $a4b6 M $a4b7 M -> $1b $ee $1f $57 $69 $b0 }T
$0961 >PC $ec >S $ee >A $b7 >X $09 >Y $66 >P $0008 $13 >M $0961 $1f >M $0962 $08 >M $0963 $1d >M $0964 $de >M $0981 $bb >M
T{ op PC S A X Y P -> $0964 $ec $ee $b7 $09 $66 }T T{ $0008 M $0961 M $0962 M $0963 M $0964 M $0981 M -> $13 $1f $08 $1d $de $bb }T
$880d >PC $f6 >S $6a >A $fa >X $0a >Y $65 >P $004e $22 >M $880d $1f >M $880e $4e >M $880f $9d >M $8810 $30 >M $88ad $e3 >M
T{ op PC S A X Y P -> $8810 $f6 $6a $fa $0a $65 }T T{ $004e M $880d M $880e M $880f M $8810 M $88ad M -> $22 $1f $4e $9d $30 $e3 }T
$73c3 >PC $4b >S $ac >A $53 >X $e2 >Y $e4 >P $001f $6a >M $7342 $47 >M $73c3 $1f >M $73c4 $1f >M $73c5 $7c >M $73c6 $e4 >M
T{ op PC S A X Y P -> $73c6 $4b $ac $53 $e2 $e4 }T T{ $001f M $7342 M $73c3 M $73c4 M $73c5 M $73c6 M -> $6a $47 $1f $1f $7c $e4 }T
$b858 >PC $b6 >S $ee >A $76 >X $88 >Y $e1 >P $00bc $0c >M $b7db $de >M $b858 $1f >M $b859 $bc >M $b85a $80 >M $b8db $8e >M
T{ op PC S A X Y P -> $b7db $b6 $ee $76 $88 $e1 }T T{ $00bc M $b7db M $b858 M $b859 M $b85a M $b8db M -> $0c $de $1f $bc $80 $8e }T
$f85a >PC $31 >S $df >A $20 >X $61 >Y $62 >P $0011 $15 >M $f811 $a5 >M $f85a $1f >M $f85b $11 >M $f85c $b4 >M
T{ op PC S A X Y P -> $f811 $31 $df $20 $61 $62 }T T{ $0011 M $f811 M $f85a M $f85b M $f85c M -> $15 $a5 $1f $11 $b4 }T
$1f68 >PC $8d >S $cb >A $76 >X $03 >Y $ec >P $0067 $a7 >M $1f68 $1f >M $1f69 $67 >M $1f6a $7b >M $1f6b $6f >M $1fe6 $b1 >M
T{ op PC S A X Y P -> $1f6b $8d $cb $76 $03 $ec }T T{ $0067 M $1f68 M $1f69 M $1f6a M $1f6b M $1fe6 M -> $a7 $1f $67 $7b $6f $b1 }T
$876f >PC $fe >S $93 >A $8f >X $37 >Y $e6 >P $00fc $c6 >M $876f $1f >M $8770 $fc >M $8771 $21 >M $8772 $36 >M $8793 $4f >M
T{ op PC S A X Y P -> $8772 $fe $93 $8f $37 $e6 }T T{ $00fc M $876f M $8770 M $8771 M $8772 M $8793 M -> $c6 $1f $fc $21 $36 $4f }T
$7e0b >PC $76 >S $16 >A $3c >X $df >Y $6b >P $00ca $64 >M $7dd5 $76 >M $7e0b $1f >M $7e0c $ca >M $7e0d $c7 >M $7ed5 $12 >M
T{ op PC S A X Y P -> $7dd5 $76 $16 $3c $df $6b }T T{ $00ca M $7dd5 M $7e0b M $7e0c M $7e0d M $7ed5 M -> $64 $76 $1f $ca $c7 $12 }T
$27e5 >PC $41 >S $69 >A $4e >X $52 >Y $a1 >P $00ec $a4 >M $2744 $82 >M $27e5 $1f >M $27e6 $ec >M $27e7 $5c >M $2844 $e6 >M
T{ op PC S A X Y P -> $2844 $41 $69 $4e $52 $a1 }T T{ $00ec M $2744 M $27e5 M $27e6 M $27e7 M $2844 M -> $a4 $82 $1f $ec $5c $e6 }T
$d325 >PC $22 >S $e2 >A $75 >X $61 >Y $6a >P $0094 $56 >M $d325 $1f >M $d326 $94 >M $d327 $b7 >M $d328 $16 >M $d3df $0b >M
T{ op PC S A X Y P -> $d328 $22 $e2 $75 $61 $6a }T T{ $0094 M $d325 M $d326 M $d327 M $d328 M $d3df M -> $56 $1f $94 $b7 $16 $0b }T
$4688 >PC $fb >S $c3 >A $e7 >X $bd >Y $25 >P $00ec $31 >M $467e $3e >M $4688 $1f >M $4689 $ec >M $468a $f3 >M
T{ op PC S A X Y P -> $467e $fb $c3 $e7 $bd $25 }T T{ $00ec M $467e M $4688 M $4689 M $468a M -> $31 $3e $1f $ec $f3 }T
$889a >PC $79 >S $cd >A $ef >X $34 >Y $a3 >P $009e $b7 >M $889a $1f >M $889b $9e >M $889c $55 >M $889d $65 >M $88f2 $00 >M
T{ op PC S A X Y P -> $889d $79 $cd $ef $34 $a3 }T T{ $009e M $889a M $889b M $889c M $889d M $88f2 M -> $b7 $1f $9e $55 $65 $00 }T
$c1aa >PC $6d >S $56 >A $c0 >X $c2 >Y $ee >P $00c0 $51 >M $c13d $99 >M $c1aa $1f >M $c1ab $c0 >M $c1ac $90 >M
T{ op PC S A X Y P -> $c13d $6d $56 $c0 $c2 $ee }T T{ $00c0 M $c13d M $c1aa M $c1ab M $c1ac M -> $51 $99 $1f $c0 $90 }T
$75aa >PC $2f >S $73 >A $c4 >X $52 >Y $66 >P $005f $bb >M $75aa $1f >M $75ab $5f >M $75ac $49 >M $75ad $d7 >M $75f6 $5c >M
T{ op PC S A X Y P -> $75ad $2f $73 $c4 $52 $66 }T T{ $005f M $75aa M $75ab M $75ac M $75ad M $75f6 M -> $bb $1f $5f $49 $d7 $5c }T
( 20 )
$fdc6 >PC $bc >S $24 >A $43 >X $41 >Y $25 >P $01bc $42 >M $15d3 $6b >M $fdc6 $20 >M $fdc7 $d3 >M $fdc8 $15 >M
T{ op PC S A X Y P -> $15d3 $ba $24 $43 $41 $25 }T T{ $01bb M $01bc M $15d3 M $fdc6 M $fdc7 M $fdc8 M -> $c8 $fd $6b $20 $d3 $15 }T
$cd20 >PC $68 >S $1e >A $99 >X $6d >Y $26 >P $0168 $ee >M $cd20 $20 >M $cd21 $83 >M $cd22 $de >M $de83 $b7 >M
T{ op PC S A X Y P -> $de83 $66 $1e $99 $6d $26 }T T{ $0167 M $0168 M $cd20 M $cd21 M $cd22 M $de83 M -> $22 $cd $20 $83 $de $b7 }T
$22b6 >PC $26 >S $19 >A $42 >X $1d >Y $ae >P $0126 $7b >M $22b6 $20 >M $22b7 $d8 >M $22b8 $28 >M $28d8 $bf >M
T{ op PC S A X Y P -> $28d8 $24 $19 $42 $1d $ae }T T{ $0125 M $0126 M $22b6 M $22b7 M $22b8 M $28d8 M -> $b8 $22 $20 $d8 $28 $bf }T
$806e >PC $91 >S $c8 >A $98 >X $40 >Y $25 >P $0191 $d2 >M $2b48 $23 >M $806e $20 >M $806f $48 >M $8070 $2b >M
T{ op PC S A X Y P -> $2b48 $8f $c8 $98 $40 $25 }T T{ $0190 M $0191 M $2b48 M $806e M $806f M $8070 M -> $70 $80 $23 $20 $48 $2b }T
$03a3 >PC $3e >S $77 >A $93 >X $28 >Y $e4 >P $013e $82 >M $03a3 $20 >M $03a4 $95 >M $03a5 $da >M $da95 $a3 >M
T{ op PC S A X Y P -> $da95 $3c $77 $93 $28 $e4 }T T{ $013d M $013e M $03a3 M $03a4 M $03a5 M $da95 M -> $a5 $03 $20 $95 $da $a3 }T
$f221 >PC $c5 >S $6a >A $df >X $b0 >Y $63 >P $01c5 $f5 >M $af0c $74 >M $f221 $20 >M $f222 $0c >M $f223 $af >M
T{ op PC S A X Y P -> $af0c $c3 $6a $df $b0 $63 }T T{ $01c4 M $01c5 M $af0c M $f221 M $f222 M $f223 M -> $23 $f2 $74 $20 $0c $af }T
$b1c3 >PC $d7 >S $1f >A $7b >X $4b >Y $ec >P $01d7 $fe >M $4bd5 $a0 >M $b1c3 $20 >M $b1c4 $d5 >M $b1c5 $4b >M
T{ op PC S A X Y P -> $4bd5 $d5 $1f $7b $4b $ec }T T{ $01d6 M $01d7 M $4bd5 M $b1c3 M $b1c4 M $b1c5 M -> $c5 $b1 $a0 $20 $d5 $4b }T
$a294 >PC $e8 >S $1e >A $b8 >X $54 >Y $a3 >P $01e8 $3c >M $728f $bb >M $a294 $20 >M $a295 $8f >M $a296 $72 >M
T{ op PC S A X Y P -> $728f $e6 $1e $b8 $54 $a3 }T T{ $01e7 M $01e8 M $728f M $a294 M $a295 M $a296 M -> $96 $a2 $bb $20 $8f $72 }T
$cb20 >PC $eb >S $b5 >A $98 >X $1f >Y $25 >P $01eb $4a >M $617d $99 >M $cb20 $20 >M $cb21 $7d >M $cb22 $61 >M
T{ op PC S A X Y P -> $617d $e9 $b5 $98 $1f $25 }T T{ $01ea M $01eb M $617d M $cb20 M $cb21 M $cb22 M -> $22 $cb $99 $20 $7d $61 }T
$1ceb >PC $9b >S $80 >A $3a >X $93 >Y $a5 >P $019b $12 >M $1ceb $20 >M $1cec $32 >M $1ced $29 >M $2932 $7d >M
T{ op PC S A X Y P -> $2932 $99 $80 $3a $93 $a5 }T T{ $019a M $019b M $1ceb M $1cec M $1ced M $2932 M -> $ed $1c $20 $32 $29 $7d }T
$f764 >PC $df >S $de >A $22 >X $a1 >Y $e6 >P $01df $43 >M $02ce $88 >M $f764 $20 >M $f765 $ce >M $f766 $02 >M
T{ op PC S A X Y P -> $02ce $dd $de $22 $a1 $e6 }T T{ $01de M $01df M $02ce M $f764 M $f765 M $f766 M -> $66 $f7 $88 $20 $ce $02 }T
$233b >PC $20 >S $71 >A $d3 >X $7e >Y $ab >P $0120 $0e >M $02f1 $1a >M $233b $20 >M $233c $f1 >M $233d $02 >M
T{ op PC S A X Y P -> $02f1 $1e $71 $d3 $7e $ab }T T{ $011f M $0120 M $02f1 M $233b M $233c M $233d M -> $3d $23 $1a $20 $f1 $02 }T
$307c >PC $43 >S $39 >A $b0 >X $16 >Y $af >P $0143 $b7 >M $307c $20 >M $307d $ab >M $307e $f8 >M $f8ab $af >M
T{ op PC S A X Y P -> $f8ab $41 $39 $b0 $16 $af }T T{ $0142 M $0143 M $307c M $307d M $307e M $f8ab M -> $7e $30 $20 $ab $f8 $af }T
$08ac >PC $f1 >S $4e >A $95 >X $1b >Y $24 >P $01f1 $84 >M $08ac $20 >M $08ad $7c >M $08ae $ac >M $ac7c $01 >M
T{ op PC S A X Y P -> $ac7c $ef $4e $95 $1b $24 }T T{ $01f0 M $01f1 M $08ac M $08ad M $08ae M $ac7c M -> $ae $08 $20 $7c $ac $01 }T
$aa32 >PC $0c >S $74 >A $8d >X $ab >Y $a7 >P $010c $67 >M $6a69 $75 >M $aa32 $20 >M $aa33 $69 >M $aa34 $6a >M
T{ op PC S A X Y P -> $6a69 $0a $74 $8d $ab $a7 }T T{ $010b M $010c M $6a69 M $aa32 M $aa33 M $aa34 M -> $34 $aa $75 $20 $69 $6a }T
$0a09 >PC $94 >S $b9 >A $af >X $6c >Y $65 >P $0194 $40 >M $0a09 $20 >M $0a0a $3b >M $0a0b $12 >M $123b $06 >M
T{ op PC S A X Y P -> $123b $92 $b9 $af $6c $65 }T T{ $0193 M $0194 M $0a09 M $0a0a M $0a0b M $123b M -> $0b $0a $20 $3b $12 $06 }T
( 21 )
$5e1b >PC $a5 >S $33 >A $3b >X $39 >Y $ab >P $0032 $00 >M $0033 $92 >M $00f7 $51 >M $5e1b $21 >M $5e1c $f7 >M $5e1d $5e >M $9200 $bd >M
T{ op PC S A X Y P -> $5e1d $a5 $31 $3b $39 $29 }T T{ $0032 M $0033 M $00f7 M $5e1b M $5e1c M $5e1d M $9200 M -> $00 $92 $51 $21 $f7 $5e $bd }T
$0d77 >PC $9c >S $be >A $d1 >X $c7 >Y $25 >P $0037 $0c >M $0038 $ae >M $0066 $c1 >M $0d77 $21 >M $0d78 $66 >M $0d79 $a3 >M $ae0c $92 >M
T{ op PC S A X Y P -> $0d79 $9c $92 $d1 $c7 $a5 }T T{ $0037 M $0038 M $0066 M $0d77 M $0d78 M $0d79 M $ae0c M -> $0c $ae $c1 $21 $66 $a3 $92 }T
$2c6b >PC $d0 >S $83 >A $c6 >X $91 >Y $ac >P $0024 $f2 >M $0025 $7a >M $005e $ef >M $2c6b $21 >M $2c6c $5e >M $2c6d $4c >M $7af2 $90 >M
T{ op PC S A X Y P -> $2c6d $d0 $80 $c6 $91 $ac }T T{ $0024 M $0025 M $005e M $2c6b M $2c6c M $2c6d M $7af2 M -> $f2 $7a $ef $21 $5e $4c $90 }T
$86e0 >PC $b9 >S $f3 >A $a0 >X $15 >Y $ef >P $0044 $ed >M $00e4 $6c >M $00e5 $84 >M $846c $e7 >M $86e0 $21 >M $86e1 $44 >M $86e2 $9e >M
T{ op PC S A X Y P -> $86e2 $b9 $e3 $a0 $15 $ed }T T{ $0044 M $00e4 M $00e5 M $846c M $86e0 M $86e1 M $86e2 M -> $ed $6c $84 $e7 $21 $44 $9e }T
$8452 >PC $66 >S $69 >A $a3 >X $f3 >Y $63 >P $0073 $92 >M $0074 $b3 >M $00d0 $33 >M $8452 $21 >M $8453 $d0 >M $8454 $b3 >M $b392 $95 >M
T{ op PC S A X Y P -> $8454 $66 $01 $a3 $f3 $61 }T T{ $0073 M $0074 M $00d0 M $8452 M $8453 M $8454 M $b392 M -> $92 $b3 $33 $21 $d0 $b3 $95 }T
$7aec >PC $f5 >S $07 >A $69 >X $b1 >Y $29 >P $0032 $5b >M $009b $da >M $009c $58 >M $58da $55 >M $7aec $21 >M $7aed $32 >M $7aee $48 >M
T{ op PC S A X Y P -> $7aee $f5 $05 $69 $b1 $29 }T T{ $0032 M $009b M $009c M $58da M $7aec M $7aed M $7aee M -> $5b $da $58 $55 $21 $32 $48 }T
$bed3 >PC $50 >S $b0 >A $b0 >X $ab >Y $a4 >P $0021 $5e >M $0022 $98 >M $0071 $9c >M $985e $49 >M $bed3 $21 >M $bed4 $71 >M $bed5 $e3 >M
T{ op PC S A X Y P -> $bed5 $50 $00 $b0 $ab $26 }T T{ $0021 M $0022 M $0071 M $985e M $bed3 M $bed4 M $bed5 M -> $5e $98 $9c $49 $21 $71 $e3 }T
$2a59 >PC $f9 >S $9c >A $f4 >X $78 >Y $ae >P $0046 $ce >M $0047 $e2 >M $0052 $80 >M $2a59 $21 >M $2a5a $52 >M $2a5b $b8 >M $e2ce $b9 >M
T{ op PC S A X Y P -> $2a5b $f9 $98 $f4 $78 $ac }T T{ $0046 M $0047 M $0052 M $2a59 M $2a5a M $2a5b M $e2ce M -> $ce $e2 $80 $21 $52 $b8 $b9 }T
$3f7c >PC $db >S $1e >A $37 >X $77 >Y $22 >P $00bb $21 >M $00f2 $66 >M $00f3 $39 >M $3966 $cd >M $3f7c $21 >M $3f7d $bb >M $3f7e $cf >M
T{ op PC S A X Y P -> $3f7e $db $0c $37 $77 $20 }T T{ $00bb M $00f2 M $00f3 M $3966 M $3f7c M $3f7d M $3f7e M -> $21 $66 $39 $cd $21 $bb $cf }T
$a735 >PC $6f >S $6b >A $e7 >X $50 >Y $63 >P $00ac $ac >M $00ad $ea >M $00c5 $14 >M $a735 $21 >M $a736 $c5 >M $a737 $2e >M $eaac $85 >M
T{ op PC S A X Y P -> $a737 $6f $01 $e7 $50 $61 }T T{ $00ac M $00ad M $00c5 M $a735 M $a736 M $a737 M $eaac M -> $ac $ea $14 $21 $c5 $2e $85 }T
$6e3f >PC $5a >S $e6 >A $7d >X $49 >Y $ac >P $004c $45 >M $00c9 $09 >M $00ca $1d >M $1d09 $07 >M $6e3f $21 >M $6e40 $4c >M $6e41 $f2 >M
T{ op PC S A X Y P -> $6e41 $5a $06 $7d $49 $2c }T T{ $004c M $00c9 M $00ca M $1d09 M $6e3f M $6e40 M $6e41 M -> $45 $09 $1d $07 $21 $4c $f2 }T
$6eca >PC $b0 >S $75 >A $bb >X $ef >Y $ea >P $0030 $ef >M $0031 $16 >M $0075 $70 >M $16ef $0d >M $6eca $21 >M $6ecb $75 >M $6ecc $b8 >M
T{ op PC S A X Y P -> $6ecc $b0 $05 $bb $ef $68 }T T{ $0030 M $0031 M $0075 M $16ef M $6eca M $6ecb M $6ecc M -> $ef $16 $70 $0d $21 $75 $b8 }T
$0375 >PC $9e >S $bf >A $bc >X $ae >Y $a5 >P $0033 $f8 >M $0034 $8f >M $0077 $f4 >M $0375 $21 >M $0376 $77 >M $0377 $fc >M $8ff8 $a1 >M
T{ op PC S A X Y P -> $0377 $9e $a1 $bc $ae $a5 }T T{ $0033 M $0034 M $0077 M $0375 M $0376 M $0377 M $8ff8 M -> $f8 $8f $f4 $21 $77 $fc $a1 }T
$29a2 >PC $70 >S $00 >A $80 >X $ad >Y $af >P $0037 $9f >M $0038 $3f >M $00b7 $d0 >M $29a2 $21 >M $29a3 $b7 >M $29a4 $d2 >M $3f9f $eb >M
T{ op PC S A X Y P -> $29a4 $70 $00 $80 $ad $2f }T T{ $0037 M $0038 M $00b7 M $29a2 M $29a3 M $29a4 M $3f9f M -> $9f $3f $d0 $21 $b7 $d2 $eb }T
$c535 >PC $5a >S $f5 >A $39 >X $1e >Y $eb >P $0088 $f0 >M $00c1 $0a >M $00c2 $db >M $c535 $21 >M $c536 $88 >M $c537 $0f >M $db0a $43 >M
T{ op PC S A X Y P -> $c537 $5a $41 $39 $1e $69 }T T{ $0088 M $00c1 M $00c2 M $c535 M $c536 M $c537 M $db0a M -> $f0 $0a $db $21 $88 $0f $43 }T
$598d >PC $29 >S $9f >A $d5 >X $1c >Y $a4 >P $0051 $52 >M $0052 $b1 >M $007c $e7 >M $598d $21 >M $598e $7c >M $598f $7a >M $b152 $dd >M
T{ op PC S A X Y P -> $598f $29 $9d $d5 $1c $a4 }T T{ $0051 M $0052 M $007c M $598d M $598e M $598f M $b152 M -> $52 $b1 $e7 $21 $7c $7a $dd }T
( 22 )
$d1ca >PC $89 >S $b0 >A $98 >X $09 >Y $a0 >P $d1ca $22 >M $d1cb $c0 >M $d1cc $03 >M
T{ op PC S A X Y P -> $d1cc $89 $b0 $98 $09 $a0 }T T{ $d1ca M $d1cb M $d1cc M -> $22 $c0 $03 }T
$90df >PC $bf >S $ec >A $0f >X $02 >Y $e8 >P $90df $22 >M $90e0 $8e >M $90e1 $f8 >M
T{ op PC S A X Y P -> $90e1 $bf $ec $0f $02 $e8 }T T{ $90df M $90e0 M $90e1 M -> $22 $8e $f8 }T
$999d >PC $d3 >S $e0 >A $2d >X $5d >Y $22 >P $999d $22 >M $999e $a1 >M $999f $6a >M
T{ op PC S A X Y P -> $999f $d3 $e0 $2d $5d $22 }T T{ $999d M $999e M $999f M -> $22 $a1 $6a }T
$4eaa >PC $7f >S $49 >A $46 >X $16 >Y $a7 >P $4eaa $22 >M $4eab $3a >M $4eac $f5 >M
T{ op PC S A X Y P -> $4eac $7f $49 $46 $16 $a7 }T T{ $4eaa M $4eab M $4eac M -> $22 $3a $f5 }T
$d110 >PC $c9 >S $5a >A $ae >X $f7 >Y $2b >P $d110 $22 >M $d111 $33 >M $d112 $7f >M
T{ op PC S A X Y P -> $d112 $c9 $5a $ae $f7 $2b }T T{ $d110 M $d111 M $d112 M -> $22 $33 $7f }T
$93bc >PC $f2 >S $1c >A $45 >X $f7 >Y $a6 >P $93bc $22 >M $93bd $2e >M $93be $75 >M
T{ op PC S A X Y P -> $93be $f2 $1c $45 $f7 $a6 }T T{ $93bc M $93bd M $93be M -> $22 $2e $75 }T
$cc08 >PC $6f >S $82 >A $b4 >X $65 >Y $6d >P $cc08 $22 >M $cc09 $fe >M $cc0a $bf >M
T{ op PC S A X Y P -> $cc0a $6f $82 $b4 $65 $6d }T T{ $cc08 M $cc09 M $cc0a M -> $22 $fe $bf }T
$ae57 >PC $89 >S $46 >A $f9 >X $fe >Y $e4 >P $ae57 $22 >M $ae58 $f2 >M $ae59 $53 >M
T{ op PC S A X Y P -> $ae59 $89 $46 $f9 $fe $e4 }T T{ $ae57 M $ae58 M $ae59 M -> $22 $f2 $53 }T
$2ebd >PC $81 >S $48 >A $ce >X $66 >Y $27 >P $2ebd $22 >M $2ebe $e2 >M $2ebf $3f >M
T{ op PC S A X Y P -> $2ebf $81 $48 $ce $66 $27 }T T{ $2ebd M $2ebe M $2ebf M -> $22 $e2 $3f }T
$0bb9 >PC $54 >S $58 >A $11 >X $e9 >Y $ef >P $0bb9 $22 >M $0bba $bd >M $0bbb $8f >M
T{ op PC S A X Y P -> $0bbb $54 $58 $11 $e9 $ef }T T{ $0bb9 M $0bba M $0bbb M -> $22 $bd $8f }T
$7022 >PC $db >S $0f >A $ca >X $f1 >Y $61 >P $7022 $22 >M $7023 $eb >M $7024 $3f >M
T{ op PC S A X Y P -> $7024 $db $0f $ca $f1 $61 }T T{ $7022 M $7023 M $7024 M -> $22 $eb $3f }T
$72e9 >PC $9d >S $ea >A $c0 >X $85 >Y $ef >P $72e9 $22 >M $72ea $53 >M $72eb $4c >M
T{ op PC S A X Y P -> $72eb $9d $ea $c0 $85 $ef }T T{ $72e9 M $72ea M $72eb M -> $22 $53 $4c }T
$fc69 >PC $0b >S $e2 >A $76 >X $e5 >Y $a1 >P $fc69 $22 >M $fc6a $c7 >M $fc6b $bd >M
T{ op PC S A X Y P -> $fc6b $0b $e2 $76 $e5 $a1 }T T{ $fc69 M $fc6a M $fc6b M -> $22 $c7 $bd }T
$ce2c >PC $b1 >S $b6 >A $a5 >X $b6 >Y $6b >P $ce2c $22 >M $ce2d $2c >M $ce2e $eb >M
T{ op PC S A X Y P -> $ce2e $b1 $b6 $a5 $b6 $6b }T T{ $ce2c M $ce2d M $ce2e M -> $22 $2c $eb }T
$f6c9 >PC $ec >S $2f >A $bf >X $44 >Y $ef >P $f6c9 $22 >M $f6ca $10 >M $f6cb $d6 >M
T{ op PC S A X Y P -> $f6cb $ec $2f $bf $44 $ef }T T{ $f6c9 M $f6ca M $f6cb M -> $22 $10 $d6 }T
$dfb0 >PC $bf >S $be >A $a1 >X $07 >Y $2f >P $dfb0 $22 >M $dfb1 $ea >M $dfb2 $2f >M
T{ op PC S A X Y P -> $dfb2 $bf $be $a1 $07 $2f }T T{ $dfb0 M $dfb1 M $dfb2 M -> $22 $ea $2f }T
( 23 )
$35ff >PC $e9 >S $cd >A $93 >X $93 >Y $ed >P $35ff $23 >M $3600 $c5 >M $3601 $e7 >M
T{ op PC S A X Y P -> $3600 $e9 $cd $93 $93 $ed }T T{ $35ff M $3600 M $3601 M -> $23 $c5 $e7 }T
$a6d1 >PC $8c >S $10 >A $21 >X $c2 >Y $a3 >P $a6d1 $23 >M $a6d2 $19 >M $a6d3 $ea >M
T{ op PC S A X Y P -> $a6d2 $8c $10 $21 $c2 $a3 }T T{ $a6d1 M $a6d2 M $a6d3 M -> $23 $19 $ea }T
$aeed >PC $62 >S $e2 >A $d0 >X $52 >Y $e4 >P $aeed $23 >M $aeee $1f >M $aeef $d6 >M
T{ op PC S A X Y P -> $aeee $62 $e2 $d0 $52 $e4 }T T{ $aeed M $aeee M $aeef M -> $23 $1f $d6 }T
$62dc >PC $03 >S $ae >A $71 >X $f3 >Y $aa >P $62dc $23 >M $62dd $a5 >M $62de $48 >M
T{ op PC S A X Y P -> $62dd $03 $ae $71 $f3 $aa }T T{ $62dc M $62dd M $62de M -> $23 $a5 $48 }T
$d364 >PC $b4 >S $df >A $34 >X $10 >Y $e4 >P $d364 $23 >M $d365 $98 >M $d366 $01 >M
T{ op PC S A X Y P -> $d365 $b4 $df $34 $10 $e4 }T T{ $d364 M $d365 M $d366 M -> $23 $98 $01 }T
$e1dd >PC $a9 >S $58 >A $f0 >X $d1 >Y $e8 >P $e1dd $23 >M $e1de $23 >M $e1df $88 >M
T{ op PC S A X Y P -> $e1de $a9 $58 $f0 $d1 $e8 }T T{ $e1dd M $e1de M $e1df M -> $23 $23 $88 }T
$9f23 >PC $e9 >S $2f >A $54 >X $4f >Y $e1 >P $9f23 $23 >M $9f24 $ee >M $9f25 $de >M
T{ op PC S A X Y P -> $9f24 $e9 $2f $54 $4f $e1 }T T{ $9f23 M $9f24 M $9f25 M -> $23 $ee $de }T
$a7e1 >PC $38 >S $bb >A $ad >X $d0 >Y $69 >P $a7e1 $23 >M $a7e2 $36 >M $a7e3 $e2 >M
T{ op PC S A X Y P -> $a7e2 $38 $bb $ad $d0 $69 }T T{ $a7e1 M $a7e2 M $a7e3 M -> $23 $36 $e2 }T
$915f >PC $21 >S $dc >A $22 >X $25 >Y $6d >P $915f $23 >M $9160 $39 >M $9161 $39 >M
T{ op PC S A X Y P -> $9160 $21 $dc $22 $25 $6d }T T{ $915f M $9160 M $9161 M -> $23 $39 $39 }T
$d7a8 >PC $2e >S $c9 >A $5b >X $fa >Y $66 >P $d7a8 $23 >M $d7a9 $36 >M $d7aa $00 >M
T{ op PC S A X Y P -> $d7a9 $2e $c9 $5b $fa $66 }T T{ $d7a8 M $d7a9 M $d7aa M -> $23 $36 $00 }T
$55aa >PC $fa >S $5d >A $f9 >X $b9 >Y $a9 >P $55aa $23 >M $55ab $f9 >M $55ac $ee >M
T{ op PC S A X Y P -> $55ab $fa $5d $f9 $b9 $a9 }T T{ $55aa M $55ab M $55ac M -> $23 $f9 $ee }T
$faa1 >PC $80 >S $39 >A $34 >X $a7 >Y $ee >P $faa1 $23 >M $faa2 $91 >M $faa3 $a4 >M
T{ op PC S A X Y P -> $faa2 $80 $39 $34 $a7 $ee }T T{ $faa1 M $faa2 M $faa3 M -> $23 $91 $a4 }T
$f652 >PC $17 >S $3b >A $38 >X $83 >Y $e7 >P $f652 $23 >M $f653 $d0 >M $f654 $86 >M
T{ op PC S A X Y P -> $f653 $17 $3b $38 $83 $e7 }T T{ $f652 M $f653 M $f654 M -> $23 $d0 $86 }T
$1b6c >PC $01 >S $fb >A $bb >X $c8 >Y $a2 >P $1b6c $23 >M $1b6d $8c >M $1b6e $b6 >M
T{ op PC S A X Y P -> $1b6d $01 $fb $bb $c8 $a2 }T T{ $1b6c M $1b6d M $1b6e M -> $23 $8c $b6 }T
$5142 >PC $63 >S $af >A $7b >X $3f >Y $e4 >P $5142 $23 >M $5143 $19 >M $5144 $33 >M
T{ op PC S A X Y P -> $5143 $63 $af $7b $3f $e4 }T T{ $5142 M $5143 M $5144 M -> $23 $19 $33 }T
$87e9 >PC $df >S $fc >A $3b >X $ed >Y $61 >P $87e9 $23 >M $87ea $55 >M $87eb $99 >M
T{ op PC S A X Y P -> $87ea $df $fc $3b $ed $61 }T T{ $87e9 M $87ea M $87eb M -> $23 $55 $99 }T
( 24 )
$ea25 >PC $08 >S $dd >A $5f >X $c2 >Y $6f >P $0036 $3d >M $ea25 $24 >M $ea26 $36 >M $ea27 $c1 >M
T{ op PC S A X Y P -> $ea27 $08 $dd $5f $c2 $2d }T T{ $0036 M $ea25 M $ea26 M $ea27 M -> $3d $24 $36 $c1 }T
$c6b8 >PC $9a >S $a5 >A $bb >X $73 >Y $a2 >P $007a $b2 >M $c6b8 $24 >M $c6b9 $7a >M $c6ba $eb >M
T{ op PC S A X Y P -> $c6ba $9a $a5 $bb $73 $a0 }T T{ $007a M $c6b8 M $c6b9 M $c6ba M -> $b2 $24 $7a $eb }T
$527c >PC $6c >S $82 >A $09 >X $e6 >Y $a7 >P $001c $4f >M $527c $24 >M $527d $1c >M $527e $ed >M
T{ op PC S A X Y P -> $527e $6c $82 $09 $e6 $65 }T T{ $001c M $527c M $527d M $527e M -> $4f $24 $1c $ed }T
$857d >PC $3b >S $a3 >A $6b >X $b4 >Y $29 >P $00e5 $3c >M $857d $24 >M $857e $e5 >M $857f $ae >M
T{ op PC S A X Y P -> $857f $3b $a3 $6b $b4 $29 }T T{ $00e5 M $857d M $857e M $857f M -> $3c $24 $e5 $ae }T
$cb49 >PC $b6 >S $8c >A $ab >X $46 >Y $a5 >P $009a $63 >M $cb49 $24 >M $cb4a $9a >M $cb4b $9c >M
T{ op PC S A X Y P -> $cb4b $b6 $8c $ab $46 $67 }T T{ $009a M $cb49 M $cb4a M $cb4b M -> $63 $24 $9a $9c }T
$2d11 >PC $22 >S $01 >A $64 >X $2a >Y $e8 >P $0030 $2e >M $2d11 $24 >M $2d12 $30 >M $2d13 $54 >M
T{ op PC S A X Y P -> $2d13 $22 $01 $64 $2a $2a }T T{ $0030 M $2d11 M $2d12 M $2d13 M -> $2e $24 $30 $54 }T
$3a1a >PC $42 >S $8a >A $1a >X $da >Y $e0 >P $00cb $68 >M $3a1a $24 >M $3a1b $cb >M $3a1c $ac >M
T{ op PC S A X Y P -> $3a1c $42 $8a $1a $da $60 }T T{ $00cb M $3a1a M $3a1b M $3a1c M -> $68 $24 $cb $ac }T
$0dd4 >PC $f8 >S $04 >A $b2 >X $29 >Y $af >P $0040 $54 >M $0dd4 $24 >M $0dd5 $40 >M $0dd6 $95 >M
T{ op PC S A X Y P -> $0dd6 $f8 $04 $b2 $29 $6d }T T{ $0040 M $0dd4 M $0dd5 M $0dd6 M -> $54 $24 $40 $95 }T
$cac2 >PC $b0 >S $8d >A $41 >X $bc >Y $a0 >P $0064 $20 >M $cac2 $24 >M $cac3 $64 >M $cac4 $85 >M
T{ op PC S A X Y P -> $cac4 $b0 $8d $41 $bc $22 }T T{ $0064 M $cac2 M $cac3 M $cac4 M -> $20 $24 $64 $85 }T
$e440 >PC $05 >S $59 >A $e9 >X $f4 >Y $22 >P $0050 $cf >M $e440 $24 >M $e441 $50 >M $e442 $a1 >M
T{ op PC S A X Y P -> $e442 $05 $59 $e9 $f4 $e0 }T T{ $0050 M $e440 M $e441 M $e442 M -> $cf $24 $50 $a1 }T
$c5f4 >PC $da >S $25 >A $b4 >X $67 >Y $21 >P $0048 $ca >M $c5f4 $24 >M $c5f5 $48 >M $c5f6 $63 >M
T{ op PC S A X Y P -> $c5f6 $da $25 $b4 $67 $e3 }T T{ $0048 M $c5f4 M $c5f5 M $c5f6 M -> $ca $24 $48 $63 }T
$af36 >PC $2b >S $22 >A $0e >X $1d >Y $6c >P $007a $1c >M $af36 $24 >M $af37 $7a >M $af38 $1c >M
T{ op PC S A X Y P -> $af38 $2b $22 $0e $1d $2e }T T{ $007a M $af36 M $af37 M $af38 M -> $1c $24 $7a $1c }T
$4465 >PC $f6 >S $de >A $41 >X $d3 >Y $a1 >P $003b $12 >M $4465 $24 >M $4466 $3b >M $4467 $32 >M
T{ op PC S A X Y P -> $4467 $f6 $de $41 $d3 $21 }T T{ $003b M $4465 M $4466 M $4467 M -> $12 $24 $3b $32 }T
$f6b2 >PC $21 >S $33 >A $f8 >X $0b >Y $a5 >P $00a1 $ed >M $f6b2 $24 >M $f6b3 $a1 >M $f6b4 $cd >M
T{ op PC S A X Y P -> $f6b4 $21 $33 $f8 $0b $e5 }T T{ $00a1 M $f6b2 M $f6b3 M $f6b4 M -> $ed $24 $a1 $cd }T
$a60f >PC $fd >S $29 >A $b0 >X $1c >Y $a1 >P $00a4 $f3 >M $a60f $24 >M $a610 $a4 >M $a611 $b1 >M
T{ op PC S A X Y P -> $a611 $fd $29 $b0 $1c $e1 }T T{ $00a4 M $a60f M $a610 M $a611 M -> $f3 $24 $a4 $b1 }T
$153f >PC $6b >S $e5 >A $25 >X $9c >Y $6b >P $0051 $79 >M $153f $24 >M $1540 $51 >M $1541 $69 >M
T{ op PC S A X Y P -> $1541 $6b $e5 $25 $9c $69 }T T{ $0051 M $153f M $1540 M $1541 M -> $79 $24 $51 $69 }T
( 25 )
$cc8a >PC $53 >S $6a >A $46 >X $95 >Y $27 >P $008a $dc >M $cc8a $25 >M $cc8b $8a >M $cc8c $3a >M
T{ op PC S A X Y P -> $cc8c $53 $48 $46 $95 $25 }T T{ $008a M $cc8a M $cc8b M $cc8c M -> $dc $25 $8a $3a }T
$cf5e >PC $27 >S $75 >A $4f >X $0f >Y $ed >P $0063 $d5 >M $cf5e $25 >M $cf5f $63 >M $cf60 $f3 >M
T{ op PC S A X Y P -> $cf60 $27 $55 $4f $0f $6d }T T{ $0063 M $cf5e M $cf5f M $cf60 M -> $d5 $25 $63 $f3 }T
$fb23 >PC $9b >S $4a >A $f4 >X $c0 >Y $a2 >P $002a $ef >M $fb23 $25 >M $fb24 $2a >M $fb25 $b9 >M
T{ op PC S A X Y P -> $fb25 $9b $4a $f4 $c0 $20 }T T{ $002a M $fb23 M $fb24 M $fb25 M -> $ef $25 $2a $b9 }T
$d055 >PC $7b >S $93 >A $dd >X $d2 >Y $67 >P $00ab $a2 >M $d055 $25 >M $d056 $ab >M $d057 $12 >M
T{ op PC S A X Y P -> $d057 $7b $82 $dd $d2 $e5 }T T{ $00ab M $d055 M $d056 M $d057 M -> $a2 $25 $ab $12 }T
$94df >PC $08 >S $a4 >A $d1 >X $88 >Y $a6 >P $0018 $c1 >M $94df $25 >M $94e0 $18 >M $94e1 $40 >M
T{ op PC S A X Y P -> $94e1 $08 $80 $d1 $88 $a4 }T T{ $0018 M $94df M $94e0 M $94e1 M -> $c1 $25 $18 $40 }T
$7e70 >PC $d8 >S $5b >A $b4 >X $47 >Y $af >P $0032 $1c >M $7e70 $25 >M $7e71 $32 >M $7e72 $43 >M
T{ op PC S A X Y P -> $7e72 $d8 $18 $b4 $47 $2d }T T{ $0032 M $7e70 M $7e71 M $7e72 M -> $1c $25 $32 $43 }T
$ac36 >PC $62 >S $c7 >A $67 >X $88 >Y $e2 >P $00ce $79 >M $ac36 $25 >M $ac37 $ce >M $ac38 $25 >M
T{ op PC S A X Y P -> $ac38 $62 $41 $67 $88 $60 }T T{ $00ce M $ac36 M $ac37 M $ac38 M -> $79 $25 $ce $25 }T
$3643 >PC $41 >S $a7 >A $4b >X $8c >Y $64 >P $003d $e8 >M $3643 $25 >M $3644 $3d >M $3645 $04 >M
T{ op PC S A X Y P -> $3645 $41 $a0 $4b $8c $e4 }T T{ $003d M $3643 M $3644 M $3645 M -> $e8 $25 $3d $04 }T
$37e0 >PC $e2 >S $38 >A $d6 >X $41 >Y $28 >P $00db $f4 >M $37e0 $25 >M $37e1 $db >M $37e2 $c8 >M
T{ op PC S A X Y P -> $37e2 $e2 $30 $d6 $41 $28 }T T{ $00db M $37e0 M $37e1 M $37e2 M -> $f4 $25 $db $c8 }T
$c513 >PC $0c >S $ab >A $1e >X $4a >Y $6d >P $0001 $37 >M $c513 $25 >M $c514 $01 >M $c515 $6d >M
T{ op PC S A X Y P -> $c515 $0c $23 $1e $4a $6d }T T{ $0001 M $c513 M $c514 M $c515 M -> $37 $25 $01 $6d }T
$0709 >PC $21 >S $89 >A $02 >X $0d >Y $6d >P $007b $6d >M $0709 $25 >M $070a $7b >M $070b $8d >M
T{ op PC S A X Y P -> $070b $21 $09 $02 $0d $6d }T T{ $007b M $0709 M $070a M $070b M -> $6d $25 $7b $8d }T
$c4bb >PC $2b >S $8d >A $30 >X $36 >Y $65 >P $00ae $8e >M $c4bb $25 >M $c4bc $ae >M $c4bd $ed >M
T{ op PC S A X Y P -> $c4bd $2b $8c $30 $36 $e5 }T T{ $00ae M $c4bb M $c4bc M $c4bd M -> $8e $25 $ae $ed }T
$f0e9 >PC $6f >S $b6 >A $7a >X $a8 >Y $2e >P $00c5 $ff >M $f0e9 $25 >M $f0ea $c5 >M $f0eb $8e >M
T{ op PC S A X Y P -> $f0eb $6f $b6 $7a $a8 $ac }T T{ $00c5 M $f0e9 M $f0ea M $f0eb M -> $ff $25 $c5 $8e }T
$5ea0 >PC $f5 >S $4a >A $ec >X $08 >Y $22 >P $00e3 $4b >M $5ea0 $25 >M $5ea1 $e3 >M $5ea2 $97 >M
T{ op PC S A X Y P -> $5ea2 $f5 $4a $ec $08 $20 }T T{ $00e3 M $5ea0 M $5ea1 M $5ea2 M -> $4b $25 $e3 $97 }T
$ec35 >PC $93 >S $d4 >A $3d >X $2d >Y $2d >P $0022 $48 >M $ec35 $25 >M $ec36 $22 >M $ec37 $eb >M
T{ op PC S A X Y P -> $ec37 $93 $40 $3d $2d $2d }T T{ $0022 M $ec35 M $ec36 M $ec37 M -> $48 $25 $22 $eb }T
$dae6 >PC $93 >S $8e >A $d7 >X $30 >Y $a3 >P $001b $c3 >M $dae6 $25 >M $dae7 $1b >M $dae8 $23 >M
T{ op PC S A X Y P -> $dae8 $93 $82 $d7 $30 $a1 }T T{ $001b M $dae6 M $dae7 M $dae8 M -> $c3 $25 $1b $23 }T
( 26 )
$564d >PC $f8 >S $33 >A $d2 >X $72 >Y $28 >P $005e $73 >M $564d $26 >M $564e $5e >M $564f $3c >M
T{ op PC S A X Y P -> $564f $f8 $33 $d2 $72 $a8 }T T{ $005e M $564d M $564e M $564f M -> $e6 $26 $5e $3c }T
$565e >PC $a2 >S $3a >A $58 >X $2c >Y $e6 >P $008b $95 >M $565e $26 >M $565f $8b >M $5660 $52 >M
T{ op PC S A X Y P -> $5660 $a2 $3a $58 $2c $65 }T T{ $008b M $565e M $565f M $5660 M -> $2a $26 $8b $52 }T
$bc6d >PC $2d >S $e2 >A $d3 >X $33 >Y $6c >P $0020 $15 >M $bc6d $26 >M $bc6e $20 >M $bc6f $75 >M
T{ op PC S A X Y P -> $bc6f $2d $e2 $d3 $33 $6c }T T{ $0020 M $bc6d M $bc6e M $bc6f M -> $2a $26 $20 $75 }T
$8bc3 >PC $83 >S $75 >A $e4 >X $ff >Y $a0 >P $0035 $69 >M $8bc3 $26 >M $8bc4 $35 >M $8bc5 $9f >M
T{ op PC S A X Y P -> $8bc5 $83 $75 $e4 $ff $a0 }T T{ $0035 M $8bc3 M $8bc4 M $8bc5 M -> $d2 $26 $35 $9f }T
$2a36 >PC $8d >S $60 >A $72 >X $2d >Y $63 >P $0062 $3b >M $2a36 $26 >M $2a37 $62 >M $2a38 $0e >M
T{ op PC S A X Y P -> $2a38 $8d $60 $72 $2d $60 }T T{ $0062 M $2a36 M $2a37 M $2a38 M -> $77 $26 $62 $0e }T
$714c >PC $df >S $f5 >A $3d >X $31 >Y $ad >P $0047 $77 >M $714c $26 >M $714d $47 >M $714e $58 >M
T{ op PC S A X Y P -> $714e $df $f5 $3d $31 $ac }T T{ $0047 M $714c M $714d M $714e M -> $ef $26 $47 $58 }T
$5872 >PC $32 >S $98 >A $ad >X $8c >Y $69 >P $009b $77 >M $5872 $26 >M $5873 $9b >M $5874 $f3 >M
T{ op PC S A X Y P -> $5874 $32 $98 $ad $8c $e8 }T T{ $009b M $5872 M $5873 M $5874 M -> $ef $26 $9b $f3 }T
$a18a >PC $7b >S $97 >A $ba >X $1b >Y $ac >P $00ab $e3 >M $a18a $26 >M $a18b $ab >M $a18c $4a >M
T{ op PC S A X Y P -> $a18c $7b $97 $ba $1b $ad }T T{ $00ab M $a18a M $a18b M $a18c M -> $c6 $26 $ab $4a }T
$a1ef >PC $fa >S $ec >A $5c >X $e7 >Y $64 >P $0044 $3c >M $a1ef $26 >M $a1f0 $44 >M $a1f1 $0a >M
T{ op PC S A X Y P -> $a1f1 $fa $ec $5c $e7 $64 }T T{ $0044 M $a1ef M $a1f0 M $a1f1 M -> $78 $26 $44 $0a }T
$e50f >PC $b1 >S $20 >A $ba >X $8d >Y $a7 >P $0000 $08 >M $e50f $26 >M $e510 $00 >M $e511 $46 >M
T{ op PC S A X Y P -> $e511 $b1 $20 $ba $8d $24 }T T{ $0000 M $e50f M $e510 M $e511 M -> $11 $26 $00 $46 }T
$63fd >PC $12 >S $ce >A $15 >X $0d >Y $a8 >P $0044 $32 >M $63fd $26 >M $63fe $44 >M $63ff $4c >M
T{ op PC S A X Y P -> $63ff $12 $ce $15 $0d $28 }T T{ $0044 M $63fd M $63fe M $63ff M -> $64 $26 $44 $4c }T
$ccba >PC $c9 >S $ac >A $ca >X $59 >Y $a0 >P $007d $2c >M $ccba $26 >M $ccbb $7d >M $ccbc $e1 >M
T{ op PC S A X Y P -> $ccbc $c9 $ac $ca $59 $20 }T T{ $007d M $ccba M $ccbb M $ccbc M -> $58 $26 $7d $e1 }T
$1ff6 >PC $62 >S $13 >A $d3 >X $a6 >Y $af >P $0098 $e8 >M $1ff6 $26 >M $1ff7 $98 >M $1ff8 $7c >M
T{ op PC S A X Y P -> $1ff8 $62 $13 $d3 $a6 $ad }T T{ $0098 M $1ff6 M $1ff7 M $1ff8 M -> $d1 $26 $98 $7c }T
$11ad >PC $0f >S $53 >A $3f >X $df >Y $a3 >P $00bc $33 >M $11ad $26 >M $11ae $bc >M $11af $50 >M
T{ op PC S A X Y P -> $11af $0f $53 $3f $df $20 }T T{ $00bc M $11ad M $11ae M $11af M -> $67 $26 $bc $50 }T
$49bd >PC $49 >S $cb >A $d1 >X $0b >Y $69 >P $00f9 $7d >M $49bd $26 >M $49be $f9 >M $49bf $db >M
T{ op PC S A X Y P -> $49bf $49 $cb $d1 $0b $e8 }T T{ $00f9 M $49bd M $49be M $49bf M -> $fb $26 $f9 $db }T
$6f1a >PC $f6 >S $cb >A $cd >X $28 >Y $66 >P $00c2 $8e >M $6f1a $26 >M $6f1b $c2 >M $6f1c $e3 >M
T{ op PC S A X Y P -> $6f1c $f6 $cb $cd $28 $65 }T T{ $00c2 M $6f1a M $6f1b M $6f1c M -> $1c $26 $c2 $e3 }T
( 27 )
$042f >PC $41 >S $77 >A $85 >X $a8 >Y $a1 >P $00f3 $ce >M $042f $27 >M $0430 $f3 >M $0431 $07 >M
T{ op PC S A X Y P -> $0431 $41 $77 $85 $a8 $a1 }T T{ $00f3 M $042f M $0430 M $0431 M -> $ca $27 $f3 $07 }T
$ffc3 >PC $0e >S $47 >A $5b >X $69 >Y $aa >P $00ed $da >M $ffc3 $27 >M $ffc4 $ed >M $ffc5 $6e >M
T{ op PC S A X Y P -> $ffc5 $0e $47 $5b $69 $aa }T T{ $00ed M $ffc3 M $ffc4 M $ffc5 M -> $da $27 $ed $6e }T
$6f47 >PC $c1 >S $64 >A $ed >X $43 >Y $29 >P $00fb $02 >M $6f47 $27 >M $6f48 $fb >M $6f49 $cc >M
T{ op PC S A X Y P -> $6f49 $c1 $64 $ed $43 $29 }T T{ $00fb M $6f47 M $6f48 M $6f49 M -> $02 $27 $fb $cc }T
$3845 >PC $88 >S $d1 >A $3f >X $05 >Y $22 >P $0005 $fe >M $3845 $27 >M $3846 $05 >M $3847 $58 >M
T{ op PC S A X Y P -> $3847 $88 $d1 $3f $05 $22 }T T{ $0005 M $3845 M $3846 M $3847 M -> $fa $27 $05 $58 }T
$a41b >PC $f0 >S $19 >A $5f >X $1a >Y $aa >P $00b2 $20 >M $a41b $27 >M $a41c $b2 >M $a41d $18 >M
T{ op PC S A X Y P -> $a41d $f0 $19 $5f $1a $aa }T T{ $00b2 M $a41b M $a41c M $a41d M -> $20 $27 $b2 $18 }T
$b4cf >PC $f6 >S $3f >A $cc >X $d4 >Y $6e >P $00e6 $fb >M $b4cf $27 >M $b4d0 $e6 >M $b4d1 $79 >M
T{ op PC S A X Y P -> $b4d1 $f6 $3f $cc $d4 $6e }T T{ $00e6 M $b4cf M $b4d0 M $b4d1 M -> $fb $27 $e6 $79 }T
$595c >PC $a0 >S $5b >A $c4 >X $37 >Y $62 >P $00f9 $70 >M $595c $27 >M $595d $f9 >M $595e $be >M
T{ op PC S A X Y P -> $595e $a0 $5b $c4 $37 $62 }T T{ $00f9 M $595c M $595d M $595e M -> $70 $27 $f9 $be }T
$da48 >PC $75 >S $af >A $ec >X $10 >Y $ef >P $00d4 $3f >M $da48 $27 >M $da49 $d4 >M $da4a $2a >M
T{ op PC S A X Y P -> $da4a $75 $af $ec $10 $ef }T T{ $00d4 M $da48 M $da49 M $da4a M -> $3b $27 $d4 $2a }T
$e89a >PC $df >S $10 >A $0e >X $fd >Y $28 >P $0086 $70 >M $e89a $27 >M $e89b $86 >M $e89c $43 >M
T{ op PC S A X Y P -> $e89c $df $10 $0e $fd $28 }T T{ $0086 M $e89a M $e89b M $e89c M -> $70 $27 $86 $43 }T
$e2cb >PC $1c >S $0d >A $16 >X $49 >Y $6d >P $0023 $16 >M $e2cb $27 >M $e2cc $23 >M $e2cd $33 >M
T{ op PC S A X Y P -> $e2cd $1c $0d $16 $49 $6d }T T{ $0023 M $e2cb M $e2cc M $e2cd M -> $12 $27 $23 $33 }T
$e225 >PC $1d >S $3d >A $13 >X $86 >Y $65 >P $0083 $7b >M $e225 $27 >M $e226 $83 >M $e227 $86 >M
T{ op PC S A X Y P -> $e227 $1d $3d $13 $86 $65 }T T{ $0083 M $e225 M $e226 M $e227 M -> $7b $27 $83 $86 }T
$77a5 >PC $79 >S $e8 >A $46 >X $ac >Y $2b >P $000c $5e >M $77a5 $27 >M $77a6 $0c >M $77a7 $c5 >M
T{ op PC S A X Y P -> $77a7 $79 $e8 $46 $ac $2b }T T{ $000c M $77a5 M $77a6 M $77a7 M -> $5a $27 $0c $c5 }T
$41bd >PC $86 >S $3e >A $dc >X $b7 >Y $ea >P $0052 $2e >M $41bd $27 >M $41be $52 >M $41bf $9f >M
T{ op PC S A X Y P -> $41bf $86 $3e $dc $b7 $ea }T T{ $0052 M $41bd M $41be M $41bf M -> $2a $27 $52 $9f }T
$8085 >PC $03 >S $c4 >A $2d >X $bb >Y $a4 >P $0066 $1e >M $8085 $27 >M $8086 $66 >M $8087 $d1 >M
T{ op PC S A X Y P -> $8087 $03 $c4 $2d $bb $a4 }T T{ $0066 M $8085 M $8086 M $8087 M -> $1a $27 $66 $d1 }T
$5c2c >PC $b1 >S $b2 >A $e0 >X $b2 >Y $e7 >P $00ee $8c >M $5c2c $27 >M $5c2d $ee >M $5c2e $e2 >M
T{ op PC S A X Y P -> $5c2e $b1 $b2 $e0 $b2 $e7 }T T{ $00ee M $5c2c M $5c2d M $5c2e M -> $88 $27 $ee $e2 }T
$d3cd >PC $05 >S $4e >A $f0 >X $b6 >Y $ae >P $0073 $e8 >M $d3cd $27 >M $d3ce $73 >M $d3cf $95 >M
T{ op PC S A X Y P -> $d3cf $05 $4e $f0 $b6 $ae }T T{ $0073 M $d3cd M $d3ce M $d3cf M -> $e8 $27 $73 $95 }T
( 28 )
$fc4b >PC $b8 >S $1b >A $73 >X $f1 >Y $e8 >P $01b8 $e0 >M $01b9 $90 >M $fc4b $28 >M $fc4c $fc >M $fc4d $3c >M
T{ op PC S A X Y P -> $fc4c $b9 $1b $73 $f1 $a0 }T T{ $01b8 M $01b9 M $fc4b M $fc4c M $fc4d M -> $e0 $90 $28 $fc $3c }T
$dd76 >PC $bf >S $e9 >A $e6 >X $c1 >Y $e5 >P $01bf $de >M $01c0 $59 >M $dd76 $28 >M $dd77 $21 >M $dd78 $ee >M
T{ op PC S A X Y P -> $dd77 $c0 $e9 $e6 $c1 $69 }T T{ $01bf M $01c0 M $dd76 M $dd77 M $dd78 M -> $de $59 $28 $21 $ee }T
$f9c4 >PC $04 >S $d2 >A $37 >X $f5 >Y $6f >P $0104 $20 >M $0105 $db >M $f9c4 $28 >M $f9c5 $1c >M $f9c6 $09 >M
T{ op PC S A X Y P -> $f9c5 $05 $d2 $37 $f5 $eb }T T{ $0104 M $0105 M $f9c4 M $f9c5 M $f9c6 M -> $20 $db $28 $1c $09 }T
$5dab >PC $44 >S $2b >A $a5 >X $c6 >Y $ef >P $0144 $96 >M $0145 $5d >M $5dab $28 >M $5dac $c5 >M $5dad $9d >M
T{ op PC S A X Y P -> $5dac $45 $2b $a5 $c6 $6d }T T{ $0144 M $0145 M $5dab M $5dac M $5dad M -> $96 $5d $28 $c5 $9d }T
$0d16 >PC $75 >S $44 >A $16 >X $a6 >Y $a7 >P $0175 $dc >M $0176 $1a >M $0d16 $28 >M $0d17 $a4 >M $0d18 $00 >M
T{ op PC S A X Y P -> $0d17 $76 $44 $16 $a6 $2a }T T{ $0175 M $0176 M $0d16 M $0d17 M $0d18 M -> $dc $1a $28 $a4 $00 }T
$22bf >PC $e5 >S $b7 >A $a7 >X $74 >Y $22 >P $01e5 $a2 >M $01e6 $bd >M $22bf $28 >M $22c0 $89 >M $22c1 $82 >M
T{ op PC S A X Y P -> $22c0 $e6 $b7 $a7 $74 $ad }T T{ $01e5 M $01e6 M $22bf M $22c0 M $22c1 M -> $a2 $bd $28 $89 $82 }T
$aafd >PC $1a >S $2c >A $18 >X $f4 >Y $6b >P $011a $6e >M $011b $1e >M $aafd $28 >M $aafe $f9 >M $aaff $51 >M
T{ op PC S A X Y P -> $aafe $1b $2c $18 $f4 $2e }T T{ $011a M $011b M $aafd M $aafe M $aaff M -> $6e $1e $28 $f9 $51 }T
$511f >PC $5b >S $b4 >A $80 >X $af >Y $27 >P $015b $28 >M $015c $f1 >M $511f $28 >M $5120 $95 >M $5121 $f2 >M
T{ op PC S A X Y P -> $5120 $5c $b4 $80 $af $e1 }T T{ $015b M $015c M $511f M $5120 M $5121 M -> $28 $f1 $28 $95 $f2 }T
$fd02 >PC $8e >S $e7 >A $71 >X $8b >Y $2c >P $018e $c1 >M $018f $82 >M $fd02 $28 >M $fd03 $df >M $fd04 $36 >M
T{ op PC S A X Y P -> $fd03 $8f $e7 $71 $8b $a2 }T T{ $018e M $018f M $fd02 M $fd03 M $fd04 M -> $c1 $82 $28 $df $36 }T
$d12d >PC $8f >S $b0 >A $23 >X $cc >Y $23 >P $018f $dd >M $0190 $30 >M $d12d $28 >M $d12e $a1 >M $d12f $f8 >M
T{ op PC S A X Y P -> $d12e $90 $b0 $23 $cc $20 }T T{ $018f M $0190 M $d12d M $d12e M $d12f M -> $dd $30 $28 $a1 $f8 }T
$592b >PC $bd >S $3f >A $90 >X $59 >Y $e4 >P $01bd $90 >M $01be $42 >M $592b $28 >M $592c $59 >M $592d $5e >M
T{ op PC S A X Y P -> $592c $be $3f $90 $59 $62 }T T{ $01bd M $01be M $592b M $592c M $592d M -> $90 $42 $28 $59 $5e }T
$acfb >PC $e0 >S $8d >A $d7 >X $72 >Y $ec >P $01e0 $0c >M $01e1 $42 >M $acfb $28 >M $acfc $db >M $acfd $f8 >M
T{ op PC S A X Y P -> $acfc $e1 $8d $d7 $72 $62 }T T{ $01e0 M $01e1 M $acfb M $acfc M $acfd M -> $0c $42 $28 $db $f8 }T
$db75 >PC $f5 >S $f1 >A $80 >X $1c >Y $6a >P $01f5 $dd >M $01f6 $dd >M $db75 $28 >M $db76 $6b >M $db77 $21 >M
T{ op PC S A X Y P -> $db76 $f6 $f1 $80 $1c $ed }T T{ $01f5 M $01f6 M $db75 M $db76 M $db77 M -> $dd $dd $28 $6b $21 }T
$fd57 >PC $0d >S $5f >A $3a >X $85 >Y $6d >P $010d $20 >M $010e $a2 >M $fd57 $28 >M $fd58 $d7 >M $fd59 $c2 >M
T{ op PC S A X Y P -> $fd58 $0e $5f $3a $85 $a2 }T T{ $010d M $010e M $fd57 M $fd58 M $fd59 M -> $20 $a2 $28 $d7 $c2 }T
$0832 >PC $62 >S $03 >A $8d >X $d0 >Y $a8 >P $0162 $3b >M $0163 $2d >M $0832 $28 >M $0833 $c6 >M $0834 $dd >M
T{ op PC S A X Y P -> $0833 $63 $03 $8d $d0 $2d }T T{ $0162 M $0163 M $0832 M $0833 M $0834 M -> $3b $2d $28 $c6 $dd }T
$6d26 >PC $46 >S $bf >A $90 >X $2a >Y $2b >P $0146 $72 >M $0147 $01 >M $6d26 $28 >M $6d27 $cb >M $6d28 $7e >M
T{ op PC S A X Y P -> $6d27 $47 $bf $90 $2a $21 }T T{ $0146 M $0147 M $6d26 M $6d27 M $6d28 M -> $72 $01 $28 $cb $7e }T
( 29 )
$eb74 >PC $6c >S $5b >A $ef >X $be >Y $e6 >P $eb74 $29 >M $eb75 $45 >M $eb76 $8d >M
T{ op PC S A X Y P -> $eb76 $6c $41 $ef $be $64 }T T{ $eb74 M $eb75 M $eb76 M -> $29 $45 $8d }T
$030a >PC $9b >S $18 >A $05 >X $7c >Y $a5 >P $030a $29 >M $030b $22 >M $030c $a3 >M
T{ op PC S A X Y P -> $030c $9b $00 $05 $7c $27 }T T{ $030a M $030b M $030c M -> $29 $22 $a3 }T
$f205 >PC $60 >S $1e >A $d4 >X $07 >Y $e5 >P $f205 $29 >M $f206 $5b >M $f207 $f0 >M
T{ op PC S A X Y P -> $f207 $60 $1a $d4 $07 $65 }T T{ $f205 M $f206 M $f207 M -> $29 $5b $f0 }T
$c400 >PC $90 >S $45 >A $df >X $19 >Y $2c >P $c400 $29 >M $c401 $c7 >M $c402 $04 >M
T{ op PC S A X Y P -> $c402 $90 $45 $df $19 $2c }T T{ $c400 M $c401 M $c402 M -> $29 $c7 $04 }T
$5438 >PC $89 >S $fe >A $94 >X $45 >Y $e9 >P $5438 $29 >M $5439 $f1 >M $543a $74 >M
T{ op PC S A X Y P -> $543a $89 $f0 $94 $45 $e9 }T T{ $5438 M $5439 M $543a M -> $29 $f1 $74 }T
$038d >PC $04 >S $89 >A $25 >X $79 >Y $ac >P $038d $29 >M $038e $31 >M $038f $bb >M
T{ op PC S A X Y P -> $038f $04 $01 $25 $79 $2c }T T{ $038d M $038e M $038f M -> $29 $31 $bb }T
$b9c8 >PC $e2 >S $5d >A $97 >X $64 >Y $ef >P $b9c8 $29 >M $b9c9 $79 >M $b9ca $8d >M
T{ op PC S A X Y P -> $b9ca $e2 $59 $97 $64 $6d }T T{ $b9c8 M $b9c9 M $b9ca M -> $29 $79 $8d }T
$ce89 >PC $e4 >S $e8 >A $56 >X $bd >Y $24 >P $ce89 $29 >M $ce8a $38 >M $ce8b $1b >M
T{ op PC S A X Y P -> $ce8b $e4 $28 $56 $bd $24 }T T{ $ce89 M $ce8a M $ce8b M -> $29 $38 $1b }T
$7769 >PC $cd >S $4f >A $1d >X $52 >Y $a7 >P $7769 $29 >M $776a $3b >M $776b $dd >M
T{ op PC S A X Y P -> $776b $cd $0b $1d $52 $25 }T T{ $7769 M $776a M $776b M -> $29 $3b $dd }T
$a906 >PC $6e >S $a2 >A $65 >X $eb >Y $2a >P $a906 $29 >M $a907 $1a >M $a908 $88 >M
T{ op PC S A X Y P -> $a908 $6e $02 $65 $eb $28 }T T{ $a906 M $a907 M $a908 M -> $29 $1a $88 }T
$d1db >PC $ab >S $ab >A $06 >X $d7 >Y $6b >P $d1db $29 >M $d1dc $dc >M $d1dd $0f >M
T{ op PC S A X Y P -> $d1dd $ab $88 $06 $d7 $e9 }T T{ $d1db M $d1dc M $d1dd M -> $29 $dc $0f }T
$c118 >PC $68 >S $ae >A $63 >X $56 >Y $23 >P $c118 $29 >M $c119 $50 >M $c11a $fa >M
T{ op PC S A X Y P -> $c11a $68 $00 $63 $56 $23 }T T{ $c118 M $c119 M $c11a M -> $29 $50 $fa }T
$07fa >PC $82 >S $bb >A $ec >X $4d >Y $2e >P $07fa $29 >M $07fb $11 >M $07fc $8a >M
T{ op PC S A X Y P -> $07fc $82 $11 $ec $4d $2c }T T{ $07fa M $07fb M $07fc M -> $29 $11 $8a }T
$e636 >PC $6f >S $c9 >A $3c >X $c3 >Y $2a >P $e636 $29 >M $e637 $8e >M $e638 $b9 >M
T{ op PC S A X Y P -> $e638 $6f $88 $3c $c3 $a8 }T T{ $e636 M $e637 M $e638 M -> $29 $8e $b9 }T
$abf7 >PC $83 >S $b0 >A $45 >X $5a >Y $ab >P $abf7 $29 >M $abf8 $d4 >M $abf9 $7b >M
T{ op PC S A X Y P -> $abf9 $83 $90 $45 $5a $a9 }T T{ $abf7 M $abf8 M $abf9 M -> $29 $d4 $7b }T
$f5dc >PC $8b >S $03 >A $ef >X $d9 >Y $ed >P $f5dc $29 >M $f5dd $84 >M $f5de $85 >M
T{ op PC S A X Y P -> $f5de $8b $00 $ef $d9 $6f }T T{ $f5dc M $f5dd M $f5de M -> $29 $84 $85 }T
( 2a )
$4712 >PC $b6 >S $e2 >A $25 >X $23 >Y $20 >P $4712 $2a >M $4713 $ff >M $4714 $c1 >M
T{ op PC S A X Y P -> $4713 $b6 $c4 $25 $23 $a1 }T T{ $4712 M $4713 M $4714 M -> $2a $ff $c1 }T
$a126 >PC $89 >S $7c >A $7f >X $22 >Y $2d >P $a126 $2a >M $a127 $55 >M $a128 $49 >M
T{ op PC S A X Y P -> $a127 $89 $f9 $7f $22 $ac }T T{ $a126 M $a127 M $a128 M -> $2a $55 $49 }T
$6153 >PC $35 >S $ad >A $63 >X $26 >Y $2f >P $6153 $2a >M $6154 $b6 >M $6155 $3f >M
T{ op PC S A X Y P -> $6154 $35 $5b $63 $26 $2d }T T{ $6153 M $6154 M $6155 M -> $2a $b6 $3f }T
$93fe >PC $5b >S $13 >A $64 >X $b2 >Y $e1 >P $93fe $2a >M $93ff $46 >M $9400 $73 >M
T{ op PC S A X Y P -> $93ff $5b $27 $64 $b2 $60 }T T{ $93fe M $93ff M $9400 M -> $2a $46 $73 }T
$5364 >PC $7b >S $6b >A $31 >X $51 >Y $ac >P $5364 $2a >M $5365 $e4 >M $5366 $a6 >M
T{ op PC S A X Y P -> $5365 $7b $d6 $31 $51 $ac }T T{ $5364 M $5365 M $5366 M -> $2a $e4 $a6 }T
$ae27 >PC $23 >S $73 >A $84 >X $e1 >Y $68 >P $ae27 $2a >M $ae28 $a6 >M $ae29 $f3 >M
T{ op PC S A X Y P -> $ae28 $23 $e6 $84 $e1 $e8 }T T{ $ae27 M $ae28 M $ae29 M -> $2a $a6 $f3 }T
$e260 >PC $97 >S $fb >A $86 >X $ab >Y $a7 >P $e260 $2a >M $e261 $12 >M $e262 $6d >M
T{ op PC S A X Y P -> $e261 $97 $f7 $86 $ab $a5 }T T{ $e260 M $e261 M $e262 M -> $2a $12 $6d }T
$c3ee >PC $5f >S $06 >A $e9 >X $48 >Y $2a >P $c3ee $2a >M $c3ef $55 >M $c3f0 $8e >M
T{ op PC S A X Y P -> $c3ef $5f $0c $e9 $48 $28 }T T{ $c3ee M $c3ef M $c3f0 M -> $2a $55 $8e }T
$8efc >PC $f6 >S $89 >A $2e >X $90 >Y $21 >P $8efc $2a >M $8efd $ba >M $8efe $68 >M
T{ op PC S A X Y P -> $8efd $f6 $13 $2e $90 $21 }T T{ $8efc M $8efd M $8efe M -> $2a $ba $68 }T
$026f >PC $cf >S $c6 >A $d4 >X $b4 >Y $a1 >P $026f $2a >M $0270 $a1 >M $0271 $e7 >M
T{ op PC S A X Y P -> $0270 $cf $8d $d4 $b4 $a1 }T T{ $026f M $0270 M $0271 M -> $2a $a1 $e7 }T
$6fd2 >PC $6e >S $2f >A $13 >X $fb >Y $a6 >P $6fd2 $2a >M $6fd3 $c4 >M $6fd4 $44 >M
T{ op PC S A X Y P -> $6fd3 $6e $5e $13 $fb $24 }T T{ $6fd2 M $6fd3 M $6fd4 M -> $2a $c4 $44 }T
$48b7 >PC $f9 >S $24 >A $45 >X $d2 >Y $ae >P $48b7 $2a >M $48b8 $55 >M $48b9 $e7 >M
T{ op PC S A X Y P -> $48b8 $f9 $48 $45 $d2 $2c }T T{ $48b7 M $48b8 M $48b9 M -> $2a $55 $e7 }T
$27d2 >PC $73 >S $d0 >A $d3 >X $44 >Y $aa >P $27d2 $2a >M $27d3 $7d >M $27d4 $2b >M
T{ op PC S A X Y P -> $27d3 $73 $a0 $d3 $44 $a9 }T T{ $27d2 M $27d3 M $27d4 M -> $2a $7d $2b }T
$60a8 >PC $91 >S $50 >A $3d >X $4f >Y $ed >P $60a8 $2a >M $60a9 $27 >M $60aa $de >M
T{ op PC S A X Y P -> $60a9 $91 $a1 $3d $4f $ec }T T{ $60a8 M $60a9 M $60aa M -> $2a $27 $de }T
$1d32 >PC $a0 >S $0a >A $1a >X $77 >Y $eb >P $1d32 $2a >M $1d33 $e2 >M $1d34 $fa >M
T{ op PC S A X Y P -> $1d33 $a0 $15 $1a $77 $68 }T T{ $1d32 M $1d33 M $1d34 M -> $2a $e2 $fa }T
$7385 >PC $ac >S $e9 >A $35 >X $d3 >Y $64 >P $7385 $2a >M $7386 $5b >M $7387 $39 >M
T{ op PC S A X Y P -> $7386 $ac $d2 $35 $d3 $e5 }T T{ $7385 M $7386 M $7387 M -> $2a $5b $39 }T
( 2b )
$7059 >PC $bc >S $a6 >A $28 >X $34 >Y $ee >P $7059 $2b >M $705a $1b >M $705b $3c >M
T{ op PC S A X Y P -> $705a $bc $a6 $28 $34 $ee }T T{ $7059 M $705a M $705b M -> $2b $1b $3c }T
$802d >PC $ed >S $80 >A $06 >X $99 >Y $21 >P $802d $2b >M $802e $8c >M $802f $10 >M
T{ op PC S A X Y P -> $802e $ed $80 $06 $99 $21 }T T{ $802d M $802e M $802f M -> $2b $8c $10 }T
$e30e >PC $9c >S $58 >A $d0 >X $05 >Y $63 >P $e30e $2b >M $e30f $55 >M $e310 $a5 >M
T{ op PC S A X Y P -> $e30f $9c $58 $d0 $05 $63 }T T{ $e30e M $e30f M $e310 M -> $2b $55 $a5 }T
$aaaf >PC $5f >S $2e >A $a5 >X $4b >Y $a4 >P $aaaf $2b >M $aab0 $3b >M $aab1 $9c >M
T{ op PC S A X Y P -> $aab0 $5f $2e $a5 $4b $a4 }T T{ $aaaf M $aab0 M $aab1 M -> $2b $3b $9c }T
$114c >PC $5f >S $d0 >A $76 >X $b9 >Y $23 >P $114c $2b >M $114d $2c >M $114e $e4 >M
T{ op PC S A X Y P -> $114d $5f $d0 $76 $b9 $23 }T T{ $114c M $114d M $114e M -> $2b $2c $e4 }T
$35b3 >PC $93 >S $92 >A $06 >X $40 >Y $e3 >P $35b3 $2b >M $35b4 $9a >M $35b5 $11 >M
T{ op PC S A X Y P -> $35b4 $93 $92 $06 $40 $e3 }T T{ $35b3 M $35b4 M $35b5 M -> $2b $9a $11 }T
$8af1 >PC $cf >S $64 >A $1c >X $58 >Y $aa >P $8af1 $2b >M $8af2 $66 >M $8af3 $1f >M
T{ op PC S A X Y P -> $8af2 $cf $64 $1c $58 $aa }T T{ $8af1 M $8af2 M $8af3 M -> $2b $66 $1f }T
$7493 >PC $45 >S $b2 >A $e0 >X $90 >Y $ae >P $7493 $2b >M $7494 $6a >M $7495 $00 >M
T{ op PC S A X Y P -> $7494 $45 $b2 $e0 $90 $ae }T T{ $7493 M $7494 M $7495 M -> $2b $6a $00 }T
$46be >PC $00 >S $cf >A $bf >X $97 >Y $6d >P $46be $2b >M $46bf $a6 >M $46c0 $8c >M
T{ op PC S A X Y P -> $46bf $00 $cf $bf $97 $6d }T T{ $46be M $46bf M $46c0 M -> $2b $a6 $8c }T
$bf7f >PC $b3 >S $23 >A $ac >X $aa >Y $6c >P $bf7f $2b >M $bf80 $97 >M $bf81 $7f >M
T{ op PC S A X Y P -> $bf80 $b3 $23 $ac $aa $6c }T T{ $bf7f M $bf80 M $bf81 M -> $2b $97 $7f }T
$6d7b >PC $cd >S $66 >A $c2 >X $86 >Y $ea >P $6d7b $2b >M $6d7c $cf >M $6d7d $33 >M
T{ op PC S A X Y P -> $6d7c $cd $66 $c2 $86 $ea }T T{ $6d7b M $6d7c M $6d7d M -> $2b $cf $33 }T
$2b74 >PC $c4 >S $fc >A $fd >X $f6 >Y $ef >P $2b74 $2b >M $2b75 $a3 >M $2b76 $eb >M
T{ op PC S A X Y P -> $2b75 $c4 $fc $fd $f6 $ef }T T{ $2b74 M $2b75 M $2b76 M -> $2b $a3 $eb }T
$b6cc >PC $51 >S $21 >A $f8 >X $31 >Y $6f >P $b6cc $2b >M $b6cd $95 >M $b6ce $8f >M
T{ op PC S A X Y P -> $b6cd $51 $21 $f8 $31 $6f }T T{ $b6cc M $b6cd M $b6ce M -> $2b $95 $8f }T
$a6a4 >PC $02 >S $b8 >A $6a >X $d5 >Y $e4 >P $a6a4 $2b >M $a6a5 $62 >M $a6a6 $22 >M
T{ op PC S A X Y P -> $a6a5 $02 $b8 $6a $d5 $e4 }T T{ $a6a4 M $a6a5 M $a6a6 M -> $2b $62 $22 }T
$12b1 >PC $03 >S $38 >A $47 >X $dc >Y $62 >P $12b1 $2b >M $12b2 $9a >M $12b3 $49 >M
T{ op PC S A X Y P -> $12b2 $03 $38 $47 $dc $62 }T T{ $12b1 M $12b2 M $12b3 M -> $2b $9a $49 }T
$b842 >PC $6f >S $6f >A $ef >X $6e >Y $a2 >P $b842 $2b >M $b843 $60 >M $b844 $13 >M
T{ op PC S A X Y P -> $b843 $6f $6f $ef $6e $a2 }T T{ $b842 M $b843 M $b844 M -> $2b $60 $13 }T
( 2c )
$20f7 >PC $43 >S $6c >A $de >X $d2 >Y $ec >P $20f7 $2c >M $20f8 $2c >M $20f9 $7e >M $20fa $ba >M $7e2c $01 >M
T{ op PC S A X Y P -> $20fa $43 $6c $de $d2 $2e }T T{ $20f7 M $20f8 M $20f9 M $20fa M $7e2c M -> $2c $2c $7e $ba $01 }T
$904e >PC $92 >S $63 >A $4c >X $34 >Y $2d >P $904e $2c >M $904f $6a >M $9050 $b6 >M $9051 $c9 >M $b66a $9d >M
T{ op PC S A X Y P -> $9051 $92 $63 $4c $34 $ad }T T{ $904e M $904f M $9050 M $9051 M $b66a M -> $2c $6a $b6 $c9 $9d }T
$348e >PC $3f >S $19 >A $89 >X $23 >Y $61 >P $348e $2c >M $348f $9f >M $3490 $ef >M $3491 $fe >M $ef9f $9e >M
T{ op PC S A X Y P -> $3491 $3f $19 $89 $23 $a1 }T T{ $348e M $348f M $3490 M $3491 M $ef9f M -> $2c $9f $ef $fe $9e }T
$c06f >PC $17 >S $26 >A $d9 >X $0b >Y $28 >P $226a $f9 >M $c06f $2c >M $c070 $6a >M $c071 $22 >M $c072 $ce >M
T{ op PC S A X Y P -> $c072 $17 $26 $d9 $0b $e8 }T T{ $226a M $c06f M $c070 M $c071 M $c072 M -> $f9 $2c $6a $22 $ce }T
$0f79 >PC $1a >S $76 >A $be >X $4e >Y $ec >P $0f79 $2c >M $0f7a $ac >M $0f7b $93 >M $0f7c $c5 >M $93ac $ba >M
T{ op PC S A X Y P -> $0f7c $1a $76 $be $4e $ac }T T{ $0f79 M $0f7a M $0f7b M $0f7c M $93ac M -> $2c $ac $93 $c5 $ba }T
$f610 >PC $9b >S $42 >A $ba >X $ad >Y $6b >P $728a $77 >M $f610 $2c >M $f611 $8a >M $f612 $72 >M $f613 $8d >M
T{ op PC S A X Y P -> $f613 $9b $42 $ba $ad $69 }T T{ $728a M $f610 M $f611 M $f612 M $f613 M -> $77 $2c $8a $72 $8d }T
$6f58 >PC $00 >S $89 >A $9e >X $40 >Y $61 >P $6f58 $2c >M $6f59 $76 >M $6f5a $83 >M $6f5b $bd >M $8376 $fe >M
T{ op PC S A X Y P -> $6f5b $00 $89 $9e $40 $e1 }T T{ $6f58 M $6f59 M $6f5a M $6f5b M $8376 M -> $2c $76 $83 $bd $fe }T
$b679 >PC $3e >S $17 >A $6e >X $bf >Y $23 >P $b679 $2c >M $b67a $1e >M $b67b $fc >M $b67c $43 >M $fc1e $23 >M
T{ op PC S A X Y P -> $b67c $3e $17 $6e $bf $21 }T T{ $b679 M $b67a M $b67b M $b67c M $fc1e M -> $2c $1e $fc $43 $23 }T
$51ac >PC $30 >S $73 >A $4c >X $f1 >Y $23 >P $51ac $2c >M $51ad $20 >M $51ae $c1 >M $51af $74 >M $c120 $54 >M
T{ op PC S A X Y P -> $51af $30 $73 $4c $f1 $61 }T T{ $51ac M $51ad M $51ae M $51af M $c120 M -> $2c $20 $c1 $74 $54 }T
$56ae >PC $e5 >S $6f >A $a1 >X $f2 >Y $29 >P $2b2c $94 >M $56ae $2c >M $56af $2c >M $56b0 $2b >M $56b1 $4e >M
T{ op PC S A X Y P -> $56b1 $e5 $6f $a1 $f2 $a9 }T T{ $2b2c M $56ae M $56af M $56b0 M $56b1 M -> $94 $2c $2c $2b $4e }T
$a84a >PC $8f >S $bd >A $49 >X $17 >Y $6b >P $9941 $19 >M $a84a $2c >M $a84b $41 >M $a84c $99 >M $a84d $d6 >M
T{ op PC S A X Y P -> $a84d $8f $bd $49 $17 $29 }T T{ $9941 M $a84a M $a84b M $a84c M $a84d M -> $19 $2c $41 $99 $d6 }T
$7b06 >PC $19 >S $2e >A $e0 >X $e9 >Y $24 >P $596f $fc >M $7b06 $2c >M $7b07 $6f >M $7b08 $59 >M $7b09 $9c >M
T{ op PC S A X Y P -> $7b09 $19 $2e $e0 $e9 $e4 }T T{ $596f M $7b06 M $7b07 M $7b08 M $7b09 M -> $fc $2c $6f $59 $9c }T
$6dfb >PC $4c >S $7d >A $fc >X $97 >Y $e9 >P $3d6a $b1 >M $6dfb $2c >M $6dfc $6a >M $6dfd $3d >M $6dfe $48 >M
T{ op PC S A X Y P -> $6dfe $4c $7d $fc $97 $a9 }T T{ $3d6a M $6dfb M $6dfc M $6dfd M $6dfe M -> $b1 $2c $6a $3d $48 }T
$0e05 >PC $2d >S $ae >A $47 >X $db >Y $60 >P $0e05 $2c >M $0e06 $28 >M $0e07 $bc >M $0e08 $04 >M $bc28 $9e >M
T{ op PC S A X Y P -> $0e08 $2d $ae $47 $db $a0 }T T{ $0e05 M $0e06 M $0e07 M $0e08 M $bc28 M -> $2c $28 $bc $04 $9e }T
$afe0 >PC $47 >S $ee >A $fc >X $c8 >Y $6f >P $64eb $35 >M $afe0 $2c >M $afe1 $eb >M $afe2 $64 >M $afe3 $56 >M
T{ op PC S A X Y P -> $afe3 $47 $ee $fc $c8 $2d }T T{ $64eb M $afe0 M $afe1 M $afe2 M $afe3 M -> $35 $2c $eb $64 $56 }T
$2bc3 >PC $b8 >S $7f >A $8f >X $60 >Y $6d >P $2bc3 $2c >M $2bc4 $7c >M $2bc5 $f8 >M $2bc6 $fb >M $f87c $17 >M
T{ op PC S A X Y P -> $2bc6 $b8 $7f $8f $60 $2d }T T{ $2bc3 M $2bc4 M $2bc5 M $2bc6 M $f87c M -> $2c $7c $f8 $fb $17 }T
( 2d )
$0f0c >PC $ef >S $dc >A $c1 >X $c0 >Y $e1 >P $0f0c $2d >M $0f0d $bb >M $0f0e $f7 >M $0f0f $df >M $f7bb $1a >M
T{ op PC S A X Y P -> $0f0f $ef $18 $c1 $c0 $61 }T T{ $0f0c M $0f0d M $0f0e M $0f0f M $f7bb M -> $2d $bb $f7 $df $1a }T
$0954 >PC $2b >S $26 >A $42 >X $08 >Y $a7 >P $0954 $2d >M $0955 $20 >M $0956 $2c >M $0957 $e1 >M $2c20 $1b >M
T{ op PC S A X Y P -> $0957 $2b $02 $42 $08 $25 }T T{ $0954 M $0955 M $0956 M $0957 M $2c20 M -> $2d $20 $2c $e1 $1b }T
$a7ce >PC $e1 >S $64 >A $4c >X $d2 >Y $6f >P $7d28 $1a >M $a7ce $2d >M $a7cf $28 >M $a7d0 $7d >M $a7d1 $f0 >M
T{ op PC S A X Y P -> $a7d1 $e1 $00 $4c $d2 $6f }T T{ $7d28 M $a7ce M $a7cf M $a7d0 M $a7d1 M -> $1a $2d $28 $7d $f0 }T
$a8a3 >PC $74 >S $71 >A $26 >X $8d >Y $ea >P $9d04 $38 >M $a8a3 $2d >M $a8a4 $04 >M $a8a5 $9d >M $a8a6 $1f >M
T{ op PC S A X Y P -> $a8a6 $74 $30 $26 $8d $68 }T T{ $9d04 M $a8a3 M $a8a4 M $a8a5 M $a8a6 M -> $38 $2d $04 $9d $1f }T
$4805 >PC $30 >S $d2 >A $57 >X $02 >Y $e8 >P $03c5 $1e >M $4805 $2d >M $4806 $c5 >M $4807 $03 >M $4808 $93 >M
T{ op PC S A X Y P -> $4808 $30 $12 $57 $02 $68 }T T{ $03c5 M $4805 M $4806 M $4807 M $4808 M -> $1e $2d $c5 $03 $93 }T
$c915 >PC $e1 >S $2c >A $de >X $16 >Y $eb >P $5a6a $b6 >M $c915 $2d >M $c916 $6a >M $c917 $5a >M $c918 $ad >M
T{ op PC S A X Y P -> $c918 $e1 $24 $de $16 $69 }T T{ $5a6a M $c915 M $c916 M $c917 M $c918 M -> $b6 $2d $6a $5a $ad }T
$80e6 >PC $e1 >S $da >A $bd >X $34 >Y $2c >P $0670 $3c >M $80e6 $2d >M $80e7 $70 >M $80e8 $06 >M $80e9 $43 >M
T{ op PC S A X Y P -> $80e9 $e1 $18 $bd $34 $2c }T T{ $0670 M $80e6 M $80e7 M $80e8 M $80e9 M -> $3c $2d $70 $06 $43 }T
$5c9a >PC $15 >S $df >A $e7 >X $2c >Y $a7 >P $5c9a $2d >M $5c9b $bc >M $5c9c $c5 >M $5c9d $93 >M $c5bc $90 >M
T{ op PC S A X Y P -> $5c9d $15 $90 $e7 $2c $a5 }T T{ $5c9a M $5c9b M $5c9c M $5c9d M $c5bc M -> $2d $bc $c5 $93 $90 }T
$bbef >PC $3c >S $ba >A $7f >X $8c >Y $ad >P $89e8 $75 >M $bbef $2d >M $bbf0 $e8 >M $bbf1 $89 >M $bbf2 $04 >M
T{ op PC S A X Y P -> $bbf2 $3c $30 $7f $8c $2d }T T{ $89e8 M $bbef M $bbf0 M $bbf1 M $bbf2 M -> $75 $2d $e8 $89 $04 }T
$acb0 >PC $ff >S $f1 >A $f1 >X $da >Y $28 >P $acb0 $2d >M $acb1 $84 >M $acb2 $da >M $acb3 $d2 >M $da84 $90 >M
T{ op PC S A X Y P -> $acb3 $ff $90 $f1 $da $a8 }T T{ $acb0 M $acb1 M $acb2 M $acb3 M $da84 M -> $2d $84 $da $d2 $90 }T
$b469 >PC $77 >S $47 >A $5f >X $b8 >Y $a9 >P $b469 $2d >M $b46a $95 >M $b46b $bc >M $b46c $4f >M $bc95 $51 >M
T{ op PC S A X Y P -> $b46c $77 $41 $5f $b8 $29 }T T{ $b469 M $b46a M $b46b M $b46c M $bc95 M -> $2d $95 $bc $4f $51 }T
$f918 >PC $06 >S $34 >A $5a >X $ad >Y $a7 >P $cf34 $94 >M $f918 $2d >M $f919 $34 >M $f91a $cf >M $f91b $a5 >M
T{ op PC S A X Y P -> $f91b $06 $14 $5a $ad $25 }T T{ $cf34 M $f918 M $f919 M $f91a M $f91b M -> $94 $2d $34 $cf $a5 }T
$8494 >PC $68 >S $50 >A $09 >X $f7 >Y $e3 >P $6a29 $d3 >M $8494 $2d >M $8495 $29 >M $8496 $6a >M $8497 $a7 >M
T{ op PC S A X Y P -> $8497 $68 $50 $09 $f7 $61 }T T{ $6a29 M $8494 M $8495 M $8496 M $8497 M -> $d3 $2d $29 $6a $a7 }T
$c6a3 >PC $3e >S $68 >A $0e >X $59 >Y $a3 >P $9b07 $ab >M $c6a3 $2d >M $c6a4 $07 >M $c6a5 $9b >M $c6a6 $5b >M
T{ op PC S A X Y P -> $c6a6 $3e $28 $0e $59 $21 }T T{ $9b07 M $c6a3 M $c6a4 M $c6a5 M $c6a6 M -> $ab $2d $07 $9b $5b }T
$6196 >PC $83 >S $10 >A $09 >X $93 >Y $e1 >P $6196 $2d >M $6197 $33 >M $6198 $f8 >M $6199 $27 >M $f833 $d2 >M
T{ op PC S A X Y P -> $6199 $83 $10 $09 $93 $61 }T T{ $6196 M $6197 M $6198 M $6199 M $f833 M -> $2d $33 $f8 $27 $d2 }T
$9e5a >PC $77 >S $d4 >A $87 >X $15 >Y $ac >P $3071 $60 >M $9e5a $2d >M $9e5b $71 >M $9e5c $30 >M $9e5d $b7 >M
T{ op PC S A X Y P -> $9e5d $77 $40 $87 $15 $2c }T T{ $3071 M $9e5a M $9e5b M $9e5c M $9e5d M -> $60 $2d $71 $30 $b7 }T
( 2e )
$6960 >PC $e6 >S $75 >A $7f >X $37 >Y $ae >P $6960 $2e >M $6961 $60 >M $6962 $ff >M $6963 $6d >M $ff60 $dd >M
T{ op PC S A X Y P -> $6963 $e6 $75 $7f $37 $ad }T T{ $6960 M $6961 M $6962 M $6963 M $ff60 M -> $2e $60 $ff $6d $ba }T
$1c1f >PC $20 >S $c0 >A $d3 >X $73 >Y $ee >P $1c1f $2e >M $1c20 $6e >M $1c21 $db >M $1c22 $ad >M $db6e $eb >M
T{ op PC S A X Y P -> $1c22 $20 $c0 $d3 $73 $ed }T T{ $1c1f M $1c20 M $1c21 M $1c22 M $db6e M -> $2e $6e $db $ad $d6 }T
$a420 >PC $40 >S $1e >A $54 >X $3e >Y $e1 >P $a420 $2e >M $a421 $7c >M $a422 $b0 >M $a423 $e5 >M $b07c $f5 >M
T{ op PC S A X Y P -> $a423 $40 $1e $54 $3e $e1 }T T{ $a420 M $a421 M $a422 M $a423 M $b07c M -> $2e $7c $b0 $e5 $eb }T
$d2d4 >PC $86 >S $a1 >A $58 >X $ca >Y $a2 >P $6f90 $19 >M $d2d4 $2e >M $d2d5 $90 >M $d2d6 $6f >M $d2d7 $7a >M
T{ op PC S A X Y P -> $d2d7 $86 $a1 $58 $ca $20 }T T{ $6f90 M $d2d4 M $d2d5 M $d2d6 M $d2d7 M -> $32 $2e $90 $6f $7a }T
$a713 >PC $6f >S $cb >A $7f >X $c5 >Y $e9 >P $99be $e1 >M $a713 $2e >M $a714 $be >M $a715 $99 >M $a716 $4a >M
T{ op PC S A X Y P -> $a716 $6f $cb $7f $c5 $e9 }T T{ $99be M $a713 M $a714 M $a715 M $a716 M -> $c3 $2e $be $99 $4a }T
$aac4 >PC $b2 >S $cf >A $4e >X $3f >Y $e9 >P $a8b6 $b7 >M $aac4 $2e >M $aac5 $b6 >M $aac6 $a8 >M $aac7 $53 >M
T{ op PC S A X Y P -> $aac7 $b2 $cf $4e $3f $69 }T T{ $a8b6 M $aac4 M $aac5 M $aac6 M $aac7 M -> $6f $2e $b6 $a8 $53 }T
$40fe >PC $64 >S $1b >A $77 >X $d4 >Y $2b >P $0671 $19 >M $40fe $2e >M $40ff $71 >M $4100 $06 >M $4101 $d8 >M
T{ op PC S A X Y P -> $4101 $64 $1b $77 $d4 $28 }T T{ $0671 M $40fe M $40ff M $4100 M $4101 M -> $33 $2e $71 $06 $d8 }T
$f7bc >PC $da >S $85 >A $1f >X $6c >Y $2a >P $f7bc $2e >M $f7bd $cb >M $f7be $f9 >M $f7bf $4a >M $f9cb $dd >M
T{ op PC S A X Y P -> $f7bf $da $85 $1f $6c $a9 }T T{ $f7bc M $f7bd M $f7be M $f7bf M $f9cb M -> $2e $cb $f9 $4a $ba }T
$0b3e >PC $27 >S $a1 >A $ee >X $ac >Y $6b >P $0b3e $2e >M $0b3f $57 >M $0b40 $4c >M $0b41 $e4 >M $4c57 $de >M
T{ op PC S A X Y P -> $0b41 $27 $a1 $ee $ac $e9 }T T{ $0b3e M $0b3f M $0b40 M $0b41 M $4c57 M -> $2e $57 $4c $e4 $bd }T
$4e29 >PC $c2 >S $d7 >A $d8 >X $b6 >Y $2e >P $4e29 $2e >M $4e2a $df >M $4e2b $dc >M $4e2c $70 >M $dcdf $77 >M
T{ op PC S A X Y P -> $4e2c $c2 $d7 $d8 $b6 $ac }T T{ $4e29 M $4e2a M $4e2b M $4e2c M $dcdf M -> $2e $df $dc $70 $ee }T
$9e77 >PC $7d >S $7f >A $d3 >X $f7 >Y $22 >P $9e77 $2e >M $9e78 $a2 >M $9e79 $a7 >M $9e7a $35 >M $a7a2 $b3 >M
T{ op PC S A X Y P -> $9e7a $7d $7f $d3 $f7 $21 }T T{ $9e77 M $9e78 M $9e79 M $9e7a M $a7a2 M -> $2e $a2 $a7 $35 $66 }T
$ce4c >PC $1a >S $22 >A $13 >X $f9 >Y $e0 >P $721e $02 >M $ce4c $2e >M $ce4d $1e >M $ce4e $72 >M $ce4f $ed >M
T{ op PC S A X Y P -> $ce4f $1a $22 $13 $f9 $60 }T T{ $721e M $ce4c M $ce4d M $ce4e M $ce4f M -> $04 $2e $1e $72 $ed }T
$ffdb >PC $a3 >S $69 >A $26 >X $c3 >Y $e4 >P $a374 $d1 >M $ffdb $2e >M $ffdc $74 >M $ffdd $a3 >M $ffde $67 >M
T{ op PC S A X Y P -> $ffde $a3 $69 $26 $c3 $e5 }T T{ $a374 M $ffdb M $ffdc M $ffdd M $ffde M -> $a2 $2e $74 $a3 $67 }T
$2980 >PC $7d >S $eb >A $f9 >X $b4 >Y $e9 >P $2980 $2e >M $2981 $9e >M $2982 $cd >M $2983 $b6 >M $cd9e $7c >M
T{ op PC S A X Y P -> $2983 $7d $eb $f9 $b4 $e8 }T T{ $2980 M $2981 M $2982 M $2983 M $cd9e M -> $2e $9e $cd $b6 $f9 }T
$247f >PC $03 >S $47 >A $51 >X $ef >Y $e8 >P $247f $2e >M $2480 $91 >M $2481 $6d >M $2482 $93 >M $6d91 $38 >M
T{ op PC S A X Y P -> $2482 $03 $47 $51 $ef $68 }T T{ $247f M $2480 M $2481 M $2482 M $6d91 M -> $2e $91 $6d $93 $70 }T
$48be >PC $2b >S $1b >A $c6 >X $8f >Y $6b >P $48be $2e >M $48bf $e6 >M $48c0 $e3 >M $48c1 $da >M $e3e6 $93 >M
T{ op PC S A X Y P -> $48c1 $2b $1b $c6 $8f $69 }T T{ $48be M $48bf M $48c0 M $48c1 M $e3e6 M -> $2e $e6 $e3 $da $27 }T
( 2f )
$b9b6 >PC $a5 >S $d8 >A $ca >X $76 >Y $eb >P $00d6 $c3 >M $b9b6 $2f >M $b9b7 $d6 >M $b9b8 $20 >M $b9d9 $13 >M
T{ op PC S A X Y P -> $b9d9 $a5 $d8 $ca $76 $eb }T T{ $00d6 M $b9b6 M $b9b7 M $b9b8 M $b9d9 M -> $c3 $2f $d6 $20 $13 }T
$7af2 >PC $d4 >S $f8 >A $77 >X $a0 >Y $a6 >P $0007 $0c >M $7a77 $e6 >M $7af2 $2f >M $7af3 $07 >M $7af4 $82 >M $7af5 $e9 >M
T{ op PC S A X Y P -> $7af5 $d4 $f8 $77 $a0 $a6 }T T{ $0007 M $7a77 M $7af2 M $7af3 M $7af4 M $7af5 M -> $0c $e6 $2f $07 $82 $e9 }T
$092b >PC $a6 >S $6e >A $63 >X $c6 >Y $a6 >P $0035 $f5 >M $092b $2f >M $092c $35 >M $092d $8b >M $092e $08 >M $09b9 $f2 >M
T{ op PC S A X Y P -> $092e $a6 $6e $63 $c6 $a6 }T T{ $0035 M $092b M $092c M $092d M $092e M $09b9 M -> $f5 $2f $35 $8b $08 $f2 }T
$1dfe >PC $b0 >S $84 >A $5a >X $07 >Y $67 >P $0009 $a6 >M $1dfe $2f >M $1dff $09 >M $1e00 $41 >M $1e01 $e6 >M $1e42 $df >M
T{ op PC S A X Y P -> $1e01 $b0 $84 $5a $07 $67 }T T{ $0009 M $1dfe M $1dff M $1e00 M $1e01 M $1e42 M -> $a6 $2f $09 $41 $e6 $df }T
$69aa >PC $26 >S $a7 >A $9b >X $84 >Y $28 >P $00df $53 >M $69aa $2f >M $69ab $df >M $69ac $22 >M $69cf $44 >M
T{ op PC S A X Y P -> $69cf $26 $a7 $9b $84 $28 }T T{ $00df M $69aa M $69ab M $69ac M $69cf M -> $53 $2f $df $22 $44 }T
$22ab >PC $30 >S $da >A $76 >X $1f >Y $6f >P $0038 $37 >M $22a1 $d8 >M $22ab $2f >M $22ac $38 >M $22ad $f3 >M $22ae $92 >M
T{ op PC S A X Y P -> $22ae $30 $da $76 $1f $6f }T T{ $0038 M $22a1 M $22ab M $22ac M $22ad M $22ae M -> $37 $d8 $2f $38 $f3 $92 }T
$65d9 >PC $43 >S $8c >A $7c >X $c1 >Y $e8 >P $0065 $66 >M $651c $0c >M $65d9 $2f >M $65da $65 >M $65db $40 >M $65dc $48 >M
T{ op PC S A X Y P -> $65dc $43 $8c $7c $c1 $e8 }T T{ $0065 M $651c M $65d9 M $65da M $65db M $65dc M -> $66 $0c $2f $65 $40 $48 }T
$14f0 >PC $33 >S $1f >A $bd >X $20 >Y $a4 >P $0016 $c9 >M $1434 $ab >M $14f0 $2f >M $14f1 $16 >M $14f2 $41 >M $1534 $6f >M
T{ op PC S A X Y P -> $1534 $33 $1f $bd $20 $a4 }T T{ $0016 M $1434 M $14f0 M $14f1 M $14f2 M $1534 M -> $c9 $ab $2f $16 $41 $6f }T
$8b2b >PC $74 >S $b4 >A $5d >X $a8 >Y $2d >P $001c $94 >M $8b2b $2f >M $8b2c $1c >M $8b2d $98 >M $8b2e $2e >M $8bc6 $0c >M
T{ op PC S A X Y P -> $8b2e $74 $b4 $5d $a8 $2d }T T{ $001c M $8b2b M $8b2c M $8b2d M $8b2e M $8bc6 M -> $94 $2f $1c $98 $2e $0c }T
$6eb1 >PC $e6 >S $55 >A $e4 >X $ba >Y $60 >P $00a2 $f2 >M $6eaa $55 >M $6eb1 $2f >M $6eb2 $a2 >M $6eb3 $f6 >M
T{ op PC S A X Y P -> $6eaa $e6 $55 $e4 $ba $60 }T T{ $00a2 M $6eaa M $6eb1 M $6eb2 M $6eb3 M -> $f2 $55 $2f $a2 $f6 }T
$db67 >PC $c9 >S $af >A $d4 >X $6c >Y $26 >P $00fb $c6 >M $db67 $2f >M $db68 $fb >M $db69 $46 >M $db6a $ae >M $dbb0 $55 >M
T{ op PC S A X Y P -> $db6a $c9 $af $d4 $6c $26 }T T{ $00fb M $db67 M $db68 M $db69 M $db6a M $dbb0 M -> $c6 $2f $fb $46 $ae $55 }T
$a403 >PC $ee >S $f0 >A $65 >X $96 >Y $61 >P $00f8 $52 >M $a38f $63 >M $a403 $2f >M $a404 $f8 >M $a405 $89 >M $a48f $64 >M
T{ op PC S A X Y P -> $a38f $ee $f0 $65 $96 $61 }T T{ $00f8 M $a38f M $a403 M $a404 M $a405 M $a48f M -> $52 $63 $2f $f8 $89 $64 }T
$a128 >PC $09 >S $3e >A $24 >X $e6 >Y $a6 >P $00a8 $b2 >M $a104 $56 >M $a128 $2f >M $a129 $a8 >M $a12a $d9 >M
T{ op PC S A X Y P -> $a104 $09 $3e $24 $e6 $a6 }T T{ $00a8 M $a104 M $a128 M $a129 M $a12a M -> $b2 $56 $2f $a8 $d9 }T
$760c >PC $70 >S $15 >A $77 >X $dc >Y $2c >P $00d6 $d0 >M $760c $2f >M $760d $d6 >M $760e $52 >M $7661 $22 >M
T{ op PC S A X Y P -> $7661 $70 $15 $77 $dc $2c }T T{ $00d6 M $760c M $760d M $760e M $7661 M -> $d0 $2f $d6 $52 $22 }T
$ccc9 >PC $ce >S $82 >A $6f >X $9d >Y $e4 >P $0079 $ed >M $ccc9 $2f >M $ccca $79 >M $cccb $33 >M $cccc $1a >M $ccff $a1 >M
T{ op PC S A X Y P -> $cccc $ce $82 $6f $9d $e4 }T T{ $0079 M $ccc9 M $ccca M $cccb M $cccc M $ccff M -> $ed $2f $79 $33 $1a $a1 }T
$692a >PC $ac >S $c5 >A $b9 >X $00 >Y $a0 >P $0003 $17 >M $692a $2f >M $692b $03 >M $692c $b1 >M $692d $77 >M $69de $2c >M
T{ op PC S A X Y P -> $692d $ac $c5 $b9 $00 $a0 }T T{ $0003 M $692a M $692b M $692c M $692d M $69de M -> $17 $2f $03 $b1 $77 $2c }T
( 30 )
$3a0c >PC $20 >S $fd >A $bb >X $67 >Y $22 >P $3a0c $30 >M $3a0d $60 >M $3a0e $b5 >M
T{ op PC S A X Y P -> $3a0e $20 $fd $bb $67 $22 }T T{ $3a0c M $3a0d M $3a0e M -> $30 $60 $b5 }T
$6b4e >PC $ba >S $ca >A $a1 >X $0a >Y $e5 >P $6b4e $30 >M $6b4f $59 >M $6b50 $a8 >M $6ba9 $92 >M
T{ op PC S A X Y P -> $6ba9 $ba $ca $a1 $0a $e5 }T T{ $6b4e M $6b4f M $6b50 M $6ba9 M -> $30 $59 $a8 $92 }T
$a1d6 >PC $d2 >S $b3 >A $27 >X $9e >Y $24 >P $a1d6 $30 >M $a1d7 $34 >M $a1d8 $0d >M
T{ op PC S A X Y P -> $a1d8 $d2 $b3 $27 $9e $24 }T T{ $a1d6 M $a1d7 M $a1d8 M -> $30 $34 $0d }T
$e5a5 >PC $32 >S $8b >A $2c >X $43 >Y $68 >P $e5a5 $30 >M $e5a6 $72 >M $e5a7 $d6 >M
T{ op PC S A X Y P -> $e5a7 $32 $8b $2c $43 $68 }T T{ $e5a5 M $e5a6 M $e5a7 M -> $30 $72 $d6 }T
$e8aa >PC $70 >S $27 >A $68 >X $20 >Y $24 >P $e8aa $30 >M $e8ab $61 >M $e8ac $71 >M
T{ op PC S A X Y P -> $e8ac $70 $27 $68 $20 $24 }T T{ $e8aa M $e8ab M $e8ac M -> $30 $61 $71 }T
$0532 >PC $13 >S $93 >A $04 >X $2f >Y $6c >P $0532 $30 >M $0533 $f1 >M $0534 $4b >M
T{ op PC S A X Y P -> $0534 $13 $93 $04 $2f $6c }T T{ $0532 M $0533 M $0534 M -> $30 $f1 $4b }T
$4b08 >PC $a0 >S $fb >A $ab >X $18 >Y $6a >P $4b08 $30 >M $4b09 $40 >M $4b0a $1e >M
T{ op PC S A X Y P -> $4b0a $a0 $fb $ab $18 $6a }T T{ $4b08 M $4b09 M $4b0a M -> $30 $40 $1e }T
$9c79 >PC $a4 >S $84 >A $f1 >X $52 >Y $64 >P $9c79 $30 >M $9c7a $fc >M $9c7b $2f >M
T{ op PC S A X Y P -> $9c7b $a4 $84 $f1 $52 $64 }T T{ $9c79 M $9c7a M $9c7b M -> $30 $fc $2f }T
$e5fc >PC $35 >S $c1 >A $33 >X $9f >Y $a4 >P $e57c $29 >M $e5fc $30 >M $e5fd $7e >M $e5fe $26 >M $e67c $30 >M
T{ op PC S A X Y P -> $e67c $35 $c1 $33 $9f $a4 }T T{ $e57c M $e5fc M $e5fd M $e5fe M $e67c M -> $29 $30 $7e $26 $30 }T
$b0ac >PC $14 >S $2d >A $98 >X $a8 >Y $65 >P $b0ac $30 >M $b0ad $53 >M $b0ae $61 >M
T{ op PC S A X Y P -> $b0ae $14 $2d $98 $a8 $65 }T T{ $b0ac M $b0ad M $b0ae M -> $30 $53 $61 }T
$9ce6 >PC $c3 >S $2f >A $c2 >X $99 >Y $a9 >P $9ca2 $be >M $9ce6 $30 >M $9ce7 $ba >M $9ce8 $6e >M
T{ op PC S A X Y P -> $9ca2 $c3 $2f $c2 $99 $a9 }T T{ $9ca2 M $9ce6 M $9ce7 M $9ce8 M -> $be $30 $ba $6e }T
$dada >PC $a4 >S $e2 >A $a6 >X $c6 >Y $ab >P $dada $30 >M $dadb $11 >M $dadc $07 >M $daed $63 >M
T{ op PC S A X Y P -> $daed $a4 $e2 $a6 $c6 $ab }T T{ $dada M $dadb M $dadc M $daed M -> $30 $11 $07 $63 }T
$ef26 >PC $d1 >S $8b >A $29 >X $82 >Y $e9 >P $ef19 $8a >M $ef26 $30 >M $ef27 $f1 >M $ef28 $de >M
T{ op PC S A X Y P -> $ef19 $d1 $8b $29 $82 $e9 }T T{ $ef19 M $ef26 M $ef27 M $ef28 M -> $8a $30 $f1 $de }T
$3db2 >PC $86 >S $e4 >A $73 >X $52 >Y $a3 >P $3d32 $de >M $3db2 $30 >M $3db3 $7e >M $3db4 $7c >M $3e32 $83 >M
T{ op PC S A X Y P -> $3e32 $86 $e4 $73 $52 $a3 }T T{ $3d32 M $3db2 M $3db3 M $3db4 M $3e32 M -> $de $30 $7e $7c $83 }T
$0412 >PC $80 >S $46 >A $85 >X $70 >Y $e6 >P $0412 $30 >M $0413 $0d >M $0414 $fa >M $0421 $92 >M
T{ op PC S A X Y P -> $0421 $80 $46 $85 $70 $e6 }T T{ $0412 M $0413 M $0414 M $0421 M -> $30 $0d $fa $92 }T
$19e2 >PC $10 >S $ce >A $a6 >X $55 >Y $21 >P $19e2 $30 >M $19e3 $6d >M $19e4 $02 >M
T{ op PC S A X Y P -> $19e4 $10 $ce $a6 $55 $21 }T T{ $19e2 M $19e3 M $19e4 M -> $30 $6d $02 }T
( 31 )
$00cc >PC $95 >S $fc >A $50 >X $d0 >Y $ab >P $002c $94 >M $002d $5b >M $00cc $31 >M $00cd $2c >M $00ce $1b >M $5c64 $62 >M
T{ op PC S A X Y P -> $00ce $95 $60 $50 $d0 $29 }T T{ $002c M $002d M $00cc M $00cd M $00ce M $5c64 M -> $94 $5b $31 $2c $1b $62 }T
$a4bf >PC $be >S $92 >A $41 >X $68 >Y $e4 >P $0075 $8b >M $0076 $7b >M $7bf3 $67 >M $a4bf $31 >M $a4c0 $75 >M $a4c1 $fa >M
T{ op PC S A X Y P -> $a4c1 $be $02 $41 $68 $64 }T T{ $0075 M $0076 M $7bf3 M $a4bf M $a4c0 M $a4c1 M -> $8b $7b $67 $31 $75 $fa }T
$ad2f >PC $b4 >S $c9 >A $c8 >X $ec >Y $ef >P $00a2 $16 >M $00a3 $5b >M $5c02 $ca >M $ad2f $31 >M $ad30 $a2 >M $ad31 $d9 >M
T{ op PC S A X Y P -> $ad31 $b4 $c8 $c8 $ec $ed }T T{ $00a2 M $00a3 M $5c02 M $ad2f M $ad30 M $ad31 M -> $16 $5b $ca $31 $a2 $d9 }T
$bb18 >PC $6a >S $5e >A $50 >X $8f >Y $65 >P $00cd $c9 >M $00ce $9e >M $9f58 $5a >M $bb18 $31 >M $bb19 $cd >M $bb1a $41 >M
T{ op PC S A X Y P -> $bb1a $6a $5a $50 $8f $65 }T T{ $00cd M $00ce M $9f58 M $bb18 M $bb19 M $bb1a M -> $c9 $9e $5a $31 $cd $41 }T
$d15c >PC $e2 >S $a1 >A $9d >X $27 >Y $e9 >P $00a7 $9e >M $00a8 $2c >M $2cc5 $a0 >M $d15c $31 >M $d15d $a7 >M $d15e $88 >M
T{ op PC S A X Y P -> $d15e $e2 $a0 $9d $27 $e9 }T T{ $00a7 M $00a8 M $2cc5 M $d15c M $d15d M $d15e M -> $9e $2c $a0 $31 $a7 $88 }T
$cda9 >PC $ca >S $e3 >A $99 >X $03 >Y $2a >P $0031 $1b >M $0032 $09 >M $091e $67 >M $cda9 $31 >M $cdaa $31 >M $cdab $0e >M
T{ op PC S A X Y P -> $cdab $ca $63 $99 $03 $28 }T T{ $0031 M $0032 M $091e M $cda9 M $cdaa M $cdab M -> $1b $09 $67 $31 $31 $0e }T
$9420 >PC $31 >S $1a >A $09 >X $68 >Y $e1 >P $00ef $58 >M $00f0 $5c >M $5cc0 $f6 >M $9420 $31 >M $9421 $ef >M $9422 $49 >M
T{ op PC S A X Y P -> $9422 $31 $12 $09 $68 $61 }T T{ $00ef M $00f0 M $5cc0 M $9420 M $9421 M $9422 M -> $58 $5c $f6 $31 $ef $49 }T
$9d68 >PC $c4 >S $b7 >A $8f >X $40 >Y $23 >P $00d3 $f2 >M $00d4 $e4 >M $9d68 $31 >M $9d69 $d3 >M $9d6a $82 >M $e532 $21 >M
T{ op PC S A X Y P -> $9d6a $c4 $21 $8f $40 $21 }T T{ $00d3 M $00d4 M $9d68 M $9d69 M $9d6a M $e532 M -> $f2 $e4 $31 $d3 $82 $21 }T
$a8b6 >PC $e2 >S $81 >A $2d >X $a9 >Y $e0 >P $000b $33 >M $000c $55 >M $55dc $12 >M $a8b6 $31 >M $a8b7 $0b >M $a8b8 $1a >M
T{ op PC S A X Y P -> $a8b8 $e2 $00 $2d $a9 $62 }T T{ $000b M $000c M $55dc M $a8b6 M $a8b7 M $a8b8 M -> $33 $55 $12 $31 $0b $1a }T
$f742 >PC $0d >S $e2 >A $1c >X $d8 >Y $a7 >P $006a $c1 >M $006b $1c >M $1d99 $f9 >M $f742 $31 >M $f743 $6a >M $f744 $67 >M
T{ op PC S A X Y P -> $f744 $0d $e0 $1c $d8 $a5 }T T{ $006a M $006b M $1d99 M $f742 M $f743 M $f744 M -> $c1 $1c $f9 $31 $6a $67 }T
$ca9d >PC $ea >S $b8 >A $6c >X $e3 >Y $2d >P $00bd $86 >M $00be $11 >M $1269 $8d >M $ca9d $31 >M $ca9e $bd >M $ca9f $21 >M
T{ op PC S A X Y P -> $ca9f $ea $88 $6c $e3 $ad }T T{ $00bd M $00be M $1269 M $ca9d M $ca9e M $ca9f M -> $86 $11 $8d $31 $bd $21 }T
$09a7 >PC $0c >S $1e >A $97 >X $38 >Y $63 >P $0000 $61 >M $00ff $88 >M $09a7 $31 >M $09a8 $ff >M $09a9 $b4 >M $61c0 $75 >M
T{ op PC S A X Y P -> $09a9 $0c $14 $97 $38 $61 }T T{ $0000 M $00ff M $09a7 M $09a8 M $09a9 M $61c0 M -> $61 $88 $31 $ff $b4 $75 }T
$0e48 >PC $2a >S $15 >A $68 >X $52 >Y $e5 >P $00f6 $d5 >M $00f7 $d5 >M $0e48 $31 >M $0e49 $f6 >M $0e4a $6d >M $d627 $64 >M
T{ op PC S A X Y P -> $0e4a $2a $04 $68 $52 $65 }T T{ $00f6 M $00f7 M $0e48 M $0e49 M $0e4a M $d627 M -> $d5 $d5 $31 $f6 $6d $64 }T
$f995 >PC $69 >S $ab >A $56 >X $a4 >Y $e8 >P $00fa $1c >M $00fb $8d >M $8dc0 $e0 >M $f995 $31 >M $f996 $fa >M $f997 $2d >M
T{ op PC S A X Y P -> $f997 $69 $a0 $56 $a4 $e8 }T T{ $00fa M $00fb M $8dc0 M $f995 M $f996 M $f997 M -> $1c $8d $e0 $31 $fa $2d }T
$f795 >PC $23 >S $c7 >A $f9 >X $60 >Y $68 >P $0018 $f7 >M $0019 $dd >M $de57 $4a >M $f795 $31 >M $f796 $18 >M $f797 $c5 >M
T{ op PC S A X Y P -> $f797 $23 $42 $f9 $60 $68 }T T{ $0018 M $0019 M $de57 M $f795 M $f796 M $f797 M -> $f7 $dd $4a $31 $18 $c5 }T
$2124 >PC $e4 >S $43 >A $99 >X $17 >Y $ae >P $0047 $2c >M $0048 $a6 >M $2124 $31 >M $2125 $47 >M $2126 $29 >M $a643 $9a >M
T{ op PC S A X Y P -> $2126 $e4 $02 $99 $17 $2c }T T{ $0047 M $0048 M $2124 M $2125 M $2126 M $a643 M -> $2c $a6 $31 $47 $29 $9a }T
( 32 )
$f6d9 >PC $05 >S $08 >A $30 >X $5f >Y $a7 >P $001a $53 >M $001b $26 >M $2653 $cb >M $f6d9 $32 >M $f6da $1a >M $f6db $59 >M
T{ op PC S A X Y P -> $f6db $05 $08 $30 $5f $25 }T T{ $001a M $001b M $2653 M $f6d9 M $f6da M $f6db M -> $53 $26 $cb $32 $1a $59 }T
$0e50 >PC $68 >S $c1 >A $0c >X $c7 >Y $68 >P $00da $8a >M $00db $8a >M $0e50 $32 >M $0e51 $da >M $0e52 $2a >M $8a8a $88 >M
T{ op PC S A X Y P -> $0e52 $68 $80 $0c $c7 $e8 }T T{ $00da M $00db M $0e50 M $0e51 M $0e52 M $8a8a M -> $8a $8a $32 $da $2a $88 }T
$b04e >PC $97 >S $bc >A $d9 >X $b3 >Y $a4 >P $0052 $5d >M $0053 $18 >M $185d $e6 >M $b04e $32 >M $b04f $52 >M $b050 $72 >M
T{ op PC S A X Y P -> $b050 $97 $a4 $d9 $b3 $a4 }T T{ $0052 M $0053 M $185d M $b04e M $b04f M $b050 M -> $5d $18 $e6 $32 $52 $72 }T
$0931 >PC $44 >S $4e >A $87 >X $3d >Y $a7 >P $0064 $2e >M $0065 $47 >M $0931 $32 >M $0932 $64 >M $0933 $d6 >M $472e $a7 >M
T{ op PC S A X Y P -> $0933 $44 $06 $87 $3d $25 }T T{ $0064 M $0065 M $0931 M $0932 M $0933 M $472e M -> $2e $47 $32 $64 $d6 $a7 }T
$ffab >PC $2d >S $33 >A $fd >X $51 >Y $ab >P $000c $2e >M $000d $c8 >M $c82e $56 >M $ffab $32 >M $ffac $0c >M $ffad $c9 >M
T{ op PC S A X Y P -> $ffad $2d $12 $fd $51 $29 }T T{ $000c M $000d M $c82e M $ffab M $ffac M $ffad M -> $2e $c8 $56 $32 $0c $c9 }T
$5519 >PC $ad >S $52 >A $11 >X $8b >Y $e4 >P $0049 $d8 >M $004a $1b >M $1bd8 $42 >M $5519 $32 >M $551a $49 >M $551b $ec >M
T{ op PC S A X Y P -> $551b $ad $42 $11 $8b $64 }T T{ $0049 M $004a M $1bd8 M $5519 M $551a M $551b M -> $d8 $1b $42 $32 $49 $ec }T
$b5ec >PC $00 >S $3a >A $7a >X $c0 >Y $a1 >P $0003 $40 >M $0004 $83 >M $8340 $70 >M $b5ec $32 >M $b5ed $03 >M $b5ee $b9 >M
T{ op PC S A X Y P -> $b5ee $00 $30 $7a $c0 $21 }T T{ $0003 M $0004 M $8340 M $b5ec M $b5ed M $b5ee M -> $40 $83 $70 $32 $03 $b9 }T
$560a >PC $73 >S $9a >A $01 >X $6a >Y $ed >P $00bf $7e >M $00c0 $b3 >M $560a $32 >M $560b $bf >M $560c $ec >M $b37e $02 >M
T{ op PC S A X Y P -> $560c $73 $02 $01 $6a $6d }T T{ $00bf M $00c0 M $560a M $560b M $560c M $b37e M -> $7e $b3 $32 $bf $ec $02 }T
$2525 >PC $e5 >S $50 >A $2c >X $ee >Y $68 >P $00b2 $99 >M $00b3 $b8 >M $2525 $32 >M $2526 $b2 >M $2527 $1f >M $b899 $6c >M
T{ op PC S A X Y P -> $2527 $e5 $40 $2c $ee $68 }T T{ $00b2 M $00b3 M $2525 M $2526 M $2527 M $b899 M -> $99 $b8 $32 $b2 $1f $6c }T
$c0b3 >PC $ca >S $79 >A $c9 >X $28 >Y $af >P $0098 $39 >M $0099 $3b >M $3b39 $5d >M $c0b3 $32 >M $c0b4 $98 >M $c0b5 $63 >M
T{ op PC S A X Y P -> $c0b5 $ca $59 $c9 $28 $2d }T T{ $0098 M $0099 M $3b39 M $c0b3 M $c0b4 M $c0b5 M -> $39 $3b $5d $32 $98 $63 }T
$f236 >PC $9f >S $4b >A $f2 >X $30 >Y $a6 >P $0052 $ba >M $0053 $b1 >M $b1ba $66 >M $f236 $32 >M $f237 $52 >M $f238 $63 >M
T{ op PC S A X Y P -> $f238 $9f $42 $f2 $30 $24 }T T{ $0052 M $0053 M $b1ba M $f236 M $f237 M $f238 M -> $ba $b1 $66 $32 $52 $63 }T
$b287 >PC $4e >S $6b >A $2c >X $25 >Y $a0 >P $0051 $1f >M $0052 $e6 >M $b287 $32 >M $b288 $51 >M $b289 $c6 >M $e61f $9f >M
T{ op PC S A X Y P -> $b289 $4e $0b $2c $25 $20 }T T{ $0051 M $0052 M $b287 M $b288 M $b289 M $e61f M -> $1f $e6 $32 $51 $c6 $9f }T
$d9d5 >PC $3b >S $e7 >A $82 >X $48 >Y $ad >P $00a4 $e3 >M $00a5 $40 >M $40e3 $81 >M $d9d5 $32 >M $d9d6 $a4 >M $d9d7 $66 >M
T{ op PC S A X Y P -> $d9d7 $3b $81 $82 $48 $ad }T T{ $00a4 M $00a5 M $40e3 M $d9d5 M $d9d6 M $d9d7 M -> $e3 $40 $81 $32 $a4 $66 }T
$641a >PC $ff >S $d2 >A $29 >X $1d >Y $22 >P $0036 $83 >M $0037 $f5 >M $641a $32 >M $641b $36 >M $641c $61 >M $f583 $23 >M
T{ op PC S A X Y P -> $641c $ff $02 $29 $1d $20 }T T{ $0036 M $0037 M $641a M $641b M $641c M $f583 M -> $83 $f5 $32 $36 $61 $23 }T
$2140 >PC $96 >S $59 >A $84 >X $d1 >Y $a8 >P $0001 $2d >M $0002 $d2 >M $2140 $32 >M $2141 $01 >M $2142 $c4 >M $d22d $ea >M
T{ op PC S A X Y P -> $2142 $96 $48 $84 $d1 $28 }T T{ $0001 M $0002 M $2140 M $2141 M $2142 M $d22d M -> $2d $d2 $32 $01 $c4 $ea }T
$2915 >PC $87 >S $1a >A $bc >X $ba >Y $ec >P $00e6 $c5 >M $00e7 $df >M $2915 $32 >M $2916 $e6 >M $2917 $4e >M $dfc5 $4e >M
T{ op PC S A X Y P -> $2917 $87 $0a $bc $ba $6c }T T{ $00e6 M $00e7 M $2915 M $2916 M $2917 M $dfc5 M -> $c5 $df $32 $e6 $4e $4e }T
( 33 )
$6eed >PC $24 >S $47 >A $5a >X $8a >Y $ea >P $6eed $33 >M $6eee $50 >M $6eef $dc >M
T{ op PC S A X Y P -> $6eee $24 $47 $5a $8a $ea }T T{ $6eed M $6eee M $6eef M -> $33 $50 $dc }T
$59d0 >PC $95 >S $9e >A $e2 >X $64 >Y $61 >P $59d0 $33 >M $59d1 $ae >M $59d2 $f1 >M
T{ op PC S A X Y P -> $59d1 $95 $9e $e2 $64 $61 }T T{ $59d0 M $59d1 M $59d2 M -> $33 $ae $f1 }T
$b3a5 >PC $96 >S $1c >A $97 >X $5b >Y $69 >P $b3a5 $33 >M $b3a6 $2f >M $b3a7 $5f >M
T{ op PC S A X Y P -> $b3a6 $96 $1c $97 $5b $69 }T T{ $b3a5 M $b3a6 M $b3a7 M -> $33 $2f $5f }T
$1145 >PC $fb >S $7c >A $fe >X $b9 >Y $6c >P $1145 $33 >M $1146 $af >M $1147 $10 >M
T{ op PC S A X Y P -> $1146 $fb $7c $fe $b9 $6c }T T{ $1145 M $1146 M $1147 M -> $33 $af $10 }T
$7dab >PC $11 >S $d5 >A $09 >X $db >Y $a0 >P $7dab $33 >M $7dac $c0 >M $7dad $45 >M
T{ op PC S A X Y P -> $7dac $11 $d5 $09 $db $a0 }T T{ $7dab M $7dac M $7dad M -> $33 $c0 $45 }T
$bcd4 >PC $41 >S $d1 >A $7b >X $50 >Y $20 >P $bcd4 $33 >M $bcd5 $f3 >M $bcd6 $f0 >M
T{ op PC S A X Y P -> $bcd5 $41 $d1 $7b $50 $20 }T T{ $bcd4 M $bcd5 M $bcd6 M -> $33 $f3 $f0 }T
$2efb >PC $6b >S $88 >A $95 >X $17 >Y $2b >P $2efb $33 >M $2efc $f9 >M $2efd $01 >M
T{ op PC S A X Y P -> $2efc $6b $88 $95 $17 $2b }T T{ $2efb M $2efc M $2efd M -> $33 $f9 $01 }T
$20e6 >PC $52 >S $0b >A $06 >X $6f >Y $a9 >P $20e6 $33 >M $20e7 $0c >M $20e8 $ed >M
T{ op PC S A X Y P -> $20e7 $52 $0b $06 $6f $a9 }T T{ $20e6 M $20e7 M $20e8 M -> $33 $0c $ed }T
$4bba >PC $9e >S $ed >A $e6 >X $86 >Y $e9 >P $4bba $33 >M $4bbb $38 >M $4bbc $ca >M
T{ op PC S A X Y P -> $4bbb $9e $ed $e6 $86 $e9 }T T{ $4bba M $4bbb M $4bbc M -> $33 $38 $ca }T
$6254 >PC $96 >S $d4 >A $1f >X $2f >Y $ef >P $6254 $33 >M $6255 $9b >M $6256 $fd >M
T{ op PC S A X Y P -> $6255 $96 $d4 $1f $2f $ef }T T{ $6254 M $6255 M $6256 M -> $33 $9b $fd }T
$98b6 >PC $2a >S $0b >A $3b >X $09 >Y $66 >P $98b6 $33 >M $98b7 $f9 >M $98b8 $64 >M
T{ op PC S A X Y P -> $98b7 $2a $0b $3b $09 $66 }T T{ $98b6 M $98b7 M $98b8 M -> $33 $f9 $64 }T
$be0f >PC $c0 >S $76 >A $59 >X $25 >Y $2c >P $be0f $33 >M $be10 $ae >M $be11 $df >M
T{ op PC S A X Y P -> $be10 $c0 $76 $59 $25 $2c }T T{ $be0f M $be10 M $be11 M -> $33 $ae $df }T
$327b >PC $5b >S $34 >A $6f >X $7b >Y $aa >P $327b $33 >M $327c $bf >M $327d $21 >M
T{ op PC S A X Y P -> $327c $5b $34 $6f $7b $aa }T T{ $327b M $327c M $327d M -> $33 $bf $21 }T
$1f7e >PC $c5 >S $d9 >A $1c >X $25 >Y $6c >P $1f7e $33 >M $1f7f $fc >M $1f80 $02 >M
T{ op PC S A X Y P -> $1f7f $c5 $d9 $1c $25 $6c }T T{ $1f7e M $1f7f M $1f80 M -> $33 $fc $02 }T
$db48 >PC $2a >S $26 >A $3d >X $c4 >Y $63 >P $db48 $33 >M $db49 $2b >M $db4a $ab >M
T{ op PC S A X Y P -> $db49 $2a $26 $3d $c4 $63 }T T{ $db48 M $db49 M $db4a M -> $33 $2b $ab }T
$5957 >PC $56 >S $10 >A $ba >X $4f >Y $6e >P $5957 $33 >M $5958 $c5 >M $5959 $ce >M
T{ op PC S A X Y P -> $5958 $56 $10 $ba $4f $6e }T T{ $5957 M $5958 M $5959 M -> $33 $c5 $ce }T
( 34 )
$3dee >PC $34 >S $81 >A $e2 >X $60 >Y $aa >P $0032 $ba >M $0050 $80 >M $3dee $34 >M $3def $50 >M $3df0 $70 >M
T{ op PC S A X Y P -> $3df0 $34 $81 $e2 $60 $a8 }T T{ $0032 M $0050 M $3dee M $3def M $3df0 M -> $ba $80 $34 $50 $70 }T
$c6d4 >PC $a5 >S $cb >A $20 >X $e3 >Y $a6 >P $001f $1d >M $00ff $1f >M $c6d4 $34 >M $c6d5 $ff >M $c6d6 $57 >M
T{ op PC S A X Y P -> $c6d6 $a5 $cb $20 $e3 $24 }T T{ $001f M $00ff M $c6d4 M $c6d5 M $c6d6 M -> $1d $1f $34 $ff $57 }T
$855e >PC $81 >S $6f >A $f1 >X $3a >Y $e3 >P $00c8 $24 >M $00d7 $07 >M $855e $34 >M $855f $d7 >M $8560 $00 >M
T{ op PC S A X Y P -> $8560 $81 $6f $f1 $3a $21 }T T{ $00c8 M $00d7 M $855e M $855f M $8560 M -> $24 $07 $34 $d7 $00 }T
$c1e2 >PC $22 >S $c1 >A $8e >X $ba >Y $ac >P $0007 $95 >M $0095 $5f >M $c1e2 $34 >M $c1e3 $07 >M $c1e4 $bb >M
T{ op PC S A X Y P -> $c1e4 $22 $c1 $8e $ba $6c }T T{ $0007 M $0095 M $c1e2 M $c1e3 M $c1e4 M -> $95 $5f $34 $07 $bb }T
$7a70 >PC $3e >S $ce >A $53 >X $4c >Y $a2 >P $0060 $12 >M $00b3 $b7 >M $7a70 $34 >M $7a71 $60 >M $7a72 $27 >M
T{ op PC S A X Y P -> $7a72 $3e $ce $53 $4c $a0 }T T{ $0060 M $00b3 M $7a70 M $7a71 M $7a72 M -> $12 $b7 $34 $60 $27 }T
$aca3 >PC $d7 >S $41 >A $7d >X $91 >Y $6a >P $0073 $b9 >M $00f6 $de >M $aca3 $34 >M $aca4 $f6 >M $aca5 $94 >M
T{ op PC S A X Y P -> $aca5 $d7 $41 $7d $91 $a8 }T T{ $0073 M $00f6 M $aca3 M $aca4 M $aca5 M -> $b9 $de $34 $f6 $94 }T
$4124 >PC $83 >S $79 >A $37 >X $e8 >Y $e7 >P $005d $d9 >M $0094 $40 >M $4124 $34 >M $4125 $5d >M $4126 $5c >M
T{ op PC S A X Y P -> $4126 $83 $79 $37 $e8 $65 }T T{ $005d M $0094 M $4124 M $4125 M $4126 M -> $d9 $40 $34 $5d $5c }T
$99ef >PC $12 >S $c3 >A $2a >X $c2 >Y $a7 >P $0077 $fa >M $00a1 $23 >M $99ef $34 >M $99f0 $77 >M $99f1 $92 >M
T{ op PC S A X Y P -> $99f1 $12 $c3 $2a $c2 $25 }T T{ $0077 M $00a1 M $99ef M $99f0 M $99f1 M -> $fa $23 $34 $77 $92 }T
$029b >PC $63 >S $2a >A $29 >X $11 >Y $63 >P $0079 $69 >M $00a2 $70 >M $029b $34 >M $029c $79 >M $029d $8e >M
T{ op PC S A X Y P -> $029d $63 $2a $29 $11 $61 }T T{ $0079 M $00a2 M $029b M $029c M $029d M -> $69 $70 $34 $79 $8e }T
$f89f >PC $b6 >S $dc >A $40 >X $f1 >Y $6e >P $003b $3e >M $007b $c4 >M $f89f $34 >M $f8a0 $3b >M $f8a1 $09 >M
T{ op PC S A X Y P -> $f8a1 $b6 $dc $40 $f1 $ec }T T{ $003b M $007b M $f89f M $f8a0 M $f8a1 M -> $3e $c4 $34 $3b $09 }T
$1e91 >PC $ed >S $ec >A $ee >X $3f >Y $ef >P $002b $08 >M $003d $64 >M $1e91 $34 >M $1e92 $3d >M $1e93 $b4 >M
T{ op PC S A X Y P -> $1e93 $ed $ec $ee $3f $2d }T T{ $002b M $003d M $1e91 M $1e92 M $1e93 M -> $08 $64 $34 $3d $b4 }T
$4d67 >PC $d1 >S $93 >A $e9 >X $74 >Y $a2 >P $00de $a5 >M $00f5 $07 >M $4d67 $34 >M $4d68 $f5 >M $4d69 $62 >M
T{ op PC S A X Y P -> $4d69 $d1 $93 $e9 $74 $a0 }T T{ $00de M $00f5 M $4d67 M $4d68 M $4d69 M -> $a5 $07 $34 $f5 $62 }T
$80e7 >PC $a2 >S $34 >A $cb >X $e5 >Y $29 >P $0031 $8d >M $0066 $f2 >M $80e7 $34 >M $80e8 $66 >M $80e9 $af >M
T{ op PC S A X Y P -> $80e9 $a2 $34 $cb $e5 $a9 }T T{ $0031 M $0066 M $80e7 M $80e8 M $80e9 M -> $8d $f2 $34 $66 $af }T
$239e >PC $88 >S $8b >A $cf >X $0f >Y $6d >P $0009 $ca >M $00d8 $98 >M $239e $34 >M $239f $09 >M $23a0 $fc >M
T{ op PC S A X Y P -> $23a0 $88 $8b $cf $0f $ad }T T{ $0009 M $00d8 M $239e M $239f M $23a0 M -> $ca $98 $34 $09 $fc }T
$723b >PC $4f >S $20 >A $5b >X $0f >Y $eb >P $0021 $07 >M $007c $d6 >M $723b $34 >M $723c $21 >M $723d $ad >M
T{ op PC S A X Y P -> $723d $4f $20 $5b $0f $eb }T T{ $0021 M $007c M $723b M $723c M $723d M -> $07 $d6 $34 $21 $ad }T
$aeb7 >PC $4f >S $7b >A $b5 >X $89 >Y $6c >P $009a $69 >M $00e5 $04 >M $aeb7 $34 >M $aeb8 $e5 >M $aeb9 $e9 >M
T{ op PC S A X Y P -> $aeb9 $4f $7b $b5 $89 $6c }T T{ $009a M $00e5 M $aeb7 M $aeb8 M $aeb9 M -> $69 $04 $34 $e5 $e9 }T
( 35 )
$2461 >PC $c3 >S $34 >A $02 >X $f0 >Y $e4 >P $00d7 $44 >M $00d9 $95 >M $2461 $35 >M $2462 $d7 >M $2463 $ed >M
T{ op PC S A X Y P -> $2463 $c3 $14 $02 $f0 $64 }T T{ $00d7 M $00d9 M $2461 M $2462 M $2463 M -> $44 $95 $35 $d7 $ed }T
$226e >PC $f8 >S $6a >A $95 >X $d6 >Y $2f >P $0036 $41 >M $00cb $db >M $226e $35 >M $226f $36 >M $2270 $12 >M
T{ op PC S A X Y P -> $2270 $f8 $4a $95 $d6 $2d }T T{ $0036 M $00cb M $226e M $226f M $2270 M -> $41 $db $35 $36 $12 }T
$36f7 >PC $5c >S $e3 >A $56 >X $cf >Y $a2 >P $0026 $22 >M $007c $a6 >M $36f7 $35 >M $36f8 $26 >M $36f9 $87 >M
T{ op PC S A X Y P -> $36f9 $5c $a2 $56 $cf $a0 }T T{ $0026 M $007c M $36f7 M $36f8 M $36f9 M -> $22 $a6 $35 $26 $87 }T
$ae9a >PC $ac >S $b0 >A $83 >X $f1 >Y $2d >P $003f $24 >M $00c2 $73 >M $ae9a $35 >M $ae9b $3f >M $ae9c $00 >M
T{ op PC S A X Y P -> $ae9c $ac $30 $83 $f1 $2d }T T{ $003f M $00c2 M $ae9a M $ae9b M $ae9c M -> $24 $73 $35 $3f $00 }T
$9bfc >PC $27 >S $ff >A $7a >X $71 >Y $a0 >P $007e $f0 >M $00f8 $22 >M $9bfc $35 >M $9bfd $7e >M $9bfe $59 >M
T{ op PC S A X Y P -> $9bfe $27 $22 $7a $71 $20 }T T{ $007e M $00f8 M $9bfc M $9bfd M $9bfe M -> $f0 $22 $35 $7e $59 }T
$5e80 >PC $a5 >S $82 >A $cc >X $6b >Y $66 >P $0060 $af >M $0094 $93 >M $5e80 $35 >M $5e81 $94 >M $5e82 $8b >M
T{ op PC S A X Y P -> $5e82 $a5 $82 $cc $6b $e4 }T T{ $0060 M $0094 M $5e80 M $5e81 M $5e82 M -> $af $93 $35 $94 $8b }T
$2353 >PC $b6 >S $9e >A $22 >X $e5 >Y $64 >P $0023 $42 >M $0045 $24 >M $2353 $35 >M $2354 $23 >M $2355 $a6 >M
T{ op PC S A X Y P -> $2355 $b6 $04 $22 $e5 $64 }T T{ $0023 M $0045 M $2353 M $2354 M $2355 M -> $42 $24 $35 $23 $a6 }T
$0a48 >PC $20 >S $d3 >A $a3 >X $ad >Y $65 >P $0007 $0b >M $0064 $23 >M $0a48 $35 >M $0a49 $64 >M $0a4a $e4 >M
T{ op PC S A X Y P -> $0a4a $20 $03 $a3 $ad $65 }T T{ $0007 M $0064 M $0a48 M $0a49 M $0a4a M -> $0b $23 $35 $64 $e4 }T
$6e8c >PC $f5 >S $60 >A $23 >X $78 >Y $20 >P $0057 $aa >M $007a $f3 >M $6e8c $35 >M $6e8d $57 >M $6e8e $3a >M
T{ op PC S A X Y P -> $6e8e $f5 $60 $23 $78 $20 }T T{ $0057 M $007a M $6e8c M $6e8d M $6e8e M -> $aa $f3 $35 $57 $3a }T
$4d52 >PC $35 >S $fe >A $81 >X $85 >Y $23 >P $0035 $76 >M $00b6 $72 >M $4d52 $35 >M $4d53 $35 >M $4d54 $85 >M
T{ op PC S A X Y P -> $4d54 $35 $72 $81 $85 $21 }T T{ $0035 M $00b6 M $4d52 M $4d53 M $4d54 M -> $76 $72 $35 $35 $85 }T
$e359 >PC $cf >S $2e >A $66 >X $43 >Y $6c >P $002e $16 >M $00c8 $bb >M $e359 $35 >M $e35a $c8 >M $e35b $22 >M
T{ op PC S A X Y P -> $e35b $cf $06 $66 $43 $6c }T T{ $002e M $00c8 M $e359 M $e35a M $e35b M -> $16 $bb $35 $c8 $22 }T
$ec6c >PC $72 >S $e5 >A $ec >X $2e >Y $6e >P $007c $83 >M $0090 $54 >M $ec6c $35 >M $ec6d $90 >M $ec6e $b2 >M
T{ op PC S A X Y P -> $ec6e $72 $81 $ec $2e $ec }T T{ $007c M $0090 M $ec6c M $ec6d M $ec6e M -> $83 $54 $35 $90 $b2 }T
$25e6 >PC $a4 >S $58 >A $c1 >X $69 >Y $e2 >P $0039 $f7 >M $0078 $8c >M $25e6 $35 >M $25e7 $78 >M $25e8 $94 >M
T{ op PC S A X Y P -> $25e8 $a4 $50 $c1 $69 $60 }T T{ $0039 M $0078 M $25e6 M $25e7 M $25e8 M -> $f7 $8c $35 $78 $94 }T
$93e3 >PC $d9 >S $0b >A $d2 >X $1c >Y $e5 >P $0068 $a1 >M $0096 $82 >M $93e3 $35 >M $93e4 $96 >M $93e5 $f9 >M
T{ op PC S A X Y P -> $93e5 $d9 $01 $d2 $1c $65 }T T{ $0068 M $0096 M $93e3 M $93e4 M $93e5 M -> $a1 $82 $35 $96 $f9 }T
$2320 >PC $cd >S $33 >A $0b >X $b8 >Y $2f >P $00bf $ef >M $00ca $71 >M $2320 $35 >M $2321 $bf >M $2322 $51 >M
T{ op PC S A X Y P -> $2322 $cd $31 $0b $b8 $2d }T T{ $00bf M $00ca M $2320 M $2321 M $2322 M -> $ef $71 $35 $bf $51 }T
$105c >PC $0a >S $26 >A $ba >X $3c >Y $e7 >P $0084 $8a >M $00ca $83 >M $105c $35 >M $105d $ca >M $105e $79 >M
T{ op PC S A X Y P -> $105e $0a $02 $ba $3c $65 }T T{ $0084 M $00ca M $105c M $105d M $105e M -> $8a $83 $35 $ca $79 }T
( 36 )
$f10e >PC $8e >S $ae >A $76 >X $dd >Y $aa >P $007c $73 >M $00f2 $19 >M $f10e $36 >M $f10f $7c >M $f110 $e3 >M
T{ op PC S A X Y P -> $f110 $8e $ae $76 $dd $28 }T T{ $007c M $00f2 M $f10e M $f10f M $f110 M -> $73 $32 $36 $7c $e3 }T
$a2ea >PC $66 >S $41 >A $22 >X $35 >Y $e4 >P $00c4 $b2 >M $00e6 $a0 >M $a2ea $36 >M $a2eb $c4 >M $a2ec $cc >M
T{ op PC S A X Y P -> $a2ec $66 $41 $22 $35 $65 }T T{ $00c4 M $00e6 M $a2ea M $a2eb M $a2ec M -> $b2 $40 $36 $c4 $cc }T
$3a31 >PC $66 >S $65 >A $58 >X $f7 >Y $62 >P $0072 $76 >M $00ca $b2 >M $3a31 $36 >M $3a32 $72 >M $3a33 $1a >M
T{ op PC S A X Y P -> $3a33 $66 $65 $58 $f7 $61 }T T{ $0072 M $00ca M $3a31 M $3a32 M $3a33 M -> $76 $64 $36 $72 $1a }T
$175e >PC $64 >S $e8 >A $0d >X $54 >Y $20 >P $000a $ec >M $0017 $a2 >M $175e $36 >M $175f $0a >M $1760 $7c >M
T{ op PC S A X Y P -> $1760 $64 $e8 $0d $54 $21 }T T{ $000a M $0017 M $175e M $175f M $1760 M -> $ec $44 $36 $0a $7c }T
$47f9 >PC $a1 >S $e1 >A $2c >X $95 >Y $28 >P $008c $a1 >M $00b8 $8c >M $47f9 $36 >M $47fa $8c >M $47fb $24 >M
T{ op PC S A X Y P -> $47fb $a1 $e1 $2c $95 $29 }T T{ $008c M $00b8 M $47f9 M $47fa M $47fb M -> $a1 $18 $36 $8c $24 }T
$23f6 >PC $41 >S $1a >A $1e >X $a5 >Y $a7 >P $0057 $df >M $0075 $b9 >M $23f6 $36 >M $23f7 $57 >M $23f8 $5d >M
T{ op PC S A X Y P -> $23f8 $41 $1a $1e $a5 $25 }T T{ $0057 M $0075 M $23f6 M $23f7 M $23f8 M -> $df $73 $36 $57 $5d }T
$3394 >PC $c7 >S $be >A $ca >X $94 >Y $a2 >P $0043 $0a >M $0079 $a5 >M $3394 $36 >M $3395 $79 >M $3396 $6b >M
T{ op PC S A X Y P -> $3396 $c7 $be $ca $94 $20 }T T{ $0043 M $0079 M $3394 M $3395 M $3396 M -> $14 $a5 $36 $79 $6b }T
$d1fd >PC $7f >S $1c >A $04 >X $30 >Y $6e >P $004e $e4 >M $0052 $c0 >M $d1fd $36 >M $d1fe $4e >M $d1ff $68 >M
T{ op PC S A X Y P -> $d1ff $7f $1c $04 $30 $ed }T T{ $004e M $0052 M $d1fd M $d1fe M $d1ff M -> $e4 $80 $36 $4e $68 }T
$6afd >PC $5a >S $cb >A $0b >X $f4 >Y $6b >P $004b $a4 >M $0056 $e4 >M $6afd $36 >M $6afe $4b >M $6aff $47 >M
T{ op PC S A X Y P -> $6aff $5a $cb $0b $f4 $e9 }T T{ $004b M $0056 M $6afd M $6afe M $6aff M -> $a4 $c9 $36 $4b $47 }T
$fd08 >PC $46 >S $69 >A $ab >X $e9 >Y $23 >P $0072 $1b >M $00c7 $2e >M $fd08 $36 >M $fd09 $c7 >M $fd0a $31 >M
T{ op PC S A X Y P -> $fd0a $46 $69 $ab $e9 $20 }T T{ $0072 M $00c7 M $fd08 M $fd09 M $fd0a M -> $37 $2e $36 $c7 $31 }T
$d487 >PC $d9 >S $23 >A $f9 >X $da >Y $66 >P $00d0 $82 >M $00d7 $8d >M $d487 $36 >M $d488 $d7 >M $d489 $3f >M
T{ op PC S A X Y P -> $d489 $d9 $23 $f9 $da $65 }T T{ $00d0 M $00d7 M $d487 M $d488 M $d489 M -> $04 $8d $36 $d7 $3f }T
$e9e3 >PC $cd >S $3e >A $6f >X $51 >Y $6e >P $001d $7a >M $008c $f4 >M $e9e3 $36 >M $e9e4 $1d >M $e9e5 $18 >M
T{ op PC S A X Y P -> $e9e5 $cd $3e $6f $51 $ed }T T{ $001d M $008c M $e9e3 M $e9e4 M $e9e5 M -> $7a $e8 $36 $1d $18 }T
$264c >PC $df >S $fa >A $ba >X $0d >Y $2b >P $0016 $41 >M $00d0 $28 >M $264c $36 >M $264d $16 >M $264e $47 >M
T{ op PC S A X Y P -> $264e $df $fa $ba $0d $28 }T T{ $0016 M $00d0 M $264c M $264d M $264e M -> $41 $51 $36 $16 $47 }T
$a195 >PC $82 >S $dd >A $a9 >X $5e >Y $24 >P $0029 $96 >M $0080 $c5 >M $a195 $36 >M $a196 $80 >M $a197 $66 >M
T{ op PC S A X Y P -> $a197 $82 $dd $a9 $5e $25 }T T{ $0029 M $0080 M $a195 M $a196 M $a197 M -> $2c $c5 $36 $80 $66 }T
$ba66 >PC $59 >S $12 >A $00 >X $f6 >Y $a7 >P $006d $23 >M $ba66 $36 >M $ba67 $6d >M $ba68 $ee >M
T{ op PC S A X Y P -> $ba68 $59 $12 $00 $f6 $24 }T T{ $006d M $ba66 M $ba67 M $ba68 M -> $47 $36 $6d $ee }T
$a34d >PC $06 >S $d3 >A $b8 >X $3e >Y $a6 >P $009b $ec >M $00e3 $45 >M $a34d $36 >M $a34e $e3 >M $a34f $30 >M
T{ op PC S A X Y P -> $a34f $06 $d3 $b8 $3e $a5 }T T{ $009b M $00e3 M $a34d M $a34e M $a34f M -> $d8 $45 $36 $e3 $30 }T
( 37 )
$e156 >PC $b1 >S $cd >A $61 >X $ff >Y $60 >P $00b7 $2a >M $e156 $37 >M $e157 $b7 >M $e158 $67 >M
T{ op PC S A X Y P -> $e158 $b1 $cd $61 $ff $60 }T T{ $00b7 M $e156 M $e157 M $e158 M -> $22 $37 $b7 $67 }T
$642f >PC $9d >S $d3 >A $c9 >X $ba >Y $67 >P $0046 $56 >M $642f $37 >M $6430 $46 >M $6431 $ef >M
T{ op PC S A X Y P -> $6431 $9d $d3 $c9 $ba $67 }T T{ $0046 M $642f M $6430 M $6431 M -> $56 $37 $46 $ef }T
$0fbb >PC $f9 >S $b3 >A $2a >X $b6 >Y $af >P $0026 $3f >M $0fbb $37 >M $0fbc $26 >M $0fbd $4e >M
T{ op PC S A X Y P -> $0fbd $f9 $b3 $2a $b6 $af }T T{ $0026 M $0fbb M $0fbc M $0fbd M -> $37 $37 $26 $4e }T
$5f62 >PC $89 >S $a1 >A $88 >X $44 >Y $6e >P $005e $64 >M $5f62 $37 >M $5f63 $5e >M $5f64 $9a >M
T{ op PC S A X Y P -> $5f64 $89 $a1 $88 $44 $6e }T T{ $005e M $5f62 M $5f63 M $5f64 M -> $64 $37 $5e $9a }T
$2db4 >PC $e5 >S $19 >A $25 >X $2d >Y $23 >P $0000 $25 >M $2db4 $37 >M $2db5 $00 >M $2db6 $59 >M
T{ op PC S A X Y P -> $2db6 $e5 $19 $25 $2d $23 }T T{ $0000 M $2db4 M $2db5 M $2db6 M -> $25 $37 $00 $59 }T
$70f2 >PC $b6 >S $cd >A $66 >X $33 >Y $a4 >P $0048 $ea >M $70f2 $37 >M $70f3 $48 >M $70f4 $a6 >M
T{ op PC S A X Y P -> $70f4 $b6 $cd $66 $33 $a4 }T T{ $0048 M $70f2 M $70f3 M $70f4 M -> $e2 $37 $48 $a6 }T
$cd82 >PC $17 >S $ed >A $46 >X $69 >Y $af >P $0038 $8f >M $cd82 $37 >M $cd83 $38 >M $cd84 $18 >M
T{ op PC S A X Y P -> $cd84 $17 $ed $46 $69 $af }T T{ $0038 M $cd82 M $cd83 M $cd84 M -> $87 $37 $38 $18 }T
$3ff2 >PC $6e >S $de >A $3a >X $fc >Y $e3 >P $0023 $dc >M $3ff2 $37 >M $3ff3 $23 >M $3ff4 $2a >M
T{ op PC S A X Y P -> $3ff4 $6e $de $3a $fc $e3 }T T{ $0023 M $3ff2 M $3ff3 M $3ff4 M -> $d4 $37 $23 $2a }T
$be2a >PC $e2 >S $37 >A $7d >X $ed >Y $a8 >P $001a $a4 >M $be2a $37 >M $be2b $1a >M $be2c $af >M
T{ op PC S A X Y P -> $be2c $e2 $37 $7d $ed $a8 }T T{ $001a M $be2a M $be2b M $be2c M -> $a4 $37 $1a $af }T
$355b >PC $ef >S $09 >A $ce >X $8e >Y $26 >P $00b1 $f3 >M $355b $37 >M $355c $b1 >M $355d $4c >M
T{ op PC S A X Y P -> $355d $ef $09 $ce $8e $26 }T T{ $00b1 M $355b M $355c M $355d M -> $f3 $37 $b1 $4c }T
$3b70 >PC $aa >S $00 >A $02 >X $1c >Y $63 >P $00ca $f7 >M $3b70 $37 >M $3b71 $ca >M $3b72 $8f >M
T{ op PC S A X Y P -> $3b72 $aa $00 $02 $1c $63 }T T{ $00ca M $3b70 M $3b71 M $3b72 M -> $f7 $37 $ca $8f }T
$969c >PC $cb >S $05 >A $60 >X $66 >Y $21 >P $00bc $f1 >M $969c $37 >M $969d $bc >M $969e $72 >M
T{ op PC S A X Y P -> $969e $cb $05 $60 $66 $21 }T T{ $00bc M $969c M $969d M $969e M -> $f1 $37 $bc $72 }T
$1fb5 >PC $b1 >S $41 >A $9e >X $03 >Y $26 >P $0038 $d4 >M $1fb5 $37 >M $1fb6 $38 >M $1fb7 $91 >M
T{ op PC S A X Y P -> $1fb7 $b1 $41 $9e $03 $26 }T T{ $0038 M $1fb5 M $1fb6 M $1fb7 M -> $d4 $37 $38 $91 }T
$8902 >PC $87 >S $81 >A $2e >X $3a >Y $ec >P $00e1 $4b >M $8902 $37 >M $8903 $e1 >M $8904 $e5 >M
T{ op PC S A X Y P -> $8904 $87 $81 $2e $3a $ec }T T{ $00e1 M $8902 M $8903 M $8904 M -> $43 $37 $e1 $e5 }T
$2f0d >PC $d1 >S $8e >A $45 >X $37 >Y $69 >P $0009 $3a >M $2f0d $37 >M $2f0e $09 >M $2f0f $7c >M
T{ op PC S A X Y P -> $2f0f $d1 $8e $45 $37 $69 }T T{ $0009 M $2f0d M $2f0e M $2f0f M -> $32 $37 $09 $7c }T
$e46a >PC $08 >S $ac >A $75 >X $22 >Y $2e >P $000a $44 >M $e46a $37 >M $e46b $0a >M $e46c $54 >M
T{ op PC S A X Y P -> $e46c $08 $ac $75 $22 $2e }T T{ $000a M $e46a M $e46b M $e46c M -> $44 $37 $0a $54 }T
( 38 )
$8f58 >PC $87 >S $39 >A $d3 >X $8a >Y $6d >P $8f58 $38 >M $8f59 $52 >M $8f5a $5f >M
T{ op PC S A X Y P -> $8f59 $87 $39 $d3 $8a $6d }T T{ $8f58 M $8f59 M $8f5a M -> $38 $52 $5f }T
$5524 >PC $56 >S $14 >A $b6 >X $a8 >Y $ab >P $5524 $38 >M $5525 $ab >M $5526 $67 >M
T{ op PC S A X Y P -> $5525 $56 $14 $b6 $a8 $ab }T T{ $5524 M $5525 M $5526 M -> $38 $ab $67 }T
$6ab0 >PC $ac >S $a4 >A $a9 >X $bc >Y $e0 >P $6ab0 $38 >M $6ab1 $46 >M $6ab2 $4a >M
T{ op PC S A X Y P -> $6ab1 $ac $a4 $a9 $bc $e1 }T T{ $6ab0 M $6ab1 M $6ab2 M -> $38 $46 $4a }T
$9145 >PC $34 >S $d0 >A $2f >X $50 >Y $6c >P $9145 $38 >M $9146 $6e >M $9147 $d7 >M
T{ op PC S A X Y P -> $9146 $34 $d0 $2f $50 $6d }T T{ $9145 M $9146 M $9147 M -> $38 $6e $d7 }T
$a6fb >PC $1a >S $43 >A $31 >X $7d >Y $ea >P $a6fb $38 >M $a6fc $d2 >M $a6fd $27 >M
T{ op PC S A X Y P -> $a6fc $1a $43 $31 $7d $eb }T T{ $a6fb M $a6fc M $a6fd M -> $38 $d2 $27 }T
$1f69 >PC $8a >S $bd >A $26 >X $8c >Y $2d >P $1f69 $38 >M $1f6a $bc >M $1f6b $79 >M
T{ op PC S A X Y P -> $1f6a $8a $bd $26 $8c $2d }T T{ $1f69 M $1f6a M $1f6b M -> $38 $bc $79 }T
$404b >PC $d2 >S $2c >A $00 >X $c0 >Y $aa >P $404b $38 >M $404c $9a >M $404d $49 >M
T{ op PC S A X Y P -> $404c $d2 $2c $00 $c0 $ab }T T{ $404b M $404c M $404d M -> $38 $9a $49 }T
$324b >PC $f3 >S $8f >A $90 >X $a9 >Y $2c >P $324b $38 >M $324c $ec >M $324d $0e >M
T{ op PC S A X Y P -> $324c $f3 $8f $90 $a9 $2d }T T{ $324b M $324c M $324d M -> $38 $ec $0e }T
$94e4 >PC $4e >S $ca >A $48 >X $97 >Y $ac >P $94e4 $38 >M $94e5 $69 >M $94e6 $62 >M
T{ op PC S A X Y P -> $94e5 $4e $ca $48 $97 $ad }T T{ $94e4 M $94e5 M $94e6 M -> $38 $69 $62 }T
$80f8 >PC $1e >S $13 >A $f8 >X $cc >Y $a3 >P $80f8 $38 >M $80f9 $2b >M $80fa $5d >M
T{ op PC S A X Y P -> $80f9 $1e $13 $f8 $cc $a3 }T T{ $80f8 M $80f9 M $80fa M -> $38 $2b $5d }T
$3afe >PC $ab >S $2f >A $1c >X $df >Y $64 >P $3afe $38 >M $3aff $be >M $3b00 $10 >M
T{ op PC S A X Y P -> $3aff $ab $2f $1c $df $65 }T T{ $3afe M $3aff M $3b00 M -> $38 $be $10 }T
$d256 >PC $29 >S $e8 >A $01 >X $d7 >Y $eb >P $d256 $38 >M $d257 $bf >M $d258 $09 >M
T{ op PC S A X Y P -> $d257 $29 $e8 $01 $d7 $eb }T T{ $d256 M $d257 M $d258 M -> $38 $bf $09 }T
$a862 >PC $fa >S $8a >A $39 >X $8f >Y $23 >P $a862 $38 >M $a863 $fc >M $a864 $1a >M
T{ op PC S A X Y P -> $a863 $fa $8a $39 $8f $23 }T T{ $a862 M $a863 M $a864 M -> $38 $fc $1a }T
$0fad >PC $20 >S $5a >A $1a >X $87 >Y $22 >P $0fad $38 >M $0fae $4a >M $0faf $7c >M
T{ op PC S A X Y P -> $0fae $20 $5a $1a $87 $23 }T T{ $0fad M $0fae M $0faf M -> $38 $4a $7c }T
$f34a >PC $2f >S $6f >A $55 >X $47 >Y $2b >P $f34a $38 >M $f34b $7b >M $f34c $11 >M
T{ op PC S A X Y P -> $f34b $2f $6f $55 $47 $2b }T T{ $f34a M $f34b M $f34c M -> $38 $7b $11 }T
$c9b1 >PC $8d >S $cf >A $cd >X $0c >Y $e2 >P $c9b1 $38 >M $c9b2 $2e >M $c9b3 $be >M
T{ op PC S A X Y P -> $c9b2 $8d $cf $cd $0c $e3 }T T{ $c9b1 M $c9b2 M $c9b3 M -> $38 $2e $be }T
( 39 )
$a069 >PC $f6 >S $5a >A $a2 >X $65 >Y $a8 >P $a069 $39 >M $a06a $73 >M $a06b $f6 >M $a06c $9c >M $f6d8 $b7 >M
T{ op PC S A X Y P -> $a06c $f6 $12 $a2 $65 $28 }T T{ $a069 M $a06a M $a06b M $a06c M $f6d8 M -> $39 $73 $f6 $9c $b7 }T
$5be0 >PC $14 >S $ca >A $39 >X $7e >Y $2d >P $3ecd $ae >M $5be0 $39 >M $5be1 $4f >M $5be2 $3e >M $5be3 $43 >M
T{ op PC S A X Y P -> $5be3 $14 $8a $39 $7e $ad }T T{ $3ecd M $5be0 M $5be1 M $5be2 M $5be3 M -> $ae $39 $4f $3e $43 }T
$433a >PC $ce >S $7c >A $8c >X $13 >Y $2a >P $433a $39 >M $433b $1e >M $433c $e5 >M $433d $bc >M $e531 $64 >M
T{ op PC S A X Y P -> $433d $ce $64 $8c $13 $28 }T T{ $433a M $433b M $433c M $433d M $e531 M -> $39 $1e $e5 $bc $64 }T
$1eb3 >PC $e0 >S $b3 >A $7e >X $e6 >Y $e1 >P $1eb3 $39 >M $1eb4 $05 >M $1eb5 $d5 >M $1eb6 $2d >M $d5eb $6b >M
T{ op PC S A X Y P -> $1eb6 $e0 $23 $7e $e6 $61 }T T{ $1eb3 M $1eb4 M $1eb5 M $1eb6 M $d5eb M -> $39 $05 $d5 $2d $6b }T
$17ab >PC $1d >S $9c >A $31 >X $f5 >Y $e7 >P $17ab $39 >M $17ac $af >M $17ad $b4 >M $17ae $50 >M $b5a4 $88 >M
T{ op PC S A X Y P -> $17ae $1d $88 $31 $f5 $e5 }T T{ $17ab M $17ac M $17ad M $17ae M $b5a4 M -> $39 $af $b4 $50 $88 }T
$5758 >PC $c3 >S $31 >A $16 >X $66 >Y $21 >P $5758 $39 >M $5759 $11 >M $575a $ca >M $575b $f5 >M $ca77 $ed >M
T{ op PC S A X Y P -> $575b $c3 $21 $16 $66 $21 }T T{ $5758 M $5759 M $575a M $575b M $ca77 M -> $39 $11 $ca $f5 $ed }T
$b86d >PC $e4 >S $a1 >A $c3 >X $20 >Y $26 >P $64a3 $80 >M $b86d $39 >M $b86e $83 >M $b86f $64 >M $b870 $dc >M
T{ op PC S A X Y P -> $b870 $e4 $80 $c3 $20 $a4 }T T{ $64a3 M $b86d M $b86e M $b86f M $b870 M -> $80 $39 $83 $64 $dc }T
$2b1c >PC $dd >S $e4 >A $a2 >X $8e >Y $e7 >P $2b1c $39 >M $2b1d $b3 >M $2b1e $c2 >M $2b1f $3e >M $c341 $ec >M
T{ op PC S A X Y P -> $2b1f $dd $e4 $a2 $8e $e5 }T T{ $2b1c M $2b1d M $2b1e M $2b1f M $c341 M -> $39 $b3 $c2 $3e $ec }T
$1772 >PC $10 >S $4d >A $85 >X $fc >Y $21 >P $069d $64 >M $1772 $39 >M $1773 $a1 >M $1774 $05 >M $1775 $37 >M
T{ op PC S A X Y P -> $1775 $10 $44 $85 $fc $21 }T T{ $069d M $1772 M $1773 M $1774 M $1775 M -> $64 $39 $a1 $05 $37 }T
$0cf5 >PC $ba >S $db >A $aa >X $13 >Y $eb >P $0cf5 $39 >M $0cf6 $e7 >M $0cf7 $5a >M $0cf8 $28 >M $5afa $10 >M
T{ op PC S A X Y P -> $0cf8 $ba $10 $aa $13 $69 }T T{ $0cf5 M $0cf6 M $0cf7 M $0cf8 M $5afa M -> $39 $e7 $5a $28 $10 }T
$d08b >PC $1c >S $8c >A $75 >X $82 >Y $64 >P $952e $2b >M $d08b $39 >M $d08c $ac >M $d08d $94 >M $d08e $70 >M
T{ op PC S A X Y P -> $d08e $1c $08 $75 $82 $64 }T T{ $952e M $d08b M $d08c M $d08d M $d08e M -> $2b $39 $ac $94 $70 }T
$47af >PC $e0 >S $fd >A $8c >X $66 >Y $28 >P $47af $39 >M $47b0 $f6 >M $47b1 $50 >M $47b2 $50 >M $515c $81 >M
T{ op PC S A X Y P -> $47b2 $e0 $81 $8c $66 $a8 }T T{ $47af M $47b0 M $47b1 M $47b2 M $515c M -> $39 $f6 $50 $50 $81 }T
$e985 >PC $c2 >S $7a >A $25 >X $18 >Y $a2 >P $94c6 $fd >M $e985 $39 >M $e986 $ae >M $e987 $94 >M $e988 $c8 >M
T{ op PC S A X Y P -> $e988 $c2 $78 $25 $18 $20 }T T{ $94c6 M $e985 M $e986 M $e987 M $e988 M -> $fd $39 $ae $94 $c8 }T
$797c >PC $b7 >S $bc >A $17 >X $dc >Y $65 >P $58de $b8 >M $797c $39 >M $797d $02 >M $797e $58 >M $797f $cd >M
T{ op PC S A X Y P -> $797f $b7 $b8 $17 $dc $e5 }T T{ $58de M $797c M $797d M $797e M $797f M -> $b8 $39 $02 $58 $cd }T
$f040 >PC $c9 >S $b6 >A $60 >X $49 >Y $ad >P $ef7a $cf >M $f040 $39 >M $f041 $31 >M $f042 $ef >M $f043 $9f >M
T{ op PC S A X Y P -> $f043 $c9 $86 $60 $49 $ad }T T{ $ef7a M $f040 M $f041 M $f042 M $f043 M -> $cf $39 $31 $ef $9f }T
$407b >PC $eb >S $33 >A $a0 >X $51 >Y $22 >P $407b $39 >M $407c $1f >M $407d $b1 >M $407e $dd >M $b170 $54 >M
T{ op PC S A X Y P -> $407e $eb $10 $a0 $51 $20 }T T{ $407b M $407c M $407d M $407e M $b170 M -> $39 $1f $b1 $dd $54 }T
( 3a )
$e8ff >PC $7a >S $24 >A $f2 >X $fc >Y $2c >P $e8ff $3a >M $e900 $25 >M $e901 $c8 >M
T{ op PC S A X Y P -> $e900 $7a $23 $f2 $fc $2c }T T{ $e8ff M $e900 M $e901 M -> $3a $25 $c8 }T
$e77d >PC $f4 >S $3b >A $47 >X $b6 >Y $a7 >P $e77d $3a >M $e77e $e1 >M $e77f $01 >M
T{ op PC S A X Y P -> $e77e $f4 $3a $47 $b6 $25 }T T{ $e77d M $e77e M $e77f M -> $3a $e1 $01 }T
$2a55 >PC $47 >S $7b >A $f1 >X $96 >Y $e6 >P $2a55 $3a >M $2a56 $10 >M $2a57 $c2 >M
T{ op PC S A X Y P -> $2a56 $47 $7a $f1 $96 $64 }T T{ $2a55 M $2a56 M $2a57 M -> $3a $10 $c2 }T
$f623 >PC $55 >S $77 >A $81 >X $e3 >Y $a2 >P $f623 $3a >M $f624 $d6 >M $f625 $cb >M
T{ op PC S A X Y P -> $f624 $55 $76 $81 $e3 $20 }T T{ $f623 M $f624 M $f625 M -> $3a $d6 $cb }T
$23c8 >PC $b9 >S $16 >A $90 >X $ac >Y $69 >P $23c8 $3a >M $23c9 $4b >M $23ca $fd >M
T{ op PC S A X Y P -> $23c9 $b9 $15 $90 $ac $69 }T T{ $23c8 M $23c9 M $23ca M -> $3a $4b $fd }T
$c076 >PC $01 >S $0b >A $39 >X $09 >Y $ee >P $c076 $3a >M $c077 $c8 >M $c078 $bd >M
T{ op PC S A X Y P -> $c077 $01 $0a $39 $09 $6c }T T{ $c076 M $c077 M $c078 M -> $3a $c8 $bd }T
$1658 >PC $24 >S $23 >A $2f >X $56 >Y $2a >P $1658 $3a >M $1659 $12 >M $165a $45 >M
T{ op PC S A X Y P -> $1659 $24 $22 $2f $56 $28 }T T{ $1658 M $1659 M $165a M -> $3a $12 $45 }T
$adc9 >PC $c9 >S $65 >A $9c >X $10 >Y $a3 >P $adc9 $3a >M $adca $f5 >M $adcb $dd >M
T{ op PC S A X Y P -> $adca $c9 $64 $9c $10 $21 }T T{ $adc9 M $adca M $adcb M -> $3a $f5 $dd }T
$5f65 >PC $07 >S $ad >A $45 >X $af >Y $ab >P $5f65 $3a >M $5f66 $da >M $5f67 $66 >M
T{ op PC S A X Y P -> $5f66 $07 $ac $45 $af $a9 }T T{ $5f65 M $5f66 M $5f67 M -> $3a $da $66 }T
$3674 >PC $95 >S $fb >A $af >X $e9 >Y $a5 >P $3674 $3a >M $3675 $c0 >M $3676 $34 >M
T{ op PC S A X Y P -> $3675 $95 $fa $af $e9 $a5 }T T{ $3674 M $3675 M $3676 M -> $3a $c0 $34 }T
$16c4 >PC $03 >S $5b >A $90 >X $11 >Y $e6 >P $16c4 $3a >M $16c5 $0e >M $16c6 $a6 >M
T{ op PC S A X Y P -> $16c5 $03 $5a $90 $11 $64 }T T{ $16c4 M $16c5 M $16c6 M -> $3a $0e $a6 }T
$a51f >PC $a4 >S $3d >A $8f >X $ad >Y $6b >P $a51f $3a >M $a520 $a1 >M $a521 $07 >M
T{ op PC S A X Y P -> $a520 $a4 $3c $8f $ad $69 }T T{ $a51f M $a520 M $a521 M -> $3a $a1 $07 }T
$57d3 >PC $3b >S $e4 >A $43 >X $64 >Y $ef >P $57d3 $3a >M $57d4 $9f >M $57d5 $2e >M
T{ op PC S A X Y P -> $57d4 $3b $e3 $43 $64 $ed }T T{ $57d3 M $57d4 M $57d5 M -> $3a $9f $2e }T
$7842 >PC $66 >S $4d >A $fe >X $49 >Y $a7 >P $7842 $3a >M $7843 $3e >M $7844 $ea >M
T{ op PC S A X Y P -> $7843 $66 $4c $fe $49 $25 }T T{ $7842 M $7843 M $7844 M -> $3a $3e $ea }T
$3588 >PC $57 >S $29 >A $69 >X $f7 >Y $e8 >P $3588 $3a >M $3589 $ec >M $358a $11 >M
T{ op PC S A X Y P -> $3589 $57 $28 $69 $f7 $68 }T T{ $3588 M $3589 M $358a M -> $3a $ec $11 }T
$fc2c >PC $fb >S $08 >A $a9 >X $89 >Y $e8 >P $fc2c $3a >M $fc2d $5c >M $fc2e $c3 >M
T{ op PC S A X Y P -> $fc2d $fb $07 $a9 $89 $68 }T T{ $fc2c M $fc2d M $fc2e M -> $3a $5c $c3 }T
( 3b )
$a871 >PC $10 >S $36 >A $17 >X $c0 >Y $af >P $a871 $3b >M $a872 $88 >M $a873 $fa >M
T{ op PC S A X Y P -> $a872 $10 $36 $17 $c0 $af }T T{ $a871 M $a872 M $a873 M -> $3b $88 $fa }T
$0b52 >PC $ff >S $64 >A $08 >X $07 >Y $21 >P $0b52 $3b >M $0b53 $db >M $0b54 $d3 >M
T{ op PC S A X Y P -> $0b53 $ff $64 $08 $07 $21 }T T{ $0b52 M $0b53 M $0b54 M -> $3b $db $d3 }T
$06a1 >PC $5b >S $9d >A $a6 >X $67 >Y $6e >P $06a1 $3b >M $06a2 $d6 >M $06a3 $a4 >M
T{ op PC S A X Y P -> $06a2 $5b $9d $a6 $67 $6e }T T{ $06a1 M $06a2 M $06a3 M -> $3b $d6 $a4 }T
$b6c3 >PC $57 >S $b3 >A $8c >X $13 >Y $ab >P $b6c3 $3b >M $b6c4 $58 >M $b6c5 $b0 >M
T{ op PC S A X Y P -> $b6c4 $57 $b3 $8c $13 $ab }T T{ $b6c3 M $b6c4 M $b6c5 M -> $3b $58 $b0 }T
$261b >PC $a2 >S $16 >A $d7 >X $e2 >Y $ac >P $261b $3b >M $261c $a5 >M $261d $70 >M
T{ op PC S A X Y P -> $261c $a2 $16 $d7 $e2 $ac }T T{ $261b M $261c M $261d M -> $3b $a5 $70 }T
$0818 >PC $1f >S $1f >A $20 >X $7e >Y $e4 >P $0818 $3b >M $0819 $51 >M $081a $34 >M
T{ op PC S A X Y P -> $0819 $1f $1f $20 $7e $e4 }T T{ $0818 M $0819 M $081a M -> $3b $51 $34 }T
$55b8 >PC $d1 >S $9a >A $02 >X $69 >Y $67 >P $55b8 $3b >M $55b9 $b9 >M $55ba $c5 >M
T{ op PC S A X Y P -> $55b9 $d1 $9a $02 $69 $67 }T T{ $55b8 M $55b9 M $55ba M -> $3b $b9 $c5 }T
$23f4 >PC $5d >S $4c >A $ea >X $41 >Y $a9 >P $23f4 $3b >M $23f5 $0f >M $23f6 $d4 >M
T{ op PC S A X Y P -> $23f5 $5d $4c $ea $41 $a9 }T T{ $23f4 M $23f5 M $23f6 M -> $3b $0f $d4 }T
$4cef >PC $a8 >S $04 >A $19 >X $3b >Y $64 >P $4cef $3b >M $4cf0 $31 >M $4cf1 $57 >M
T{ op PC S A X Y P -> $4cf0 $a8 $04 $19 $3b $64 }T T{ $4cef M $4cf0 M $4cf1 M -> $3b $31 $57 }T
$0a43 >PC $8d >S $34 >A $5b >X $be >Y $6a >P $0a43 $3b >M $0a44 $54 >M $0a45 $2c >M
T{ op PC S A X Y P -> $0a44 $8d $34 $5b $be $6a }T T{ $0a43 M $0a44 M $0a45 M -> $3b $54 $2c }T
$4fa4 >PC $07 >S $75 >A $6b >X $c9 >Y $a0 >P $4fa4 $3b >M $4fa5 $fc >M $4fa6 $9d >M
T{ op PC S A X Y P -> $4fa5 $07 $75 $6b $c9 $a0 }T T{ $4fa4 M $4fa5 M $4fa6 M -> $3b $fc $9d }T
$618e >PC $b5 >S $62 >A $17 >X $4e >Y $2e >P $618e $3b >M $618f $e3 >M $6190 $08 >M
T{ op PC S A X Y P -> $618f $b5 $62 $17 $4e $2e }T T{ $618e M $618f M $6190 M -> $3b $e3 $08 }T
$b12d >PC $9c >S $2b >A $21 >X $7a >Y $e4 >P $b12d $3b >M $b12e $06 >M $b12f $2b >M
T{ op PC S A X Y P -> $b12e $9c $2b $21 $7a $e4 }T T{ $b12d M $b12e M $b12f M -> $3b $06 $2b }T
$d71d >PC $f4 >S $f4 >A $63 >X $92 >Y $e2 >P $d71d $3b >M $d71e $77 >M $d71f $19 >M
T{ op PC S A X Y P -> $d71e $f4 $f4 $63 $92 $e2 }T T{ $d71d M $d71e M $d71f M -> $3b $77 $19 }T
$a29c >PC $9a >S $6e >A $f1 >X $c2 >Y $a5 >P $a29c $3b >M $a29d $d4 >M $a29e $e7 >M
T{ op PC S A X Y P -> $a29d $9a $6e $f1 $c2 $a5 }T T{ $a29c M $a29d M $a29e M -> $3b $d4 $e7 }T
$8c7e >PC $e4 >S $65 >A $78 >X $a0 >Y $24 >P $8c7e $3b >M $8c7f $13 >M $8c80 $2a >M
T{ op PC S A X Y P -> $8c7f $e4 $65 $78 $a0 $24 }T T{ $8c7e M $8c7f M $8c80 M -> $3b $13 $2a }T
( 3c )
$dce9 >PC $02 >S $a5 >A $b2 >X $3d >Y $a2 >P $555d $ea >M $dce9 $3c >M $dcea $ab >M $dceb $54 >M $dcec $dc >M
T{ op PC S A X Y P -> $dcec $02 $a5 $b2 $3d $e0 }T T{ $555d M $dce9 M $dcea M $dceb M $dcec M -> $ea $3c $ab $54 $dc }T
$eaf9 >PC $c3 >S $72 >A $07 >X $a4 >Y $66 >P $9734 $f0 >M $eaf9 $3c >M $eafa $2d >M $eafb $97 >M $eafc $a6 >M
T{ op PC S A X Y P -> $eafc $c3 $72 $07 $a4 $e4 }T T{ $9734 M $eaf9 M $eafa M $eafb M $eafc M -> $f0 $3c $2d $97 $a6 }T
$5165 >PC $22 >S $68 >A $f4 >X $44 >Y $a3 >P $236e $e7 >M $5165 $3c >M $5166 $7a >M $5167 $22 >M $5168 $ae >M
T{ op PC S A X Y P -> $5168 $22 $68 $f4 $44 $e1 }T T{ $236e M $5165 M $5166 M $5167 M $5168 M -> $e7 $3c $7a $22 $ae }T
$5acf >PC $64 >S $4b >A $7a >X $bf >Y $ad >P $1acc $ef >M $5acf $3c >M $5ad0 $52 >M $5ad1 $1a >M $5ad2 $2a >M
T{ op PC S A X Y P -> $5ad2 $64 $4b $7a $bf $ed }T T{ $1acc M $5acf M $5ad0 M $5ad1 M $5ad2 M -> $ef $3c $52 $1a $2a }T
$3da9 >PC $de >S $d0 >A $0a >X $34 >Y $60 >P $3da9 $3c >M $3daa $dd >M $3dab $7d >M $3dac $0a >M $7de7 $34 >M
T{ op PC S A X Y P -> $3dac $de $d0 $0a $34 $20 }T T{ $3da9 M $3daa M $3dab M $3dac M $7de7 M -> $3c $dd $7d $0a $34 }T
$66db >PC $82 >S $92 >A $66 >X $90 >Y $ad >P $251b $20 >M $66db $3c >M $66dc $b5 >M $66dd $24 >M $66de $3f >M
T{ op PC S A X Y P -> $66de $82 $92 $66 $90 $2f }T T{ $251b M $66db M $66dc M $66dd M $66de M -> $20 $3c $b5 $24 $3f }T
$4668 >PC $68 >S $ca >A $5e >X $5e >Y $a5 >P $43c8 $81 >M $4668 $3c >M $4669 $6a >M $466a $43 >M $466b $e7 >M
T{ op PC S A X Y P -> $466b $68 $ca $5e $5e $a5 }T T{ $43c8 M $4668 M $4669 M $466a M $466b M -> $81 $3c $6a $43 $e7 }T
$0d65 >PC $ef >S $6a >A $c9 >X $34 >Y $e8 >P $0d65 $3c >M $0d66 $70 >M $0d67 $9e >M $0d68 $27 >M $9f39 $75 >M
T{ op PC S A X Y P -> $0d68 $ef $6a $c9 $34 $68 }T T{ $0d65 M $0d66 M $0d67 M $0d68 M $9f39 M -> $3c $70 $9e $27 $75 }T
$9a29 >PC $18 >S $fa >A $51 >X $a2 >Y $62 >P $9a29 $3c >M $9a2a $a9 >M $9a2b $e5 >M $9a2c $34 >M $e5fa $12 >M
T{ op PC S A X Y P -> $9a2c $18 $fa $51 $a2 $20 }T T{ $9a29 M $9a2a M $9a2b M $9a2c M $e5fa M -> $3c $a9 $e5 $34 $12 }T
$0616 >PC $f8 >S $23 >A $02 >X $4a >Y $a3 >P $0616 $3c >M $0617 $dd >M $0618 $3c >M $0619 $d2 >M $3cdf $6f >M
T{ op PC S A X Y P -> $0619 $f8 $23 $02 $4a $61 }T T{ $0616 M $0617 M $0618 M $0619 M $3cdf M -> $3c $dd $3c $d2 $6f }T
$f135 >PC $57 >S $39 >A $f1 >X $57 >Y $a0 >P $943f $d8 >M $f135 $3c >M $f136 $4e >M $f137 $93 >M $f138 $56 >M
T{ op PC S A X Y P -> $f138 $57 $39 $f1 $57 $e0 }T T{ $943f M $f135 M $f136 M $f137 M $f138 M -> $d8 $3c $4e $93 $56 }T
$8761 >PC $5e >S $b2 >A $90 >X $27 >Y $2e >P $037b $73 >M $8761 $3c >M $8762 $eb >M $8763 $02 >M $8764 $72 >M
T{ op PC S A X Y P -> $8764 $5e $b2 $90 $27 $6c }T T{ $037b M $8761 M $8762 M $8763 M $8764 M -> $73 $3c $eb $02 $72 }T
$a6a7 >PC $03 >S $76 >A $05 >X $b8 >Y $a4 >P $6d17 $32 >M $a6a7 $3c >M $a6a8 $12 >M $a6a9 $6d >M $a6aa $02 >M
T{ op PC S A X Y P -> $a6aa $03 $76 $05 $b8 $24 }T T{ $6d17 M $a6a7 M $a6a8 M $a6a9 M $a6aa M -> $32 $3c $12 $6d $02 }T
$4a00 >PC $cf >S $80 >A $1b >X $5c >Y $e2 >P $4a00 $3c >M $4a01 $86 >M $4a02 $da >M $4a03 $81 >M $daa1 $9a >M
T{ op PC S A X Y P -> $4a03 $cf $80 $1b $5c $a0 }T T{ $4a00 M $4a01 M $4a02 M $4a03 M $daa1 M -> $3c $86 $da $81 $9a }T
$3eee >PC $14 >S $25 >A $71 >X $16 >Y $a0 >P $2b1a $50 >M $3eee $3c >M $3eef $a9 >M $3ef0 $2a >M $3ef1 $b0 >M
T{ op PC S A X Y P -> $3ef1 $14 $25 $71 $16 $62 }T T{ $2b1a M $3eee M $3eef M $3ef0 M $3ef1 M -> $50 $3c $a9 $2a $b0 }T
$ed42 >PC $86 >S $28 >A $a0 >X $c7 >Y $2c >P $70fa $7c >M $ed42 $3c >M $ed43 $5a >M $ed44 $70 >M $ed45 $31 >M
T{ op PC S A X Y P -> $ed45 $86 $28 $a0 $c7 $6c }T T{ $70fa M $ed42 M $ed43 M $ed44 M $ed45 M -> $7c $3c $5a $70 $31 }T
( 3d )
$9b34 >PC $74 >S $e9 >A $e3 >X $0c >Y $a8 >P $9b34 $3d >M $9b35 $d4 >M $9b36 $d0 >M $9b37 $cc >M $d1b7 $47 >M
T{ op PC S A X Y P -> $9b37 $74 $41 $e3 $0c $28 }T T{ $9b34 M $9b35 M $9b36 M $9b37 M $d1b7 M -> $3d $d4 $d0 $cc $47 }T
$de81 >PC $b2 >S $ba >A $7c >X $14 >Y $2b >P $389b $03 >M $de81 $3d >M $de82 $1f >M $de83 $38 >M $de84 $8a >M
T{ op PC S A X Y P -> $de84 $b2 $02 $7c $14 $29 }T T{ $389b M $de81 M $de82 M $de83 M $de84 M -> $03 $3d $1f $38 $8a }T
$7c13 >PC $5c >S $a5 >A $ea >X $fd >Y $6d >P $7c13 $3d >M $7c14 $2d >M $7c15 $d8 >M $7c16 $af >M $d917 $85 >M
T{ op PC S A X Y P -> $7c16 $5c $85 $ea $fd $ed }T T{ $7c13 M $7c14 M $7c15 M $7c16 M $d917 M -> $3d $2d $d8 $af $85 }T
$a578 >PC $51 >S $40 >A $50 >X $3e >Y $6c >P $6791 $cb >M $a578 $3d >M $a579 $41 >M $a57a $67 >M $a57b $52 >M
T{ op PC S A X Y P -> $a57b $51 $40 $50 $3e $6c }T T{ $6791 M $a578 M $a579 M $a57a M $a57b M -> $cb $3d $41 $67 $52 }T
$156b >PC $0b >S $21 >A $1a >X $08 >Y $aa >P $156b $3d >M $156c $1e >M $156d $43 >M $156e $ed >M $4338 $b9 >M
T{ op PC S A X Y P -> $156e $0b $21 $1a $08 $28 }T T{ $156b M $156c M $156d M $156e M $4338 M -> $3d $1e $43 $ed $b9 }T
$0747 >PC $d8 >S $76 >A $d0 >X $c3 >Y $6b >P $0747 $3d >M $0748 $34 >M $0749 $69 >M $074a $9f >M $6a04 $2d >M
T{ op PC S A X Y P -> $074a $d8 $24 $d0 $c3 $69 }T T{ $0747 M $0748 M $0749 M $074a M $6a04 M -> $3d $34 $69 $9f $2d }T
$25e0 >PC $50 >S $72 >A $cd >X $e4 >Y $e4 >P $25e0 $3d >M $25e1 $27 >M $25e2 $51 >M $25e3 $d3 >M $51f4 $15 >M
T{ op PC S A X Y P -> $25e3 $50 $10 $cd $e4 $64 }T T{ $25e0 M $25e1 M $25e2 M $25e3 M $51f4 M -> $3d $27 $51 $d3 $15 }T
$bffc >PC $45 >S $5e >A $52 >X $aa >Y $a7 >P $bffc $3d >M $bffd $d3 >M $bffe $bf >M $bfff $91 >M $c025 $a5 >M
T{ op PC S A X Y P -> $bfff $45 $04 $52 $aa $25 }T T{ $bffc M $bffd M $bffe M $bfff M $c025 M -> $3d $d3 $bf $91 $a5 }T
$946e >PC $57 >S $22 >A $ac >X $92 >Y $a0 >P $946e $3d >M $946f $c8 >M $9470 $e1 >M $9471 $ac >M $e274 $55 >M
T{ op PC S A X Y P -> $9471 $57 $00 $ac $92 $22 }T T{ $946e M $946f M $9470 M $9471 M $e274 M -> $3d $c8 $e1 $ac $55 }T
$cbcd >PC $e1 >S $64 >A $80 >X $e2 >Y $6e >P $0dc2 $c0 >M $cbcd $3d >M $cbce $42 >M $cbcf $0d >M $cbd0 $a5 >M
T{ op PC S A X Y P -> $cbd0 $e1 $40 $80 $e2 $6c }T T{ $0dc2 M $cbcd M $cbce M $cbcf M $cbd0 M -> $c0 $3d $42 $0d $a5 }T
$9640 >PC $88 >S $0c >A $5b >X $5d >Y $a0 >P $7e4e $02 >M $9640 $3d >M $9641 $f3 >M $9642 $7d >M $9643 $1c >M
T{ op PC S A X Y P -> $9643 $88 $00 $5b $5d $22 }T T{ $7e4e M $9640 M $9641 M $9642 M $9643 M -> $02 $3d $f3 $7d $1c }T
$c004 >PC $4d >S $ab >A $a6 >X $36 >Y $25 >P $653d $30 >M $c004 $3d >M $c005 $97 >M $c006 $64 >M $c007 $d4 >M
T{ op PC S A X Y P -> $c007 $4d $20 $a6 $36 $25 }T T{ $653d M $c004 M $c005 M $c006 M $c007 M -> $30 $3d $97 $64 $d4 }T
$97c6 >PC $68 >S $b9 >A $99 >X $43 >Y $23 >P $32db $fa >M $97c6 $3d >M $97c7 $42 >M $97c8 $32 >M $97c9 $43 >M
T{ op PC S A X Y P -> $97c9 $68 $b8 $99 $43 $a1 }T T{ $32db M $97c6 M $97c7 M $97c8 M $97c9 M -> $fa $3d $42 $32 $43 }T
$484a >PC $a6 >S $4f >A $d9 >X $4c >Y $6d >P $484a $3d >M $484b $31 >M $484c $d9 >M $484d $aa >M $da0a $a7 >M
T{ op PC S A X Y P -> $484d $a6 $07 $d9 $4c $6d }T T{ $484a M $484b M $484c M $484d M $da0a M -> $3d $31 $d9 $aa $a7 }T
$4127 >PC $ba >S $95 >A $b3 >X $75 >Y $a8 >P $4127 $3d >M $4128 $4e >M $4129 $a9 >M $412a $95 >M $aa01 $8c >M
T{ op PC S A X Y P -> $412a $ba $84 $b3 $75 $a8 }T T{ $4127 M $4128 M $4129 M $412a M $aa01 M -> $3d $4e $a9 $95 $8c }T
$19d6 >PC $d2 >S $07 >A $62 >X $0f >Y $a2 >P $19d6 $3d >M $19d7 $2d >M $19d8 $f3 >M $19d9 $9d >M $f38f $ce >M
T{ op PC S A X Y P -> $19d9 $d2 $06 $62 $0f $20 }T T{ $19d6 M $19d7 M $19d8 M $19d9 M $f38f M -> $3d $2d $f3 $9d $ce }T
( 3e )
$0055 >PC $57 >S $34 >A $d8 >X $05 >Y $a5 >P $0055 $3e >M $0056 $a1 >M $0057 $6b >M $0058 $be >M $6c79 $96 >M
T{ op PC S A X Y P -> $0058 $57 $34 $d8 $05 $25 }T T{ $0055 M $0056 M $0057 M $0058 M $6c79 M -> $3e $a1 $6b $be $2d }T
$2498 >PC $27 >S $d6 >A $9c >X $a8 >Y $6d >P $2498 $3e >M $2499 $fe >M $249a $ed >M $249b $f9 >M $ee9a $dc >M
T{ op PC S A X Y P -> $249b $27 $d6 $9c $a8 $ed }T T{ $2498 M $2499 M $249a M $249b M $ee9a M -> $3e $fe $ed $f9 $b9 }T
$8c1d >PC $b6 >S $a3 >A $a9 >X $7e >Y $a5 >P $8c1d $3e >M $8c1e $a5 >M $8c1f $cc >M $8c20 $42 >M $cd4e $85 >M
T{ op PC S A X Y P -> $8c20 $b6 $a3 $a9 $7e $25 }T T{ $8c1d M $8c1e M $8c1f M $8c20 M $cd4e M -> $3e $a5 $cc $42 $0b }T
$9b63 >PC $9d >S $a0 >A $3c >X $98 >Y $e3 >P $9b63 $3e >M $9b64 $a1 >M $9b65 $b8 >M $9b66 $d3 >M $b8dd $70 >M
T{ op PC S A X Y P -> $9b66 $9d $a0 $3c $98 $e0 }T T{ $9b63 M $9b64 M $9b65 M $9b66 M $b8dd M -> $3e $a1 $b8 $d3 $e1 }T
$40bd >PC $07 >S $10 >A $87 >X $30 >Y $2f >P $40bd $3e >M $40be $4c >M $40bf $b1 >M $40c0 $fe >M $b1d3 $c9 >M
T{ op PC S A X Y P -> $40c0 $07 $10 $87 $30 $ad }T T{ $40bd M $40be M $40bf M $40c0 M $b1d3 M -> $3e $4c $b1 $fe $93 }T
$3cbd >PC $82 >S $96 >A $15 >X $d9 >Y $64 >P $3cbd $3e >M $3cbe $69 >M $3cbf $a2 >M $3cc0 $7b >M $a27e $d1 >M
T{ op PC S A X Y P -> $3cc0 $82 $96 $15 $d9 $e5 }T T{ $3cbd M $3cbe M $3cbf M $3cc0 M $a27e M -> $3e $69 $a2 $7b $a2 }T
$3038 >PC $05 >S $03 >A $e4 >X $dd >Y $68 >P $3038 $3e >M $3039 $18 >M $303a $59 >M $303b $d0 >M $59fc $a3 >M
T{ op PC S A X Y P -> $303b $05 $03 $e4 $dd $69 }T T{ $3038 M $3039 M $303a M $303b M $59fc M -> $3e $18 $59 $d0 $46 }T
$d242 >PC $0e >S $81 >A $c9 >X $43 >Y $67 >P $d242 $3e >M $d243 $be >M $d244 $e9 >M $d245 $b3 >M $ea87 $4b >M
T{ op PC S A X Y P -> $d245 $0e $81 $c9 $43 $e4 }T T{ $d242 M $d243 M $d244 M $d245 M $ea87 M -> $3e $be $e9 $b3 $97 }T
$4aa8 >PC $4e >S $6c >A $84 >X $fb >Y $ec >P $4aa8 $3e >M $4aa9 $40 >M $4aaa $e7 >M $4aab $94 >M $e7c4 $40 >M
T{ op PC S A X Y P -> $4aab $4e $6c $84 $fb $ec }T T{ $4aa8 M $4aa9 M $4aaa M $4aab M $e7c4 M -> $3e $40 $e7 $94 $80 }T
$26be >PC $dc >S $7f >A $ee >X $49 >Y $af >P $26be $3e >M $26bf $88 >M $26c0 $43 >M $26c1 $57 >M $4476 $e4 >M
T{ op PC S A X Y P -> $26c1 $dc $7f $ee $49 $ad }T T{ $26be M $26bf M $26c0 M $26c1 M $4476 M -> $3e $88 $43 $57 $c9 }T
$40fe >PC $17 >S $37 >A $b0 >X $fd >Y $6f >P $40fe $3e >M $40ff $4b >M $4100 $c1 >M $4101 $be >M $c1fb $8b >M
T{ op PC S A X Y P -> $4101 $17 $37 $b0 $fd $6d }T T{ $40fe M $40ff M $4100 M $4101 M $c1fb M -> $3e $4b $c1 $be $17 }T
$4282 >PC $45 >S $ac >A $d2 >X $ca >Y $a9 >P $4282 $3e >M $4283 $79 >M $4284 $65 >M $4285 $b5 >M $664b $28 >M
T{ op PC S A X Y P -> $4285 $45 $ac $d2 $ca $28 }T T{ $4282 M $4283 M $4284 M $4285 M $664b M -> $3e $79 $65 $b5 $51 }T
$566a >PC $23 >S $f1 >A $98 >X $09 >Y $2c >P $566a $3e >M $566b $f2 >M $566c $70 >M $566d $33 >M $718a $13 >M
T{ op PC S A X Y P -> $566d $23 $f1 $98 $09 $2c }T T{ $566a M $566b M $566c M $566d M $718a M -> $3e $f2 $70 $33 $26 }T
$4bf5 >PC $7c >S $ef >A $85 >X $52 >Y $6b >P $4bf5 $3e >M $4bf6 $04 >M $4bf7 $7a >M $4bf8 $6f >M $7a89 $d9 >M
T{ op PC S A X Y P -> $4bf8 $7c $ef $85 $52 $e9 }T T{ $4bf5 M $4bf6 M $4bf7 M $4bf8 M $7a89 M -> $3e $04 $7a $6f $b3 }T
$8d78 >PC $74 >S $51 >A $5a >X $4e >Y $eb >P $18ab $0e >M $8d78 $3e >M $8d79 $51 >M $8d7a $18 >M $8d7b $b4 >M
T{ op PC S A X Y P -> $8d7b $74 $51 $5a $4e $68 }T T{ $18ab M $8d78 M $8d79 M $8d7a M $8d7b M -> $1d $3e $51 $18 $b4 }T
$f7aa >PC $2d >S $16 >A $5f >X $da >Y $67 >P $d2b3 $b7 >M $f7aa $3e >M $f7ab $54 >M $f7ac $d2 >M $f7ad $05 >M
T{ op PC S A X Y P -> $f7ad $2d $16 $5f $da $65 }T T{ $d2b3 M $f7aa M $f7ab M $f7ac M $f7ad M -> $6f $3e $54 $d2 $05 }T
( 3f )
$c75f >PC $2d >S $3f >A $b1 >X $c1 >Y $6e >P $00e1 $1e >M $c75f $3f >M $c760 $e1 >M $c761 $6b >M $c762 $e6 >M $c7cd $b0 >M
T{ op PC S A X Y P -> $c762 $2d $3f $b1 $c1 $6e }T T{ $00e1 M $c75f M $c760 M $c761 M $c762 M $c7cd M -> $1e $3f $e1 $6b $e6 $b0 }T
$69fd >PC $90 >S $58 >A $0e >X $c3 >Y $ae >P $00c9 $3d >M $69fd $3f >M $69fe $c9 >M $69ff $71 >M $6a00 $b5 >M $6a71 $2b >M
T{ op PC S A X Y P -> $6a00 $90 $58 $0e $c3 $ae }T T{ $00c9 M $69fd M $69fe M $69ff M $6a00 M $6a71 M -> $3d $3f $c9 $71 $b5 $2b }T
$6f7d >PC $e0 >S $f8 >A $6d >X $3b >Y $25 >P $0055 $2a >M $6f24 $b0 >M $6f7d $3f >M $6f7e $55 >M $6f7f $a4 >M $6f80 $fa >M
T{ op PC S A X Y P -> $6f80 $e0 $f8 $6d $3b $25 }T T{ $0055 M $6f24 M $6f7d M $6f7e M $6f7f M $6f80 M -> $2a $b0 $3f $55 $a4 $fa }T
$200f >PC $3b >S $68 >A $c5 >X $88 >Y $a2 >P $0049 $46 >M $200a $a9 >M $200f $3f >M $2010 $49 >M $2011 $f8 >M
T{ op PC S A X Y P -> $200a $3b $68 $c5 $88 $a2 }T T{ $0049 M $200a M $200f M $2010 M $2011 M -> $46 $a9 $3f $49 $f8 }T
$a505 >PC $37 >S $fa >A $4d >X $2c >Y $a4 >P $001a $cd >M $a505 $3f >M $a506 $1a >M $a507 $07 >M $a508 $2e >M $a50f $53 >M
T{ op PC S A X Y P -> $a508 $37 $fa $4d $2c $a4 }T T{ $001a M $a505 M $a506 M $a507 M $a508 M $a50f M -> $cd $3f $1a $07 $2e $53 }T
$e76d >PC $a8 >S $0e >A $11 >X $5c >Y $e5 >P $00d3 $94 >M $e712 $07 >M $e76d $3f >M $e76e $d3 >M $e76f $a2 >M
T{ op PC S A X Y P -> $e712 $a8 $0e $11 $5c $e5 }T T{ $00d3 M $e712 M $e76d M $e76e M $e76f M -> $94 $07 $3f $d3 $a2 }T
$542d >PC $f9 >S $28 >A $d2 >X $e5 >Y $67 >P $004e $8c >M $542d $3f >M $542e $4e >M $542f $59 >M $5430 $0c >M $5489 $62 >M
T{ op PC S A X Y P -> $5430 $f9 $28 $d2 $e5 $67 }T T{ $004e M $542d M $542e M $542f M $5430 M $5489 M -> $8c $3f $4e $59 $0c $62 }T
$d86a >PC $3d >S $e3 >A $4c >X $6d >Y $67 >P $006f $44 >M $d82b $8f >M $d86a $3f >M $d86b $6f >M $d86c $be >M
T{ op PC S A X Y P -> $d82b $3d $e3 $4c $6d $67 }T T{ $006f M $d82b M $d86a M $d86b M $d86c M -> $44 $8f $3f $6f $be }T
$b9ed >PC $df >S $69 >A $dc >X $91 >Y $65 >P $00be $96 >M $b93d $db >M $b9ed $3f >M $b9ee $be >M $b9ef $4d >M $ba3d $70 >M
T{ op PC S A X Y P -> $ba3d $df $69 $dc $91 $65 }T T{ $00be M $b93d M $b9ed M $b9ee M $b9ef M $ba3d M -> $96 $db $3f $be $4d $70 }T
$7a1a >PC $02 >S $2b >A $45 >X $a4 >Y $20 >P $00db $d7 >M $7a1a $3f >M $7a1b $db >M $7a1c $71 >M $7a8e $d1 >M
T{ op PC S A X Y P -> $7a8e $02 $2b $45 $a4 $20 }T T{ $00db M $7a1a M $7a1b M $7a1c M $7a8e M -> $d7 $3f $db $71 $d1 }T
$1f13 >PC $9b >S $65 >A $f4 >X $5d >Y $a4 >P $0037 $50 >M $1ed6 $ce >M $1f13 $3f >M $1f14 $37 >M $1f15 $c0 >M $1fd6 $20 >M
T{ op PC S A X Y P -> $1ed6 $9b $65 $f4 $5d $a4 }T T{ $0037 M $1ed6 M $1f13 M $1f14 M $1f15 M $1fd6 M -> $50 $ce $3f $37 $c0 $20 }T
$a9ff >PC $a0 >S $85 >A $2f >X $67 >Y $ef >P $0032 $8a >M $a9ff $3f >M $aa00 $32 >M $aa01 $ad >M $aa02 $25 >M $aaaf $41 >M
T{ op PC S A X Y P -> $aa02 $a0 $85 $2f $67 $ef }T T{ $0032 M $a9ff M $aa00 M $aa01 M $aa02 M $aaaf M -> $8a $3f $32 $ad $25 $41 }T
$f4db >PC $e8 >S $ca >A $6c >X $9f >Y $65 >P $00ef $e4 >M $f4c8 $9d >M $f4db $3f >M $f4dc $ef >M $f4dd $ea >M
T{ op PC S A X Y P -> $f4c8 $e8 $ca $6c $9f $65 }T T{ $00ef M $f4c8 M $f4db M $f4dc M $f4dd M -> $e4 $9d $3f $ef $ea }T
$8e50 >PC $54 >S $9a >A $47 >X $1c >Y $e5 >P $0077 $f6 >M $8de3 $5b >M $8e50 $3f >M $8e51 $77 >M $8e52 $90 >M $8ee3 $ac >M
T{ op PC S A X Y P -> $8de3 $54 $9a $47 $1c $e5 }T T{ $0077 M $8de3 M $8e50 M $8e51 M $8e52 M $8ee3 M -> $f6 $5b $3f $77 $90 $ac }T
$cfe7 >PC $ed >S $d2 >A $fd >X $82 >Y $21 >P $00ad $c5 >M $cfb6 $aa >M $cfe7 $3f >M $cfe8 $ad >M $cfe9 $cc >M
T{ op PC S A X Y P -> $cfb6 $ed $d2 $fd $82 $21 }T T{ $00ad M $cfb6 M $cfe7 M $cfe8 M $cfe9 M -> $c5 $aa $3f $ad $cc }T
$958d >PC $1b >S $3d >A $36 >X $4f >Y $af >P $0056 $f8 >M $958c $b3 >M $958d $3f >M $958e $56 >M $958f $fc >M $9590 $8e >M
T{ op PC S A X Y P -> $9590 $1b $3d $36 $4f $af }T T{ $0056 M $958c M $958d M $958e M $958f M $9590 M -> $f8 $b3 $3f $56 $fc $8e }T
( 40 )
$3b45 >PC $0a >S $26 >A $f3 >X $04 >Y $aa >P $010a $84 >M $010b $43 >M $010c $35 >M $010d $14 >M $1435 $fe >M $3b45 $40 >M $3b46 $3d >M $3b47 $9c >M
T{ op PC S A X Y P -> $1435 $0d $26 $f3 $04 $63 }T T{ $010a M $010b M $010c M $010d M $1435 M $3b45 M $3b46 M $3b47 M -> $84 $43 $35 $14 $fe $40 $3d $9c }T
$a0d0 >PC $90 >S $31 >A $6b >X $67 >Y $a8 >P $0190 $d0 >M $0191 $04 >M $0192 $40 >M $0193 $8a >M $8a40 $a5 >M $a0d0 $40 >M $a0d1 $f3 >M $a0d2 $4b >M
T{ op PC S A X Y P -> $8a40 $93 $31 $6b $67 $24 }T T{ $0190 M $0191 M $0192 M $0193 M $8a40 M $a0d0 M $a0d1 M $a0d2 M -> $d0 $04 $40 $8a $a5 $40 $f3 $4b }T
$8c74 >PC $76 >S $47 >A $b3 >X $53 >Y $a5 >P $0176 $be >M $0177 $2b >M $0178 $48 >M $0179 $66 >M $6648 $7e >M $8c74 $40 >M $8c75 $f6 >M $8c76 $c6 >M
T{ op PC S A X Y P -> $6648 $79 $47 $b3 $53 $2b }T T{ $0176 M $0177 M $0178 M $0179 M $6648 M $8c74 M $8c75 M $8c76 M -> $be $2b $48 $66 $7e $40 $f6 $c6 }T
$8ec4 >PC $00 >S $09 >A $79 >X $cd >Y $a7 >P $0100 $f8 >M $0101 $49 >M $0102 $33 >M $0103 $e6 >M $8ec4 $40 >M $8ec5 $bb >M $8ec6 $84 >M $e633 $c5 >M
T{ op PC S A X Y P -> $e633 $03 $09 $79 $cd $69 }T T{ $0100 M $0101 M $0102 M $0103 M $8ec4 M $8ec5 M $8ec6 M $e633 M -> $f8 $49 $33 $e6 $40 $bb $84 $c5 }T
$466c >PC $e4 >S $b2 >A $cd >X $83 >Y $67 >P $001b $75 >M $01e4 $db >M $01e5 $ea >M $01e6 $1b >M $01e7 $00 >M $466c $40 >M $466d $04 >M $466e $2b >M
T{ op PC S A X Y P -> $001b $e7 $b2 $cd $83 $ea }T T{ $001b M $01e4 M $01e5 M $01e6 M $01e7 M $466c M $466d M $466e M -> $75 $db $ea $1b $00 $40 $04 $2b }T
$d231 >PC $14 >S $b8 >A $75 >X $e2 >Y $6c >P $0114 $b1 >M $0115 $bf >M $0116 $ec >M $0117 $1b >M $1bec $89 >M $d231 $40 >M $d232 $69 >M $d233 $7d >M
T{ op PC S A X Y P -> $1bec $17 $b8 $75 $e2 $af }T T{ $0114 M $0115 M $0116 M $0117 M $1bec M $d231 M $d232 M $d233 M -> $b1 $bf $ec $1b $89 $40 $69 $7d }T
$4437 >PC $47 >S $12 >A $ab >X $b7 >Y $a9 >P $0147 $01 >M $0148 $d6 >M $0149 $2c >M $014a $23 >M $232c $38 >M $4437 $40 >M $4438 $63 >M $4439 $16 >M
T{ op PC S A X Y P -> $232c $4a $12 $ab $b7 $e6 }T T{ $0147 M $0148 M $0149 M $014a M $232c M $4437 M $4438 M $4439 M -> $01 $d6 $2c $23 $38 $40 $63 $16 }T
$11bb >PC $91 >S $b3 >A $a6 >X $d4 >Y $21 >P $0191 $31 >M $0192 $5f >M $0193 $71 >M $0194 $b1 >M $11bb $40 >M $11bc $d5 >M $11bd $f3 >M $b171 $77 >M
T{ op PC S A X Y P -> $b171 $94 $b3 $a6 $d4 $6f }T T{ $0191 M $0192 M $0193 M $0194 M $11bb M $11bc M $11bd M $b171 M -> $31 $5f $71 $b1 $40 $d5 $f3 $77 }T
$e878 >PC $cc >S $9e >A $09 >X $7f >Y $ee >P $01cc $5a >M $01cd $f1 >M $01ce $05 >M $01cf $94 >M $9405 $5c >M $e878 $40 >M $e879 $f3 >M $e87a $2f >M
T{ op PC S A X Y P -> $9405 $cf $9e $09 $7f $e1 }T T{ $01cc M $01cd M $01ce M $01cf M $9405 M $e878 M $e879 M $e87a M -> $5a $f1 $05 $94 $5c $40 $f3 $2f }T
$ee5d >PC $c2 >S $b9 >A $eb >X $88 >Y $a1 >P $01c2 $6f >M $01c3 $dc >M $01c4 $60 >M $01c5 $67 >M $6760 $e9 >M $ee5d $40 >M $ee5e $33 >M $ee5f $90 >M
T{ op PC S A X Y P -> $6760 $c5 $b9 $eb $88 $ec }T T{ $01c2 M $01c3 M $01c4 M $01c5 M $6760 M $ee5d M $ee5e M $ee5f M -> $6f $dc $60 $67 $e9 $40 $33 $90 }T
$1922 >PC $7e >S $a7 >A $39 >X $40 >Y $62 >P $017e $20 >M $017f $48 >M $0180 $82 >M $0181 $cf >M $1922 $40 >M $1923 $ee >M $1924 $50 >M $cf82 $ad >M
T{ op PC S A X Y P -> $cf82 $81 $a7 $39 $40 $68 }T T{ $017e M $017f M $0180 M $0181 M $1922 M $1923 M $1924 M $cf82 M -> $20 $48 $82 $cf $40 $ee $50 $ad }T
$4e67 >PC $14 >S $28 >A $45 >X $66 >Y $6c >P $0114 $55 >M $0115 $f9 >M $0116 $c9 >M $0117 $3b >M $3bc9 $c3 >M $4e67 $40 >M $4e68 $85 >M $4e69 $48 >M
T{ op PC S A X Y P -> $3bc9 $17 $28 $45 $66 $e9 }T T{ $0114 M $0115 M $0116 M $0117 M $3bc9 M $4e67 M $4e68 M $4e69 M -> $55 $f9 $c9 $3b $c3 $40 $85 $48 }T
$5d61 >PC $f9 >S $de >A $d5 >X $40 >Y $6b >P $01f9 $ee >M $01fa $7c >M $01fb $39 >M $01fc $83 >M $5d61 $40 >M $5d62 $a1 >M $5d63 $90 >M $8339 $32 >M
T{ op PC S A X Y P -> $8339 $fc $de $d5 $40 $6c }T T{ $01f9 M $01fa M $01fb M $01fc M $5d61 M $5d62 M $5d63 M $8339 M -> $ee $7c $39 $83 $40 $a1 $90 $32 }T
$a9d6 >PC $37 >S $1b >A $a5 >X $c7 >Y $e2 >P $0137 $88 >M $0138 $a7 >M $0139 $fc >M $013a $96 >M $96fc $d9 >M $a9d6 $40 >M $a9d7 $31 >M $a9d8 $16 >M
T{ op PC S A X Y P -> $96fc $3a $1b $a5 $c7 $a7 }T T{ $0137 M $0138 M $0139 M $013a M $96fc M $a9d6 M $a9d7 M $a9d8 M -> $88 $a7 $fc $96 $d9 $40 $31 $16 }T
$ff10 >PC $74 >S $fb >A $e0 >X $d5 >Y $a6 >P $0174 $f1 >M $0175 $88 >M $0176 $28 >M $0177 $54 >M $5428 $a7 >M $ff10 $40 >M $ff11 $61 >M $ff12 $88 >M
T{ op PC S A X Y P -> $5428 $77 $fb $e0 $d5 $a8 }T T{ $0174 M $0175 M $0176 M $0177 M $5428 M $ff10 M $ff11 M $ff12 M -> $f1 $88 $28 $54 $a7 $40 $61 $88 }T
$ac13 >PC $84 >S $1a >A $83 >X $b5 >Y $e7 >P $0184 $88 >M $0185 $70 >M $0186 $cc >M $0187 $e9 >M $ac13 $40 >M $ac14 $57 >M $ac15 $71 >M $e9cc $06 >M
T{ op PC S A X Y P -> $e9cc $87 $1a $83 $b5 $60 }T T{ $0184 M $0185 M $0186 M $0187 M $ac13 M $ac14 M $ac15 M $e9cc M -> $88 $70 $cc $e9 $40 $57 $71 $06 }T
( 41 )
$4be5 >PC $57 >S $eb >A $54 >X $d4 >Y $a5 >P $0004 $99 >M $0058 $8b >M $0059 $8b >M $4be5 $41 >M $4be6 $04 >M $4be7 $ef >M $8b8b $38 >M
T{ op PC S A X Y P -> $4be7 $57 $d3 $54 $d4 $a5 }T T{ $0004 M $0058 M $0059 M $4be5 M $4be6 M $4be7 M $8b8b M -> $99 $8b $8b $41 $04 $ef $38 }T
$f366 >PC $bd >S $62 >A $b7 >X $6d >Y $62 >P $0000 $f0 >M $0001 $ab >M $0049 $c0 >M $abf0 $75 >M $f366 $41 >M $f367 $49 >M $f368 $dd >M
T{ op PC S A X Y P -> $f368 $bd $17 $b7 $6d $60 }T T{ $0000 M $0001 M $0049 M $abf0 M $f366 M $f367 M $f368 M -> $f0 $ab $c0 $75 $41 $49 $dd }T
$4d93 >PC $38 >S $4f >A $d6 >X $cc >Y $64 >P $0024 $1d >M $0025 $3b >M $004e $cd >M $3b1d $62 >M $4d93 $41 >M $4d94 $4e >M $4d95 $7c >M
T{ op PC S A X Y P -> $4d95 $38 $2d $d6 $cc $64 }T T{ $0024 M $0025 M $004e M $3b1d M $4d93 M $4d94 M $4d95 M -> $1d $3b $cd $62 $41 $4e $7c }T
$7431 >PC $56 >S $82 >A $c8 >X $6b >Y $e7 >P $0092 $83 >M $0093 $eb >M $00ca $b0 >M $7431 $41 >M $7432 $ca >M $7433 $fa >M $eb83 $c9 >M
T{ op PC S A X Y P -> $7433 $56 $4b $c8 $6b $65 }T T{ $0092 M $0093 M $00ca M $7431 M $7432 M $7433 M $eb83 M -> $83 $eb $b0 $41 $ca $fa $c9 }T
$97e8 >PC $32 >S $0d >A $86 >X $06 >Y $a6 >P $006e $59 >M $00f4 $50 >M $00f5 $4d >M $4d50 $fe >M $97e8 $41 >M $97e9 $6e >M $97ea $16 >M
T{ op PC S A X Y P -> $97ea $32 $f3 $86 $06 $a4 }T T{ $006e M $00f4 M $00f5 M $4d50 M $97e8 M $97e9 M $97ea M -> $59 $50 $4d $fe $41 $6e $16 }T
$29d3 >PC $05 >S $10 >A $63 >X $95 >Y $e9 >P $000b $00 >M $000c $b5 >M $00a8 $87 >M $29d3 $41 >M $29d4 $a8 >M $29d5 $1c >M $b500 $a8 >M
T{ op PC S A X Y P -> $29d5 $05 $b8 $63 $95 $e9 }T T{ $000b M $000c M $00a8 M $29d3 M $29d4 M $29d5 M $b500 M -> $00 $b5 $87 $41 $a8 $1c $a8 }T
$6ff1 >PC $b0 >S $9c >A $7b >X $86 >Y $21 >P $0028 $a8 >M $0029 $08 >M $00ad $12 >M $08a8 $1d >M $6ff1 $41 >M $6ff2 $ad >M $6ff3 $26 >M
T{ op PC S A X Y P -> $6ff3 $b0 $81 $7b $86 $a1 }T T{ $0028 M $0029 M $00ad M $08a8 M $6ff1 M $6ff2 M $6ff3 M -> $a8 $08 $12 $1d $41 $ad $26 }T
$eff5 >PC $92 >S $33 >A $5f >X $74 >Y $ef >P $0024 $8d >M $0083 $f9 >M $0084 $e5 >M $e5f9 $13 >M $eff5 $41 >M $eff6 $24 >M $eff7 $a9 >M
T{ op PC S A X Y P -> $eff7 $92 $20 $5f $74 $6d }T T{ $0024 M $0083 M $0084 M $e5f9 M $eff5 M $eff6 M $eff7 M -> $8d $f9 $e5 $13 $41 $24 $a9 }T
$39d6 >PC $31 >S $a3 >A $3e >X $c4 >Y $e3 >P $001e $e3 >M $005c $57 >M $005d $f3 >M $39d6 $41 >M $39d7 $1e >M $39d8 $f5 >M $f357 $4f >M
T{ op PC S A X Y P -> $39d8 $31 $ec $3e $c4 $e1 }T T{ $001e M $005c M $005d M $39d6 M $39d7 M $39d8 M $f357 M -> $e3 $57 $f3 $41 $1e $f5 $4f }T
$937b >PC $cb >S $bf >A $33 >X $8a >Y $ad >P $007c $44 >M $00af $93 >M $00b0 $57 >M $5793 $85 >M $937b $41 >M $937c $7c >M $937d $07 >M
T{ op PC S A X Y P -> $937d $cb $3a $33 $8a $2d }T T{ $007c M $00af M $00b0 M $5793 M $937b M $937c M $937d M -> $44 $93 $57 $85 $41 $7c $07 }T
$f4ad >PC $4f >S $10 >A $cd >X $0f >Y $ea >P $007b $54 >M $007c $2b >M $00ae $9e >M $2b54 $c7 >M $f4ad $41 >M $f4ae $ae >M $f4af $16 >M
T{ op PC S A X Y P -> $f4af $4f $d7 $cd $0f $e8 }T T{ $007b M $007c M $00ae M $2b54 M $f4ad M $f4ae M $f4af M -> $54 $2b $9e $c7 $41 $ae $16 }T
$c020 >PC $e0 >S $31 >A $8e >X $8d >Y $ed >P $0089 $7d >M $008a $93 >M $00fb $99 >M $937d $eb >M $c020 $41 >M $c021 $fb >M $c022 $ef >M
T{ op PC S A X Y P -> $c022 $e0 $da $8e $8d $ed }T T{ $0089 M $008a M $00fb M $937d M $c020 M $c021 M $c022 M -> $7d $93 $99 $eb $41 $fb $ef }T
$5cf1 >PC $c7 >S $fa >A $02 >X $78 >Y $ae >P $00ca $58 >M $00cc $ec >M $00cd $06 >M $06ec $c3 >M $5cf1 $41 >M $5cf2 $ca >M $5cf3 $93 >M
T{ op PC S A X Y P -> $5cf3 $c7 $39 $02 $78 $2c }T T{ $00ca M $00cc M $00cd M $06ec M $5cf1 M $5cf2 M $5cf3 M -> $58 $ec $06 $c3 $41 $ca $93 }T
$792c >PC $80 >S $cd >A $4c >X $9c >Y $af >P $0047 $35 >M $0048 $81 >M $00fb $85 >M $792c $41 >M $792d $fb >M $792e $84 >M $8135 $5a >M
T{ op PC S A X Y P -> $792e $80 $97 $4c $9c $ad }T T{ $0047 M $0048 M $00fb M $792c M $792d M $792e M $8135 M -> $35 $81 $85 $41 $fb $84 $5a }T
$33a8 >PC $d1 >S $c1 >A $6e >X $f6 >Y $ef >P $0083 $4f >M $00f1 $bb >M $00f2 $bb >M $33a8 $41 >M $33a9 $83 >M $33aa $5c >M $bbbb $f7 >M
T{ op PC S A X Y P -> $33aa $d1 $36 $6e $f6 $6d }T T{ $0083 M $00f1 M $00f2 M $33a8 M $33a9 M $33aa M $bbbb M -> $4f $bb $bb $41 $83 $5c $f7 }T
$78b0 >PC $9c >S $75 >A $de >X $41 >Y $e3 >P $0075 $06 >M $0076 $bc >M $0097 $2d >M $78b0 $41 >M $78b1 $97 >M $78b2 $3c >M $bc06 $26 >M
T{ op PC S A X Y P -> $78b2 $9c $53 $de $41 $61 }T T{ $0075 M $0076 M $0097 M $78b0 M $78b1 M $78b2 M $bc06 M -> $06 $bc $2d $41 $97 $3c $26 }T
( 42 )
$1ef7 >PC $e4 >S $24 >A $84 >X $cb >Y $2c >P $1ef7 $42 >M $1ef8 $e7 >M $1ef9 $94 >M
T{ op PC S A X Y P -> $1ef9 $e4 $24 $84 $cb $2c }T T{ $1ef7 M $1ef8 M $1ef9 M -> $42 $e7 $94 }T
$2deb >PC $f5 >S $50 >A $5c >X $10 >Y $20 >P $2deb $42 >M $2dec $89 >M $2ded $c1 >M
T{ op PC S A X Y P -> $2ded $f5 $50 $5c $10 $20 }T T{ $2deb M $2dec M $2ded M -> $42 $89 $c1 }T
$d6c0 >PC $14 >S $18 >A $7d >X $09 >Y $61 >P $d6c0 $42 >M $d6c1 $e3 >M $d6c2 $22 >M
T{ op PC S A X Y P -> $d6c2 $14 $18 $7d $09 $61 }T T{ $d6c0 M $d6c1 M $d6c2 M -> $42 $e3 $22 }T
$b6d1 >PC $6c >S $fe >A $17 >X $d1 >Y $6c >P $b6d1 $42 >M $b6d2 $31 >M $b6d3 $2c >M
T{ op PC S A X Y P -> $b6d3 $6c $fe $17 $d1 $6c }T T{ $b6d1 M $b6d2 M $b6d3 M -> $42 $31 $2c }T
$280c >PC $12 >S $69 >A $23 >X $65 >Y $a0 >P $280c $42 >M $280d $ca >M $280e $de >M
T{ op PC S A X Y P -> $280e $12 $69 $23 $65 $a0 }T T{ $280c M $280d M $280e M -> $42 $ca $de }T
$5cd9 >PC $95 >S $24 >A $0e >X $15 >Y $a8 >P $5cd9 $42 >M $5cda $bc >M $5cdb $57 >M
T{ op PC S A X Y P -> $5cdb $95 $24 $0e $15 $a8 }T T{ $5cd9 M $5cda M $5cdb M -> $42 $bc $57 }T
$41dc >PC $a6 >S $24 >A $af >X $af >Y $67 >P $41dc $42 >M $41dd $4a >M $41de $d8 >M
T{ op PC S A X Y P -> $41de $a6 $24 $af $af $67 }T T{ $41dc M $41dd M $41de M -> $42 $4a $d8 }T
$de50 >PC $59 >S $8c >A $e7 >X $64 >Y $28 >P $de50 $42 >M $de51 $87 >M $de52 $59 >M
T{ op PC S A X Y P -> $de52 $59 $8c $e7 $64 $28 }T T{ $de50 M $de51 M $de52 M -> $42 $87 $59 }T
$4520 >PC $de >S $ab >A $71 >X $c1 >Y $20 >P $4520 $42 >M $4521 $c4 >M $4522 $07 >M
T{ op PC S A X Y P -> $4522 $de $ab $71 $c1 $20 }T T{ $4520 M $4521 M $4522 M -> $42 $c4 $07 }T
$2133 >PC $59 >S $1e >A $d5 >X $2c >Y $68 >P $2133 $42 >M $2134 $04 >M $2135 $ac >M
T{ op PC S A X Y P -> $2135 $59 $1e $d5 $2c $68 }T T{ $2133 M $2134 M $2135 M -> $42 $04 $ac }T
$c03f >PC $96 >S $5b >A $c9 >X $3b >Y $ee >P $c03f $42 >M $c040 $31 >M $c041 $01 >M
T{ op PC S A X Y P -> $c041 $96 $5b $c9 $3b $ee }T T{ $c03f M $c040 M $c041 M -> $42 $31 $01 }T
$baf3 >PC $bb >S $11 >A $81 >X $16 >Y $e6 >P $baf3 $42 >M $baf4 $e9 >M $baf5 $24 >M
T{ op PC S A X Y P -> $baf5 $bb $11 $81 $16 $e6 }T T{ $baf3 M $baf4 M $baf5 M -> $42 $e9 $24 }T
$dbf0 >PC $b4 >S $98 >A $63 >X $91 >Y $af >P $dbf0 $42 >M $dbf1 $b4 >M $dbf2 $d6 >M
T{ op PC S A X Y P -> $dbf2 $b4 $98 $63 $91 $af }T T{ $dbf0 M $dbf1 M $dbf2 M -> $42 $b4 $d6 }T
$496d >PC $6f >S $b3 >A $69 >X $56 >Y $28 >P $496d $42 >M $496e $7f >M $496f $9b >M
T{ op PC S A X Y P -> $496f $6f $b3 $69 $56 $28 }T T{ $496d M $496e M $496f M -> $42 $7f $9b }T
$8336 >PC $9c >S $68 >A $09 >X $7a >Y $ad >P $8336 $42 >M $8337 $78 >M $8338 $e2 >M
T{ op PC S A X Y P -> $8338 $9c $68 $09 $7a $ad }T T{ $8336 M $8337 M $8338 M -> $42 $78 $e2 }T
$2eed >PC $07 >S $58 >A $f5 >X $58 >Y $ee >P $2eed $42 >M $2eee $3b >M $2eef $22 >M
T{ op PC S A X Y P -> $2eef $07 $58 $f5 $58 $ee }T T{ $2eed M $2eee M $2eef M -> $42 $3b $22 }T
( 43 )
$2d02 >PC $4c >S $ff >A $57 >X $e6 >Y $af >P $2d02 $43 >M $2d03 $1c >M $2d04 $e5 >M
T{ op PC S A X Y P -> $2d03 $4c $ff $57 $e6 $af }T T{ $2d02 M $2d03 M $2d04 M -> $43 $1c $e5 }T
$a3be >PC $79 >S $ed >A $b0 >X $5c >Y $a2 >P $a3be $43 >M $a3bf $f0 >M $a3c0 $66 >M
T{ op PC S A X Y P -> $a3bf $79 $ed $b0 $5c $a2 }T T{ $a3be M $a3bf M $a3c0 M -> $43 $f0 $66 }T
$3bfc >PC $30 >S $c6 >A $83 >X $51 >Y $6a >P $3bfc $43 >M $3bfd $02 >M $3bfe $fa >M
T{ op PC S A X Y P -> $3bfd $30 $c6 $83 $51 $6a }T T{ $3bfc M $3bfd M $3bfe M -> $43 $02 $fa }T
$60d0 >PC $12 >S $57 >A $5b >X $bd >Y $6a >P $60d0 $43 >M $60d1 $b1 >M $60d2 $10 >M
T{ op PC S A X Y P -> $60d1 $12 $57 $5b $bd $6a }T T{ $60d0 M $60d1 M $60d2 M -> $43 $b1 $10 }T
$c865 >PC $45 >S $e2 >A $72 >X $41 >Y $e1 >P $c865 $43 >M $c866 $49 >M $c867 $64 >M
T{ op PC S A X Y P -> $c866 $45 $e2 $72 $41 $e1 }T T{ $c865 M $c866 M $c867 M -> $43 $49 $64 }T
$6024 >PC $a9 >S $8f >A $f0 >X $a8 >Y $ac >P $6024 $43 >M $6025 $2a >M $6026 $5e >M
T{ op PC S A X Y P -> $6025 $a9 $8f $f0 $a8 $ac }T T{ $6024 M $6025 M $6026 M -> $43 $2a $5e }T
$4aa7 >PC $5b >S $2b >A $b4 >X $ac >Y $ed >P $4aa7 $43 >M $4aa8 $e7 >M $4aa9 $4a >M
T{ op PC S A X Y P -> $4aa8 $5b $2b $b4 $ac $ed }T T{ $4aa7 M $4aa8 M $4aa9 M -> $43 $e7 $4a }T
$777b >PC $ee >S $05 >A $38 >X $9f >Y $a5 >P $777b $43 >M $777c $6a >M $777d $51 >M
T{ op PC S A X Y P -> $777c $ee $05 $38 $9f $a5 }T T{ $777b M $777c M $777d M -> $43 $6a $51 }T
$6616 >PC $ab >S $73 >A $1e >X $42 >Y $63 >P $6616 $43 >M $6617 $87 >M $6618 $47 >M
T{ op PC S A X Y P -> $6617 $ab $73 $1e $42 $63 }T T{ $6616 M $6617 M $6618 M -> $43 $87 $47 }T
$946a >PC $c1 >S $0d >A $83 >X $d7 >Y $6b >P $946a $43 >M $946b $dc >M $946c $ad >M
T{ op PC S A X Y P -> $946b $c1 $0d $83 $d7 $6b }T T{ $946a M $946b M $946c M -> $43 $dc $ad }T
$49b8 >PC $66 >S $77 >A $ae >X $49 >Y $a0 >P $49b8 $43 >M $49b9 $3d >M $49ba $bd >M
T{ op PC S A X Y P -> $49b9 $66 $77 $ae $49 $a0 }T T{ $49b8 M $49b9 M $49ba M -> $43 $3d $bd }T
$74ef >PC $62 >S $6c >A $47 >X $1a >Y $61 >P $74ef $43 >M $74f0 $46 >M $74f1 $df >M
T{ op PC S A X Y P -> $74f0 $62 $6c $47 $1a $61 }T T{ $74ef M $74f0 M $74f1 M -> $43 $46 $df }T
$6799 >PC $a4 >S $95 >A $74 >X $8e >Y $24 >P $6799 $43 >M $679a $1c >M $679b $79 >M
T{ op PC S A X Y P -> $679a $a4 $95 $74 $8e $24 }T T{ $6799 M $679a M $679b M -> $43 $1c $79 }T
$06de >PC $76 >S $70 >A $cf >X $9b >Y $24 >P $06de $43 >M $06df $fc >M $06e0 $f7 >M
T{ op PC S A X Y P -> $06df $76 $70 $cf $9b $24 }T T{ $06de M $06df M $06e0 M -> $43 $fc $f7 }T
$226f >PC $b8 >S $15 >A $82 >X $3a >Y $6f >P $226f $43 >M $2270 $f7 >M $2271 $ab >M
T{ op PC S A X Y P -> $2270 $b8 $15 $82 $3a $6f }T T{ $226f M $2270 M $2271 M -> $43 $f7 $ab }T
$1cf3 >PC $e5 >S $50 >A $68 >X $c4 >Y $a1 >P $1cf3 $43 >M $1cf4 $ad >M $1cf5 $74 >M
T{ op PC S A X Y P -> $1cf4 $e5 $50 $68 $c4 $a1 }T T{ $1cf3 M $1cf4 M $1cf5 M -> $43 $ad $74 }T
( 44 )
$2be1 >PC $a1 >S $b1 >A $96 >X $ef >Y $6e >P $0024 $75 >M $2be1 $44 >M $2be2 $24 >M $2be3 $02 >M
T{ op PC S A X Y P -> $2be3 $a1 $b1 $96 $ef $6e }T T{ $0024 M $2be1 M $2be2 M $2be3 M -> $75 $44 $24 $02 }T
$cfe7 >PC $2d >S $6a >A $cf >X $3a >Y $67 >P $004e $7a >M $cfe7 $44 >M $cfe8 $4e >M $cfe9 $c3 >M
T{ op PC S A X Y P -> $cfe9 $2d $6a $cf $3a $67 }T T{ $004e M $cfe7 M $cfe8 M $cfe9 M -> $7a $44 $4e $c3 }T
$1fbd >PC $72 >S $f7 >A $36 >X $55 >Y $a7 >P $00b8 $7e >M $1fbd $44 >M $1fbe $b8 >M $1fbf $72 >M
T{ op PC S A X Y P -> $1fbf $72 $f7 $36 $55 $a7 }T T{ $00b8 M $1fbd M $1fbe M $1fbf M -> $7e $44 $b8 $72 }T
$ac4b >PC $2d >S $22 >A $d7 >X $3d >Y $eb >P $00ad $d7 >M $ac4b $44 >M $ac4c $ad >M $ac4d $db >M
T{ op PC S A X Y P -> $ac4d $2d $22 $d7 $3d $eb }T T{ $00ad M $ac4b M $ac4c M $ac4d M -> $d7 $44 $ad $db }T
$28a8 >PC $39 >S $b5 >A $18 >X $94 >Y $28 >P $0076 $72 >M $28a8 $44 >M $28a9 $76 >M $28aa $23 >M
T{ op PC S A X Y P -> $28aa $39 $b5 $18 $94 $28 }T T{ $0076 M $28a8 M $28a9 M $28aa M -> $72 $44 $76 $23 }T
$a404 >PC $a1 >S $44 >A $ee >X $21 >Y $a7 >P $009e $e6 >M $a404 $44 >M $a405 $9e >M $a406 $28 >M
T{ op PC S A X Y P -> $a406 $a1 $44 $ee $21 $a7 }T T{ $009e M $a404 M $a405 M $a406 M -> $e6 $44 $9e $28 }T
$1db6 >PC $5d >S $1c >A $6b >X $6c >Y $e6 >P $00fc $1e >M $1db6 $44 >M $1db7 $fc >M $1db8 $5e >M
T{ op PC S A X Y P -> $1db8 $5d $1c $6b $6c $e6 }T T{ $00fc M $1db6 M $1db7 M $1db8 M -> $1e $44 $fc $5e }T
$ec03 >PC $da >S $b0 >A $57 >X $cf >Y $a3 >P $0097 $11 >M $ec03 $44 >M $ec04 $97 >M $ec05 $e1 >M
T{ op PC S A X Y P -> $ec05 $da $b0 $57 $cf $a3 }T T{ $0097 M $ec03 M $ec04 M $ec05 M -> $11 $44 $97 $e1 }T
$a488 >PC $f0 >S $46 >A $f3 >X $b4 >Y $e2 >P $007d $4e >M $a488 $44 >M $a489 $7d >M $a48a $a9 >M
T{ op PC S A X Y P -> $a48a $f0 $46 $f3 $b4 $e2 }T T{ $007d M $a488 M $a489 M $a48a M -> $4e $44 $7d $a9 }T
$c795 >PC $74 >S $82 >A $e4 >X $34 >Y $a8 >P $00f2 $aa >M $c795 $44 >M $c796 $f2 >M $c797 $ca >M
T{ op PC S A X Y P -> $c797 $74 $82 $e4 $34 $a8 }T T{ $00f2 M $c795 M $c796 M $c797 M -> $aa $44 $f2 $ca }T
$9ed1 >PC $e3 >S $1b >A $4c >X $15 >Y $67 >P $00f2 $7f >M $9ed1 $44 >M $9ed2 $f2 >M $9ed3 $cf >M
T{ op PC S A X Y P -> $9ed3 $e3 $1b $4c $15 $67 }T T{ $00f2 M $9ed1 M $9ed2 M $9ed3 M -> $7f $44 $f2 $cf }T
$9a81 >PC $30 >S $a1 >A $24 >X $85 >Y $e4 >P $000d $14 >M $9a81 $44 >M $9a82 $0d >M $9a83 $a3 >M
T{ op PC S A X Y P -> $9a83 $30 $a1 $24 $85 $e4 }T T{ $000d M $9a81 M $9a82 M $9a83 M -> $14 $44 $0d $a3 }T
$a1c0 >PC $1b >S $97 >A $cc >X $72 >Y $60 >P $002f $c6 >M $a1c0 $44 >M $a1c1 $2f >M $a1c2 $12 >M
T{ op PC S A X Y P -> $a1c2 $1b $97 $cc $72 $60 }T T{ $002f M $a1c0 M $a1c1 M $a1c2 M -> $c6 $44 $2f $12 }T
$3c34 >PC $b4 >S $47 >A $51 >X $01 >Y $af >P $0022 $c4 >M $3c34 $44 >M $3c35 $22 >M $3c36 $71 >M
T{ op PC S A X Y P -> $3c36 $b4 $47 $51 $01 $af }T T{ $0022 M $3c34 M $3c35 M $3c36 M -> $c4 $44 $22 $71 }T
$a3ce >PC $d8 >S $3b >A $90 >X $3c >Y $2b >P $0056 $2a >M $a3ce $44 >M $a3cf $56 >M $a3d0 $74 >M
T{ op PC S A X Y P -> $a3d0 $d8 $3b $90 $3c $2b }T T{ $0056 M $a3ce M $a3cf M $a3d0 M -> $2a $44 $56 $74 }T
$aa68 >PC $d6 >S $4b >A $d2 >X $03 >Y $61 >P $0045 $23 >M $aa68 $44 >M $aa69 $45 >M $aa6a $23 >M
T{ op PC S A X Y P -> $aa6a $d6 $4b $d2 $03 $61 }T T{ $0045 M $aa68 M $aa69 M $aa6a M -> $23 $44 $45 $23 }T
( 45 )
$5b26 >PC $ed >S $92 >A $be >X $97 >Y $2a >P $003a $e6 >M $5b26 $45 >M $5b27 $3a >M $5b28 $6d >M
T{ op PC S A X Y P -> $5b28 $ed $74 $be $97 $28 }T T{ $003a M $5b26 M $5b27 M $5b28 M -> $e6 $45 $3a $6d }T
$9391 >PC $46 >S $06 >A $86 >X $53 >Y $2b >P $0057 $40 >M $9391 $45 >M $9392 $57 >M $9393 $6f >M
T{ op PC S A X Y P -> $9393 $46 $46 $86 $53 $29 }T T{ $0057 M $9391 M $9392 M $9393 M -> $40 $45 $57 $6f }T
$7dc7 >PC $2e >S $dc >A $d2 >X $ab >Y $2e >P $002a $71 >M $7dc7 $45 >M $7dc8 $2a >M $7dc9 $0f >M
T{ op PC S A X Y P -> $7dc9 $2e $ad $d2 $ab $ac }T T{ $002a M $7dc7 M $7dc8 M $7dc9 M -> $71 $45 $2a $0f }T
$c13c >PC $aa >S $20 >A $3e >X $6b >Y $a9 >P $0016 $fb >M $c13c $45 >M $c13d $16 >M $c13e $7f >M
T{ op PC S A X Y P -> $c13e $aa $db $3e $6b $a9 }T T{ $0016 M $c13c M $c13d M $c13e M -> $fb $45 $16 $7f }T
$1f33 >PC $fd >S $60 >A $62 >X $94 >Y $e5 >P $00e7 $fe >M $1f33 $45 >M $1f34 $e7 >M $1f35 $2d >M
T{ op PC S A X Y P -> $1f35 $fd $9e $62 $94 $e5 }T T{ $00e7 M $1f33 M $1f34 M $1f35 M -> $fe $45 $e7 $2d }T
$1934 >PC $5a >S $3a >A $fc >X $47 >Y $66 >P $00e1 $f2 >M $1934 $45 >M $1935 $e1 >M $1936 $3a >M
T{ op PC S A X Y P -> $1936 $5a $c8 $fc $47 $e4 }T T{ $00e1 M $1934 M $1935 M $1936 M -> $f2 $45 $e1 $3a }T
$df69 >PC $fb >S $49 >A $3b >X $b6 >Y $23 >P $0021 $ba >M $df69 $45 >M $df6a $21 >M $df6b $aa >M
T{ op PC S A X Y P -> $df6b $fb $f3 $3b $b6 $a1 }T T{ $0021 M $df69 M $df6a M $df6b M -> $ba $45 $21 $aa }T
$67f6 >PC $11 >S $32 >A $48 >X $76 >Y $a9 >P $008a $9a >M $67f6 $45 >M $67f7 $8a >M $67f8 $bf >M
T{ op PC S A X Y P -> $67f8 $11 $a8 $48 $76 $a9 }T T{ $008a M $67f6 M $67f7 M $67f8 M -> $9a $45 $8a $bf }T
$7d51 >PC $6f >S $88 >A $ee >X $c0 >Y $65 >P $004f $7d >M $7d51 $45 >M $7d52 $4f >M $7d53 $50 >M
T{ op PC S A X Y P -> $7d53 $6f $f5 $ee $c0 $e5 }T T{ $004f M $7d51 M $7d52 M $7d53 M -> $7d $45 $4f $50 }T
$21b3 >PC $60 >S $02 >A $97 >X $d7 >Y $2e >P $0061 $6c >M $21b3 $45 >M $21b4 $61 >M $21b5 $a9 >M
T{ op PC S A X Y P -> $21b5 $60 $6e $97 $d7 $2c }T T{ $0061 M $21b3 M $21b4 M $21b5 M -> $6c $45 $61 $a9 }T
$c3ad >PC $75 >S $5b >A $84 >X $2e >Y $28 >P $00c4 $00 >M $c3ad $45 >M $c3ae $c4 >M $c3af $d9 >M
T{ op PC S A X Y P -> $c3af $75 $5b $84 $2e $28 }T T{ $00c4 M $c3ad M $c3ae M $c3af M -> $00 $45 $c4 $d9 }T
$62c7 >PC $b4 >S $6a >A $90 >X $0b >Y $6e >P $0006 $1d >M $62c7 $45 >M $62c8 $06 >M $62c9 $b2 >M
T{ op PC S A X Y P -> $62c9 $b4 $77 $90 $0b $6c }T T{ $0006 M $62c7 M $62c8 M $62c9 M -> $1d $45 $06 $b2 }T
$62d3 >PC $04 >S $2a >A $cf >X $86 >Y $e0 >P $0058 $7b >M $62d3 $45 >M $62d4 $58 >M $62d5 $9c >M
T{ op PC S A X Y P -> $62d5 $04 $51 $cf $86 $60 }T T{ $0058 M $62d3 M $62d4 M $62d5 M -> $7b $45 $58 $9c }T
$a46b >PC $e3 >S $0a >A $76 >X $4e >Y $2e >P $0051 $01 >M $a46b $45 >M $a46c $51 >M $a46d $9f >M
T{ op PC S A X Y P -> $a46d $e3 $0b $76 $4e $2c }T T{ $0051 M $a46b M $a46c M $a46d M -> $01 $45 $51 $9f }T
$7bd8 >PC $ab >S $7e >A $7d >X $ff >Y $a2 >P $008f $9c >M $7bd8 $45 >M $7bd9 $8f >M $7bda $93 >M
T{ op PC S A X Y P -> $7bda $ab $e2 $7d $ff $a0 }T T{ $008f M $7bd8 M $7bd9 M $7bda M -> $9c $45 $8f $93 }T
$965a >PC $89 >S $0d >A $57 >X $81 >Y $a8 >P $00f8 $82 >M $965a $45 >M $965b $f8 >M $965c $90 >M
T{ op PC S A X Y P -> $965c $89 $8f $57 $81 $a8 }T T{ $00f8 M $965a M $965b M $965c M -> $82 $45 $f8 $90 }T
( 46 )
$e6ce >PC $f5 >S $f2 >A $db >X $cd >Y $2f >P $00c3 $07 >M $e6ce $46 >M $e6cf $c3 >M $e6d0 $74 >M
T{ op PC S A X Y P -> $e6d0 $f5 $f2 $db $cd $2d }T T{ $00c3 M $e6ce M $e6cf M $e6d0 M -> $03 $46 $c3 $74 }T
$9595 >PC $39 >S $14 >A $12 >X $60 >Y $a7 >P $0007 $c7 >M $9595 $46 >M $9596 $07 >M $9597 $17 >M
T{ op PC S A X Y P -> $9597 $39 $14 $12 $60 $25 }T T{ $0007 M $9595 M $9596 M $9597 M -> $63 $46 $07 $17 }T
$beb5 >PC $1c >S $d4 >A $8d >X $34 >Y $ae >P $00cb $f4 >M $beb5 $46 >M $beb6 $cb >M $beb7 $97 >M
T{ op PC S A X Y P -> $beb7 $1c $d4 $8d $34 $2c }T T{ $00cb M $beb5 M $beb6 M $beb7 M -> $7a $46 $cb $97 }T
$5bb2 >PC $d7 >S $74 >A $39 >X $31 >Y $ae >P $00a0 $f1 >M $5bb2 $46 >M $5bb3 $a0 >M $5bb4 $7f >M
T{ op PC S A X Y P -> $5bb4 $d7 $74 $39 $31 $2d }T T{ $00a0 M $5bb2 M $5bb3 M $5bb4 M -> $78 $46 $a0 $7f }T
$72df >PC $bf >S $ea >A $8e >X $70 >Y $e4 >P $00ba $f6 >M $72df $46 >M $72e0 $ba >M $72e1 $9d >M
T{ op PC S A X Y P -> $72e1 $bf $ea $8e $70 $64 }T T{ $00ba M $72df M $72e0 M $72e1 M -> $7b $46 $ba $9d }T
$321c >PC $2c >S $a3 >A $31 >X $5a >Y $63 >P $005c $0d >M $321c $46 >M $321d $5c >M $321e $b4 >M
T{ op PC S A X Y P -> $321e $2c $a3 $31 $5a $61 }T T{ $005c M $321c M $321d M $321e M -> $06 $46 $5c $b4 }T
$052d >PC $1e >S $cc >A $81 >X $c7 >Y $ef >P $00fd $df >M $052d $46 >M $052e $fd >M $052f $4f >M
T{ op PC S A X Y P -> $052f $1e $cc $81 $c7 $6d }T T{ $00fd M $052d M $052e M $052f M -> $6f $46 $fd $4f }T
$6356 >PC $bc >S $aa >A $a8 >X $ca >Y $25 >P $004b $e6 >M $6356 $46 >M $6357 $4b >M $6358 $b7 >M
T{ op PC S A X Y P -> $6358 $bc $aa $a8 $ca $24 }T T{ $004b M $6356 M $6357 M $6358 M -> $73 $46 $4b $b7 }T
$39be >PC $a7 >S $25 >A $c4 >X $3b >Y $e3 >P $001f $72 >M $39be $46 >M $39bf $1f >M $39c0 $4a >M
T{ op PC S A X Y P -> $39c0 $a7 $25 $c4 $3b $60 }T T{ $001f M $39be M $39bf M $39c0 M -> $39 $46 $1f $4a }T
$3925 >PC $33 >S $0f >A $ab >X $00 >Y $a2 >P $006b $75 >M $3925 $46 >M $3926 $6b >M $3927 $43 >M
T{ op PC S A X Y P -> $3927 $33 $0f $ab $00 $21 }T T{ $006b M $3925 M $3926 M $3927 M -> $3a $46 $6b $43 }T
$dca1 >PC $9b >S $c3 >A $31 >X $b9 >Y $e6 >P $00b7 $4f >M $dca1 $46 >M $dca2 $b7 >M $dca3 $15 >M
T{ op PC S A X Y P -> $dca3 $9b $c3 $31 $b9 $65 }T T{ $00b7 M $dca1 M $dca2 M $dca3 M -> $27 $46 $b7 $15 }T
$010a >PC $9a >S $cc >A $c0 >X $6e >Y $e1 >P $0042 $ea >M $010a $46 >M $010b $42 >M $010c $e6 >M
T{ op PC S A X Y P -> $010c $9a $cc $c0 $6e $60 }T T{ $0042 M $010a M $010b M $010c M -> $75 $46 $42 $e6 }T
$2001 >PC $ce >S $23 >A $62 >X $31 >Y $6c >P $0098 $bc >M $2001 $46 >M $2002 $98 >M $2003 $c6 >M
T{ op PC S A X Y P -> $2003 $ce $23 $62 $31 $6c }T T{ $0098 M $2001 M $2002 M $2003 M -> $5e $46 $98 $c6 }T
$7107 >PC $a8 >S $32 >A $53 >X $3d >Y $6a >P $0043 $da >M $7107 $46 >M $7108 $43 >M $7109 $1c >M
T{ op PC S A X Y P -> $7109 $a8 $32 $53 $3d $68 }T T{ $0043 M $7107 M $7108 M $7109 M -> $6d $46 $43 $1c }T
$b3dd >PC $bb >S $fe >A $e8 >X $6a >Y $67 >P $007a $d4 >M $b3dd $46 >M $b3de $7a >M $b3df $b0 >M
T{ op PC S A X Y P -> $b3df $bb $fe $e8 $6a $64 }T T{ $007a M $b3dd M $b3de M $b3df M -> $6a $46 $7a $b0 }T
$cdba >PC $29 >S $e6 >A $fa >X $82 >Y $a3 >P $0064 $db >M $cdba $46 >M $cdbb $64 >M $cdbc $85 >M
T{ op PC S A X Y P -> $cdbc $29 $e6 $fa $82 $21 }T T{ $0064 M $cdba M $cdbb M $cdbc M -> $6d $46 $64 $85 }T
( 47 )
$780d >PC $68 >S $de >A $1f >X $01 >Y $ec >P $008d $8b >M $780d $47 >M $780e $8d >M $780f $6d >M
T{ op PC S A X Y P -> $780f $68 $de $1f $01 $ec }T T{ $008d M $780d M $780e M $780f M -> $8b $47 $8d $6d }T
$30d7 >PC $ba >S $c5 >A $b3 >X $1c >Y $67 >P $0069 $86 >M $30d7 $47 >M $30d8 $69 >M $30d9 $58 >M
T{ op PC S A X Y P -> $30d9 $ba $c5 $b3 $1c $67 }T T{ $0069 M $30d7 M $30d8 M $30d9 M -> $86 $47 $69 $58 }T
$75a1 >PC $47 >S $8e >A $4f >X $eb >Y $ae >P $00f9 $88 >M $75a1 $47 >M $75a2 $f9 >M $75a3 $2f >M
T{ op PC S A X Y P -> $75a3 $47 $8e $4f $eb $ae }T T{ $00f9 M $75a1 M $75a2 M $75a3 M -> $88 $47 $f9 $2f }T
$a0b1 >PC $9e >S $8b >A $ea >X $7f >Y $e9 >P $00a6 $ea >M $a0b1 $47 >M $a0b2 $a6 >M $a0b3 $0d >M
T{ op PC S A X Y P -> $a0b3 $9e $8b $ea $7f $e9 }T T{ $00a6 M $a0b1 M $a0b2 M $a0b3 M -> $ea $47 $a6 $0d }T
$4c4f >PC $e0 >S $c5 >A $39 >X $c4 >Y $a4 >P $0062 $32 >M $4c4f $47 >M $4c50 $62 >M $4c51 $04 >M
T{ op PC S A X Y P -> $4c51 $e0 $c5 $39 $c4 $a4 }T T{ $0062 M $4c4f M $4c50 M $4c51 M -> $22 $47 $62 $04 }T
$f323 >PC $f9 >S $ba >A $bd >X $08 >Y $6c >P $004a $4a >M $f323 $47 >M $f324 $4a >M $f325 $d1 >M
T{ op PC S A X Y P -> $f325 $f9 $ba $bd $08 $6c }T T{ $004a M $f323 M $f324 M $f325 M -> $4a $47 $4a $d1 }T
$5cc9 >PC $a1 >S $da >A $e6 >X $27 >Y $e3 >P $0069 $65 >M $5cc9 $47 >M $5cca $69 >M $5ccb $00 >M
T{ op PC S A X Y P -> $5ccb $a1 $da $e6 $27 $e3 }T T{ $0069 M $5cc9 M $5cca M $5ccb M -> $65 $47 $69 $00 }T
$1e5c >PC $01 >S $40 >A $c0 >X $b8 >Y $63 >P $006c $26 >M $1e5c $47 >M $1e5d $6c >M $1e5e $38 >M
T{ op PC S A X Y P -> $1e5e $01 $40 $c0 $b8 $63 }T T{ $006c M $1e5c M $1e5d M $1e5e M -> $26 $47 $6c $38 }T
$4fd0 >PC $ba >S $3b >A $99 >X $5d >Y $27 >P $00ca $8a >M $4fd0 $47 >M $4fd1 $ca >M $4fd2 $03 >M
T{ op PC S A X Y P -> $4fd2 $ba $3b $99 $5d $27 }T T{ $00ca M $4fd0 M $4fd1 M $4fd2 M -> $8a $47 $ca $03 }T
$05b4 >PC $c1 >S $32 >A $a7 >X $87 >Y $e8 >P $0001 $23 >M $05b4 $47 >M $05b5 $01 >M $05b6 $f5 >M
T{ op PC S A X Y P -> $05b6 $c1 $32 $a7 $87 $e8 }T T{ $0001 M $05b4 M $05b5 M $05b6 M -> $23 $47 $01 $f5 }T
$d99b >PC $dd >S $20 >A $8d >X $7e >Y $6e >P $005e $62 >M $d99b $47 >M $d99c $5e >M $d99d $59 >M
T{ op PC S A X Y P -> $d99d $dd $20 $8d $7e $6e }T T{ $005e M $d99b M $d99c M $d99d M -> $62 $47 $5e $59 }T
$7ba3 >PC $c3 >S $2c >A $cc >X $8c >Y $24 >P $00a8 $56 >M $7ba3 $47 >M $7ba4 $a8 >M $7ba5 $f3 >M
T{ op PC S A X Y P -> $7ba5 $c3 $2c $cc $8c $24 }T T{ $00a8 M $7ba3 M $7ba4 M $7ba5 M -> $46 $47 $a8 $f3 }T
$57f5 >PC $80 >S $89 >A $cd >X $35 >Y $a7 >P $001d $05 >M $57f5 $47 >M $57f6 $1d >M $57f7 $ae >M
T{ op PC S A X Y P -> $57f7 $80 $89 $cd $35 $a7 }T T{ $001d M $57f5 M $57f6 M $57f7 M -> $05 $47 $1d $ae }T
$c7b5 >PC $32 >S $a8 >A $f9 >X $0b >Y $21 >P $005f $7c >M $c7b5 $47 >M $c7b6 $5f >M $c7b7 $84 >M
T{ op PC S A X Y P -> $c7b7 $32 $a8 $f9 $0b $21 }T T{ $005f M $c7b5 M $c7b6 M $c7b7 M -> $6c $47 $5f $84 }T
$6021 >PC $36 >S $a7 >A $8f >X $25 >Y $ef >P $0010 $7a >M $6021 $47 >M $6022 $10 >M $6023 $69 >M
T{ op PC S A X Y P -> $6023 $36 $a7 $8f $25 $ef }T T{ $0010 M $6021 M $6022 M $6023 M -> $6a $47 $10 $69 }T
$c68c >PC $ef >S $8c >A $ce >X $c8 >Y $62 >P $005f $cc >M $c68c $47 >M $c68d $5f >M $c68e $75 >M
T{ op PC S A X Y P -> $c68e $ef $8c $ce $c8 $62 }T T{ $005f M $c68c M $c68d M $c68e M -> $cc $47 $5f $75 }T
( 48 )
$c4a3 >PC $22 >S $94 >A $f5 >X $62 >Y $ee >P $c4a3 $48 >M $c4a4 $aa >M $c4a5 $f5 >M
T{ op PC S A X Y P -> $c4a4 $21 $94 $f5 $62 $ee }T T{ $0122 M $c4a3 M $c4a4 M $c4a5 M -> $94 $48 $aa $f5 }T
$2e16 >PC $3f >S $5a >A $84 >X $f0 >Y $6b >P $2e16 $48 >M $2e17 $9a >M $2e18 $5d >M
T{ op PC S A X Y P -> $2e17 $3e $5a $84 $f0 $6b }T T{ $013f M $2e16 M $2e17 M $2e18 M -> $5a $48 $9a $5d }T
$c635 >PC $38 >S $eb >A $c2 >X $5e >Y $e3 >P $c635 $48 >M $c636 $2d >M $c637 $e6 >M
T{ op PC S A X Y P -> $c636 $37 $eb $c2 $5e $e3 }T T{ $0138 M $c635 M $c636 M $c637 M -> $eb $48 $2d $e6 }T
$2e2b >PC $ca >S $62 >A $22 >X $63 >Y $ab >P $2e2b $48 >M $2e2c $7f >M $2e2d $bb >M
T{ op PC S A X Y P -> $2e2c $c9 $62 $22 $63 $ab }T T{ $01ca M $2e2b M $2e2c M $2e2d M -> $62 $48 $7f $bb }T
$3696 >PC $bc >S $58 >A $e2 >X $e7 >Y $24 >P $3696 $48 >M $3697 $d0 >M $3698 $ce >M
T{ op PC S A X Y P -> $3697 $bb $58 $e2 $e7 $24 }T T{ $01bc M $3696 M $3697 M $3698 M -> $58 $48 $d0 $ce }T
$e2b8 >PC $c5 >S $eb >A $9f >X $6d >Y $e9 >P $e2b8 $48 >M $e2b9 $b9 >M $e2ba $5a >M
T{ op PC S A X Y P -> $e2b9 $c4 $eb $9f $6d $e9 }T T{ $01c5 M $e2b8 M $e2b9 M $e2ba M -> $eb $48 $b9 $5a }T
$786a >PC $50 >S $49 >A $a5 >X $f0 >Y $28 >P $786a $48 >M $786b $c6 >M $786c $26 >M
T{ op PC S A X Y P -> $786b $4f $49 $a5 $f0 $28 }T T{ $0150 M $786a M $786b M $786c M -> $49 $48 $c6 $26 }T
$7c7c >PC $56 >S $fa >A $d5 >X $f5 >Y $ed >P $7c7c $48 >M $7c7d $a0 >M $7c7e $8f >M
T{ op PC S A X Y P -> $7c7d $55 $fa $d5 $f5 $ed }T T{ $0156 M $7c7c M $7c7d M $7c7e M -> $fa $48 $a0 $8f }T
$763e >PC $f5 >S $3a >A $02 >X $cc >Y $24 >P $763e $48 >M $763f $2f >M $7640 $5a >M
T{ op PC S A X Y P -> $763f $f4 $3a $02 $cc $24 }T T{ $01f5 M $763e M $763f M $7640 M -> $3a $48 $2f $5a }T
$4fee >PC $66 >S $fe >A $5d >X $78 >Y $a8 >P $4fee $48 >M $4fef $9e >M $4ff0 $be >M
T{ op PC S A X Y P -> $4fef $65 $fe $5d $78 $a8 }T T{ $0166 M $4fee M $4fef M $4ff0 M -> $fe $48 $9e $be }T
$d8b5 >PC $24 >S $fa >A $41 >X $b4 >Y $ee >P $d8b5 $48 >M $d8b6 $42 >M $d8b7 $8f >M
T{ op PC S A X Y P -> $d8b6 $23 $fa $41 $b4 $ee }T T{ $0124 M $d8b5 M $d8b6 M $d8b7 M -> $fa $48 $42 $8f }T
$e3b5 >PC $9a >S $45 >A $08 >X $0c >Y $ee >P $e3b5 $48 >M $e3b6 $5a >M $e3b7 $39 >M
T{ op PC S A X Y P -> $e3b6 $99 $45 $08 $0c $ee }T T{ $019a M $e3b5 M $e3b6 M $e3b7 M -> $45 $48 $5a $39 }T
$4485 >PC $5d >S $88 >A $cc >X $cf >Y $26 >P $4485 $48 >M $4486 $9c >M $4487 $2e >M
T{ op PC S A X Y P -> $4486 $5c $88 $cc $cf $26 }T T{ $015d M $4485 M $4486 M $4487 M -> $88 $48 $9c $2e }T
$df63 >PC $8b >S $3f >A $75 >X $b0 >Y $27 >P $df63 $48 >M $df64 $e9 >M $df65 $17 >M
T{ op PC S A X Y P -> $df64 $8a $3f $75 $b0 $27 }T T{ $018b M $df63 M $df64 M $df65 M -> $3f $48 $e9 $17 }T
$fbc3 >PC $6c >S $f9 >A $31 >X $f3 >Y $ac >P $fbc3 $48 >M $fbc4 $63 >M $fbc5 $3b >M
T{ op PC S A X Y P -> $fbc4 $6b $f9 $31 $f3 $ac }T T{ $016c M $fbc3 M $fbc4 M $fbc5 M -> $f9 $48 $63 $3b }T
$a2e1 >PC $03 >S $c9 >A $95 >X $6b >Y $ea >P $a2e1 $48 >M $a2e2 $bc >M $a2e3 $68 >M
T{ op PC S A X Y P -> $a2e2 $02 $c9 $95 $6b $ea }T T{ $0103 M $a2e1 M $a2e2 M $a2e3 M -> $c9 $48 $bc $68 }T
( 49 )
$97ac >PC $4c >S $4e >A $d3 >X $dc >Y $6e >P $97ac $49 >M $97ad $5b >M $97ae $d1 >M
T{ op PC S A X Y P -> $97ae $4c $15 $d3 $dc $6c }T T{ $97ac M $97ad M $97ae M -> $49 $5b $d1 }T
$d157 >PC $e3 >S $e8 >A $38 >X $89 >Y $23 >P $d157 $49 >M $d158 $63 >M $d159 $ed >M
T{ op PC S A X Y P -> $d159 $e3 $8b $38 $89 $a1 }T T{ $d157 M $d158 M $d159 M -> $49 $63 $ed }T
$faf3 >PC $ef >S $2c >A $db >X $70 >Y $27 >P $faf3 $49 >M $faf4 $ec >M $faf5 $12 >M
T{ op PC S A X Y P -> $faf5 $ef $c0 $db $70 $a5 }T T{ $faf3 M $faf4 M $faf5 M -> $49 $ec $12 }T
$636f >PC $62 >S $52 >A $69 >X $32 >Y $2f >P $636f $49 >M $6370 $27 >M $6371 $72 >M
T{ op PC S A X Y P -> $6371 $62 $75 $69 $32 $2d }T T{ $636f M $6370 M $6371 M -> $49 $27 $72 }T
$7df0 >PC $92 >S $a2 >A $8f >X $61 >Y $69 >P $7df0 $49 >M $7df1 $e6 >M $7df2 $28 >M
T{ op PC S A X Y P -> $7df2 $92 $44 $8f $61 $69 }T T{ $7df0 M $7df1 M $7df2 M -> $49 $e6 $28 }T
$710a >PC $65 >S $78 >A $f9 >X $06 >Y $69 >P $710a $49 >M $710b $d7 >M $710c $35 >M
T{ op PC S A X Y P -> $710c $65 $af $f9 $06 $e9 }T T{ $710a M $710b M $710c M -> $49 $d7 $35 }T
$e507 >PC $76 >S $93 >A $60 >X $68 >Y $29 >P $e507 $49 >M $e508 $72 >M $e509 $e3 >M
T{ op PC S A X Y P -> $e509 $76 $e1 $60 $68 $a9 }T T{ $e507 M $e508 M $e509 M -> $49 $72 $e3 }T
$5cdb >PC $51 >S $44 >A $d6 >X $e6 >Y $29 >P $5cdb $49 >M $5cdc $78 >M $5cdd $e1 >M
T{ op PC S A X Y P -> $5cdd $51 $3c $d6 $e6 $29 }T T{ $5cdb M $5cdc M $5cdd M -> $49 $78 $e1 }T
$f81d >PC $05 >S $5a >A $54 >X $96 >Y $a6 >P $f81d $49 >M $f81e $38 >M $f81f $9e >M
T{ op PC S A X Y P -> $f81f $05 $62 $54 $96 $24 }T T{ $f81d M $f81e M $f81f M -> $49 $38 $9e }T
$0a7c >PC $39 >S $d0 >A $eb >X $a6 >Y $6d >P $0a7c $49 >M $0a7d $a5 >M $0a7e $dc >M
T{ op PC S A X Y P -> $0a7e $39 $75 $eb $a6 $6d }T T{ $0a7c M $0a7d M $0a7e M -> $49 $a5 $dc }T
$05a4 >PC $78 >S $d7 >A $cb >X $86 >Y $e0 >P $05a4 $49 >M $05a5 $ca >M $05a6 $5f >M
T{ op PC S A X Y P -> $05a6 $78 $1d $cb $86 $60 }T T{ $05a4 M $05a5 M $05a6 M -> $49 $ca $5f }T
$49f1 >PC $18 >S $5a >A $a5 >X $c1 >Y $2d >P $49f1 $49 >M $49f2 $5d >M $49f3 $e0 >M
T{ op PC S A X Y P -> $49f3 $18 $07 $a5 $c1 $2d }T T{ $49f1 M $49f2 M $49f3 M -> $49 $5d $e0 }T
$e4b2 >PC $81 >S $9b >A $23 >X $31 >Y $6c >P $e4b2 $49 >M $e4b3 $02 >M $e4b4 $6d >M
T{ op PC S A X Y P -> $e4b4 $81 $99 $23 $31 $ec }T T{ $e4b2 M $e4b3 M $e4b4 M -> $49 $02 $6d }T
$cdc9 >PC $dc >S $84 >A $a0 >X $e5 >Y $2f >P $cdc9 $49 >M $cdca $3c >M $cdcb $bb >M
T{ op PC S A X Y P -> $cdcb $dc $b8 $a0 $e5 $ad }T T{ $cdc9 M $cdca M $cdcb M -> $49 $3c $bb }T
$576d >PC $fb >S $28 >A $f9 >X $52 >Y $24 >P $576d $49 >M $576e $b1 >M $576f $44 >M
T{ op PC S A X Y P -> $576f $fb $99 $f9 $52 $a4 }T T{ $576d M $576e M $576f M -> $49 $b1 $44 }T
$7eaa >PC $de >S $ac >A $d3 >X $41 >Y $e2 >P $7eaa $49 >M $7eab $9a >M $7eac $61 >M
T{ op PC S A X Y P -> $7eac $de $36 $d3 $41 $60 }T T{ $7eaa M $7eab M $7eac M -> $49 $9a $61 }T
( 4a )
$f74a >PC $42 >S $33 >A $c4 >X $eb >Y $68 >P $f74a $4a >M $f74b $02 >M $f74c $17 >M
T{ op PC S A X Y P -> $f74b $42 $19 $c4 $eb $69 }T T{ $f74a M $f74b M $f74c M -> $4a $02 $17 }T
$d885 >PC $58 >S $da >A $1a >X $90 >Y $6e >P $d885 $4a >M $d886 $fe >M $d887 $76 >M
T{ op PC S A X Y P -> $d886 $58 $6d $1a $90 $6c }T T{ $d885 M $d886 M $d887 M -> $4a $fe $76 }T
$b4e8 >PC $75 >S $18 >A $7f >X $c2 >Y $20 >P $b4e8 $4a >M $b4e9 $9a >M $b4ea $34 >M
T{ op PC S A X Y P -> $b4e9 $75 $0c $7f $c2 $20 }T T{ $b4e8 M $b4e9 M $b4ea M -> $4a $9a $34 }T
$f50d >PC $5e >S $6e >A $84 >X $86 >Y $29 >P $f50d $4a >M $f50e $1e >M $f50f $98 >M
T{ op PC S A X Y P -> $f50e $5e $37 $84 $86 $28 }T T{ $f50d M $f50e M $f50f M -> $4a $1e $98 }T
$7127 >PC $12 >S $26 >A $bc >X $42 >Y $ac >P $7127 $4a >M $7128 $66 >M $7129 $d9 >M
T{ op PC S A X Y P -> $7128 $12 $13 $bc $42 $2c }T T{ $7127 M $7128 M $7129 M -> $4a $66 $d9 }T
$d4d8 >PC $fa >S $64 >A $f1 >X $ee >Y $e7 >P $d4d8 $4a >M $d4d9 $c5 >M $d4da $eb >M
T{ op PC S A X Y P -> $d4d9 $fa $32 $f1 $ee $64 }T T{ $d4d8 M $d4d9 M $d4da M -> $4a $c5 $eb }T
$9540 >PC $a9 >S $2c >A $a4 >X $57 >Y $e5 >P $9540 $4a >M $9541 $c9 >M $9542 $10 >M
T{ op PC S A X Y P -> $9541 $a9 $16 $a4 $57 $64 }T T{ $9540 M $9541 M $9542 M -> $4a $c9 $10 }T
$7b7d >PC $7b >S $b4 >A $86 >X $43 >Y $25 >P $7b7d $4a >M $7b7e $81 >M $7b7f $db >M
T{ op PC S A X Y P -> $7b7e $7b $5a $86 $43 $24 }T T{ $7b7d M $7b7e M $7b7f M -> $4a $81 $db }T
$2cbc >PC $e3 >S $dc >A $bb >X $bc >Y $e7 >P $2cbc $4a >M $2cbd $c6 >M $2cbe $1c >M
T{ op PC S A X Y P -> $2cbd $e3 $6e $bb $bc $64 }T T{ $2cbc M $2cbd M $2cbe M -> $4a $c6 $1c }T
$aea0 >PC $a0 >S $c6 >A $3f >X $d3 >Y $ef >P $aea0 $4a >M $aea1 $74 >M $aea2 $fe >M
T{ op PC S A X Y P -> $aea1 $a0 $63 $3f $d3 $6c }T T{ $aea0 M $aea1 M $aea2 M -> $4a $74 $fe }T
$2756 >PC $46 >S $a1 >A $a5 >X $a0 >Y $e9 >P $2756 $4a >M $2757 $13 >M $2758 $9f >M
T{ op PC S A X Y P -> $2757 $46 $50 $a5 $a0 $69 }T T{ $2756 M $2757 M $2758 M -> $4a $13 $9f }T
$ed76 >PC $ad >S $6b >A $ec >X $37 >Y $eb >P $ed76 $4a >M $ed77 $29 >M $ed78 $13 >M
T{ op PC S A X Y P -> $ed77 $ad $35 $ec $37 $69 }T T{ $ed76 M $ed77 M $ed78 M -> $4a $29 $13 }T
$2fe4 >PC $c7 >S $96 >A $dc >X $e8 >Y $63 >P $2fe4 $4a >M $2fe5 $8d >M $2fe6 $92 >M
T{ op PC S A X Y P -> $2fe5 $c7 $4b $dc $e8 $60 }T T{ $2fe4 M $2fe5 M $2fe6 M -> $4a $8d $92 }T
$c7c5 >PC $92 >S $ab >A $45 >X $d0 >Y $a9 >P $c7c5 $4a >M $c7c6 $f9 >M $c7c7 $b1 >M
T{ op PC S A X Y P -> $c7c6 $92 $55 $45 $d0 $29 }T T{ $c7c5 M $c7c6 M $c7c7 M -> $4a $f9 $b1 }T
$f5ba >PC $c7 >S $06 >A $76 >X $89 >Y $20 >P $f5ba $4a >M $f5bb $11 >M $f5bc $df >M
T{ op PC S A X Y P -> $f5bb $c7 $03 $76 $89 $20 }T T{ $f5ba M $f5bb M $f5bc M -> $4a $11 $df }T
$f34c >PC $36 >S $ed >A $f4 >X $f1 >Y $ad >P $f34c $4a >M $f34d $85 >M $f34e $6e >M
T{ op PC S A X Y P -> $f34d $36 $76 $f4 $f1 $2d }T T{ $f34c M $f34d M $f34e M -> $4a $85 $6e }T
( 4b )
$d0b8 >PC $b2 >S $f9 >A $ed >X $ad >Y $65 >P $d0b8 $4b >M $d0b9 $2e >M $d0ba $8a >M
T{ op PC S A X Y P -> $d0b9 $b2 $f9 $ed $ad $65 }T T{ $d0b8 M $d0b9 M $d0ba M -> $4b $2e $8a }T
$645a >PC $ed >S $ee >A $27 >X $9e >Y $24 >P $645a $4b >M $645b $09 >M $645c $ca >M
T{ op PC S A X Y P -> $645b $ed $ee $27 $9e $24 }T T{ $645a M $645b M $645c M -> $4b $09 $ca }T
$128c >PC $1c >S $8b >A $0e >X $5d >Y $20 >P $128c $4b >M $128d $05 >M $128e $9a >M
T{ op PC S A X Y P -> $128d $1c $8b $0e $5d $20 }T T{ $128c M $128d M $128e M -> $4b $05 $9a }T
$3e0b >PC $18 >S $3e >A $62 >X $7d >Y $a1 >P $3e0b $4b >M $3e0c $82 >M $3e0d $12 >M
T{ op PC S A X Y P -> $3e0c $18 $3e $62 $7d $a1 }T T{ $3e0b M $3e0c M $3e0d M -> $4b $82 $12 }T
$4fb2 >PC $56 >S $b3 >A $e9 >X $19 >Y $a9 >P $4fb2 $4b >M $4fb3 $e0 >M $4fb4 $aa >M
T{ op PC S A X Y P -> $4fb3 $56 $b3 $e9 $19 $a9 }T T{ $4fb2 M $4fb3 M $4fb4 M -> $4b $e0 $aa }T
$cf13 >PC $15 >S $c9 >A $64 >X $ad >Y $a9 >P $cf13 $4b >M $cf14 $e1 >M $cf15 $e0 >M
T{ op PC S A X Y P -> $cf14 $15 $c9 $64 $ad $a9 }T T{ $cf13 M $cf14 M $cf15 M -> $4b $e1 $e0 }T
$a972 >PC $c3 >S $f0 >A $d1 >X $b9 >Y $e7 >P $a972 $4b >M $a973 $3c >M $a974 $9b >M
T{ op PC S A X Y P -> $a973 $c3 $f0 $d1 $b9 $e7 }T T{ $a972 M $a973 M $a974 M -> $4b $3c $9b }T
$1289 >PC $6f >S $1d >A $71 >X $bc >Y $27 >P $1289 $4b >M $128a $cc >M $128b $87 >M
T{ op PC S A X Y P -> $128a $6f $1d $71 $bc $27 }T T{ $1289 M $128a M $128b M -> $4b $cc $87 }T
$3ec0 >PC $2d >S $8a >A $96 >X $21 >Y $eb >P $3ec0 $4b >M $3ec1 $4c >M $3ec2 $14 >M
T{ op PC S A X Y P -> $3ec1 $2d $8a $96 $21 $eb }T T{ $3ec0 M $3ec1 M $3ec2 M -> $4b $4c $14 }T
$e393 >PC $af >S $f7 >A $f3 >X $02 >Y $a5 >P $e393 $4b >M $e394 $8b >M $e395 $97 >M
T{ op PC S A X Y P -> $e394 $af $f7 $f3 $02 $a5 }T T{ $e393 M $e394 M $e395 M -> $4b $8b $97 }T
$b0cc >PC $3f >S $81 >A $5f >X $bb >Y $64 >P $b0cc $4b >M $b0cd $5d >M $b0ce $cb >M
T{ op PC S A X Y P -> $b0cd $3f $81 $5f $bb $64 }T T{ $b0cc M $b0cd M $b0ce M -> $4b $5d $cb }T
$7a94 >PC $0f >S $e3 >A $f2 >X $74 >Y $26 >P $7a94 $4b >M $7a95 $0c >M $7a96 $27 >M
T{ op PC S A X Y P -> $7a95 $0f $e3 $f2 $74 $26 }T T{ $7a94 M $7a95 M $7a96 M -> $4b $0c $27 }T
$12fa >PC $cb >S $63 >A $12 >X $72 >Y $ef >P $12fa $4b >M $12fb $7e >M $12fc $5f >M
T{ op PC S A X Y P -> $12fb $cb $63 $12 $72 $ef }T T{ $12fa M $12fb M $12fc M -> $4b $7e $5f }T
$a5c6 >PC $5c >S $7a >A $02 >X $b2 >Y $ad >P $a5c6 $4b >M $a5c7 $32 >M $a5c8 $04 >M
T{ op PC S A X Y P -> $a5c7 $5c $7a $02 $b2 $ad }T T{ $a5c6 M $a5c7 M $a5c8 M -> $4b $32 $04 }T
$c7cc >PC $d6 >S $92 >A $e6 >X $d4 >Y $61 >P $c7cc $4b >M $c7cd $7a >M $c7ce $90 >M
T{ op PC S A X Y P -> $c7cd $d6 $92 $e6 $d4 $61 }T T{ $c7cc M $c7cd M $c7ce M -> $4b $7a $90 }T
$efbc >PC $61 >S $8d >A $a4 >X $c9 >Y $ab >P $efbc $4b >M $efbd $9d >M $efbe $c4 >M
T{ op PC S A X Y P -> $efbd $61 $8d $a4 $c9 $ab }T T{ $efbc M $efbd M $efbe M -> $4b $9d $c4 }T
( 4c )
$bc7b >PC $41 >S $5c >A $73 >X $33 >Y $6f >P $bc7b $4c >M $bc7c $ba >M $bc7d $d0 >M $d0ba $e7 >M
T{ op PC S A X Y P -> $d0ba $41 $5c $73 $33 $6f }T T{ $bc7b M $bc7c M $bc7d M $d0ba M -> $4c $ba $d0 $e7 }T
$778a >PC $0e >S $41 >A $32 >X $bb >Y $6f >P $177f $c1 >M $778a $4c >M $778b $7f >M $778c $17 >M
T{ op PC S A X Y P -> $177f $0e $41 $32 $bb $6f }T T{ $177f M $778a M $778b M $778c M -> $c1 $4c $7f $17 }T
$54fc >PC $5e >S $1b >A $f3 >X $79 >Y $20 >P $54fc $4c >M $54fd $0f >M $54fe $93 >M $930f $52 >M
T{ op PC S A X Y P -> $930f $5e $1b $f3 $79 $20 }T T{ $54fc M $54fd M $54fe M $930f M -> $4c $0f $93 $52 }T
$bed0 >PC $1e >S $21 >A $d8 >X $14 >Y $65 >P $6675 $a1 >M $bed0 $4c >M $bed1 $75 >M $bed2 $66 >M
T{ op PC S A X Y P -> $6675 $1e $21 $d8 $14 $65 }T T{ $6675 M $bed0 M $bed1 M $bed2 M -> $a1 $4c $75 $66 }T
$39b3 >PC $e1 >S $c3 >A $92 >X $75 >Y $2e >P $39b3 $4c >M $39b4 $a1 >M $39b5 $64 >M $64a1 $80 >M
T{ op PC S A X Y P -> $64a1 $e1 $c3 $92 $75 $2e }T T{ $39b3 M $39b4 M $39b5 M $64a1 M -> $4c $a1 $64 $80 }T
$9799 >PC $22 >S $e8 >A $b3 >X $97 >Y $ee >P $9799 $4c >M $979a $1f >M $979b $d6 >M $d61f $3a >M
T{ op PC S A X Y P -> $d61f $22 $e8 $b3 $97 $ee }T T{ $9799 M $979a M $979b M $d61f M -> $4c $1f $d6 $3a }T
$cd80 >PC $4d >S $91 >A $55 >X $65 >Y $ee >P $013d $a0 >M $cd80 $4c >M $cd81 $3d >M $cd82 $01 >M
T{ op PC S A X Y P -> $013d $4d $91 $55 $65 $ee }T T{ $013d M $cd80 M $cd81 M $cd82 M -> $a0 $4c $3d $01 }T
$e1b9 >PC $74 >S $cf >A $1f >X $70 >Y $e0 >P $d5bc $0e >M $e1b9 $4c >M $e1ba $bc >M $e1bb $d5 >M
T{ op PC S A X Y P -> $d5bc $74 $cf $1f $70 $e0 }T T{ $d5bc M $e1b9 M $e1ba M $e1bb M -> $0e $4c $bc $d5 }T
$fdb1 >PC $6e >S $69 >A $38 >X $07 >Y $a9 >P $5869 $6c >M $fdb1 $4c >M $fdb2 $69 >M $fdb3 $58 >M
T{ op PC S A X Y P -> $5869 $6e $69 $38 $07 $a9 }T T{ $5869 M $fdb1 M $fdb2 M $fdb3 M -> $6c $4c $69 $58 }T
$ff16 >PC $84 >S $e4 >A $14 >X $ad >Y $61 >P $f268 $66 >M $ff16 $4c >M $ff17 $68 >M $ff18 $f2 >M
T{ op PC S A X Y P -> $f268 $84 $e4 $14 $ad $61 }T T{ $f268 M $ff16 M $ff17 M $ff18 M -> $66 $4c $68 $f2 }T
$9cfd >PC $dd >S $14 >A $4c >X $5c >Y $ec >P $9cfd $4c >M $9cfe $50 >M $9cff $be >M $be50 $58 >M
T{ op PC S A X Y P -> $be50 $dd $14 $4c $5c $ec }T T{ $9cfd M $9cfe M $9cff M $be50 M -> $4c $50 $be $58 }T
$8b56 >PC $9a >S $01 >A $93 >X $37 >Y $63 >P $5daf $ca >M $8b56 $4c >M $8b57 $af >M $8b58 $5d >M
T{ op PC S A X Y P -> $5daf $9a $01 $93 $37 $63 }T T{ $5daf M $8b56 M $8b57 M $8b58 M -> $ca $4c $af $5d }T
$cedc >PC $0d >S $e6 >A $01 >X $c2 >Y $60 >P $2ff9 $ff >M $cedc $4c >M $cedd $f9 >M $cede $2f >M
T{ op PC S A X Y P -> $2ff9 $0d $e6 $01 $c2 $60 }T T{ $2ff9 M $cedc M $cedd M $cede M -> $ff $4c $f9 $2f }T
$fb6f >PC $7b >S $c8 >A $68 >X $a8 >Y $a1 >P $5ef7 $b4 >M $fb6f $4c >M $fb70 $f7 >M $fb71 $5e >M
T{ op PC S A X Y P -> $5ef7 $7b $c8 $68 $a8 $a1 }T T{ $5ef7 M $fb6f M $fb70 M $fb71 M -> $b4 $4c $f7 $5e }T
$25d6 >PC $36 >S $35 >A $12 >X $cc >Y $a4 >P $25d6 $4c >M $25d7 $98 >M $25d8 $fa >M $fa98 $0e >M
T{ op PC S A X Y P -> $fa98 $36 $35 $12 $cc $a4 }T T{ $25d6 M $25d7 M $25d8 M $fa98 M -> $4c $98 $fa $0e }T
$c423 >PC $55 >S $5b >A $14 >X $30 >Y $2b >P $755d $3b >M $c423 $4c >M $c424 $5d >M $c425 $75 >M
T{ op PC S A X Y P -> $755d $55 $5b $14 $30 $2b }T T{ $755d M $c423 M $c424 M $c425 M -> $3b $4c $5d $75 }T
( 4d )
$9c1a >PC $ca >S $22 >A $00 >X $35 >Y $a7 >P $8a51 $a0 >M $9c1a $4d >M $9c1b $51 >M $9c1c $8a >M $9c1d $ad >M
T{ op PC S A X Y P -> $9c1d $ca $82 $00 $35 $a5 }T T{ $8a51 M $9c1a M $9c1b M $9c1c M $9c1d M -> $a0 $4d $51 $8a $ad }T
$543a >PC $6c >S $9e >A $7f >X $2d >Y $60 >P $543a $4d >M $543b $13 >M $543c $cd >M $543d $c4 >M $cd13 $d8 >M
T{ op PC S A X Y P -> $543d $6c $46 $7f $2d $60 }T T{ $543a M $543b M $543c M $543d M $cd13 M -> $4d $13 $cd $c4 $d8 }T
$5158 >PC $0e >S $13 >A $b7 >X $a7 >Y $eb >P $5158 $4d >M $5159 $f7 >M $515a $d6 >M $515b $14 >M $d6f7 $d6 >M
T{ op PC S A X Y P -> $515b $0e $c5 $b7 $a7 $e9 }T T{ $5158 M $5159 M $515a M $515b M $d6f7 M -> $4d $f7 $d6 $14 $d6 }T
$b873 >PC $68 >S $be >A $79 >X $9c >Y $68 >P $b873 $4d >M $b874 $9b >M $b875 $f1 >M $b876 $ce >M $f19b $c1 >M
T{ op PC S A X Y P -> $b876 $68 $7f $79 $9c $68 }T T{ $b873 M $b874 M $b875 M $b876 M $f19b M -> $4d $9b $f1 $ce $c1 }T
$53e8 >PC $ee >S $27 >A $55 >X $77 >Y $65 >P $1602 $da >M $53e8 $4d >M $53e9 $02 >M $53ea $16 >M $53eb $30 >M
T{ op PC S A X Y P -> $53eb $ee $fd $55 $77 $e5 }T T{ $1602 M $53e8 M $53e9 M $53ea M $53eb M -> $da $4d $02 $16 $30 }T
$4db4 >PC $71 >S $5d >A $a6 >X $ef >Y $23 >P $4db4 $4d >M $4db5 $29 >M $4db6 $90 >M $4db7 $9f >M $9029 $5a >M
T{ op PC S A X Y P -> $4db7 $71 $07 $a6 $ef $21 }T T{ $4db4 M $4db5 M $4db6 M $4db7 M $9029 M -> $4d $29 $90 $9f $5a }T
$408e >PC $07 >S $35 >A $45 >X $de >Y $64 >P $408e $4d >M $408f $20 >M $4090 $df >M $4091 $5d >M $df20 $2a >M
T{ op PC S A X Y P -> $4091 $07 $1f $45 $de $64 }T T{ $408e M $408f M $4090 M $4091 M $df20 M -> $4d $20 $df $5d $2a }T
$3b36 >PC $73 >S $7e >A $26 >X $58 >Y $2b >P $3b36 $4d >M $3b37 $0d >M $3b38 $6e >M $3b39 $7d >M $6e0d $8e >M
T{ op PC S A X Y P -> $3b39 $73 $f0 $26 $58 $a9 }T T{ $3b36 M $3b37 M $3b38 M $3b39 M $6e0d M -> $4d $0d $6e $7d $8e }T
$7984 >PC $7f >S $f2 >A $b9 >X $98 >Y $2b >P $2a87 $5a >M $7984 $4d >M $7985 $87 >M $7986 $2a >M $7987 $6f >M
T{ op PC S A X Y P -> $7987 $7f $a8 $b9 $98 $a9 }T T{ $2a87 M $7984 M $7985 M $7986 M $7987 M -> $5a $4d $87 $2a $6f }T
$812d >PC $e4 >S $08 >A $1b >X $7e >Y $a5 >P $3873 $d9 >M $812d $4d >M $812e $73 >M $812f $38 >M $8130 $43 >M
T{ op PC S A X Y P -> $8130 $e4 $d1 $1b $7e $a5 }T T{ $3873 M $812d M $812e M $812f M $8130 M -> $d9 $4d $73 $38 $43 }T
$b500 >PC $50 >S $6c >A $83 >X $48 >Y $ea >P $b500 $4d >M $b501 $76 >M $b502 $e1 >M $b503 $de >M $e176 $73 >M
T{ op PC S A X Y P -> $b503 $50 $1f $83 $48 $68 }T T{ $b500 M $b501 M $b502 M $b503 M $e176 M -> $4d $76 $e1 $de $73 }T
$2c5d >PC $fe >S $41 >A $31 >X $1c >Y $a2 >P $2c5d $4d >M $2c5e $64 >M $2c5f $ff >M $2c60 $27 >M $ff64 $17 >M
T{ op PC S A X Y P -> $2c60 $fe $56 $31 $1c $20 }T T{ $2c5d M $2c5e M $2c5f M $2c60 M $ff64 M -> $4d $64 $ff $27 $17 }T
$4e59 >PC $2f >S $99 >A $0f >X $82 >Y $a4 >P $4e59 $4d >M $4e5a $24 >M $4e5b $77 >M $4e5c $e6 >M $7724 $ac >M
T{ op PC S A X Y P -> $4e5c $2f $35 $0f $82 $24 }T T{ $4e59 M $4e5a M $4e5b M $4e5c M $7724 M -> $4d $24 $77 $e6 $ac }T
$e44e >PC $44 >S $ed >A $28 >X $ba >Y $27 >P $3c9e $09 >M $e44e $4d >M $e44f $9e >M $e450 $3c >M $e451 $07 >M
T{ op PC S A X Y P -> $e451 $44 $e4 $28 $ba $a5 }T T{ $3c9e M $e44e M $e44f M $e450 M $e451 M -> $09 $4d $9e $3c $07 }T
$af51 >PC $68 >S $3f >A $fa >X $6d >Y $22 >P $778e $31 >M $af51 $4d >M $af52 $8e >M $af53 $77 >M $af54 $c8 >M
T{ op PC S A X Y P -> $af54 $68 $0e $fa $6d $20 }T T{ $778e M $af51 M $af52 M $af53 M $af54 M -> $31 $4d $8e $77 $c8 }T
$e9e0 >PC $d5 >S $bb >A $ca >X $1a >Y $65 >P $e9e0 $4d >M $e9e1 $85 >M $e9e2 $ff >M $e9e3 $20 >M $ff85 $47 >M
T{ op PC S A X Y P -> $e9e3 $d5 $fc $ca $1a $e5 }T T{ $e9e0 M $e9e1 M $e9e2 M $e9e3 M $ff85 M -> $4d $85 $ff $20 $47 }T
( 4e )
$cd7d >PC $d1 >S $b3 >A $ec >X $da >Y $ee >P $bf14 $16 >M $cd7d $4e >M $cd7e $14 >M $cd7f $bf >M $cd80 $8e >M
T{ op PC S A X Y P -> $cd80 $d1 $b3 $ec $da $6c }T T{ $bf14 M $cd7d M $cd7e M $cd7f M $cd80 M -> $0b $4e $14 $bf $8e }T
$7183 >PC $0f >S $47 >A $b9 >X $b7 >Y $e5 >P $0615 $ca >M $7183 $4e >M $7184 $15 >M $7185 $06 >M $7186 $91 >M
T{ op PC S A X Y P -> $7186 $0f $47 $b9 $b7 $64 }T T{ $0615 M $7183 M $7184 M $7185 M $7186 M -> $65 $4e $15 $06 $91 }T
$ecba >PC $f8 >S $55 >A $73 >X $81 >Y $a1 >P $37f5 $7e >M $ecba $4e >M $ecbb $f5 >M $ecbc $37 >M $ecbd $27 >M
T{ op PC S A X Y P -> $ecbd $f8 $55 $73 $81 $20 }T T{ $37f5 M $ecba M $ecbb M $ecbc M $ecbd M -> $3f $4e $f5 $37 $27 }T
$9874 >PC $d6 >S $ed >A $7a >X $79 >Y $28 >P $90d7 $56 >M $9874 $4e >M $9875 $d7 >M $9876 $90 >M $9877 $23 >M
T{ op PC S A X Y P -> $9877 $d6 $ed $7a $79 $28 }T T{ $90d7 M $9874 M $9875 M $9876 M $9877 M -> $2b $4e $d7 $90 $23 }T
$931c >PC $20 >S $46 >A $6f >X $94 >Y $ac >P $931c $4e >M $931d $43 >M $931e $e5 >M $931f $fc >M $e543 $66 >M
T{ op PC S A X Y P -> $931f $20 $46 $6f $94 $2c }T T{ $931c M $931d M $931e M $931f M $e543 M -> $4e $43 $e5 $fc $33 }T
$89ac >PC $b7 >S $2d >A $17 >X $77 >Y $a7 >P $89ac $4e >M $89ad $c7 >M $89ae $eb >M $89af $4d >M $ebc7 $d8 >M
T{ op PC S A X Y P -> $89af $b7 $2d $17 $77 $24 }T T{ $89ac M $89ad M $89ae M $89af M $ebc7 M -> $4e $c7 $eb $4d $6c }T
$1ad6 >PC $32 >S $c3 >A $f6 >X $4a >Y $af >P $1ad6 $4e >M $1ad7 $aa >M $1ad8 $49 >M $1ad9 $af >M $49aa $54 >M
T{ op PC S A X Y P -> $1ad9 $32 $c3 $f6 $4a $2c }T T{ $1ad6 M $1ad7 M $1ad8 M $1ad9 M $49aa M -> $4e $aa $49 $af $2a }T
$319a >PC $d8 >S $05 >A $59 >X $48 >Y $e0 >P $319a $4e >M $319b $35 >M $319c $bd >M $319d $84 >M $bd35 $26 >M
T{ op PC S A X Y P -> $319d $d8 $05 $59 $48 $60 }T T{ $319a M $319b M $319c M $319d M $bd35 M -> $4e $35 $bd $84 $13 }T
$a44c >PC $57 >S $8e >A $b0 >X $e2 >Y $25 >P $a44c $4e >M $a44d $ed >M $a44e $f8 >M $a44f $64 >M $f8ed $69 >M
T{ op PC S A X Y P -> $a44f $57 $8e $b0 $e2 $25 }T T{ $a44c M $a44d M $a44e M $a44f M $f8ed M -> $4e $ed $f8 $64 $34 }T
$641c >PC $12 >S $f4 >A $d0 >X $1b >Y $ae >P $39b7 $8a >M $641c $4e >M $641d $b7 >M $641e $39 >M $641f $96 >M
T{ op PC S A X Y P -> $641f $12 $f4 $d0 $1b $2c }T T{ $39b7 M $641c M $641d M $641e M $641f M -> $45 $4e $b7 $39 $96 }T
$80c4 >PC $a6 >S $6c >A $a8 >X $42 >Y $66 >P $1706 $65 >M $80c4 $4e >M $80c5 $06 >M $80c6 $17 >M $80c7 $6f >M
T{ op PC S A X Y P -> $80c7 $a6 $6c $a8 $42 $65 }T T{ $1706 M $80c4 M $80c5 M $80c6 M $80c7 M -> $32 $4e $06 $17 $6f }T
$15c8 >PC $b4 >S $c5 >A $ee >X $6b >Y $ed >P $15c8 $4e >M $15c9 $eb >M $15ca $5c >M $15cb $22 >M $5ceb $89 >M
T{ op PC S A X Y P -> $15cb $b4 $c5 $ee $6b $6d }T T{ $15c8 M $15c9 M $15ca M $15cb M $5ceb M -> $4e $eb $5c $22 $44 }T
$9760 >PC $58 >S $f5 >A $21 >X $81 >Y $6c >P $1771 $36 >M $9760 $4e >M $9761 $71 >M $9762 $17 >M $9763 $51 >M
T{ op PC S A X Y P -> $9763 $58 $f5 $21 $81 $6c }T T{ $1771 M $9760 M $9761 M $9762 M $9763 M -> $1b $4e $71 $17 $51 }T
$61fb >PC $8e >S $a1 >A $cb >X $94 >Y $2a >P $61fb $4e >M $61fc $52 >M $61fd $b9 >M $61fe $f5 >M $b952 $d4 >M
T{ op PC S A X Y P -> $61fe $8e $a1 $cb $94 $28 }T T{ $61fb M $61fc M $61fd M $61fe M $b952 M -> $4e $52 $b9 $f5 $6a }T
$88be >PC $08 >S $62 >A $91 >X $24 >Y $ae >P $741f $0f >M $88be $4e >M $88bf $1f >M $88c0 $74 >M $88c1 $b0 >M
T{ op PC S A X Y P -> $88c1 $08 $62 $91 $24 $2d }T T{ $741f M $88be M $88bf M $88c0 M $88c1 M -> $07 $4e $1f $74 $b0 }T
$c607 >PC $d7 >S $6f >A $4b >X $ab >Y $a0 >P $5bd8 $dd >M $c607 $4e >M $c608 $d8 >M $c609 $5b >M $c60a $f1 >M
T{ op PC S A X Y P -> $c60a $d7 $6f $4b $ab $21 }T T{ $5bd8 M $c607 M $c608 M $c609 M $c60a M -> $6e $4e $d8 $5b $f1 }T
( 4f )
$bf68 >PC $54 >S $f1 >A $9f >X $21 >Y $64 >P $000d $bf >M $bf13 $ce >M $bf68 $4f >M $bf69 $0d >M $bf6a $a8 >M $bf6b $e8 >M
T{ op PC S A X Y P -> $bf6b $54 $f1 $9f $21 $64 }T T{ $000d M $bf13 M $bf68 M $bf69 M $bf6a M $bf6b M -> $bf $ce $4f $0d $a8 $e8 }T
$df74 >PC $70 >S $53 >A $c7 >X $70 >Y $6b >P $005d $2a >M $df5a $c3 >M $df74 $4f >M $df75 $5d >M $df76 $e3 >M
T{ op PC S A X Y P -> $df5a $70 $53 $c7 $70 $6b }T T{ $005d M $df5a M $df74 M $df75 M $df76 M -> $2a $c3 $4f $5d $e3 }T
$d5cd >PC $22 >S $fe >A $48 >X $c5 >Y $a2 >P $00f2 $ee >M $d542 $96 >M $d5cd $4f >M $d5ce $f2 >M $d5cf $72 >M $d642 $14 >M
T{ op PC S A X Y P -> $d642 $22 $fe $48 $c5 $a2 }T T{ $00f2 M $d542 M $d5cd M $d5ce M $d5cf M $d642 M -> $ee $96 $4f $f2 $72 $14 }T
$4a04 >PC $99 >S $6d >A $59 >X $ee >Y $af >P $0020 $d9 >M $4a04 $4f >M $4a05 $20 >M $4a06 $6b >M $4a07 $a0 >M $4a72 $93 >M
T{ op PC S A X Y P -> $4a07 $99 $6d $59 $ee $af }T T{ $0020 M $4a04 M $4a05 M $4a06 M $4a07 M $4a72 M -> $d9 $4f $20 $6b $a0 $93 }T
$c59b >PC $2f >S $b4 >A $9f >X $3a >Y $a9 >P $00b8 $e7 >M $c519 $04 >M $c59b $4f >M $c59c $b8 >M $c59d $7b >M $c619 $08 >M
T{ op PC S A X Y P -> $c619 $2f $b4 $9f $3a $a9 }T T{ $00b8 M $c519 M $c59b M $c59c M $c59d M $c619 M -> $e7 $04 $4f $b8 $7b $08 }T
$5e03 >PC $8a >S $1b >A $bd >X $5e >Y $63 >P $00fa $f3 >M $5e03 $4f >M $5e04 $fa >M $5e05 $9e >M $5e06 $fa >M $5ea4 $d3 >M
T{ op PC S A X Y P -> $5e06 $8a $1b $bd $5e $63 }T T{ $00fa M $5e03 M $5e04 M $5e05 M $5e06 M $5ea4 M -> $f3 $4f $fa $9e $fa $d3 }T
$5c2a >PC $2a >S $2c >A $66 >X $0f >Y $22 >P $0050 $1b >M $5c2a $4f >M $5c2b $50 >M $5c2c $41 >M $5c2d $1f >M $5c6e $56 >M
T{ op PC S A X Y P -> $5c2d $2a $2c $66 $0f $22 }T T{ $0050 M $5c2a M $5c2b M $5c2c M $5c2d M $5c6e M -> $1b $4f $50 $41 $1f $56 }T
$a586 >PC $a6 >S $99 >A $c7 >X $f4 >Y $ae >P $00ad $6c >M $a586 $4f >M $a587 $ad >M $a588 $2a >M $a5b3 $11 >M
T{ op PC S A X Y P -> $a5b3 $a6 $99 $c7 $f4 $ae }T T{ $00ad M $a586 M $a587 M $a588 M $a5b3 M -> $6c $4f $ad $2a $11 }T
$8c8c >PC $ab >S $5d >A $8f >X $9b >Y $61 >P $002f $63 >M $8c36 $54 >M $8c8c $4f >M $8c8d $2f >M $8c8e $a7 >M
T{ op PC S A X Y P -> $8c36 $ab $5d $8f $9b $61 }T T{ $002f M $8c36 M $8c8c M $8c8d M $8c8e M -> $63 $54 $4f $2f $a7 }T
$4b45 >PC $24 >S $04 >A $77 >X $2f >Y $62 >P $00ac $0a >M $4ae3 $fc >M $4b45 $4f >M $4b46 $ac >M $4b47 $9b >M $4be3 $14 >M
T{ op PC S A X Y P -> $4ae3 $24 $04 $77 $2f $62 }T T{ $00ac M $4ae3 M $4b45 M $4b46 M $4b47 M $4be3 M -> $0a $fc $4f $ac $9b $14 }T
$5eee >PC $1f >S $65 >A $9c >X $13 >Y $e3 >P $0024 $cf >M $5e2a $ec >M $5eee $4f >M $5eef $24 >M $5ef0 $39 >M $5f2a $eb >M
T{ op PC S A X Y P -> $5f2a $1f $65 $9c $13 $e3 }T T{ $0024 M $5e2a M $5eee M $5eef M $5ef0 M $5f2a M -> $cf $ec $4f $24 $39 $eb }T
$b4bd >PC $93 >S $76 >A $c3 >X $c2 >Y $e6 >P $00f5 $a2 >M $b4bd $4f >M $b4be $f5 >M $b4bf $05 >M $b4c5 $98 >M
T{ op PC S A X Y P -> $b4c5 $93 $76 $c3 $c2 $e6 }T T{ $00f5 M $b4bd M $b4be M $b4bf M $b4c5 M -> $a2 $4f $f5 $05 $98 }T
$b499 >PC $56 >S $23 >A $42 >X $50 >Y $e7 >P $00d8 $04 >M $b478 $07 >M $b499 $4f >M $b49a $d8 >M $b49b $dc >M
T{ op PC S A X Y P -> $b478 $56 $23 $42 $50 $e7 }T T{ $00d8 M $b478 M $b499 M $b49a M $b49b M -> $04 $07 $4f $d8 $dc }T
$a7c1 >PC $f4 >S $ec >A $80 >X $d7 >Y $24 >P $0025 $b5 >M $a7c1 $4f >M $a7c2 $25 >M $a7c3 $2f >M $a7c4 $16 >M $a7f3 $57 >M
T{ op PC S A X Y P -> $a7c4 $f4 $ec $80 $d7 $24 }T T{ $0025 M $a7c1 M $a7c2 M $a7c3 M $a7c4 M $a7f3 M -> $b5 $4f $25 $2f $16 $57 }T
$6731 >PC $57 >S $4a >A $9e >X $a0 >Y $e8 >P $002f $3f >M $6731 $4f >M $6732 $2f >M $6733 $3b >M $6734 $a8 >M $676f $2c >M
T{ op PC S A X Y P -> $6734 $57 $4a $9e $a0 $e8 }T T{ $002f M $6731 M $6732 M $6733 M $6734 M $676f M -> $3f $4f $2f $3b $a8 $2c }T
$cca9 >PC $a8 >S $57 >A $6f >X $44 >Y $e2 >P $00fe $25 >M $cc6b $b8 >M $cca9 $4f >M $ccaa $fe >M $ccab $bf >M
T{ op PC S A X Y P -> $cc6b $a8 $57 $6f $44 $e2 }T T{ $00fe M $cc6b M $cca9 M $ccaa M $ccab M -> $25 $b8 $4f $fe $bf }T
( 50 )
$8ddd >PC $8c >S $6f >A $5c >X $36 >Y $e0 >P $8ddd $50 >M $8dde $20 >M $8ddf $5f >M
T{ op PC S A X Y P -> $8ddf $8c $6f $5c $36 $e0 }T T{ $8ddd M $8dde M $8ddf M -> $50 $20 $5f }T
$a882 >PC $42 >S $59 >A $51 >X $40 >Y $22 >P $a882 $50 >M $a883 $62 >M $a884 $44 >M $a8e6 $c9 >M
T{ op PC S A X Y P -> $a8e6 $42 $59 $51 $40 $22 }T T{ $a882 M $a883 M $a884 M $a8e6 M -> $50 $62 $44 $c9 }T
$5566 >PC $1e >S $5c >A $a0 >X $a1 >Y $24 >P $5547 $92 >M $5566 $50 >M $5567 $df >M $5568 $a1 >M
T{ op PC S A X Y P -> $5547 $1e $5c $a0 $a1 $24 }T T{ $5547 M $5566 M $5567 M $5568 M -> $92 $50 $df $a1 }T
$18e3 >PC $9c >S $5e >A $a5 >X $1b >Y $e3 >P $18e3 $50 >M $18e4 $3f >M $18e5 $02 >M
T{ op PC S A X Y P -> $18e5 $9c $5e $a5 $1b $e3 }T T{ $18e3 M $18e4 M $18e5 M -> $50 $3f $02 }T
$64e3 >PC $14 >S $e8 >A $a8 >X $e9 >Y $27 >P $6414 $24 >M $64e3 $50 >M $64e4 $2f >M $64e5 $a1 >M $6514 $95 >M
T{ op PC S A X Y P -> $6514 $14 $e8 $a8 $e9 $27 }T T{ $6414 M $64e3 M $64e4 M $64e5 M $6514 M -> $24 $50 $2f $a1 $95 }T
$a5b6 >PC $d8 >S $13 >A $7b >X $9e >Y $63 >P $a5b6 $50 >M $a5b7 $d5 >M $a5b8 $1b >M
T{ op PC S A X Y P -> $a5b8 $d8 $13 $7b $9e $63 }T T{ $a5b6 M $a5b7 M $a5b8 M -> $50 $d5 $1b }T
$33ff >PC $8c >S $f4 >A $a8 >X $f3 >Y $23 >P $33c4 $16 >M $33ff $50 >M $3400 $c3 >M $3401 $f6 >M $34c4 $5c >M
T{ op PC S A X Y P -> $33c4 $8c $f4 $a8 $f3 $23 }T T{ $33c4 M $33ff M $3400 M $3401 M $34c4 M -> $16 $50 $c3 $f6 $5c }T
$5ddf >PC $63 >S $e3 >A $f6 >X $8c >Y $6d >P $5ddf $50 >M $5de0 $b9 >M $5de1 $81 >M
T{ op PC S A X Y P -> $5de1 $63 $e3 $f6 $8c $6d }T T{ $5ddf M $5de0 M $5de1 M -> $50 $b9 $81 }T
$0f2a >PC $b3 >S $1b >A $a0 >X $03 >Y $61 >P $0f2a $50 >M $0f2b $9d >M $0f2c $ac >M
T{ op PC S A X Y P -> $0f2c $b3 $1b $a0 $03 $61 }T T{ $0f2a M $0f2b M $0f2c M -> $50 $9d $ac }T
$b4c4 >PC $3b >S $15 >A $8d >X $ce >Y $e3 >P $b4c4 $50 >M $b4c5 $8d >M $b4c6 $6e >M
T{ op PC S A X Y P -> $b4c6 $3b $15 $8d $ce $e3 }T T{ $b4c4 M $b4c5 M $b4c6 M -> $50 $8d $6e }T
$55ad >PC $c5 >S $89 >A $ec >X $c6 >Y $e6 >P $55ad $50 >M $55ae $87 >M $55af $98 >M
T{ op PC S A X Y P -> $55af $c5 $89 $ec $c6 $e6 }T T{ $55ad M $55ae M $55af M -> $50 $87 $98 }T
$e98c >PC $98 >S $64 >A $f5 >X $38 >Y $a7 >P $e962 $ea >M $e98c $50 >M $e98d $d4 >M $e98e $27 >M
T{ op PC S A X Y P -> $e962 $98 $64 $f5 $38 $a7 }T T{ $e962 M $e98c M $e98d M $e98e M -> $ea $50 $d4 $27 }T
$4e6a >PC $d6 >S $af >A $02 >X $22 >Y $ec >P $4e6a $50 >M $4e6b $14 >M $4e6c $56 >M
T{ op PC S A X Y P -> $4e6c $d6 $af $02 $22 $ec }T T{ $4e6a M $4e6b M $4e6c M -> $50 $14 $56 }T
$003d >PC $b1 >S $ae >A $70 >X $18 >Y $a3 >P $003d $50 >M $003e $39 >M $003f $97 >M $0078 $89 >M
T{ op PC S A X Y P -> $0078 $b1 $ae $70 $18 $a3 }T T{ $003d M $003e M $003f M $0078 M -> $50 $39 $97 $89 }T
$7ba0 >PC $fc >S $cd >A $2d >X $92 >Y $e3 >P $7ba0 $50 >M $7ba1 $2a >M $7ba2 $25 >M
T{ op PC S A X Y P -> $7ba2 $fc $cd $2d $92 $e3 }T T{ $7ba0 M $7ba1 M $7ba2 M -> $50 $2a $25 }T
$08d8 >PC $54 >S $c1 >A $77 >X $16 >Y $ab >P $08d8 $50 >M $08d9 $23 >M $08da $74 >M $08fd $ed >M
T{ op PC S A X Y P -> $08fd $54 $c1 $77 $16 $ab }T T{ $08d8 M $08d9 M $08da M $08fd M -> $50 $23 $74 $ed }T
( 51 )
$5b66 >PC $1a >S $69 >A $1b >X $1e >Y $e7 >P $0042 $b3 >M $0043 $9e >M $5b66 $51 >M $5b67 $42 >M $5b68 $8d >M $9ed1 $d4 >M
T{ op PC S A X Y P -> $5b68 $1a $bd $1b $1e $e5 }T T{ $0042 M $0043 M $5b66 M $5b67 M $5b68 M $9ed1 M -> $b3 $9e $51 $42 $8d $d4 }T
$6fa4 >PC $ae >S $45 >A $7f >X $c4 >Y $6e >P $00cc $91 >M $00cd $13 >M $1455 $a5 >M $6fa4 $51 >M $6fa5 $cc >M $6fa6 $b7 >M
T{ op PC S A X Y P -> $6fa6 $ae $e0 $7f $c4 $ec }T T{ $00cc M $00cd M $1455 M $6fa4 M $6fa5 M $6fa6 M -> $91 $13 $a5 $51 $cc $b7 }T
$f0d8 >PC $87 >S $7b >A $75 >X $29 >Y $ae >P $00c9 $b6 >M $00ca $3c >M $3cdf $5a >M $f0d8 $51 >M $f0d9 $c9 >M $f0da $76 >M
T{ op PC S A X Y P -> $f0da $87 $21 $75 $29 $2c }T T{ $00c9 M $00ca M $3cdf M $f0d8 M $f0d9 M $f0da M -> $b6 $3c $5a $51 $c9 $76 }T
$f1bd >PC $35 >S $9e >A $ad >X $ab >Y $64 >P $007a $91 >M $007b $20 >M $213c $b5 >M $f1bd $51 >M $f1be $7a >M $f1bf $71 >M
T{ op PC S A X Y P -> $f1bf $35 $2b $ad $ab $64 }T T{ $007a M $007b M $213c M $f1bd M $f1be M $f1bf M -> $91 $20 $b5 $51 $7a $71 }T
$f360 >PC $e7 >S $5d >A $af >X $e2 >Y $e6 >P $0037 $d5 >M $0038 $dd >M $deb7 $7c >M $f360 $51 >M $f361 $37 >M $f362 $07 >M
T{ op PC S A X Y P -> $f362 $e7 $21 $af $e2 $64 }T T{ $0037 M $0038 M $deb7 M $f360 M $f361 M $f362 M -> $d5 $dd $7c $51 $37 $07 }T
$0a22 >PC $23 >S $5d >A $7b >X $a1 >Y $68 >P $00ad $42 >M $00ae $05 >M $05e3 $73 >M $0a22 $51 >M $0a23 $ad >M $0a24 $f6 >M
T{ op PC S A X Y P -> $0a24 $23 $2e $7b $a1 $68 }T T{ $00ad M $00ae M $05e3 M $0a22 M $0a23 M $0a24 M -> $42 $05 $73 $51 $ad $f6 }T
$ca85 >PC $73 >S $94 >A $bb >X $ea >Y $aa >P $0087 $41 >M $0088 $93 >M $942b $da >M $ca85 $51 >M $ca86 $87 >M $ca87 $4d >M
T{ op PC S A X Y P -> $ca87 $73 $4e $bb $ea $28 }T T{ $0087 M $0088 M $942b M $ca85 M $ca86 M $ca87 M -> $41 $93 $da $51 $87 $4d }T
$4410 >PC $42 >S $86 >A $8f >X $ff >Y $a7 >P $00d2 $fb >M $00d3 $4d >M $4410 $51 >M $4411 $d2 >M $4412 $61 >M $4efa $b0 >M
T{ op PC S A X Y P -> $4412 $42 $36 $8f $ff $25 }T T{ $00d2 M $00d3 M $4410 M $4411 M $4412 M $4efa M -> $fb $4d $51 $d2 $61 $b0 }T
$ae38 >PC $13 >S $a9 >A $b2 >X $3e >Y $e3 >P $0016 $3d >M $0017 $b6 >M $ae38 $51 >M $ae39 $16 >M $ae3a $be >M $b67b $3f >M
T{ op PC S A X Y P -> $ae3a $13 $96 $b2 $3e $e1 }T T{ $0016 M $0017 M $ae38 M $ae39 M $ae3a M $b67b M -> $3d $b6 $51 $16 $be $3f }T
$008a >PC $36 >S $d9 >A $90 >X $e2 >Y $66 >P $0087 $2b >M $0088 $cc >M $008a $51 >M $008b $87 >M $008c $54 >M $cd0d $1d >M
T{ op PC S A X Y P -> $008c $36 $c4 $90 $e2 $e4 }T T{ $0087 M $0088 M $008a M $008b M $008c M $cd0d M -> $2b $cc $51 $87 $54 $1d }T
$41a2 >PC $a3 >S $c9 >A $d2 >X $04 >Y $65 >P $00f4 $70 >M $00f5 $fe >M $41a2 $51 >M $41a3 $f4 >M $41a4 $1a >M $fe74 $ad >M
T{ op PC S A X Y P -> $41a4 $a3 $64 $d2 $04 $65 }T T{ $00f4 M $00f5 M $41a2 M $41a3 M $41a4 M $fe74 M -> $70 $fe $51 $f4 $1a $ad }T
$c74c >PC $1e >S $5d >A $13 >X $56 >Y $e7 >P $0016 $ff >M $0017 $00 >M $0155 $03 >M $c74c $51 >M $c74d $16 >M $c74e $53 >M
T{ op PC S A X Y P -> $c74e $1e $5e $13 $56 $65 }T T{ $0016 M $0017 M $0155 M $c74c M $c74d M $c74e M -> $ff $00 $03 $51 $16 $53 }T
$6e78 >PC $8b >S $17 >A $3c >X $f2 >Y $24 >P $0023 $72 >M $0024 $57 >M $5864 $4b >M $6e78 $51 >M $6e79 $23 >M $6e7a $4b >M
T{ op PC S A X Y P -> $6e7a $8b $5c $3c $f2 $24 }T T{ $0023 M $0024 M $5864 M $6e78 M $6e79 M $6e7a M -> $72 $57 $4b $51 $23 $4b }T
$4bce >PC $1b >S $88 >A $bc >X $fb >Y $ec >P $00d9 $3f >M $00da $21 >M $223a $45 >M $4bce $51 >M $4bcf $d9 >M $4bd0 $58 >M
T{ op PC S A X Y P -> $4bd0 $1b $cd $bc $fb $ec }T T{ $00d9 M $00da M $223a M $4bce M $4bcf M $4bd0 M -> $3f $21 $45 $51 $d9 $58 }T
$be37 >PC $0f >S $27 >A $d7 >X $df >Y $ef >P $0065 $89 >M $0066 $62 >M $6368 $dc >M $be37 $51 >M $be38 $65 >M $be39 $9e >M
T{ op PC S A X Y P -> $be39 $0f $fb $d7 $df $ed }T T{ $0065 M $0066 M $6368 M $be37 M $be38 M $be39 M -> $89 $62 $dc $51 $65 $9e }T
$728a >PC $c6 >S $8e >A $d0 >X $de >Y $26 >P $0009 $86 >M $000a $7a >M $728a $51 >M $728b $09 >M $728c $03 >M $7b64 $5f >M
T{ op PC S A X Y P -> $728c $c6 $d1 $d0 $de $a4 }T T{ $0009 M $000a M $728a M $728b M $728c M $7b64 M -> $86 $7a $51 $09 $03 $5f }T
( 52 )
$74aa >PC $04 >S $8d >A $3b >X $ff >Y $2b >P $0019 $03 >M $001a $4e >M $4e03 $ac >M $74aa $52 >M $74ab $19 >M $74ac $73 >M
T{ op PC S A X Y P -> $74ac $04 $21 $3b $ff $29 }T T{ $0019 M $001a M $4e03 M $74aa M $74ab M $74ac M -> $03 $4e $ac $52 $19 $73 }T
$2b5e >PC $e9 >S $3e >A $55 >X $25 >Y $e2 >P $00dd $70 >M $00de $ff >M $2b5e $52 >M $2b5f $dd >M $2b60 $ff >M $ff70 $32 >M
T{ op PC S A X Y P -> $2b60 $e9 $0c $55 $25 $60 }T T{ $00dd M $00de M $2b5e M $2b5f M $2b60 M $ff70 M -> $70 $ff $52 $dd $ff $32 }T
$1bc5 >PC $42 >S $12 >A $2c >X $8c >Y $ae >P $00a8 $5e >M $00a9 $29 >M $1bc5 $52 >M $1bc6 $a8 >M $1bc7 $3c >M $295e $a4 >M
T{ op PC S A X Y P -> $1bc7 $42 $b6 $2c $8c $ac }T T{ $00a8 M $00a9 M $1bc5 M $1bc6 M $1bc7 M $295e M -> $5e $29 $52 $a8 $3c $a4 }T
$4864 >PC $58 >S $8e >A $ea >X $96 >Y $a6 >P $001a $5d >M $001b $d9 >M $4864 $52 >M $4865 $1a >M $4866 $e1 >M $d95d $37 >M
T{ op PC S A X Y P -> $4866 $58 $b9 $ea $96 $a4 }T T{ $001a M $001b M $4864 M $4865 M $4866 M $d95d M -> $5d $d9 $52 $1a $e1 $37 }T
$759c >PC $7d >S $48 >A $6e >X $f5 >Y $a5 >P $00e4 $9d >M $00e5 $a4 >M $759c $52 >M $759d $e4 >M $759e $0d >M $a49d $51 >M
T{ op PC S A X Y P -> $759e $7d $19 $6e $f5 $25 }T T{ $00e4 M $00e5 M $759c M $759d M $759e M $a49d M -> $9d $a4 $52 $e4 $0d $51 }T
$cb53 >PC $27 >S $82 >A $72 >X $5f >Y $e8 >P $0087 $e8 >M $0088 $5b >M $5be8 $de >M $cb53 $52 >M $cb54 $87 >M $cb55 $e7 >M
T{ op PC S A X Y P -> $cb55 $27 $5c $72 $5f $68 }T T{ $0087 M $0088 M $5be8 M $cb53 M $cb54 M $cb55 M -> $e8 $5b $de $52 $87 $e7 }T
$f700 >PC $7a >S $5c >A $c6 >X $df >Y $e1 >P $000c $bd >M $000d $39 >M $39bd $42 >M $f700 $52 >M $f701 $0c >M $f702 $1b >M
T{ op PC S A X Y P -> $f702 $7a $1e $c6 $df $61 }T T{ $000c M $000d M $39bd M $f700 M $f701 M $f702 M -> $bd $39 $42 $52 $0c $1b }T
$71c2 >PC $9b >S $d0 >A $6e >X $08 >Y $e6 >P $0065 $db >M $0066 $fb >M $71c2 $52 >M $71c3 $65 >M $71c4 $77 >M $fbdb $9c >M
T{ op PC S A X Y P -> $71c4 $9b $4c $6e $08 $64 }T T{ $0065 M $0066 M $71c2 M $71c3 M $71c4 M $fbdb M -> $db $fb $52 $65 $77 $9c }T
$ea20 >PC $1d >S $d0 >A $84 >X $f4 >Y $a5 >P $00f1 $9c >M $00f2 $ef >M $ea20 $52 >M $ea21 $f1 >M $ea22 $0f >M $ef9c $28 >M
T{ op PC S A X Y P -> $ea22 $1d $f8 $84 $f4 $a5 }T T{ $00f1 M $00f2 M $ea20 M $ea21 M $ea22 M $ef9c M -> $9c $ef $52 $f1 $0f $28 }T
$574c >PC $bd >S $f8 >A $03 >X $e2 >Y $e7 >P $00e7 $4c >M $00e8 $62 >M $574c $52 >M $574d $e7 >M $574e $4f >M $624c $c7 >M
T{ op PC S A X Y P -> $574e $bd $3f $03 $e2 $65 }T T{ $00e7 M $00e8 M $574c M $574d M $574e M $624c M -> $4c $62 $52 $e7 $4f $c7 }T
$905c >PC $f6 >S $05 >A $ac >X $83 >Y $e5 >P $00b9 $83 >M $00ba $e7 >M $905c $52 >M $905d $b9 >M $905e $96 >M $e783 $4c >M
T{ op PC S A X Y P -> $905e $f6 $49 $ac $83 $65 }T T{ $00b9 M $00ba M $905c M $905d M $905e M $e783 M -> $83 $e7 $52 $b9 $96 $4c }T
$4565 >PC $cf >S $24 >A $17 >X $1b >Y $6d >P $0065 $6d >M $0066 $0e >M $0e6d $35 >M $4565 $52 >M $4566 $65 >M $4567 $08 >M
T{ op PC S A X Y P -> $4567 $cf $11 $17 $1b $6d }T T{ $0065 M $0066 M $0e6d M $4565 M $4566 M $4567 M -> $6d $0e $35 $52 $65 $08 }T
$4cfa >PC $9f >S $1b >A $fe >X $b4 >Y $65 >P $007a $5a >M $007b $5f >M $4cfa $52 >M $4cfb $7a >M $4cfc $9d >M $5f5a $7b >M
T{ op PC S A X Y P -> $4cfc $9f $60 $fe $b4 $65 }T T{ $007a M $007b M $4cfa M $4cfb M $4cfc M $5f5a M -> $5a $5f $52 $7a $9d $7b }T
$0fe2 >PC $a4 >S $21 >A $0e >X $dd >Y $e3 >P $0076 $61 >M $0077 $7c >M $0fe2 $52 >M $0fe3 $76 >M $0fe4 $2f >M $7c61 $7b >M
T{ op PC S A X Y P -> $0fe4 $a4 $5a $0e $dd $61 }T T{ $0076 M $0077 M $0fe2 M $0fe3 M $0fe4 M $7c61 M -> $61 $7c $52 $76 $2f $7b }T
$c8db >PC $41 >S $e2 >A $43 >X $61 >Y $69 >P $00f7 $3b >M $00f8 $47 >M $473b $6d >M $c8db $52 >M $c8dc $f7 >M $c8dd $41 >M
T{ op PC S A X Y P -> $c8dd $41 $8f $43 $61 $e9 }T T{ $00f7 M $00f8 M $473b M $c8db M $c8dc M $c8dd M -> $3b $47 $6d $52 $f7 $41 }T
$ad7c >PC $87 >S $19 >A $17 >X $e4 >Y $e5 >P $008d $22 >M $008e $1e >M $1e22 $0d >M $ad7c $52 >M $ad7d $8d >M $ad7e $bd >M
T{ op PC S A X Y P -> $ad7e $87 $14 $17 $e4 $65 }T T{ $008d M $008e M $1e22 M $ad7c M $ad7d M $ad7e M -> $22 $1e $0d $52 $8d $bd }T
( 53 )
$5c12 >PC $e3 >S $c8 >A $90 >X $88 >Y $e5 >P $5c12 $53 >M $5c13 $ae >M $5c14 $92 >M
T{ op PC S A X Y P -> $5c13 $e3 $c8 $90 $88 $e5 }T T{ $5c12 M $5c13 M $5c14 M -> $53 $ae $92 }T
$9dbe >PC $02 >S $b5 >A $6c >X $ac >Y $eb >P $9dbe $53 >M $9dbf $e4 >M $9dc0 $8a >M
T{ op PC S A X Y P -> $9dbf $02 $b5 $6c $ac $eb }T T{ $9dbe M $9dbf M $9dc0 M -> $53 $e4 $8a }T
$3a7b >PC $82 >S $7c >A $ed >X $23 >Y $66 >P $3a7b $53 >M $3a7c $7a >M $3a7d $ee >M
T{ op PC S A X Y P -> $3a7c $82 $7c $ed $23 $66 }T T{ $3a7b M $3a7c M $3a7d M -> $53 $7a $ee }T
$cc9f >PC $1f >S $ef >A $c9 >X $2d >Y $ea >P $cc9f $53 >M $cca0 $88 >M $cca1 $14 >M
T{ op PC S A X Y P -> $cca0 $1f $ef $c9 $2d $ea }T T{ $cc9f M $cca0 M $cca1 M -> $53 $88 $14 }T
$62cb >PC $10 >S $5e >A $8b >X $fc >Y $e9 >P $62cb $53 >M $62cc $aa >M $62cd $86 >M
T{ op PC S A X Y P -> $62cc $10 $5e $8b $fc $e9 }T T{ $62cb M $62cc M $62cd M -> $53 $aa $86 }T
$bebc >PC $c5 >S $23 >A $aa >X $5a >Y $ab >P $bebc $53 >M $bebd $d6 >M $bebe $a4 >M
T{ op PC S A X Y P -> $bebd $c5 $23 $aa $5a $ab }T T{ $bebc M $bebd M $bebe M -> $53 $d6 $a4 }T
$0ae4 >PC $c8 >S $65 >A $d9 >X $2d >Y $ab >P $0ae4 $53 >M $0ae5 $7e >M $0ae6 $e6 >M
T{ op PC S A X Y P -> $0ae5 $c8 $65 $d9 $2d $ab }T T{ $0ae4 M $0ae5 M $0ae6 M -> $53 $7e $e6 }T
$58a9 >PC $8e >S $d7 >A $7c >X $c8 >Y $a5 >P $58a9 $53 >M $58aa $a6 >M $58ab $8a >M
T{ op PC S A X Y P -> $58aa $8e $d7 $7c $c8 $a5 }T T{ $58a9 M $58aa M $58ab M -> $53 $a6 $8a }T
$8acc >PC $ed >S $f8 >A $0f >X $dd >Y $6d >P $8acc $53 >M $8acd $ef >M $8ace $75 >M
T{ op PC S A X Y P -> $8acd $ed $f8 $0f $dd $6d }T T{ $8acc M $8acd M $8ace M -> $53 $ef $75 }T
$80c6 >PC $c2 >S $98 >A $4a >X $7c >Y $6d >P $80c6 $53 >M $80c7 $f5 >M $80c8 $cc >M
T{ op PC S A X Y P -> $80c7 $c2 $98 $4a $7c $6d }T T{ $80c6 M $80c7 M $80c8 M -> $53 $f5 $cc }T
$27db >PC $a5 >S $fb >A $36 >X $88 >Y $63 >P $27db $53 >M $27dc $85 >M $27dd $35 >M
T{ op PC S A X Y P -> $27dc $a5 $fb $36 $88 $63 }T T{ $27db M $27dc M $27dd M -> $53 $85 $35 }T
$c499 >PC $89 >S $b8 >A $0c >X $15 >Y $a1 >P $c499 $53 >M $c49a $27 >M $c49b $36 >M
T{ op PC S A X Y P -> $c49a $89 $b8 $0c $15 $a1 }T T{ $c499 M $c49a M $c49b M -> $53 $27 $36 }T
$8148 >PC $6f >S $77 >A $9a >X $a3 >Y $69 >P $8148 $53 >M $8149 $f4 >M $814a $6a >M
T{ op PC S A X Y P -> $8149 $6f $77 $9a $a3 $69 }T T{ $8148 M $8149 M $814a M -> $53 $f4 $6a }T
$1658 >PC $f2 >S $0e >A $be >X $3a >Y $22 >P $1658 $53 >M $1659 $76 >M $165a $62 >M
T{ op PC S A X Y P -> $1659 $f2 $0e $be $3a $22 }T T{ $1658 M $1659 M $165a M -> $53 $76 $62 }T
$e05a >PC $f4 >S $d9 >A $a8 >X $ba >Y $62 >P $e05a $53 >M $e05b $f7 >M $e05c $df >M
T{ op PC S A X Y P -> $e05b $f4 $d9 $a8 $ba $62 }T T{ $e05a M $e05b M $e05c M -> $53 $f7 $df }T
$c129 >PC $24 >S $14 >A $17 >X $13 >Y $6e >P $c129 $53 >M $c12a $70 >M $c12b $f2 >M
T{ op PC S A X Y P -> $c12a $24 $14 $17 $13 $6e }T T{ $c129 M $c12a M $c12b M -> $53 $70 $f2 }T
( 54 )
$ae6b >PC $27 >S $67 >A $45 >X $d3 >Y $24 >P $009d $35 >M $00e2 $4c >M $ae6b $54 >M $ae6c $9d >M $ae6d $01 >M
T{ op PC S A X Y P -> $ae6d $27 $67 $45 $d3 $24 }T T{ $009d M $00e2 M $ae6b M $ae6c M $ae6d M -> $35 $4c $54 $9d $01 }T
$90c3 >PC $c9 >S $d7 >A $f1 >X $59 >Y $67 >P $00d2 $a9 >M $00e1 $f1 >M $90c3 $54 >M $90c4 $e1 >M $90c5 $f4 >M
T{ op PC S A X Y P -> $90c5 $c9 $d7 $f1 $59 $67 }T T{ $00d2 M $00e1 M $90c3 M $90c4 M $90c5 M -> $a9 $f1 $54 $e1 $f4 }T
$b987 >PC $48 >S $20 >A $78 >X $a5 >Y $ea >P $000e $7e >M $0096 $c2 >M $b987 $54 >M $b988 $96 >M $b989 $88 >M
T{ op PC S A X Y P -> $b989 $48 $20 $78 $a5 $ea }T T{ $000e M $0096 M $b987 M $b988 M $b989 M -> $7e $c2 $54 $96 $88 }T
$c213 >PC $04 >S $a8 >A $8f >X $f7 >Y $6c >P $0061 $13 >M $00d2 $87 >M $c213 $54 >M $c214 $d2 >M $c215 $8b >M
T{ op PC S A X Y P -> $c215 $04 $a8 $8f $f7 $6c }T T{ $0061 M $00d2 M $c213 M $c214 M $c215 M -> $13 $87 $54 $d2 $8b }T
$6c6c >PC $56 >S $a0 >A $cc >X $da >Y $a5 >P $0003 $24 >M $0037 $4b >M $6c6c $54 >M $6c6d $37 >M $6c6e $29 >M
T{ op PC S A X Y P -> $6c6e $56 $a0 $cc $da $a5 }T T{ $0003 M $0037 M $6c6c M $6c6d M $6c6e M -> $24 $4b $54 $37 $29 }T
$1131 >PC $80 >S $ed >A $c6 >X $e2 >Y $69 >P $0085 $58 >M $00bf $10 >M $1131 $54 >M $1132 $bf >M $1133 $a5 >M
T{ op PC S A X Y P -> $1133 $80 $ed $c6 $e2 $69 }T T{ $0085 M $00bf M $1131 M $1132 M $1133 M -> $58 $10 $54 $bf $a5 }T
$da80 >PC $cd >S $6e >A $68 >X $40 >Y $6a >P $006f $e0 >M $00d7 $33 >M $da80 $54 >M $da81 $6f >M $da82 $a3 >M
T{ op PC S A X Y P -> $da82 $cd $6e $68 $40 $6a }T T{ $006f M $00d7 M $da80 M $da81 M $da82 M -> $e0 $33 $54 $6f $a3 }T
$996a >PC $79 >S $1b >A $06 >X $fa >Y $63 >P $001e $6c >M $0024 $69 >M $996a $54 >M $996b $1e >M $996c $c7 >M
T{ op PC S A X Y P -> $996c $79 $1b $06 $fa $63 }T T{ $001e M $0024 M $996a M $996b M $996c M -> $6c $69 $54 $1e $c7 }T
$3f0c >PC $4d >S $b7 >A $59 >X $be >Y $29 >P $0002 $67 >M $005b $87 >M $3f0c $54 >M $3f0d $02 >M $3f0e $7d >M
T{ op PC S A X Y P -> $3f0e $4d $b7 $59 $be $29 }T T{ $0002 M $005b M $3f0c M $3f0d M $3f0e M -> $67 $87 $54 $02 $7d }T
$2b34 >PC $53 >S $1f >A $a4 >X $d1 >Y $2b >P $005d $e0 >M $00b9 $c8 >M $2b34 $54 >M $2b35 $b9 >M $2b36 $c3 >M
T{ op PC S A X Y P -> $2b36 $53 $1f $a4 $d1 $2b }T T{ $005d M $00b9 M $2b34 M $2b35 M $2b36 M -> $e0 $c8 $54 $b9 $c3 }T
$1749 >PC $c9 >S $3f >A $15 >X $a7 >Y $e5 >P $0017 $ac >M $002c $2f >M $1749 $54 >M $174a $17 >M $174b $d1 >M
T{ op PC S A X Y P -> $174b $c9 $3f $15 $a7 $e5 }T T{ $0017 M $002c M $1749 M $174a M $174b M -> $ac $2f $54 $17 $d1 }T
$8c96 >PC $86 >S $ba >A $97 >X $4e >Y $66 >P $0022 $64 >M $008b $8b >M $8c96 $54 >M $8c97 $8b >M $8c98 $08 >M
T{ op PC S A X Y P -> $8c98 $86 $ba $97 $4e $66 }T T{ $0022 M $008b M $8c96 M $8c97 M $8c98 M -> $64 $8b $54 $8b $08 }T
$2b3a >PC $86 >S $7b >A $1d >X $19 >Y $e7 >P $0081 $30 >M $009e $bc >M $2b3a $54 >M $2b3b $81 >M $2b3c $59 >M
T{ op PC S A X Y P -> $2b3c $86 $7b $1d $19 $e7 }T T{ $0081 M $009e M $2b3a M $2b3b M $2b3c M -> $30 $bc $54 $81 $59 }T
$cd20 >PC $da >S $35 >A $e2 >X $d9 >Y $ea >P $00ac $85 >M $00ca $86 >M $cd20 $54 >M $cd21 $ca >M $cd22 $d4 >M
T{ op PC S A X Y P -> $cd22 $da $35 $e2 $d9 $ea }T T{ $00ac M $00ca M $cd20 M $cd21 M $cd22 M -> $85 $86 $54 $ca $d4 }T
$d98d >PC $7d >S $0b >A $eb >X $11 >Y $ec >P $0071 $20 >M $0086 $5c >M $d98d $54 >M $d98e $86 >M $d98f $ce >M
T{ op PC S A X Y P -> $d98f $7d $0b $eb $11 $ec }T T{ $0071 M $0086 M $d98d M $d98e M $d98f M -> $20 $5c $54 $86 $ce }T
$4ad6 >PC $12 >S $2d >A $ed >X $65 >Y $27 >P $0054 $6c >M $0067 $ce >M $4ad6 $54 >M $4ad7 $67 >M $4ad8 $9d >M
T{ op PC S A X Y P -> $4ad8 $12 $2d $ed $65 $27 }T T{ $0054 M $0067 M $4ad6 M $4ad7 M $4ad8 M -> $6c $ce $54 $67 $9d }T
( 55 )
$a6c9 >PC $23 >S $71 >A $e6 >X $01 >Y $66 >P $00bc $ab >M $00d6 $f4 >M $a6c9 $55 >M $a6ca $d6 >M $a6cb $22 >M
T{ op PC S A X Y P -> $a6cb $23 $da $e6 $01 $e4 }T T{ $00bc M $00d6 M $a6c9 M $a6ca M $a6cb M -> $ab $f4 $55 $d6 $22 }T
$c199 >PC $7c >S $d5 >A $c9 >X $b5 >Y $68 >P $007c $3b >M $00b3 $e7 >M $c199 $55 >M $c19a $b3 >M $c19b $5e >M
T{ op PC S A X Y P -> $c19b $7c $ee $c9 $b5 $e8 }T T{ $007c M $00b3 M $c199 M $c19a M $c19b M -> $3b $e7 $55 $b3 $5e }T
$21b4 >PC $57 >S $2f >A $7c >X $4d >Y $e6 >P $0039 $36 >M $00bd $3a >M $21b4 $55 >M $21b5 $bd >M $21b6 $91 >M
T{ op PC S A X Y P -> $21b6 $57 $19 $7c $4d $64 }T T{ $0039 M $00bd M $21b4 M $21b5 M $21b6 M -> $36 $3a $55 $bd $91 }T
$1586 >PC $3a >S $76 >A $7e >X $99 >Y $62 >P $0003 $b3 >M $0085 $ec >M $1586 $55 >M $1587 $85 >M $1588 $e6 >M
T{ op PC S A X Y P -> $1588 $3a $c5 $7e $99 $e0 }T T{ $0003 M $0085 M $1586 M $1587 M $1588 M -> $b3 $ec $55 $85 $e6 }T
$40ef >PC $4c >S $62 >A $2b >X $f9 >Y $62 >P $00d1 $82 >M $00fc $f9 >M $40ef $55 >M $40f0 $d1 >M $40f1 $97 >M
T{ op PC S A X Y P -> $40f1 $4c $9b $2b $f9 $e0 }T T{ $00d1 M $00fc M $40ef M $40f0 M $40f1 M -> $82 $f9 $55 $d1 $97 }T
$a8b3 >PC $3d >S $1e >A $2b >X $44 >Y $e8 >P $002b $38 >M $0056 $d9 >M $a8b3 $55 >M $a8b4 $2b >M $a8b5 $21 >M
T{ op PC S A X Y P -> $a8b5 $3d $c7 $2b $44 $e8 }T T{ $002b M $0056 M $a8b3 M $a8b4 M $a8b5 M -> $38 $d9 $55 $2b $21 }T
$3f81 >PC $32 >S $1b >A $25 >X $24 >Y $68 >P $00d5 $51 >M $00fa $ed >M $3f81 $55 >M $3f82 $d5 >M $3f83 $cf >M
T{ op PC S A X Y P -> $3f83 $32 $f6 $25 $24 $e8 }T T{ $00d5 M $00fa M $3f81 M $3f82 M $3f83 M -> $51 $ed $55 $d5 $cf }T
$1383 >PC $da >S $ff >A $d7 >X $a8 >Y $6e >P $00cc $a2 >M $00f5 $b8 >M $1383 $55 >M $1384 $f5 >M $1385 $c1 >M
T{ op PC S A X Y P -> $1385 $da $5d $d7 $a8 $6c }T T{ $00cc M $00f5 M $1383 M $1384 M $1385 M -> $a2 $b8 $55 $f5 $c1 }T
$9b80 >PC $01 >S $1d >A $cd >X $c5 >Y $e2 >P $00ad $f4 >M $00e0 $c2 >M $9b80 $55 >M $9b81 $e0 >M $9b82 $b4 >M
T{ op PC S A X Y P -> $9b82 $01 $e9 $cd $c5 $e0 }T T{ $00ad M $00e0 M $9b80 M $9b81 M $9b82 M -> $f4 $c2 $55 $e0 $b4 }T
$c480 >PC $d9 >S $46 >A $fe >X $f7 >Y $ee >P $0094 $5f >M $0096 $8a >M $c480 $55 >M $c481 $96 >M $c482 $25 >M
T{ op PC S A X Y P -> $c482 $d9 $19 $fe $f7 $6c }T T{ $0094 M $0096 M $c480 M $c481 M $c482 M -> $5f $8a $55 $96 $25 }T
$fc84 >PC $69 >S $b1 >A $51 >X $89 >Y $ed >P $0095 $a4 >M $00e6 $b5 >M $fc84 $55 >M $fc85 $95 >M $fc86 $bd >M
T{ op PC S A X Y P -> $fc86 $69 $04 $51 $89 $6d }T T{ $0095 M $00e6 M $fc84 M $fc85 M $fc86 M -> $a4 $b5 $55 $95 $bd }T
$a4f0 >PC $36 >S $56 >A $0e >X $27 >Y $6c >P $0002 $32 >M $0010 $16 >M $a4f0 $55 >M $a4f1 $02 >M $a4f2 $b1 >M
T{ op PC S A X Y P -> $a4f2 $36 $40 $0e $27 $6c }T T{ $0002 M $0010 M $a4f0 M $a4f1 M $a4f2 M -> $32 $16 $55 $02 $b1 }T
$65b5 >PC $16 >S $cd >A $f3 >X $4e >Y $e8 >P $00cf $8c >M $00dc $2b >M $65b5 $55 >M $65b6 $dc >M $65b7 $c1 >M
T{ op PC S A X Y P -> $65b7 $16 $41 $f3 $4e $68 }T T{ $00cf M $00dc M $65b5 M $65b6 M $65b7 M -> $8c $2b $55 $dc $c1 }T
$dc55 >PC $ed >S $0b >A $e8 >X $4c >Y $61 >P $0018 $7e >M $0030 $1d >M $dc55 $55 >M $dc56 $30 >M $dc57 $e0 >M
T{ op PC S A X Y P -> $dc57 $ed $75 $e8 $4c $61 }T T{ $0018 M $0030 M $dc55 M $dc56 M $dc57 M -> $7e $1d $55 $30 $e0 }T
$c761 >PC $ab >S $4d >A $6c >X $a7 >Y $a9 >P $004d $38 >M $00b9 $03 >M $c761 $55 >M $c762 $4d >M $c763 $6a >M
T{ op PC S A X Y P -> $c763 $ab $4e $6c $a7 $29 }T T{ $004d M $00b9 M $c761 M $c762 M $c763 M -> $38 $03 $55 $4d $6a }T
$299e >PC $47 >S $e7 >A $01 >X $ba >Y $62 >P $0060 $b3 >M $0061 $3a >M $299e $55 >M $299f $60 >M $29a0 $4a >M
T{ op PC S A X Y P -> $29a0 $47 $dd $01 $ba $e0 }T T{ $0060 M $0061 M $299e M $299f M $29a0 M -> $b3 $3a $55 $60 $4a }T
( 56 )
$eef3 >PC $f4 >S $ed >A $ff >X $b2 >Y $6e >P $00e2 $2a >M $00e3 $1e >M $eef3 $56 >M $eef4 $e3 >M $eef5 $35 >M
T{ op PC S A X Y P -> $eef5 $f4 $ed $ff $b2 $6c }T T{ $00e2 M $00e3 M $eef3 M $eef4 M $eef5 M -> $15 $1e $56 $e3 $35 }T
$563e >PC $4d >S $2f >A $52 >X $a5 >Y $a8 >P $0045 $75 >M $0097 $9e >M $563e $56 >M $563f $45 >M $5640 $11 >M
T{ op PC S A X Y P -> $5640 $4d $2f $52 $a5 $28 }T T{ $0045 M $0097 M $563e M $563f M $5640 M -> $75 $4f $56 $45 $11 }T
$ae81 >PC $b1 >S $9b >A $e3 >X $92 >Y $aa >P $0021 $cc >M $003e $2c >M $ae81 $56 >M $ae82 $3e >M $ae83 $eb >M
T{ op PC S A X Y P -> $ae83 $b1 $9b $e3 $92 $28 }T T{ $0021 M $003e M $ae81 M $ae82 M $ae83 M -> $66 $2c $56 $3e $eb }T
$2c9c >PC $d5 >S $59 >A $f3 >X $3a >Y $eb >P $00a7 $28 >M $00b4 $b9 >M $2c9c $56 >M $2c9d $b4 >M $2c9e $85 >M
T{ op PC S A X Y P -> $2c9e $d5 $59 $f3 $3a $68 }T T{ $00a7 M $00b4 M $2c9c M $2c9d M $2c9e M -> $14 $b9 $56 $b4 $85 }T
$b1d8 >PC $7c >S $84 >A $c6 >X $40 >Y $67 >P $002f $37 >M $00f5 $83 >M $b1d8 $56 >M $b1d9 $2f >M $b1da $83 >M
T{ op PC S A X Y P -> $b1da $7c $84 $c6 $40 $65 }T T{ $002f M $00f5 M $b1d8 M $b1d9 M $b1da M -> $37 $41 $56 $2f $83 }T
$16a4 >PC $b1 >S $ef >A $cb >X $bd >Y $69 >P $0065 $db >M $009a $4a >M $16a4 $56 >M $16a5 $9a >M $16a6 $9d >M
T{ op PC S A X Y P -> $16a6 $b1 $ef $cb $bd $69 }T T{ $0065 M $009a M $16a4 M $16a5 M $16a6 M -> $6d $4a $56 $9a $9d }T
$50fa >PC $2b >S $d3 >A $58 >X $fa >Y $62 >P $0076 $5e >M $00ce $61 >M $50fa $56 >M $50fb $76 >M $50fc $dc >M
T{ op PC S A X Y P -> $50fc $2b $d3 $58 $fa $61 }T T{ $0076 M $00ce M $50fa M $50fb M $50fc M -> $5e $30 $56 $76 $dc }T
$dfc1 >PC $e6 >S $7f >A $ff >X $a9 >Y $ae >P $00d2 $94 >M $00d3 $a7 >M $dfc1 $56 >M $dfc2 $d3 >M $dfc3 $87 >M
T{ op PC S A X Y P -> $dfc3 $e6 $7f $ff $a9 $2c }T T{ $00d2 M $00d3 M $dfc1 M $dfc2 M $dfc3 M -> $4a $a7 $56 $d3 $87 }T
$9581 >PC $9d >S $b5 >A $c6 >X $5d >Y $a0 >P $00c2 $25 >M $00fc $3b >M $9581 $56 >M $9582 $fc >M $9583 $b5 >M
T{ op PC S A X Y P -> $9583 $9d $b5 $c6 $5d $21 }T T{ $00c2 M $00fc M $9581 M $9582 M $9583 M -> $12 $3b $56 $fc $b5 }T
$b9ba >PC $2c >S $81 >A $b2 >X $3c >Y $2f >P $000c $5a >M $005a $58 >M $b9ba $56 >M $b9bb $5a >M $b9bc $46 >M
T{ op PC S A X Y P -> $b9bc $2c $81 $b2 $3c $2c }T T{ $000c M $005a M $b9ba M $b9bb M $b9bc M -> $2d $58 $56 $5a $46 }T
$6631 >PC $fd >S $14 >A $85 >X $12 >Y $22 >P $001b $bb >M $00a0 $c0 >M $6631 $56 >M $6632 $1b >M $6633 $6c >M
T{ op PC S A X Y P -> $6633 $fd $14 $85 $12 $20 }T T{ $001b M $00a0 M $6631 M $6632 M $6633 M -> $bb $60 $56 $1b $6c }T
$3f69 >PC $f2 >S $e2 >A $d2 >X $a6 >Y $23 >P $00a0 $a0 >M $00ce $2d >M $3f69 $56 >M $3f6a $ce >M $3f6b $18 >M
T{ op PC S A X Y P -> $3f6b $f2 $e2 $d2 $a6 $20 }T T{ $00a0 M $00ce M $3f69 M $3f6a M $3f6b M -> $50 $2d $56 $ce $18 }T
$d171 >PC $52 >S $e6 >A $33 >X $77 >Y $eb >P $009c $5c >M $00cf $a1 >M $d171 $56 >M $d172 $9c >M $d173 $9e >M
T{ op PC S A X Y P -> $d173 $52 $e6 $33 $77 $69 }T T{ $009c M $00cf M $d171 M $d172 M $d173 M -> $5c $50 $56 $9c $9e }T
$907b >PC $a6 >S $c2 >A $ae >X $25 >Y $24 >P $003f $f3 >M $0091 $56 >M $907b $56 >M $907c $91 >M $907d $9a >M
T{ op PC S A X Y P -> $907d $a6 $c2 $ae $25 $25 }T T{ $003f M $0091 M $907b M $907c M $907d M -> $79 $56 $56 $91 $9a }T
$0fe0 >PC $46 >S $c8 >A $bd >X $ae >Y $21 >P $0098 $14 >M $00db $97 >M $0fe0 $56 >M $0fe1 $db >M $0fe2 $68 >M
T{ op PC S A X Y P -> $0fe2 $46 $c8 $bd $ae $20 }T T{ $0098 M $00db M $0fe0 M $0fe1 M $0fe2 M -> $0a $97 $56 $db $68 }T
$50ce >PC $70 >S $3e >A $bb >X $04 >Y $ee >P $009a $cc >M $00df $5c >M $50ce $56 >M $50cf $df >M $50d0 $90 >M
T{ op PC S A X Y P -> $50d0 $70 $3e $bb $04 $6c }T T{ $009a M $00df M $50ce M $50cf M $50d0 M -> $66 $5c $56 $df $90 }T
( 57 )
$f781 >PC $b8 >S $9c >A $34 >X $50 >Y $2f >P $0062 $24 >M $f781 $57 >M $f782 $62 >M $f783 $72 >M
T{ op PC S A X Y P -> $f783 $b8 $9c $34 $50 $2f }T T{ $0062 M $f781 M $f782 M $f783 M -> $04 $57 $62 $72 }T
$e047 >PC $67 >S $ad >A $09 >X $76 >Y $6b >P $00b3 $a6 >M $e047 $57 >M $e048 $b3 >M $e049 $e3 >M
T{ op PC S A X Y P -> $e049 $67 $ad $09 $76 $6b }T T{ $00b3 M $e047 M $e048 M $e049 M -> $86 $57 $b3 $e3 }T
$1685 >PC $d1 >S $e4 >A $0b >X $e0 >Y $65 >P $006d $3f >M $1685 $57 >M $1686 $6d >M $1687 $e4 >M
T{ op PC S A X Y P -> $1687 $d1 $e4 $0b $e0 $65 }T T{ $006d M $1685 M $1686 M $1687 M -> $1f $57 $6d $e4 }T
$5861 >PC $50 >S $2e >A $43 >X $76 >Y $ac >P $00b9 $8d >M $5861 $57 >M $5862 $b9 >M $5863 $fe >M
T{ op PC S A X Y P -> $5863 $50 $2e $43 $76 $ac }T T{ $00b9 M $5861 M $5862 M $5863 M -> $8d $57 $b9 $fe }T
$8e1c >PC $47 >S $82 >A $5e >X $2e >Y $27 >P $0071 $17 >M $8e1c $57 >M $8e1d $71 >M $8e1e $11 >M
T{ op PC S A X Y P -> $8e1e $47 $82 $5e $2e $27 }T T{ $0071 M $8e1c M $8e1d M $8e1e M -> $17 $57 $71 $11 }T
$384f >PC $3a >S $51 >A $9d >X $9d >Y $a9 >P $000f $0e >M $384f $57 >M $3850 $0f >M $3851 $45 >M
T{ op PC S A X Y P -> $3851 $3a $51 $9d $9d $a9 }T T{ $000f M $384f M $3850 M $3851 M -> $0e $57 $0f $45 }T
$fb46 >PC $5c >S $54 >A $c5 >X $a2 >Y $e0 >P $0061 $77 >M $fb46 $57 >M $fb47 $61 >M $fb48 $44 >M
T{ op PC S A X Y P -> $fb48 $5c $54 $c5 $a2 $e0 }T T{ $0061 M $fb46 M $fb47 M $fb48 M -> $57 $57 $61 $44 }T
$c4b5 >PC $3c >S $fd >A $3f >X $47 >Y $ef >P $0030 $80 >M $c4b5 $57 >M $c4b6 $30 >M $c4b7 $cb >M
T{ op PC S A X Y P -> $c4b7 $3c $fd $3f $47 $ef }T T{ $0030 M $c4b5 M $c4b6 M $c4b7 M -> $80 $57 $30 $cb }T
$0818 >PC $65 >S $9f >A $59 >X $60 >Y $68 >P $0039 $56 >M $0818 $57 >M $0819 $39 >M $081a $06 >M
T{ op PC S A X Y P -> $081a $65 $9f $59 $60 $68 }T T{ $0039 M $0818 M $0819 M $081a M -> $56 $57 $39 $06 }T
$0056 >PC $4d >S $95 >A $ae >X $6c >Y $a6 >P $0056 $57 >M $0057 $ac >M $0058 $15 >M $00ac $38 >M
T{ op PC S A X Y P -> $0058 $4d $95 $ae $6c $a6 }T T{ $0056 M $0057 M $0058 M $00ac M -> $57 $ac $15 $18 }T
$3389 >PC $ad >S $d6 >A $98 >X $86 >Y $e5 >P $00a1 $f8 >M $3389 $57 >M $338a $a1 >M $338b $10 >M
T{ op PC S A X Y P -> $338b $ad $d6 $98 $86 $e5 }T T{ $00a1 M $3389 M $338a M $338b M -> $d8 $57 $a1 $10 }T
$f63b >PC $35 >S $12 >A $75 >X $43 >Y $6e >P $0062 $d1 >M $f63b $57 >M $f63c $62 >M $f63d $0e >M
T{ op PC S A X Y P -> $f63d $35 $12 $75 $43 $6e }T T{ $0062 M $f63b M $f63c M $f63d M -> $d1 $57 $62 $0e }T
$5566 >PC $8c >S $21 >A $5e >X $9e >Y $e5 >P $00e4 $54 >M $5566 $57 >M $5567 $e4 >M $5568 $92 >M
T{ op PC S A X Y P -> $5568 $8c $21 $5e $9e $e5 }T T{ $00e4 M $5566 M $5567 M $5568 M -> $54 $57 $e4 $92 }T
$c79d >PC $6f >S $d6 >A $1a >X $9c >Y $20 >P $00bf $de >M $c79d $57 >M $c79e $bf >M $c79f $71 >M
T{ op PC S A X Y P -> $c79f $6f $d6 $1a $9c $20 }T T{ $00bf M $c79d M $c79e M $c79f M -> $de $57 $bf $71 }T
$b045 >PC $ff >S $c6 >A $97 >X $43 >Y $e0 >P $00ad $f1 >M $b045 $57 >M $b046 $ad >M $b047 $e2 >M
T{ op PC S A X Y P -> $b047 $ff $c6 $97 $43 $e0 }T T{ $00ad M $b045 M $b046 M $b047 M -> $d1 $57 $ad $e2 }T
$a9f2 >PC $0c >S $01 >A $b9 >X $d3 >Y $a3 >P $00d4 $27 >M $a9f2 $57 >M $a9f3 $d4 >M $a9f4 $e4 >M
T{ op PC S A X Y P -> $a9f4 $0c $01 $b9 $d3 $a3 }T T{ $00d4 M $a9f2 M $a9f3 M $a9f4 M -> $07 $57 $d4 $e4 }T
( 58 )
$aa5f >PC $14 >S $ee >A $70 >X $88 >Y $e8 >P $aa5f $58 >M $aa60 $b1 >M $aa61 $60 >M
T{ op PC S A X Y P -> $aa60 $14 $ee $70 $88 $e8 }T T{ $aa5f M $aa60 M $aa61 M -> $58 $b1 $60 }T
$01bb >PC $4a >S $2f >A $fd >X $fa >Y $25 >P $01bb $58 >M $01bc $c0 >M $01bd $b9 >M
T{ op PC S A X Y P -> $01bc $4a $2f $fd $fa $21 }T T{ $01bb M $01bc M $01bd M -> $58 $c0 $b9 }T
$6188 >PC $b2 >S $42 >A $7a >X $d5 >Y $26 >P $6188 $58 >M $6189 $53 >M $618a $5c >M
T{ op PC S A X Y P -> $6189 $b2 $42 $7a $d5 $22 }T T{ $6188 M $6189 M $618a M -> $58 $53 $5c }T
$458e >PC $8e >S $20 >A $e5 >X $6c >Y $ec >P $458e $58 >M $458f $22 >M $4590 $61 >M
T{ op PC S A X Y P -> $458f $8e $20 $e5 $6c $e8 }T T{ $458e M $458f M $4590 M -> $58 $22 $61 }T
$3f92 >PC $3a >S $dd >A $47 >X $7d >Y $a3 >P $3f92 $58 >M $3f93 $47 >M $3f94 $9f >M
T{ op PC S A X Y P -> $3f93 $3a $dd $47 $7d $a3 }T T{ $3f92 M $3f93 M $3f94 M -> $58 $47 $9f }T
$e77d >PC $c1 >S $af >A $9f >X $78 >Y $2c >P $e77d $58 >M $e77e $af >M $e77f $b8 >M
T{ op PC S A X Y P -> $e77e $c1 $af $9f $78 $28 }T T{ $e77d M $e77e M $e77f M -> $58 $af $b8 }T
$7b58 >PC $b6 >S $00 >A $58 >X $18 >Y $a1 >P $7b58 $58 >M $7b59 $03 >M $7b5a $6f >M
T{ op PC S A X Y P -> $7b59 $b6 $00 $58 $18 $a1 }T T{ $7b58 M $7b59 M $7b5a M -> $58 $03 $6f }T
$9d87 >PC $4c >S $01 >A $ca >X $58 >Y $a6 >P $9d87 $58 >M $9d88 $c0 >M $9d89 $5f >M
T{ op PC S A X Y P -> $9d88 $4c $01 $ca $58 $a2 }T T{ $9d87 M $9d88 M $9d89 M -> $58 $c0 $5f }T
$93f3 >PC $46 >S $c2 >A $fb >X $f0 >Y $2a >P $93f3 $58 >M $93f4 $1b >M $93f5 $c0 >M
T{ op PC S A X Y P -> $93f4 $46 $c2 $fb $f0 $2a }T T{ $93f3 M $93f4 M $93f5 M -> $58 $1b $c0 }T
$ada0 >PC $02 >S $28 >A $ac >X $3e >Y $e8 >P $ada0 $58 >M $ada1 $eb >M $ada2 $99 >M
T{ op PC S A X Y P -> $ada1 $02 $28 $ac $3e $e8 }T T{ $ada0 M $ada1 M $ada2 M -> $58 $eb $99 }T
$545a >PC $74 >S $fb >A $a6 >X $3f >Y $a3 >P $545a $58 >M $545b $b5 >M $545c $65 >M
T{ op PC S A X Y P -> $545b $74 $fb $a6 $3f $a3 }T T{ $545a M $545b M $545c M -> $58 $b5 $65 }T
$fb1b >PC $25 >S $3e >A $bb >X $4b >Y $6d >P $fb1b $58 >M $fb1c $8e >M $fb1d $49 >M
T{ op PC S A X Y P -> $fb1c $25 $3e $bb $4b $69 }T T{ $fb1b M $fb1c M $fb1d M -> $58 $8e $49 }T
$4e51 >PC $e3 >S $1e >A $d6 >X $17 >Y $ed >P $4e51 $58 >M $4e52 $68 >M $4e53 $6b >M
T{ op PC S A X Y P -> $4e52 $e3 $1e $d6 $17 $e9 }T T{ $4e51 M $4e52 M $4e53 M -> $58 $68 $6b }T
$47eb >PC $a2 >S $3f >A $9f >X $65 >Y $6b >P $47eb $58 >M $47ec $0f >M $47ed $31 >M
T{ op PC S A X Y P -> $47ec $a2 $3f $9f $65 $6b }T T{ $47eb M $47ec M $47ed M -> $58 $0f $31 }T
$7102 >PC $d6 >S $2c >A $c1 >X $95 >Y $25 >P $7102 $58 >M $7103 $5a >M $7104 $60 >M
T{ op PC S A X Y P -> $7103 $d6 $2c $c1 $95 $21 }T T{ $7102 M $7103 M $7104 M -> $58 $5a $60 }T
$d64e >PC $ee >S $50 >A $38 >X $08 >Y $a3 >P $d64e $58 >M $d64f $ee >M $d650 $7c >M
T{ op PC S A X Y P -> $d64f $ee $50 $38 $08 $a3 }T T{ $d64e M $d64f M $d650 M -> $58 $ee $7c }T
( 59 )
$112d >PC $31 >S $c4 >A $bd >X $a8 >Y $2f >P $112d $59 >M $112e $0b >M $112f $1d >M $1130 $58 >M $1db3 $a0 >M
T{ op PC S A X Y P -> $1130 $31 $64 $bd $a8 $2d }T T{ $112d M $112e M $112f M $1130 M $1db3 M -> $59 $0b $1d $58 $a0 }T
$9f59 >PC $83 >S $68 >A $18 >X $e1 >Y $65 >P $7416 $03 >M $9f59 $59 >M $9f5a $35 >M $9f5b $73 >M $9f5c $6e >M
T{ op PC S A X Y P -> $9f5c $83 $6b $18 $e1 $65 }T T{ $7416 M $9f59 M $9f5a M $9f5b M $9f5c M -> $03 $59 $35 $73 $6e }T
$5495 >PC $5e >S $d6 >A $ab >X $6d >Y $e5 >P $15ef $dd >M $5495 $59 >M $5496 $82 >M $5497 $15 >M $5498 $8a >M
T{ op PC S A X Y P -> $5498 $5e $0b $ab $6d $65 }T T{ $15ef M $5495 M $5496 M $5497 M $5498 M -> $dd $59 $82 $15 $8a }T
$524d >PC $41 >S $2a >A $e1 >X $fe >Y $6a >P $33ec $b3 >M $524d $59 >M $524e $ee >M $524f $32 >M $5250 $aa >M
T{ op PC S A X Y P -> $5250 $41 $99 $e1 $fe $e8 }T T{ $33ec M $524d M $524e M $524f M $5250 M -> $b3 $59 $ee $32 $aa }T
$7ae5 >PC $30 >S $d9 >A $3a >X $78 >Y $a8 >P $7ae5 $59 >M $7ae6 $75 >M $7ae7 $9b >M $7ae8 $94 >M $9bed $fa >M
T{ op PC S A X Y P -> $7ae8 $30 $23 $3a $78 $28 }T T{ $7ae5 M $7ae6 M $7ae7 M $7ae8 M $9bed M -> $59 $75 $9b $94 $fa }T
$db9d >PC $40 >S $47 >A $d6 >X $d3 >Y $21 >P $8e55 $54 >M $db9d $59 >M $db9e $82 >M $db9f $8d >M $dba0 $ed >M
T{ op PC S A X Y P -> $dba0 $40 $13 $d6 $d3 $21 }T T{ $8e55 M $db9d M $db9e M $db9f M $dba0 M -> $54 $59 $82 $8d $ed }T
$9da1 >PC $79 >S $a2 >A $9d >X $64 >Y $60 >P $9da1 $59 >M $9da2 $52 >M $9da3 $d4 >M $9da4 $d0 >M $d4b6 $00 >M
T{ op PC S A X Y P -> $9da4 $79 $a2 $9d $64 $e0 }T T{ $9da1 M $9da2 M $9da3 M $9da4 M $d4b6 M -> $59 $52 $d4 $d0 $00 }T
$110b >PC $fb >S $50 >A $51 >X $9b >Y $e6 >P $110b $59 >M $110c $5c >M $110d $25 >M $110e $4f >M $25f7 $fa >M
T{ op PC S A X Y P -> $110e $fb $aa $51 $9b $e4 }T T{ $110b M $110c M $110d M $110e M $25f7 M -> $59 $5c $25 $4f $fa }T
$b1dd >PC $13 >S $21 >A $5a >X $76 >Y $20 >P $69b5 $f9 >M $b1dd $59 >M $b1de $3f >M $b1df $69 >M $b1e0 $bc >M
T{ op PC S A X Y P -> $b1e0 $13 $d8 $5a $76 $a0 }T T{ $69b5 M $b1dd M $b1de M $b1df M $b1e0 M -> $f9 $59 $3f $69 $bc }T
$e09f >PC $a6 >S $bb >A $3c >X $9f >Y $a9 >P $7dec $fc >M $e09f $59 >M $e0a0 $4d >M $e0a1 $7d >M $e0a2 $73 >M
T{ op PC S A X Y P -> $e0a2 $a6 $47 $3c $9f $29 }T T{ $7dec M $e09f M $e0a0 M $e0a1 M $e0a2 M -> $fc $59 $4d $7d $73 }T
$e905 >PC $e4 >S $c4 >A $a7 >X $a7 >Y $2d >P $822e $e7 >M $e905 $59 >M $e906 $87 >M $e907 $81 >M $e908 $16 >M
T{ op PC S A X Y P -> $e908 $e4 $23 $a7 $a7 $2d }T T{ $822e M $e905 M $e906 M $e907 M $e908 M -> $e7 $59 $87 $81 $16 }T
$746f >PC $27 >S $8b >A $5a >X $d0 >Y $6c >P $1415 $d3 >M $746f $59 >M $7470 $45 >M $7471 $13 >M $7472 $db >M
T{ op PC S A X Y P -> $7472 $27 $58 $5a $d0 $6c }T T{ $1415 M $746f M $7470 M $7471 M $7472 M -> $d3 $59 $45 $13 $db }T
$577d >PC $91 >S $78 >A $b7 >X $5e >Y $61 >P $577d $59 >M $577e $c5 >M $577f $c6 >M $5780 $9d >M $c723 $55 >M
T{ op PC S A X Y P -> $5780 $91 $2d $b7 $5e $61 }T T{ $577d M $577e M $577f M $5780 M $c723 M -> $59 $c5 $c6 $9d $55 }T
$6f63 >PC $0d >S $e1 >A $87 >X $86 >Y $25 >P $6f63 $59 >M $6f64 $bc >M $6f65 $d9 >M $6f66 $92 >M $da42 $6e >M
T{ op PC S A X Y P -> $6f66 $0d $8f $87 $86 $a5 }T T{ $6f63 M $6f64 M $6f65 M $6f66 M $da42 M -> $59 $bc $d9 $92 $6e }T
$3b55 >PC $21 >S $10 >A $74 >X $cd >Y $29 >P $17ef $b9 >M $3b55 $59 >M $3b56 $22 >M $3b57 $17 >M $3b58 $de >M
T{ op PC S A X Y P -> $3b58 $21 $a9 $74 $cd $a9 }T T{ $17ef M $3b55 M $3b56 M $3b57 M $3b58 M -> $b9 $59 $22 $17 $de }T
$60b6 >PC $b3 >S $c4 >A $92 >X $5c >Y $e3 >P $2794 $3c >M $60b6 $59 >M $60b7 $38 >M $60b8 $27 >M $60b9 $cc >M
T{ op PC S A X Y P -> $60b9 $b3 $f8 $92 $5c $e1 }T T{ $2794 M $60b6 M $60b7 M $60b8 M $60b9 M -> $3c $59 $38 $27 $cc }T
( 5a )
$a35c >PC $53 >S $a3 >A $a1 >X $cb >Y $e3 >P $a35c $5a >M $a35d $f8 >M $a35e $cd >M
T{ op PC S A X Y P -> $a35d $52 $a3 $a1 $cb $e3 }T T{ $0153 M $a35c M $a35d M $a35e M -> $cb $5a $f8 $cd }T
$1cc6 >PC $63 >S $ba >A $56 >X $73 >Y $a7 >P $1cc6 $5a >M $1cc7 $4f >M $1cc8 $ba >M
T{ op PC S A X Y P -> $1cc7 $62 $ba $56 $73 $a7 }T T{ $0163 M $1cc6 M $1cc7 M $1cc8 M -> $73 $5a $4f $ba }T
$21fa >PC $0f >S $a6 >A $ad >X $c8 >Y $ef >P $21fa $5a >M $21fb $93 >M $21fc $df >M
T{ op PC S A X Y P -> $21fb $0e $a6 $ad $c8 $ef }T T{ $010f M $21fa M $21fb M $21fc M -> $c8 $5a $93 $df }T
$a1fd >PC $57 >S $0e >A $2a >X $3c >Y $66 >P $a1fd $5a >M $a1fe $cc >M $a1ff $52 >M
T{ op PC S A X Y P -> $a1fe $56 $0e $2a $3c $66 }T T{ $0157 M $a1fd M $a1fe M $a1ff M -> $3c $5a $cc $52 }T
$4eb5 >PC $0a >S $c3 >A $ee >X $b0 >Y $69 >P $4eb5 $5a >M $4eb6 $1d >M $4eb7 $a5 >M
T{ op PC S A X Y P -> $4eb6 $09 $c3 $ee $b0 $69 }T T{ $010a M $4eb5 M $4eb6 M $4eb7 M -> $b0 $5a $1d $a5 }T
$4422 >PC $9b >S $27 >A $62 >X $1f >Y $ee >P $4422 $5a >M $4423 $93 >M $4424 $79 >M
T{ op PC S A X Y P -> $4423 $9a $27 $62 $1f $ee }T T{ $019b M $4422 M $4423 M $4424 M -> $1f $5a $93 $79 }T
$6ca5 >PC $5e >S $58 >A $f8 >X $74 >Y $62 >P $6ca5 $5a >M $6ca6 $71 >M $6ca7 $79 >M
T{ op PC S A X Y P -> $6ca6 $5d $58 $f8 $74 $62 }T T{ $015e M $6ca5 M $6ca6 M $6ca7 M -> $74 $5a $71 $79 }T
$5cfd >PC $84 >S $57 >A $6d >X $f8 >Y $ee >P $5cfd $5a >M $5cfe $7f >M $5cff $7c >M
T{ op PC S A X Y P -> $5cfe $83 $57 $6d $f8 $ee }T T{ $0184 M $5cfd M $5cfe M $5cff M -> $f8 $5a $7f $7c }T
$6458 >PC $b0 >S $38 >A $66 >X $d9 >Y $e4 >P $6458 $5a >M $6459 $1e >M $645a $ae >M
T{ op PC S A X Y P -> $6459 $af $38 $66 $d9 $e4 }T T{ $01b0 M $6458 M $6459 M $645a M -> $d9 $5a $1e $ae }T
$3183 >PC $3b >S $e1 >A $27 >X $3e >Y $e7 >P $3183 $5a >M $3184 $fe >M $3185 $7a >M
T{ op PC S A X Y P -> $3184 $3a $e1 $27 $3e $e7 }T T{ $013b M $3183 M $3184 M $3185 M -> $3e $5a $fe $7a }T
$8f80 >PC $6e >S $47 >A $78 >X $6a >Y $ac >P $8f80 $5a >M $8f81 $5c >M $8f82 $4a >M
T{ op PC S A X Y P -> $8f81 $6d $47 $78 $6a $ac }T T{ $016e M $8f80 M $8f81 M $8f82 M -> $6a $5a $5c $4a }T
$da57 >PC $e8 >S $1c >A $99 >X $f9 >Y $2a >P $da57 $5a >M $da58 $2b >M $da59 $0e >M
T{ op PC S A X Y P -> $da58 $e7 $1c $99 $f9 $2a }T T{ $01e8 M $da57 M $da58 M $da59 M -> $f9 $5a $2b $0e }T
$4cbe >PC $f2 >S $ed >A $8d >X $cc >Y $6e >P $4cbe $5a >M $4cbf $77 >M $4cc0 $dc >M
T{ op PC S A X Y P -> $4cbf $f1 $ed $8d $cc $6e }T T{ $01f2 M $4cbe M $4cbf M $4cc0 M -> $cc $5a $77 $dc }T
$057a >PC $3e >S $b5 >A $d3 >X $39 >Y $e0 >P $057a $5a >M $057b $91 >M $057c $21 >M
T{ op PC S A X Y P -> $057b $3d $b5 $d3 $39 $e0 }T T{ $013e M $057a M $057b M $057c M -> $39 $5a $91 $21 }T
$e1a5 >PC $ae >S $bf >A $f7 >X $66 >Y $2a >P $e1a5 $5a >M $e1a6 $9c >M $e1a7 $69 >M
T{ op PC S A X Y P -> $e1a6 $ad $bf $f7 $66 $2a }T T{ $01ae M $e1a5 M $e1a6 M $e1a7 M -> $66 $5a $9c $69 }T
$3256 >PC $24 >S $56 >A $ad >X $2d >Y $e0 >P $3256 $5a >M $3257 $d8 >M $3258 $b1 >M
T{ op PC S A X Y P -> $3257 $23 $56 $ad $2d $e0 }T T{ $0124 M $3256 M $3257 M $3258 M -> $2d $5a $d8 $b1 }T
( 5b )
$83a8 >PC $24 >S $e4 >A $d4 >X $a9 >Y $2e >P $83a8 $5b >M $83a9 $e4 >M $83aa $9d >M
T{ op PC S A X Y P -> $83a9 $24 $e4 $d4 $a9 $2e }T T{ $83a8 M $83a9 M $83aa M -> $5b $e4 $9d }T
$2f82 >PC $83 >S $5f >A $90 >X $55 >Y $64 >P $2f82 $5b >M $2f83 $19 >M $2f84 $75 >M
T{ op PC S A X Y P -> $2f83 $83 $5f $90 $55 $64 }T T{ $2f82 M $2f83 M $2f84 M -> $5b $19 $75 }T
$9dd9 >PC $77 >S $44 >A $d9 >X $14 >Y $a4 >P $9dd9 $5b >M $9dda $18 >M $9ddb $35 >M
T{ op PC S A X Y P -> $9dda $77 $44 $d9 $14 $a4 }T T{ $9dd9 M $9dda M $9ddb M -> $5b $18 $35 }T
$55eb >PC $f4 >S $e0 >A $db >X $c2 >Y $a2 >P $55eb $5b >M $55ec $df >M $55ed $d9 >M
T{ op PC S A X Y P -> $55ec $f4 $e0 $db $c2 $a2 }T T{ $55eb M $55ec M $55ed M -> $5b $df $d9 }T
$46e5 >PC $38 >S $1a >A $5d >X $98 >Y $65 >P $46e5 $5b >M $46e6 $e7 >M $46e7 $d9 >M
T{ op PC S A X Y P -> $46e6 $38 $1a $5d $98 $65 }T T{ $46e5 M $46e6 M $46e7 M -> $5b $e7 $d9 }T
$ea61 >PC $6d >S $c1 >A $64 >X $5b >Y $e6 >P $ea61 $5b >M $ea62 $a6 >M $ea63 $f9 >M
T{ op PC S A X Y P -> $ea62 $6d $c1 $64 $5b $e6 }T T{ $ea61 M $ea62 M $ea63 M -> $5b $a6 $f9 }T
$289b >PC $f1 >S $9c >A $94 >X $93 >Y $e3 >P $289b $5b >M $289c $07 >M $289d $73 >M
T{ op PC S A X Y P -> $289c $f1 $9c $94 $93 $e3 }T T{ $289b M $289c M $289d M -> $5b $07 $73 }T
$9f2d >PC $e2 >S $d4 >A $6a >X $5a >Y $2a >P $9f2d $5b >M $9f2e $7c >M $9f2f $ea >M
T{ op PC S A X Y P -> $9f2e $e2 $d4 $6a $5a $2a }T T{ $9f2d M $9f2e M $9f2f M -> $5b $7c $ea }T
$80f5 >PC $f5 >S $de >A $7b >X $ef >Y $eb >P $80f5 $5b >M $80f6 $e0 >M $80f7 $24 >M
T{ op PC S A X Y P -> $80f6 $f5 $de $7b $ef $eb }T T{ $80f5 M $80f6 M $80f7 M -> $5b $e0 $24 }T
$34bd >PC $ca >S $4c >A $44 >X $9c >Y $66 >P $34bd $5b >M $34be $11 >M $34bf $5d >M
T{ op PC S A X Y P -> $34be $ca $4c $44 $9c $66 }T T{ $34bd M $34be M $34bf M -> $5b $11 $5d }T
$0102 >PC $92 >S $1a >A $1c >X $79 >Y $ae >P $0102 $5b >M $0103 $ee >M $0104 $17 >M
T{ op PC S A X Y P -> $0103 $92 $1a $1c $79 $ae }T T{ $0102 M $0103 M $0104 M -> $5b $ee $17 }T
$5438 >PC $5e >S $44 >A $c2 >X $8a >Y $a6 >P $5438 $5b >M $5439 $47 >M $543a $80 >M
T{ op PC S A X Y P -> $5439 $5e $44 $c2 $8a $a6 }T T{ $5438 M $5439 M $543a M -> $5b $47 $80 }T
$144a >PC $fd >S $1f >A $51 >X $a0 >Y $e1 >P $144a $5b >M $144b $da >M $144c $db >M
T{ op PC S A X Y P -> $144b $fd $1f $51 $a0 $e1 }T T{ $144a M $144b M $144c M -> $5b $da $db }T
$d98c >PC $0d >S $6d >A $b8 >X $67 >Y $6f >P $d98c $5b >M $d98d $f3 >M $d98e $20 >M
T{ op PC S A X Y P -> $d98d $0d $6d $b8 $67 $6f }T T{ $d98c M $d98d M $d98e M -> $5b $f3 $20 }T
$ad24 >PC $00 >S $75 >A $3d >X $1b >Y $ee >P $ad24 $5b >M $ad25 $b0 >M $ad26 $7f >M
T{ op PC S A X Y P -> $ad25 $00 $75 $3d $1b $ee }T T{ $ad24 M $ad25 M $ad26 M -> $5b $b0 $7f }T
$0ed2 >PC $2f >S $00 >A $b6 >X $36 >Y $6b >P $0ed2 $5b >M $0ed3 $53 >M $0ed4 $45 >M
T{ op PC S A X Y P -> $0ed3 $2f $00 $b6 $36 $6b }T T{ $0ed2 M $0ed3 M $0ed4 M -> $5b $53 $45 }T
( 5c )
$7df2 >PC $a2 >S $71 >A $83 >X $cb >Y $e1 >P $7df2 $5c >M $7df3 $05 >M $7df4 $c9 >M $7df5 $20 >M
T{ op PC S A X Y P -> $7df5 $a2 $71 $83 $cb $e1 }T T{ $7df2 M $7df3 M $7df4 M $7df5 M -> $5c $05 $c9 $20 }T
$dd1e >PC $14 >S $b2 >A $fb >X $42 >Y $62 >P $dd1e $5c >M $dd1f $17 >M $dd20 $28 >M $dd21 $30 >M
T{ op PC S A X Y P -> $dd21 $14 $b2 $fb $42 $62 }T T{ $dd1e M $dd1f M $dd20 M $dd21 M -> $5c $17 $28 $30 }T
$96d8 >PC $51 >S $91 >A $83 >X $ed >Y $6c >P $96d8 $5c >M $96d9 $f3 >M $96da $6e >M $96db $be >M
T{ op PC S A X Y P -> $96db $51 $91 $83 $ed $6c }T T{ $96d8 M $96d9 M $96da M $96db M -> $5c $f3 $6e $be }T
$a063 >PC $1d >S $b2 >A $88 >X $aa >Y $a1 >P $a063 $5c >M $a064 $08 >M $a065 $25 >M $a066 $88 >M
T{ op PC S A X Y P -> $a066 $1d $b2 $88 $aa $a1 }T T{ $a063 M $a064 M $a065 M $a066 M -> $5c $08 $25 $88 }T
$b82c >PC $01 >S $e5 >A $17 >X $2a >Y $ee >P $b82c $5c >M $b82d $82 >M $b82e $bd >M $b82f $64 >M
T{ op PC S A X Y P -> $b82f $01 $e5 $17 $2a $ee }T T{ $b82c M $b82d M $b82e M $b82f M -> $5c $82 $bd $64 }T
$e62d >PC $09 >S $22 >A $51 >X $17 >Y $ed >P $e62d $5c >M $e62e $ae >M $e62f $4c >M $e630 $13 >M
T{ op PC S A X Y P -> $e630 $09 $22 $51 $17 $ed }T T{ $e62d M $e62e M $e62f M $e630 M -> $5c $ae $4c $13 }T
$a3fc >PC $7c >S $2f >A $22 >X $f8 >Y $25 >P $a3fc $5c >M $a3fd $3c >M $a3fe $bc >M $a3ff $55 >M
T{ op PC S A X Y P -> $a3ff $7c $2f $22 $f8 $25 }T T{ $a3fc M $a3fd M $a3fe M $a3ff M -> $5c $3c $bc $55 }T
$3808 >PC $38 >S $f6 >A $15 >X $96 >Y $6c >P $3808 $5c >M $3809 $3a >M $380a $dc >M $380b $2c >M
T{ op PC S A X Y P -> $380b $38 $f6 $15 $96 $6c }T T{ $3808 M $3809 M $380a M $380b M -> $5c $3a $dc $2c }T
$1a1e >PC $7a >S $bc >A $a5 >X $06 >Y $ee >P $1a1e $5c >M $1a1f $b4 >M $1a20 $ee >M $1a21 $e2 >M
T{ op PC S A X Y P -> $1a21 $7a $bc $a5 $06 $ee }T T{ $1a1e M $1a1f M $1a20 M $1a21 M -> $5c $b4 $ee $e2 }T
$46a3 >PC $14 >S $cc >A $ba >X $c5 >Y $e9 >P $46a3 $5c >M $46a4 $0a >M $46a5 $01 >M $46a6 $06 >M
T{ op PC S A X Y P -> $46a6 $14 $cc $ba $c5 $e9 }T T{ $46a3 M $46a4 M $46a5 M $46a6 M -> $5c $0a $01 $06 }T
$cdc5 >PC $e9 >S $ce >A $8e >X $29 >Y $6f >P $cdc5 $5c >M $cdc6 $26 >M $cdc7 $b3 >M $cdc8 $31 >M
T{ op PC S A X Y P -> $cdc8 $e9 $ce $8e $29 $6f }T T{ $cdc5 M $cdc6 M $cdc7 M $cdc8 M -> $5c $26 $b3 $31 }T
$59b8 >PC $40 >S $d0 >A $9b >X $9a >Y $60 >P $59b8 $5c >M $59b9 $bf >M $59ba $3d >M $59bb $00 >M
T{ op PC S A X Y P -> $59bb $40 $d0 $9b $9a $60 }T T{ $59b8 M $59b9 M $59ba M $59bb M -> $5c $bf $3d $00 }T
$99e8 >PC $c0 >S $33 >A $c5 >X $b6 >Y $60 >P $99e8 $5c >M $99e9 $6f >M $99ea $b4 >M $99eb $6a >M
T{ op PC S A X Y P -> $99eb $c0 $33 $c5 $b6 $60 }T T{ $99e8 M $99e9 M $99ea M $99eb M -> $5c $6f $b4 $6a }T
$c6c9 >PC $33 >S $f2 >A $40 >X $77 >Y $6d >P $c6c9 $5c >M $c6ca $bb >M $c6cb $48 >M $c6cc $35 >M
T{ op PC S A X Y P -> $c6cc $33 $f2 $40 $77 $6d }T T{ $c6c9 M $c6ca M $c6cb M $c6cc M -> $5c $bb $48 $35 }T
$c725 >PC $b5 >S $cb >A $b4 >X $b6 >Y $61 >P $c725 $5c >M $c726 $97 >M $c727 $7c >M $c728 $8b >M
T{ op PC S A X Y P -> $c728 $b5 $cb $b4 $b6 $61 }T T{ $c725 M $c726 M $c727 M $c728 M -> $5c $97 $7c $8b }T
$c5cf >PC $bd >S $fa >A $32 >X $a8 >Y $28 >P $c5cf $5c >M $c5d0 $e9 >M $c5d1 $20 >M $c5d2 $59 >M
T{ op PC S A X Y P -> $c5d2 $bd $fa $32 $a8 $28 }T T{ $c5cf M $c5d0 M $c5d1 M $c5d2 M -> $5c $e9 $20 $59 }T
( 5d )
$8181 >PC $04 >S $c5 >A $36 >X $a5 >Y $a0 >P $40a7 $6d >M $8181 $5d >M $8182 $71 >M $8183 $40 >M $8184 $4d >M
T{ op PC S A X Y P -> $8184 $04 $a8 $36 $a5 $a0 }T T{ $40a7 M $8181 M $8182 M $8183 M $8184 M -> $6d $5d $71 $40 $4d }T
$c0fd >PC $7d >S $88 >A $a1 >X $8c >Y $e4 >P $c0fd $5d >M $c0fe $93 >M $c0ff $c4 >M $c100 $92 >M $c534 $e9 >M
T{ op PC S A X Y P -> $c100 $7d $61 $a1 $8c $64 }T T{ $c0fd M $c0fe M $c0ff M $c100 M $c534 M -> $5d $93 $c4 $92 $e9 }T
$b8c7 >PC $84 >S $73 >A $82 >X $46 >Y $ef >P $62e9 $08 >M $b8c7 $5d >M $b8c8 $67 >M $b8c9 $62 >M $b8ca $a2 >M
T{ op PC S A X Y P -> $b8ca $84 $7b $82 $46 $6d }T T{ $62e9 M $b8c7 M $b8c8 M $b8c9 M $b8ca M -> $08 $5d $67 $62 $a2 }T
$8dcf >PC $3f >S $8a >A $58 >X $3e >Y $67 >P $70f7 $1b >M $8dcf $5d >M $8dd0 $9f >M $8dd1 $70 >M $8dd2 $98 >M
T{ op PC S A X Y P -> $8dd2 $3f $91 $58 $3e $e5 }T T{ $70f7 M $8dcf M $8dd0 M $8dd1 M $8dd2 M -> $1b $5d $9f $70 $98 }T
$e967 >PC $02 >S $1a >A $eb >X $b0 >Y $a2 >P $5e5c $12 >M $e967 $5d >M $e968 $71 >M $e969 $5d >M $e96a $ea >M
T{ op PC S A X Y P -> $e96a $02 $08 $eb $b0 $20 }T T{ $5e5c M $e967 M $e968 M $e969 M $e96a M -> $12 $5d $71 $5d $ea }T
$09f8 >PC $f8 >S $b4 >A $f3 >X $b6 >Y $2c >P $09f8 $5d >M $09f9 $66 >M $09fa $db >M $09fb $0b >M $dc59 $fc >M
T{ op PC S A X Y P -> $09fb $f8 $48 $f3 $b6 $2c }T T{ $09f8 M $09f9 M $09fa M $09fb M $dc59 M -> $5d $66 $db $0b $fc }T
$a49c >PC $91 >S $ad >A $ca >X $2b >Y $e3 >P $358b $85 >M $a49c $5d >M $a49d $c1 >M $a49e $34 >M $a49f $2e >M
T{ op PC S A X Y P -> $a49f $91 $28 $ca $2b $61 }T T{ $358b M $a49c M $a49d M $a49e M $a49f M -> $85 $5d $c1 $34 $2e }T
$e464 >PC $6c >S $44 >A $fb >X $54 >Y $ac >P $6911 $00 >M $e464 $5d >M $e465 $16 >M $e466 $68 >M $e467 $8c >M
T{ op PC S A X Y P -> $e467 $6c $44 $fb $54 $2c }T T{ $6911 M $e464 M $e465 M $e466 M $e467 M -> $00 $5d $16 $68 $8c }T
$9171 >PC $68 >S $0c >A $55 >X $37 >Y $6f >P $1a23 $8c >M $9171 $5d >M $9172 $ce >M $9173 $19 >M $9174 $3c >M
T{ op PC S A X Y P -> $9174 $68 $80 $55 $37 $ed }T T{ $1a23 M $9171 M $9172 M $9173 M $9174 M -> $8c $5d $ce $19 $3c }T
$6a9a >PC $f6 >S $ca >A $57 >X $6a >Y $2a >P $6a9a $5d >M $6a9b $ea >M $6a9c $94 >M $6a9d $b6 >M $9541 $6a >M
T{ op PC S A X Y P -> $6a9d $f6 $a0 $57 $6a $a8 }T T{ $6a9a M $6a9b M $6a9c M $6a9d M $9541 M -> $5d $ea $94 $b6 $6a }T
$1d18 >PC $a3 >S $a5 >A $b4 >X $ec >Y $21 >P $1d18 $5d >M $1d19 $ec >M $1d1a $94 >M $1d1b $ef >M $95a0 $fa >M
T{ op PC S A X Y P -> $1d1b $a3 $5f $b4 $ec $21 }T T{ $1d18 M $1d19 M $1d1a M $1d1b M $95a0 M -> $5d $ec $94 $ef $fa }T
$a097 >PC $80 >S $b3 >A $3d >X $0b >Y $e2 >P $8d18 $6a >M $a097 $5d >M $a098 $db >M $a099 $8c >M $a09a $4b >M
T{ op PC S A X Y P -> $a09a $80 $d9 $3d $0b $e0 }T T{ $8d18 M $a097 M $a098 M $a099 M $a09a M -> $6a $5d $db $8c $4b }T
$0e59 >PC $7e >S $c7 >A $bd >X $55 >Y $a1 >P $0e59 $5d >M $0e5a $de >M $0e5b $12 >M $0e5c $e2 >M $139b $d6 >M
T{ op PC S A X Y P -> $0e5c $7e $11 $bd $55 $21 }T T{ $0e59 M $0e5a M $0e5b M $0e5c M $139b M -> $5d $de $12 $e2 $d6 }T
$3782 >PC $db >S $58 >A $8b >X $fa >Y $ed >P $3782 $5d >M $3783 $27 >M $3784 $f2 >M $3785 $f5 >M $f2b2 $8c >M
T{ op PC S A X Y P -> $3785 $db $d4 $8b $fa $ed }T T{ $3782 M $3783 M $3784 M $3785 M $f2b2 M -> $5d $27 $f2 $f5 $8c }T
$ad9a >PC $dd >S $45 >A $be >X $c5 >Y $a6 >P $ad9a $5d >M $ad9b $37 >M $ad9c $b5 >M $ad9d $ec >M $b5f5 $46 >M
T{ op PC S A X Y P -> $ad9d $dd $03 $be $c5 $24 }T T{ $ad9a M $ad9b M $ad9c M $ad9d M $b5f5 M -> $5d $37 $b5 $ec $46 }T
$670b >PC $bc >S $97 >A $f5 >X $8c >Y $69 >P $670b $5d >M $670c $87 >M $670d $71 >M $670e $e8 >M $727c $6e >M
T{ op PC S A X Y P -> $670e $bc $f9 $f5 $8c $e9 }T T{ $670b M $670c M $670d M $670e M $727c M -> $5d $87 $71 $e8 $6e }T
( 5e )
$b357 >PC $6d >S $4d >A $20 >X $de >Y $ad >P $6d65 $7a >M $b357 $5e >M $b358 $45 >M $b359 $6d >M $b35a $c8 >M
T{ op PC S A X Y P -> $b35a $6d $4d $20 $de $2c }T T{ $6d65 M $b357 M $b358 M $b359 M $b35a M -> $3d $5e $45 $6d $c8 }T
$806b >PC $3b >S $65 >A $74 >X $d4 >Y $a2 >P $3e42 $f3 >M $806b $5e >M $806c $ce >M $806d $3d >M $806e $ae >M
T{ op PC S A X Y P -> $806e $3b $65 $74 $d4 $21 }T T{ $3e42 M $806b M $806c M $806d M $806e M -> $79 $5e $ce $3d $ae }T
$34dd >PC $a9 >S $1d >A $54 >X $02 >Y $69 >P $34dd $5e >M $34de $da >M $34df $8b >M $34e0 $37 >M $8c2e $bf >M
T{ op PC S A X Y P -> $34e0 $a9 $1d $54 $02 $69 }T T{ $34dd M $34de M $34df M $34e0 M $8c2e M -> $5e $da $8b $37 $5f }T
$4922 >PC $f4 >S $9a >A $49 >X $c7 >Y $ac >P $4922 $5e >M $4923 $43 >M $4924 $5f >M $4925 $e5 >M $5f8c $ef >M
T{ op PC S A X Y P -> $4925 $f4 $9a $49 $c7 $2d }T T{ $4922 M $4923 M $4924 M $4925 M $5f8c M -> $5e $43 $5f $e5 $77 }T
$d65c >PC $3b >S $13 >A $c3 >X $5c >Y $21 >P $3daf $59 >M $d65c $5e >M $d65d $ec >M $d65e $3c >M $d65f $63 >M
T{ op PC S A X Y P -> $d65f $3b $13 $c3 $5c $21 }T T{ $3daf M $d65c M $d65d M $d65e M $d65f M -> $2c $5e $ec $3c $63 }T
$0605 >PC $02 >S $59 >A $f2 >X $3e >Y $67 >P $0605 $5e >M $0606 $01 >M $0607 $63 >M $0608 $2d >M $63f3 $ae >M
T{ op PC S A X Y P -> $0608 $02 $59 $f2 $3e $64 }T T{ $0605 M $0606 M $0607 M $0608 M $63f3 M -> $5e $01 $63 $2d $57 }T
$6703 >PC $79 >S $2b >A $c7 >X $0f >Y $6d >P $6703 $5e >M $6704 $11 >M $6705 $b2 >M $6706 $9d >M $b2d8 $89 >M
T{ op PC S A X Y P -> $6706 $79 $2b $c7 $0f $6d }T T{ $6703 M $6704 M $6705 M $6706 M $b2d8 M -> $5e $11 $b2 $9d $44 }T
$1cc3 >PC $7a >S $d4 >A $74 >X $f2 >Y $a4 >P $1cc3 $5e >M $1cc4 $63 >M $1cc5 $7b >M $1cc6 $9b >M $7bd7 $e0 >M
T{ op PC S A X Y P -> $1cc6 $7a $d4 $74 $f2 $24 }T T{ $1cc3 M $1cc4 M $1cc5 M $1cc6 M $7bd7 M -> $5e $63 $7b $9b $70 }T
$f98d >PC $95 >S $b7 >A $c8 >X $c9 >Y $e7 >P $d81e $a0 >M $f98d $5e >M $f98e $56 >M $f98f $d7 >M $f990 $92 >M
T{ op PC S A X Y P -> $f990 $95 $b7 $c8 $c9 $64 }T T{ $d81e M $f98d M $f98e M $f98f M $f990 M -> $50 $5e $56 $d7 $92 }T
$68d2 >PC $b2 >S $d1 >A $8b >X $04 >Y $24 >P $58f7 $9f >M $68d2 $5e >M $68d3 $6c >M $68d4 $58 >M $68d5 $10 >M
T{ op PC S A X Y P -> $68d5 $b2 $d1 $8b $04 $25 }T T{ $58f7 M $68d2 M $68d3 M $68d4 M $68d5 M -> $4f $5e $6c $58 $10 }T
$9b37 >PC $a6 >S $8a >A $8b >X $1d >Y $60 >P $9b37 $5e >M $9b38 $49 >M $9b39 $b3 >M $9b3a $3f >M $b3d4 $aa >M
T{ op PC S A X Y P -> $9b3a $a6 $8a $8b $1d $60 }T T{ $9b37 M $9b38 M $9b39 M $9b3a M $b3d4 M -> $5e $49 $b3 $3f $55 }T
$681e >PC $f0 >S $23 >A $82 >X $33 >Y $27 >P $681e $5e >M $681f $39 >M $6820 $bd >M $6821 $78 >M $bdbb $0b >M
T{ op PC S A X Y P -> $6821 $f0 $23 $82 $33 $25 }T T{ $681e M $681f M $6820 M $6821 M $bdbb M -> $5e $39 $bd $78 $05 }T
$85a6 >PC $b3 >S $9c >A $38 >X $19 >Y $ef >P $85a6 $5e >M $85a7 $4c >M $85a8 $ca >M $85a9 $a3 >M $ca84 $8d >M
T{ op PC S A X Y P -> $85a9 $b3 $9c $38 $19 $6d }T T{ $85a6 M $85a7 M $85a8 M $85a9 M $ca84 M -> $5e $4c $ca $a3 $46 }T
$b7a8 >PC $02 >S $0c >A $d4 >X $fe >Y $aa >P $b7a8 $5e >M $b7a9 $23 >M $b7aa $df >M $b7ab $1f >M $dff7 $22 >M
T{ op PC S A X Y P -> $b7ab $02 $0c $d4 $fe $28 }T T{ $b7a8 M $b7a9 M $b7aa M $b7ab M $dff7 M -> $5e $23 $df $1f $11 }T
$eb13 >PC $0e >S $3c >A $48 >X $45 >Y $ab >P $eb13 $5e >M $eb14 $18 >M $eb15 $fc >M $eb16 $89 >M $fc60 $30 >M
T{ op PC S A X Y P -> $eb16 $0e $3c $48 $45 $28 }T T{ $eb13 M $eb14 M $eb15 M $eb16 M $fc60 M -> $5e $18 $fc $89 $18 }T
$4d7f >PC $ed >S $bf >A $2a >X $34 >Y $a8 >P $4d7f $5e >M $4d80 $9a >M $4d81 $c2 >M $4d82 $96 >M $c2c4 $54 >M
T{ op PC S A X Y P -> $4d82 $ed $bf $2a $34 $28 }T T{ $4d7f M $4d80 M $4d81 M $4d82 M $c2c4 M -> $5e $9a $c2 $96 $2a }T
( 5f )
$826c >PC $73 >S $ce >A $3d >X $66 >Y $e0 >P $007c $99 >M $826c $5f >M $826d $7c >M $826e $32 >M $82a1 $5a >M
T{ op PC S A X Y P -> $82a1 $73 $ce $3d $66 $e0 }T T{ $007c M $826c M $826d M $826e M $82a1 M -> $99 $5f $7c $32 $5a }T
$584e >PC $52 >S $66 >A $20 >X $9c >Y $21 >P $00dd $b8 >M $584e $5f >M $584f $dd >M $5850 $11 >M $5851 $5e >M $5862 $90 >M
T{ op PC S A X Y P -> $5851 $52 $66 $20 $9c $21 }T T{ $00dd M $584e M $584f M $5850 M $5851 M $5862 M -> $b8 $5f $dd $11 $5e $90 }T
$7729 >PC $94 >S $f7 >A $35 >X $9e >Y $2c >P $0051 $5c >M $76fc $fc >M $7729 $5f >M $772a $51 >M $772b $d0 >M $77fc $ef >M
T{ op PC S A X Y P -> $76fc $94 $f7 $35 $9e $2c }T T{ $0051 M $76fc M $7729 M $772a M $772b M $77fc M -> $5c $fc $5f $51 $d0 $ef }T
$529f >PC $55 >S $3c >A $a4 >X $42 >Y $65 >P $00b6 $0d >M $5276 $c3 >M $529f $5f >M $52a0 $b6 >M $52a1 $d4 >M
T{ op PC S A X Y P -> $5276 $55 $3c $a4 $42 $65 }T T{ $00b6 M $5276 M $529f M $52a0 M $52a1 M -> $0d $c3 $5f $b6 $d4 }T
$434c >PC $fc >S $15 >A $59 >X $da >Y $e8 >P $00d1 $a1 >M $434c $5f >M $434d $d1 >M $434e $38 >M $434f $22 >M $4387 $69 >M
T{ op PC S A X Y P -> $434f $fc $15 $59 $da $e8 }T T{ $00d1 M $434c M $434d M $434e M $434f M $4387 M -> $a1 $5f $d1 $38 $22 $69 }T
$e57e >PC $83 >S $9c >A $b9 >X $a2 >Y $a9 >P $009d $f6 >M $e521 $66 >M $e57e $5f >M $e57f $9d >M $e580 $a0 >M $e581 $10 >M
T{ op PC S A X Y P -> $e581 $83 $9c $b9 $a2 $a9 }T T{ $009d M $e521 M $e57e M $e57f M $e580 M $e581 M -> $f6 $66 $5f $9d $a0 $10 }T
$c48c >PC $d8 >S $87 >A $57 >X $eb >Y $e6 >P $008b $72 >M $c42d $c2 >M $c48c $5f >M $c48d $8b >M $c48e $9e >M $c48f $51 >M
T{ op PC S A X Y P -> $c48f $d8 $87 $57 $eb $e6 }T T{ $008b M $c42d M $c48c M $c48d M $c48e M $c48f M -> $72 $c2 $5f $8b $9e $51 }T
$17aa >PC $70 >S $d5 >A $86 >X $da >Y $63 >P $00cd $78 >M $175c $e8 >M $17aa $5f >M $17ab $cd >M $17ac $af >M $17ad $89 >M
T{ op PC S A X Y P -> $17ad $70 $d5 $86 $da $63 }T T{ $00cd M $175c M $17aa M $17ab M $17ac M $17ad M -> $78 $e8 $5f $cd $af $89 }T
$5786 >PC $fb >S $de >A $20 >X $2e >Y $a9 >P $0098 $51 >M $5786 $5f >M $5787 $98 >M $5788 $68 >M $57f1 $0f >M
T{ op PC S A X Y P -> $57f1 $fb $de $20 $2e $a9 }T T{ $0098 M $5786 M $5787 M $5788 M $57f1 M -> $51 $5f $98 $68 $0f }T
$b9e5 >PC $8e >S $76 >A $cc >X $c2 >Y $a6 >P $00a1 $ca >M $b93f $05 >M $b9e5 $5f >M $b9e6 $a1 >M $b9e7 $57 >M $ba3f $c0 >M
T{ op PC S A X Y P -> $ba3f $8e $76 $cc $c2 $a6 }T T{ $00a1 M $b93f M $b9e5 M $b9e6 M $b9e7 M $ba3f M -> $ca $05 $5f $a1 $57 $c0 }T
$1e45 >PC $55 >S $7c >A $54 >X $8b >Y $e5 >P $00ef $2b >M $1e45 $5f >M $1e46 $ef >M $1e47 $3e >M $1e48 $63 >M $1e86 $9e >M
T{ op PC S A X Y P -> $1e48 $55 $7c $54 $8b $e5 }T T{ $00ef M $1e45 M $1e46 M $1e47 M $1e48 M $1e86 M -> $2b $5f $ef $3e $63 $9e }T
$f7cb >PC $73 >S $97 >A $af >X $b1 >Y $a8 >P $00e7 $6a >M $f763 $73 >M $f7cb $5f >M $f7cc $e7 >M $f7cd $95 >M $f7ce $e2 >M
T{ op PC S A X Y P -> $f7ce $73 $97 $af $b1 $a8 }T T{ $00e7 M $f763 M $f7cb M $f7cc M $f7cd M $f7ce M -> $6a $73 $5f $e7 $95 $e2 }T
$ca73 >PC $c1 >S $47 >A $ab >X $6c >Y $6c >P $00a2 $32 >M $ca73 $5f >M $ca74 $a2 >M $ca75 $78 >M $ca76 $33 >M $caee $25 >M
T{ op PC S A X Y P -> $ca76 $c1 $47 $ab $6c $6c }T T{ $00a2 M $ca73 M $ca74 M $ca75 M $ca76 M $caee M -> $32 $5f $a2 $78 $33 $25 }T
$67d9 >PC $94 >S $c4 >A $a3 >X $f5 >Y $64 >P $00fd $31 >M $678e $93 >M $67d9 $5f >M $67da $fd >M $67db $b2 >M $67dc $ce >M
T{ op PC S A X Y P -> $67dc $94 $c4 $a3 $f5 $64 }T T{ $00fd M $678e M $67d9 M $67da M $67db M $67dc M -> $31 $93 $5f $fd $b2 $ce }T
$6bbe >PC $b5 >S $d3 >A $00 >X $8b >Y $e2 >P $00e4 $a6 >M $6b11 $22 >M $6bbe $5f >M $6bbf $e4 >M $6bc0 $50 >M $6bc1 $9e >M
T{ op PC S A X Y P -> $6bc1 $b5 $d3 $00 $8b $e2 }T T{ $00e4 M $6b11 M $6bbe M $6bbf M $6bc0 M $6bc1 M -> $a6 $22 $5f $e4 $50 $9e }T
$3bd0 >PC $38 >S $14 >A $bd >X $3b >Y $2b >P $0052 $f5 >M $3bd0 $5f >M $3bd1 $52 >M $3bd2 $1b >M $3bd3 $7f >M $3bee $24 >M
T{ op PC S A X Y P -> $3bd3 $38 $14 $bd $3b $2b }T T{ $0052 M $3bd0 M $3bd1 M $3bd2 M $3bd3 M $3bee M -> $f5 $5f $52 $1b $7f $24 }T
( 60 )
$1c2b >PC $f6 >S $1a >A $55 >X $c1 >Y $a0 >P $01f6 $3c >M $01f7 $d8 >M $01f8 $c3 >M $1c2b $60 >M $1c2c $39 >M $1c2d $d6 >M $c3d8 $ca >M $c3d9 $f2 >M
T{ op PC S A X Y P -> $c3d9 $f8 $1a $55 $c1 $a0 }T T{ $01f6 M $01f7 M $01f8 M $1c2b M $1c2c M $1c2d M $c3d8 M $c3d9 M -> $3c $d8 $c3 $60 $39 $d6 $ca $f2 }T
$92c8 >PC $fa >S $3e >A $af >X $e6 >Y $e2 >P $01fa $96 >M $01fb $b2 >M $01fc $8e >M $8eb2 $81 >M $8eb3 $a6 >M $92c8 $60 >M $92c9 $0c >M $92ca $f0 >M
T{ op PC S A X Y P -> $8eb3 $fc $3e $af $e6 $e2 }T T{ $01fa M $01fb M $01fc M $8eb2 M $8eb3 M $92c8 M $92c9 M $92ca M -> $96 $b2 $8e $81 $a6 $60 $0c $f0 }T
$98ed >PC $df >S $51 >A $65 >X $3a >Y $ef >P $01df $30 >M $01e0 $56 >M $01e1 $a5 >M $98ed $60 >M $98ee $44 >M $98ef $1d >M $a556 $79 >M $a557 $d8 >M
T{ op PC S A X Y P -> $a557 $e1 $51 $65 $3a $ef }T T{ $01df M $01e0 M $01e1 M $98ed M $98ee M $98ef M $a556 M $a557 M -> $30 $56 $a5 $60 $44 $1d $79 $d8 }T
$485f >PC $5e >S $74 >A $db >X $6c >Y $e5 >P $015e $ad >M $015f $3d >M $0160 $b0 >M $485f $60 >M $4860 $85 >M $4861 $5b >M $b03d $ac >M $b03e $83 >M
T{ op PC S A X Y P -> $b03e $60 $74 $db $6c $e5 }T T{ $015e M $015f M $0160 M $485f M $4860 M $4861 M $b03d M $b03e M -> $ad $3d $b0 $60 $85 $5b $ac $83 }T
$8461 >PC $8d >S $f5 >A $8e >X $ae >Y $62 >P $018d $d2 >M $018e $7b >M $018f $b0 >M $8461 $60 >M $8462 $17 >M $8463 $55 >M $b07b $d4 >M $b07c $d5 >M
T{ op PC S A X Y P -> $b07c $8f $f5 $8e $ae $62 }T T{ $018d M $018e M $018f M $8461 M $8462 M $8463 M $b07b M $b07c M -> $d2 $7b $b0 $60 $17 $55 $d4 $d5 }T
$9a9a >PC $c8 >S $e1 >A $1b >X $66 >Y $a2 >P $01c8 $7e >M $01c9 $bf >M $01ca $fd >M $9a9a $60 >M $9a9b $2b >M $9a9c $71 >M $fdbf $71 >M $fdc0 $c3 >M
T{ op PC S A X Y P -> $fdc0 $ca $e1 $1b $66 $a2 }T T{ $01c8 M $01c9 M $01ca M $9a9a M $9a9b M $9a9c M $fdbf M $fdc0 M -> $7e $bf $fd $60 $2b $71 $71 $c3 }T
$ce84 >PC $5b >S $29 >A $8a >X $0f >Y $66 >P $015b $b5 >M $015c $a3 >M $015d $2b >M $2ba3 $53 >M $2ba4 $2c >M $ce84 $60 >M $ce85 $0c >M $ce86 $5c >M
T{ op PC S A X Y P -> $2ba4 $5d $29 $8a $0f $66 }T T{ $015b M $015c M $015d M $2ba3 M $2ba4 M $ce84 M $ce85 M $ce86 M -> $b5 $a3 $2b $53 $2c $60 $0c $5c }T
$79dc >PC $af >S $d5 >A $5b >X $61 >Y $ed >P $01af $a9 >M $01b0 $bf >M $01b1 $fb >M $79dc $60 >M $79dd $dc >M $79de $bb >M $fbbf $2c >M $fbc0 $46 >M
T{ op PC S A X Y P -> $fbc0 $b1 $d5 $5b $61 $ed }T T{ $01af M $01b0 M $01b1 M $79dc M $79dd M $79de M $fbbf M $fbc0 M -> $a9 $bf $fb $60 $dc $bb $2c $46 }T
$fe9f >PC $9d >S $24 >A $50 >X $f8 >Y $6e >P $019d $f3 >M $019e $5a >M $019f $da >M $da5a $9f >M $da5b $83 >M $fe9f $60 >M $fea0 $22 >M $fea1 $4c >M
T{ op PC S A X Y P -> $da5b $9f $24 $50 $f8 $6e }T T{ $019d M $019e M $019f M $da5a M $da5b M $fe9f M $fea0 M $fea1 M -> $f3 $5a $da $9f $83 $60 $22 $4c }T
$caa0 >PC $6a >S $c3 >A $3d >X $9a >Y $a9 >P $016a $97 >M $016b $3b >M $016c $c3 >M $c33b $d1 >M $c33c $96 >M $caa0 $60 >M $caa1 $e5 >M $caa2 $c0 >M
T{ op PC S A X Y P -> $c33c $6c $c3 $3d $9a $a9 }T T{ $016a M $016b M $016c M $c33b M $c33c M $caa0 M $caa1 M $caa2 M -> $97 $3b $c3 $d1 $96 $60 $e5 $c0 }T
$c154 >PC $78 >S $8e >A $06 >X $0b >Y $ab >P $0178 $01 >M $0179 $ee >M $017a $d2 >M $c154 $60 >M $c155 $50 >M $c156 $1a >M $d2ee $e8 >M $d2ef $2a >M
T{ op PC S A X Y P -> $d2ef $7a $8e $06 $0b $ab }T T{ $0178 M $0179 M $017a M $c154 M $c155 M $c156 M $d2ee M $d2ef M -> $01 $ee $d2 $60 $50 $1a $e8 $2a }T
$74ef >PC $d9 >S $9d >A $11 >X $f9 >Y $ec >P $01d9 $5c >M $01da $86 >M $01db $db >M $74ef $60 >M $74f0 $b0 >M $74f1 $57 >M $db86 $68 >M $db87 $ba >M
T{ op PC S A X Y P -> $db87 $db $9d $11 $f9 $ec }T T{ $01d9 M $01da M $01db M $74ef M $74f0 M $74f1 M $db86 M $db87 M -> $5c $86 $db $60 $b0 $57 $68 $ba }T
$0eba >PC $91 >S $f0 >A $b1 >X $0d >Y $a9 >P $0191 $34 >M $0192 $0f >M $0193 $1e >M $0eba $60 >M $0ebb $df >M $0ebc $c2 >M $1e0f $19 >M $1e10 $11 >M
T{ op PC S A X Y P -> $1e10 $93 $f0 $b1 $0d $a9 }T T{ $0191 M $0192 M $0193 M $0eba M $0ebb M $0ebc M $1e0f M $1e10 M -> $34 $0f $1e $60 $df $c2 $19 $11 }T
$2f71 >PC $32 >S $0a >A $d3 >X $7a >Y $23 >P $0132 $08 >M $0133 $ab >M $0134 $0e >M $0eab $92 >M $0eac $87 >M $2f71 $60 >M $2f72 $c7 >M $2f73 $b3 >M
T{ op PC S A X Y P -> $0eac $34 $0a $d3 $7a $23 }T T{ $0132 M $0133 M $0134 M $0eab M $0eac M $2f71 M $2f72 M $2f73 M -> $08 $ab $0e $92 $87 $60 $c7 $b3 }T
$ff98 >PC $ee >S $af >A $fd >X $a5 >Y $a5 >P $01ee $7b >M $01ef $38 >M $01f0 $e0 >M $e038 $18 >M $e039 $8a >M $ff98 $60 >M $ff99 $d2 >M $ff9a $ce >M
T{ op PC S A X Y P -> $e039 $f0 $af $fd $a5 $a5 }T T{ $01ee M $01ef M $01f0 M $e038 M $e039 M $ff98 M $ff99 M $ff9a M -> $7b $38 $e0 $18 $8a $60 $d2 $ce }T
$428e >PC $62 >S $12 >A $26 >X $b7 >Y $ad >P $0162 $6e >M $0163 $92 >M $0164 $f0 >M $428e $60 >M $428f $ab >M $4290 $a4 >M $f092 $2d >M $f093 $33 >M
T{ op PC S A X Y P -> $f093 $64 $12 $26 $b7 $ad }T T{ $0162 M $0163 M $0164 M $428e M $428f M $4290 M $f092 M $f093 M -> $6e $92 $f0 $60 $ab $a4 $2d $33 }T
( 61 )
$8a33 >PC $c9 >S $16 >A $c1 >X $9a >Y $a5 >P $0009 $3a >M $000a $2c >M $0048 $3b >M $2c3a $4a >M $8a33 $61 >M $8a34 $48 >M $8a35 $21 >M
T{ op PC S A X Y P -> $8a35 $c9 $61 $c1 $9a $24 }T T{ $0009 M $000a M $0048 M $2c3a M $8a33 M $8a34 M $8a35 M -> $3a $2c $3b $4a $61 $48 $21 }T
$ccce >PC $ee >S $14 >A $60 >X $a4 >Y $20 >P $0053 $69 >M $00b3 $55 >M $00b4 $46 >M $4655 $5b >M $ccce $61 >M $cccf $53 >M $ccd0 $8c >M
T{ op PC S A X Y P -> $ccd0 $ee $6f $60 $a4 $20 }T T{ $0053 M $00b3 M $00b4 M $4655 M $ccce M $cccf M $ccd0 M -> $69 $55 $46 $5b $61 $53 $8c }T
$c93a >PC $79 >S $8d >A $2d >X $b6 >Y $ea >P $00ca $00 >M $00f7 $9b >M $00f8 $7e >M $7e9b $40 >M $c93a $61 >M $c93b $ca >M $c93c $60 >M
T{ op PC S A X Y P -> $c93c $79 $33 $2d $b6 $29 }T T{ $00ca M $00f7 M $00f8 M $7e9b M $c93a M $c93b M $c93c M -> $00 $9b $7e $40 $61 $ca $60 }T
$94fb >PC $03 >S $41 >A $8b >X $04 >Y $64 >P $0026 $3d >M $0027 $c1 >M $009b $b9 >M $94fb $61 >M $94fc $9b >M $94fd $66 >M $c13d $9b >M
T{ op PC S A X Y P -> $94fd $03 $dc $8b $04 $a4 }T T{ $0026 M $0027 M $009b M $94fb M $94fc M $94fd M $c13d M -> $3d $c1 $b9 $61 $9b $66 $9b }T
$ca86 >PC $1b >S $8b >A $3a >X $7f >Y $ed >P $006e $d5 >M $00a8 $fe >M $00a9 $81 >M $81fe $70 >M $ca86 $61 >M $ca87 $6e >M $ca88 $0e >M
T{ op PC S A X Y P -> $ca88 $1b $62 $3a $7f $2d }T T{ $006e M $00a8 M $00a9 M $81fe M $ca86 M $ca87 M $ca88 M -> $d5 $fe $81 $70 $61 $6e $0e }T
$53ae >PC $ab >S $dc >A $be >X $31 >Y $6b >P $007f $42 >M $0080 $0f >M $00c1 $bf >M $0f42 $36 >M $53ae $61 >M $53af $c1 >M $53b0 $63 >M
T{ op PC S A X Y P -> $53b0 $ab $79 $be $31 $29 }T T{ $007f M $0080 M $00c1 M $0f42 M $53ae M $53af M $53b0 M -> $42 $0f $bf $36 $61 $c1 $63 }T
$af97 >PC $14 >S $e2 >A $03 >X $08 >Y $e2 >P $0030 $15 >M $0054 $c4 >M $0057 $30 >M $0058 $00 >M $af97 $61 >M $af98 $54 >M $af99 $11 >M
T{ op PC S A X Y P -> $af99 $14 $f7 $03 $08 $a0 }T T{ $0030 M $0054 M $0057 M $0058 M $af97 M $af98 M $af99 M -> $15 $c4 $30 $00 $61 $54 $11 }T
$e6b1 >PC $69 >S $ae >A $8e >X $cd >Y $af >P $008d $a9 >M $008e $29 >M $00ff $2e >M $29a9 $26 >M $e6b1 $61 >M $e6b2 $ff >M $e6b3 $13 >M
T{ op PC S A X Y P -> $e6b3 $69 $3b $8e $cd $2d }T T{ $008d M $008e M $00ff M $29a9 M $e6b1 M $e6b2 M $e6b3 M -> $a9 $29 $2e $26 $61 $ff $13 }T
$ff53 >PC $3a >S $ea >A $66 >X $90 >Y $69 >P $0078 $29 >M $00de $0d >M $00df $72 >M $720d $9b >M $ff53 $61 >M $ff54 $78 >M $ff55 $e2 >M
T{ op PC S A X Y P -> $ff55 $3a $ec $66 $90 $a9 }T T{ $0078 M $00de M $00df M $720d M $ff53 M $ff54 M $ff55 M -> $29 $0d $72 $9b $61 $78 $e2 }T
$dfa6 >PC $e5 >S $9d >A $06 >X $ee >Y $67 >P $0076 $d1 >M $007c $8a >M $007d $48 >M $488a $82 >M $dfa6 $61 >M $dfa7 $76 >M $dfa8 $68 >M
T{ op PC S A X Y P -> $dfa8 $e5 $20 $06 $ee $65 }T T{ $0076 M $007c M $007d M $488a M $dfa6 M $dfa7 M $dfa8 M -> $d1 $8a $48 $82 $61 $76 $68 }T
$5fed >PC $5c >S $2d >A $f9 >X $8b >Y $ab >P $0058 $85 >M $0059 $7e >M $005f $8f >M $5fed $61 >M $5fee $5f >M $5fef $11 >M $7e85 $dc >M
T{ op PC S A X Y P -> $5fef $5c $60 $f9 $8b $29 }T T{ $0058 M $0059 M $005f M $5fed M $5fee M $5fef M $7e85 M -> $85 $7e $8f $61 $5f $11 $dc }T
$062c >PC $f0 >S $d9 >A $37 >X $b3 >Y $a9 >P $0055 $94 >M $008c $92 >M $008d $08 >M $062c $61 >M $062d $55 >M $062e $6b >M $0892 $c4 >M
T{ op PC S A X Y P -> $062e $f0 $04 $37 $b3 $29 }T T{ $0055 M $008c M $008d M $062c M $062d M $062e M $0892 M -> $94 $92 $08 $61 $55 $6b $c4 }T
$7157 >PC $df >S $5a >A $ec >X $8f >Y $e9 >P $004f $f3 >M $0050 $ca >M $0063 $13 >M $7157 $61 >M $7158 $63 >M $7159 $2c >M $caf3 $1d >M
T{ op PC S A X Y P -> $7159 $df $7e $ec $8f $28 }T T{ $004f M $0050 M $0063 M $7157 M $7158 M $7159 M $caf3 M -> $f3 $ca $13 $61 $63 $2c $1d }T
$003e >PC $f4 >S $27 >A $6c >X $16 >Y $ac >P $002b $ae >M $003e $61 >M $003f $2b >M $0040 $87 >M $0097 $9b >M $0098 $97 >M $979b $80 >M
T{ op PC S A X Y P -> $0040 $f4 $07 $6c $16 $2d }T T{ $002b M $003e M $003f M $0040 M $0097 M $0098 M $979b M -> $ae $61 $2b $87 $9b $97 $80 }T
$8edc >PC $74 >S $e9 >A $c1 >X $3a >Y $68 >P $003c $3c >M $00fd $0a >M $00fe $3d >M $3d0a $d4 >M $8edc $61 >M $8edd $3c >M $8ede $6d >M
T{ op PC S A X Y P -> $8ede $74 $23 $c1 $3a $29 }T T{ $003c M $00fd M $00fe M $3d0a M $8edc M $8edd M $8ede M -> $3c $0a $3d $d4 $61 $3c $6d }T
$dfd9 >PC $18 >S $9c >A $51 >X $12 >Y $a4 >P $0034 $61 >M $0035 $c7 >M $00e3 $d0 >M $c761 $76 >M $dfd9 $61 >M $dfda $e3 >M $dfdb $8f >M
T{ op PC S A X Y P -> $dfdb $18 $12 $51 $12 $25 }T T{ $0034 M $0035 M $00e3 M $c761 M $dfd9 M $dfda M $dfdb M -> $61 $c7 $d0 $76 $61 $e3 $8f }T
( 62 )
$5911 >PC $b0 >S $7a >A $f9 >X $60 >Y $26 >P $5911 $62 >M $5912 $23 >M $5913 $83 >M
T{ op PC S A X Y P -> $5913 $b0 $7a $f9 $60 $26 }T T{ $5911 M $5912 M $5913 M -> $62 $23 $83 }T
$0e69 >PC $17 >S $10 >A $a3 >X $73 >Y $6e >P $0e69 $62 >M $0e6a $b6 >M $0e6b $3c >M
T{ op PC S A X Y P -> $0e6b $17 $10 $a3 $73 $6e }T T{ $0e69 M $0e6a M $0e6b M -> $62 $b6 $3c }T
$6029 >PC $c4 >S $dc >A $a1 >X $3c >Y $2e >P $6029 $62 >M $602a $64 >M $602b $d5 >M
T{ op PC S A X Y P -> $602b $c4 $dc $a1 $3c $2e }T T{ $6029 M $602a M $602b M -> $62 $64 $d5 }T
$5314 >PC $76 >S $7e >A $b0 >X $55 >Y $af >P $5314 $62 >M $5315 $59 >M $5316 $bb >M
T{ op PC S A X Y P -> $5316 $76 $7e $b0 $55 $af }T T{ $5314 M $5315 M $5316 M -> $62 $59 $bb }T
$8baf >PC $af >S $1a >A $5f >X $86 >Y $a1 >P $8baf $62 >M $8bb0 $0b >M $8bb1 $3f >M
T{ op PC S A X Y P -> $8bb1 $af $1a $5f $86 $a1 }T T{ $8baf M $8bb0 M $8bb1 M -> $62 $0b $3f }T
$5ef5 >PC $75 >S $87 >A $37 >X $65 >Y $e0 >P $5ef5 $62 >M $5ef6 $e2 >M $5ef7 $2f >M
T{ op PC S A X Y P -> $5ef7 $75 $87 $37 $65 $e0 }T T{ $5ef5 M $5ef6 M $5ef7 M -> $62 $e2 $2f }T
$bd0b >PC $03 >S $99 >A $2b >X $c6 >Y $ee >P $bd0b $62 >M $bd0c $ad >M $bd0d $30 >M
T{ op PC S A X Y P -> $bd0d $03 $99 $2b $c6 $ee }T T{ $bd0b M $bd0c M $bd0d M -> $62 $ad $30 }T
$3024 >PC $f3 >S $80 >A $cb >X $c1 >Y $25 >P $3024 $62 >M $3025 $aa >M $3026 $cf >M
T{ op PC S A X Y P -> $3026 $f3 $80 $cb $c1 $25 }T T{ $3024 M $3025 M $3026 M -> $62 $aa $cf }T
$0ca5 >PC $96 >S $c8 >A $c1 >X $20 >Y $a5 >P $0ca5 $62 >M $0ca6 $3d >M $0ca7 $24 >M
T{ op PC S A X Y P -> $0ca7 $96 $c8 $c1 $20 $a5 }T T{ $0ca5 M $0ca6 M $0ca7 M -> $62 $3d $24 }T
$ff57 >PC $3c >S $bc >A $14 >X $79 >Y $63 >P $ff57 $62 >M $ff58 $ea >M $ff59 $cf >M
T{ op PC S A X Y P -> $ff59 $3c $bc $14 $79 $63 }T T{ $ff57 M $ff58 M $ff59 M -> $62 $ea $cf }T
$b0b7 >PC $b4 >S $4c >A $06 >X $f4 >Y $61 >P $b0b7 $62 >M $b0b8 $66 >M $b0b9 $fe >M
T{ op PC S A X Y P -> $b0b9 $b4 $4c $06 $f4 $61 }T T{ $b0b7 M $b0b8 M $b0b9 M -> $62 $66 $fe }T
$f187 >PC $91 >S $09 >A $e8 >X $fa >Y $a7 >P $f187 $62 >M $f188 $a4 >M $f189 $0a >M
T{ op PC S A X Y P -> $f189 $91 $09 $e8 $fa $a7 }T T{ $f187 M $f188 M $f189 M -> $62 $a4 $0a }T
$d832 >PC $e8 >S $91 >A $fd >X $77 >Y $ae >P $d832 $62 >M $d833 $6f >M $d834 $ac >M
T{ op PC S A X Y P -> $d834 $e8 $91 $fd $77 $ae }T T{ $d832 M $d833 M $d834 M -> $62 $6f $ac }T
$d36c >PC $9c >S $1e >A $b8 >X $17 >Y $2a >P $d36c $62 >M $d36d $f9 >M $d36e $a1 >M
T{ op PC S A X Y P -> $d36e $9c $1e $b8 $17 $2a }T T{ $d36c M $d36d M $d36e M -> $62 $f9 $a1 }T
$a785 >PC $99 >S $50 >A $da >X $1a >Y $23 >P $a785 $62 >M $a786 $38 >M $a787 $5b >M
T{ op PC S A X Y P -> $a787 $99 $50 $da $1a $23 }T T{ $a785 M $a786 M $a787 M -> $62 $38 $5b }T
$e696 >PC $14 >S $24 >A $2c >X $db >Y $24 >P $e696 $62 >M $e697 $ce >M $e698 $bf >M
T{ op PC S A X Y P -> $e698 $14 $24 $2c $db $24 }T T{ $e696 M $e697 M $e698 M -> $62 $ce $bf }T
( 63 )
$987e >PC $43 >S $61 >A $1c >X $ce >Y $61 >P $987e $63 >M $987f $46 >M $9880 $be >M
T{ op PC S A X Y P -> $987f $43 $61 $1c $ce $61 }T T{ $987e M $987f M $9880 M -> $63 $46 $be }T
$e71c >PC $e7 >S $50 >A $33 >X $3b >Y $e9 >P $e71c $63 >M $e71d $93 >M $e71e $50 >M
T{ op PC S A X Y P -> $e71d $e7 $50 $33 $3b $e9 }T T{ $e71c M $e71d M $e71e M -> $63 $93 $50 }T
$6cb5 >PC $bf >S $91 >A $0d >X $72 >Y $22 >P $6cb5 $63 >M $6cb6 $4d >M $6cb7 $7a >M
T{ op PC S A X Y P -> $6cb6 $bf $91 $0d $72 $22 }T T{ $6cb5 M $6cb6 M $6cb7 M -> $63 $4d $7a }T
$da29 >PC $0f >S $98 >A $93 >X $23 >Y $2e >P $da29 $63 >M $da2a $55 >M $da2b $d0 >M
T{ op PC S A X Y P -> $da2a $0f $98 $93 $23 $2e }T T{ $da29 M $da2a M $da2b M -> $63 $55 $d0 }T
$dbb6 >PC $a4 >S $1f >A $37 >X $34 >Y $e7 >P $dbb6 $63 >M $dbb7 $35 >M $dbb8 $6a >M
T{ op PC S A X Y P -> $dbb7 $a4 $1f $37 $34 $e7 }T T{ $dbb6 M $dbb7 M $dbb8 M -> $63 $35 $6a }T
$e7e7 >PC $72 >S $c4 >A $2f >X $b9 >Y $a1 >P $e7e7 $63 >M $e7e8 $5a >M $e7e9 $40 >M
T{ op PC S A X Y P -> $e7e8 $72 $c4 $2f $b9 $a1 }T T{ $e7e7 M $e7e8 M $e7e9 M -> $63 $5a $40 }T
$0d1b >PC $fd >S $28 >A $87 >X $14 >Y $a1 >P $0d1b $63 >M $0d1c $24 >M $0d1d $6c >M
T{ op PC S A X Y P -> $0d1c $fd $28 $87 $14 $a1 }T T{ $0d1b M $0d1c M $0d1d M -> $63 $24 $6c }T
$f11b >PC $5b >S $eb >A $c5 >X $08 >Y $e5 >P $f11b $63 >M $f11c $92 >M $f11d $25 >M
T{ op PC S A X Y P -> $f11c $5b $eb $c5 $08 $e5 }T T{ $f11b M $f11c M $f11d M -> $63 $92 $25 }T
$f508 >PC $eb >S $07 >A $d7 >X $05 >Y $65 >P $f508 $63 >M $f509 $70 >M $f50a $a3 >M
T{ op PC S A X Y P -> $f509 $eb $07 $d7 $05 $65 }T T{ $f508 M $f509 M $f50a M -> $63 $70 $a3 }T
$13d3 >PC $20 >S $b8 >A $ab >X $e0 >Y $21 >P $13d3 $63 >M $13d4 $90 >M $13d5 $ac >M
T{ op PC S A X Y P -> $13d4 $20 $b8 $ab $e0 $21 }T T{ $13d3 M $13d4 M $13d5 M -> $63 $90 $ac }T
$8e53 >PC $8a >S $d7 >A $6f >X $91 >Y $ec >P $8e53 $63 >M $8e54 $51 >M $8e55 $69 >M
T{ op PC S A X Y P -> $8e54 $8a $d7 $6f $91 $ec }T T{ $8e53 M $8e54 M $8e55 M -> $63 $51 $69 }T
$38f4 >PC $98 >S $9f >A $2a >X $56 >Y $2e >P $38f4 $63 >M $38f5 $7b >M $38f6 $e4 >M
T{ op PC S A X Y P -> $38f5 $98 $9f $2a $56 $2e }T T{ $38f4 M $38f5 M $38f6 M -> $63 $7b $e4 }T
$d585 >PC $11 >S $72 >A $7c >X $e1 >Y $66 >P $d585 $63 >M $d586 $f1 >M $d587 $a5 >M
T{ op PC S A X Y P -> $d586 $11 $72 $7c $e1 $66 }T T{ $d585 M $d586 M $d587 M -> $63 $f1 $a5 }T
$74f6 >PC $cf >S $10 >A $27 >X $c5 >Y $23 >P $74f6 $63 >M $74f7 $b3 >M $74f8 $63 >M
T{ op PC S A X Y P -> $74f7 $cf $10 $27 $c5 $23 }T T{ $74f6 M $74f7 M $74f8 M -> $63 $b3 $63 }T
$aabc >PC $cc >S $7e >A $80 >X $d5 >Y $23 >P $aabc $63 >M $aabd $6e >M $aabe $81 >M
T{ op PC S A X Y P -> $aabd $cc $7e $80 $d5 $23 }T T{ $aabc M $aabd M $aabe M -> $63 $6e $81 }T
$03db >PC $3d >S $4e >A $b0 >X $66 >Y $a4 >P $03db $63 >M $03dc $e3 >M $03dd $0e >M
T{ op PC S A X Y P -> $03dc $3d $4e $b0 $66 $a4 }T T{ $03db M $03dc M $03dd M -> $63 $e3 $0e }T
( 64 )
$f2df >PC $c6 >S $c4 >A $2e >X $31 >Y $ea >P $f2df $64 >M $f2e0 $e3 >M $f2e1 $53 >M
T{ op PC S A X Y P -> $f2e1 $c6 $c4 $2e $31 $ea }T T{ $00e3 M $f2df M $f2e0 M $f2e1 M -> $00 $64 $e3 $53 }T
$cc95 >PC $a6 >S $94 >A $2e >X $f5 >Y $6f >P $cc95 $64 >M $cc96 $94 >M $cc97 $38 >M
T{ op PC S A X Y P -> $cc97 $a6 $94 $2e $f5 $6f }T T{ $0094 M $cc95 M $cc96 M $cc97 M -> $00 $64 $94 $38 }T
$66f3 >PC $07 >S $d8 >A $de >X $e7 >Y $e3 >P $66f3 $64 >M $66f4 $78 >M $66f5 $46 >M
T{ op PC S A X Y P -> $66f5 $07 $d8 $de $e7 $e3 }T T{ $0078 M $66f3 M $66f4 M $66f5 M -> $00 $64 $78 $46 }T
$717c >PC $b3 >S $e5 >A $e2 >X $28 >Y $2b >P $717c $64 >M $717d $15 >M $717e $06 >M
T{ op PC S A X Y P -> $717e $b3 $e5 $e2 $28 $2b }T T{ $0015 M $717c M $717d M $717e M -> $00 $64 $15 $06 }T
$f2fd >PC $64 >S $67 >A $08 >X $90 >Y $2f >P $f2fd $64 >M $f2fe $36 >M $f2ff $5e >M
T{ op PC S A X Y P -> $f2ff $64 $67 $08 $90 $2f }T T{ $0036 M $f2fd M $f2fe M $f2ff M -> $00 $64 $36 $5e }T
$0f02 >PC $d7 >S $fb >A $d1 >X $1c >Y $2b >P $0f02 $64 >M $0f03 $72 >M $0f04 $46 >M
T{ op PC S A X Y P -> $0f04 $d7 $fb $d1 $1c $2b }T T{ $0072 M $0f02 M $0f03 M $0f04 M -> $00 $64 $72 $46 }T
$32eb >PC $2d >S $5a >A $bd >X $70 >Y $25 >P $32eb $64 >M $32ec $7d >M $32ed $2c >M
T{ op PC S A X Y P -> $32ed $2d $5a $bd $70 $25 }T T{ $007d M $32eb M $32ec M $32ed M -> $00 $64 $7d $2c }T
$2881 >PC $bf >S $ea >A $59 >X $43 >Y $ac >P $2881 $64 >M $2882 $fd >M $2883 $16 >M
T{ op PC S A X Y P -> $2883 $bf $ea $59 $43 $ac }T T{ $00fd M $2881 M $2882 M $2883 M -> $00 $64 $fd $16 }T
$688d >PC $0e >S $1b >A $9c >X $98 >Y $e5 >P $688d $64 >M $688e $a5 >M $688f $f4 >M
T{ op PC S A X Y P -> $688f $0e $1b $9c $98 $e5 }T T{ $00a5 M $688d M $688e M $688f M -> $00 $64 $a5 $f4 }T
$2345 >PC $08 >S $06 >A $5a >X $0a >Y $62 >P $2345 $64 >M $2346 $1f >M $2347 $28 >M
T{ op PC S A X Y P -> $2347 $08 $06 $5a $0a $62 }T T{ $001f M $2345 M $2346 M $2347 M -> $00 $64 $1f $28 }T
$4a2d >PC $cd >S $3a >A $f2 >X $73 >Y $60 >P $4a2d $64 >M $4a2e $4e >M $4a2f $36 >M
T{ op PC S A X Y P -> $4a2f $cd $3a $f2 $73 $60 }T T{ $004e M $4a2d M $4a2e M $4a2f M -> $00 $64 $4e $36 }T
$5777 >PC $c3 >S $cd >A $0d >X $f9 >Y $e6 >P $5777 $64 >M $5778 $49 >M $5779 $a2 >M
T{ op PC S A X Y P -> $5779 $c3 $cd $0d $f9 $e6 }T T{ $0049 M $5777 M $5778 M $5779 M -> $00 $64 $49 $a2 }T
$d439 >PC $83 >S $bf >A $0d >X $2e >Y $ec >P $d439 $64 >M $d43a $6e >M $d43b $a0 >M
T{ op PC S A X Y P -> $d43b $83 $bf $0d $2e $ec }T T{ $006e M $d439 M $d43a M $d43b M -> $00 $64 $6e $a0 }T
$0e4f >PC $99 >S $05 >A $ea >X $37 >Y $ed >P $0e4f $64 >M $0e50 $bd >M $0e51 $75 >M
T{ op PC S A X Y P -> $0e51 $99 $05 $ea $37 $ed }T T{ $00bd M $0e4f M $0e50 M $0e51 M -> $00 $64 $bd $75 }T
$8b8f >PC $75 >S $44 >A $5b >X $d9 >Y $e2 >P $8b8f $64 >M $8b90 $48 >M $8b91 $84 >M
T{ op PC S A X Y P -> $8b91 $75 $44 $5b $d9 $e2 }T T{ $0048 M $8b8f M $8b90 M $8b91 M -> $00 $64 $48 $84 }T
$8b76 >PC $cd >S $75 >A $ad >X $33 >Y $e4 >P $8b76 $64 >M $8b77 $86 >M $8b78 $51 >M
T{ op PC S A X Y P -> $8b78 $cd $75 $ad $33 $e4 }T T{ $0086 M $8b76 M $8b77 M $8b78 M -> $00 $64 $86 $51 }T
( 65 )
$abb3 >PC $1c >S $44 >A $d3 >X $78 >Y $a3 >P $0084 $73 >M $abb3 $65 >M $abb4 $84 >M $abb5 $e0 >M
T{ op PC S A X Y P -> $abb5 $1c $b8 $d3 $78 $e0 }T T{ $0084 M $abb3 M $abb4 M $abb5 M -> $73 $65 $84 $e0 }T
$f37c >PC $ba >S $3f >A $bd >X $5d >Y $21 >P $0028 $12 >M $f37c $65 >M $f37d $28 >M $f37e $0d >M
T{ op PC S A X Y P -> $f37e $ba $52 $bd $5d $20 }T T{ $0028 M $f37c M $f37d M $f37e M -> $12 $65 $28 $0d }T
$945e >PC $63 >S $f8 >A $e7 >X $a5 >Y $eb >P $00a8 $26 >M $945e $65 >M $945f $a8 >M $9460 $cf >M
T{ op PC S A X Y P -> $9460 $63 $85 $e7 $a5 $a9 }T T{ $00a8 M $945e M $945f M $9460 M -> $26 $65 $a8 $cf }T
$b841 >PC $a1 >S $65 >A $65 >X $f6 >Y $e9 >P $0026 $6e >M $b841 $65 >M $b842 $26 >M $b843 $c1 >M
T{ op PC S A X Y P -> $b843 $a1 $3a $65 $f6 $69 }T T{ $0026 M $b841 M $b842 M $b843 M -> $6e $65 $26 $c1 }T
$e59d >PC $c4 >S $bc >A $83 >X $f2 >Y $ac >P $0029 $30 >M $e59d $65 >M $e59e $29 >M $e59f $01 >M
T{ op PC S A X Y P -> $e59f $c4 $52 $83 $f2 $2d }T T{ $0029 M $e59d M $e59e M $e59f M -> $30 $65 $29 $01 }T
$3aee >PC $68 >S $60 >A $ae >X $2c >Y $a3 >P $0043 $90 >M $3aee $65 >M $3aef $43 >M $3af0 $bc >M
T{ op PC S A X Y P -> $3af0 $68 $f1 $ae $2c $a0 }T T{ $0043 M $3aee M $3aef M $3af0 M -> $90 $65 $43 $bc }T
$9a9c >PC $3d >S $77 >A $74 >X $7a >Y $2d >P $002d $25 >M $9a9c $65 >M $9a9d $2d >M $9a9e $77 >M
T{ op PC S A X Y P -> $9a9e $3d $03 $74 $7a $6d }T T{ $002d M $9a9c M $9a9d M $9a9e M -> $25 $65 $2d $77 }T
$5c63 >PC $48 >S $f1 >A $19 >X $3d >Y $ae >P $0010 $e1 >M $5c63 $65 >M $5c64 $10 >M $5c65 $17 >M
T{ op PC S A X Y P -> $5c65 $48 $32 $19 $3d $2d }T T{ $0010 M $5c63 M $5c64 M $5c65 M -> $e1 $65 $10 $17 }T
$2e3c >PC $98 >S $57 >A $f7 >X $be >Y $e2 >P $00fe $d9 >M $2e3c $65 >M $2e3d $fe >M $2e3e $79 >M
T{ op PC S A X Y P -> $2e3e $98 $30 $f7 $be $21 }T T{ $00fe M $2e3c M $2e3d M $2e3e M -> $d9 $65 $fe $79 }T
$4cbe >PC $a6 >S $ce >A $0f >X $19 >Y $ef >P $0072 $95 >M $4cbe $65 >M $4cbf $72 >M $4cc0 $6b >M
T{ op PC S A X Y P -> $4cc0 $a6 $ca $0f $19 $ed }T T{ $0072 M $4cbe M $4cbf M $4cc0 M -> $95 $65 $72 $6b }T
$6f20 >PC $d2 >S $ce >A $ea >X $46 >Y $2c >P $00cf $2b >M $6f20 $65 >M $6f21 $cf >M $6f22 $12 >M
T{ op PC S A X Y P -> $6f22 $d2 $5f $ea $46 $2d }T T{ $00cf M $6f20 M $6f21 M $6f22 M -> $2b $65 $cf $12 }T
$b250 >PC $0e >S $81 >A $3b >X $b5 >Y $a0 >P $00df $9d >M $b250 $65 >M $b251 $df >M $b252 $03 >M
T{ op PC S A X Y P -> $b252 $0e $1e $3b $b5 $61 }T T{ $00df M $b250 M $b251 M $b252 M -> $9d $65 $df $03 }T
$381e >PC $67 >S $89 >A $31 >X $71 >Y $e8 >P $00aa $f6 >M $381e $65 >M $381f $aa >M $3820 $0d >M
T{ op PC S A X Y P -> $3820 $67 $e5 $31 $71 $a9 }T T{ $00aa M $381e M $381f M $3820 M -> $f6 $65 $aa $0d }T
$d7bf >PC $3a >S $bb >A $3f >X $53 >Y $60 >P $0053 $a3 >M $d7bf $65 >M $d7c0 $53 >M $d7c1 $57 >M
T{ op PC S A X Y P -> $d7c1 $3a $5e $3f $53 $61 }T T{ $0053 M $d7bf M $d7c0 M $d7c1 M -> $a3 $65 $53 $57 }T
$56b9 >PC $60 >S $8c >A $82 >X $6a >Y $e5 >P $008d $1a >M $56b9 $65 >M $56ba $8d >M $56bb $e0 >M
T{ op PC S A X Y P -> $56bb $60 $a7 $82 $6a $a4 }T T{ $008d M $56b9 M $56ba M $56bb M -> $1a $65 $8d $e0 }T
$a12c >PC $24 >S $7e >A $6f >X $60 >Y $6d >P $0013 $18 >M $a12c $65 >M $a12d $13 >M $a12e $ef >M
T{ op PC S A X Y P -> $a12e $24 $9d $6f $60 $ec }T T{ $0013 M $a12c M $a12d M $a12e M -> $18 $65 $13 $ef }T
( 66 )
$807d >PC $b3 >S $e1 >A $52 >X $f6 >Y $ee >P $0052 $ea >M $807d $66 >M $807e $52 >M $807f $21 >M
T{ op PC S A X Y P -> $807f $b3 $e1 $52 $f6 $6c }T T{ $0052 M $807d M $807e M $807f M -> $75 $66 $52 $21 }T
$2b6d >PC $c4 >S $d5 >A $47 >X $d2 >Y $28 >P $005e $da >M $2b6d $66 >M $2b6e $5e >M $2b6f $c2 >M
T{ op PC S A X Y P -> $2b6f $c4 $d5 $47 $d2 $28 }T T{ $005e M $2b6d M $2b6e M $2b6f M -> $6d $66 $5e $c2 }T
$1f02 >PC $04 >S $ef >A $a6 >X $e6 >Y $e4 >P $00bb $5b >M $1f02 $66 >M $1f03 $bb >M $1f04 $80 >M
T{ op PC S A X Y P -> $1f04 $04 $ef $a6 $e6 $65 }T T{ $00bb M $1f02 M $1f03 M $1f04 M -> $2d $66 $bb $80 }T
$493c >PC $66 >S $71 >A $67 >X $a4 >Y $27 >P $0077 $d7 >M $493c $66 >M $493d $77 >M $493e $ca >M
T{ op PC S A X Y P -> $493e $66 $71 $67 $a4 $a5 }T T{ $0077 M $493c M $493d M $493e M -> $eb $66 $77 $ca }T
$c2ac >PC $ad >S $74 >A $c8 >X $74 >Y $a5 >P $00b1 $fb >M $c2ac $66 >M $c2ad $b1 >M $c2ae $74 >M
T{ op PC S A X Y P -> $c2ae $ad $74 $c8 $74 $a5 }T T{ $00b1 M $c2ac M $c2ad M $c2ae M -> $fd $66 $b1 $74 }T
$8d25 >PC $98 >S $ea >A $ba >X $92 >Y $ef >P $0030 $ab >M $8d25 $66 >M $8d26 $30 >M $8d27 $90 >M
T{ op PC S A X Y P -> $8d27 $98 $ea $ba $92 $ed }T T{ $0030 M $8d25 M $8d26 M $8d27 M -> $d5 $66 $30 $90 }T
$0211 >PC $31 >S $6a >A $b9 >X $31 >Y $e8 >P $00c4 $27 >M $0211 $66 >M $0212 $c4 >M $0213 $31 >M
T{ op PC S A X Y P -> $0213 $31 $6a $b9 $31 $69 }T T{ $00c4 M $0211 M $0212 M $0213 M -> $13 $66 $c4 $31 }T
$52f0 >PC $04 >S $90 >A $75 >X $15 >Y $60 >P $00b0 $c5 >M $52f0 $66 >M $52f1 $b0 >M $52f2 $87 >M
T{ op PC S A X Y P -> $52f2 $04 $90 $75 $15 $61 }T T{ $00b0 M $52f0 M $52f1 M $52f2 M -> $62 $66 $b0 $87 }T
$d3bb >PC $3b >S $4e >A $48 >X $81 >Y $28 >P $003e $4f >M $d3bb $66 >M $d3bc $3e >M $d3bd $e3 >M
T{ op PC S A X Y P -> $d3bd $3b $4e $48 $81 $29 }T T{ $003e M $d3bb M $d3bc M $d3bd M -> $27 $66 $3e $e3 }T
$44c5 >PC $35 >S $33 >A $d9 >X $a3 >Y $a8 >P $0016 $72 >M $44c5 $66 >M $44c6 $16 >M $44c7 $79 >M
T{ op PC S A X Y P -> $44c7 $35 $33 $d9 $a3 $28 }T T{ $0016 M $44c5 M $44c6 M $44c7 M -> $39 $66 $16 $79 }T
$2e4f >PC $85 >S $46 >A $38 >X $62 >Y $6f >P $0060 $5f >M $2e4f $66 >M $2e50 $60 >M $2e51 $a5 >M
T{ op PC S A X Y P -> $2e51 $85 $46 $38 $62 $ed }T T{ $0060 M $2e4f M $2e50 M $2e51 M -> $af $66 $60 $a5 }T
$72df >PC $d4 >S $4b >A $a2 >X $c2 >Y $ef >P $0002 $98 >M $72df $66 >M $72e0 $02 >M $72e1 $9a >M
T{ op PC S A X Y P -> $72e1 $d4 $4b $a2 $c2 $ec }T T{ $0002 M $72df M $72e0 M $72e1 M -> $cc $66 $02 $9a }T
$a796 >PC $be >S $9e >A $a6 >X $e3 >Y $24 >P $0059 $73 >M $a796 $66 >M $a797 $59 >M $a798 $c3 >M
T{ op PC S A X Y P -> $a798 $be $9e $a6 $e3 $25 }T T{ $0059 M $a796 M $a797 M $a798 M -> $39 $66 $59 $c3 }T
$3452 >PC $cf >S $45 >A $f0 >X $0d >Y $ee >P $005e $6f >M $3452 $66 >M $3453 $5e >M $3454 $df >M
T{ op PC S A X Y P -> $3454 $cf $45 $f0 $0d $6d }T T{ $005e M $3452 M $3453 M $3454 M -> $37 $66 $5e $df }T
$2b28 >PC $11 >S $dc >A $f6 >X $02 >Y $ed >P $00d9 $4d >M $2b28 $66 >M $2b29 $d9 >M $2b2a $bf >M
T{ op PC S A X Y P -> $2b2a $11 $dc $f6 $02 $ed }T T{ $00d9 M $2b28 M $2b29 M $2b2a M -> $a6 $66 $d9 $bf }T
$a9b5 >PC $d6 >S $ae >A $fb >X $82 >Y $2a >P $0094 $35 >M $a9b5 $66 >M $a9b6 $94 >M $a9b7 $8d >M
T{ op PC S A X Y P -> $a9b7 $d6 $ae $fb $82 $29 }T T{ $0094 M $a9b5 M $a9b6 M $a9b7 M -> $1a $66 $94 $8d }T
( 67 )
$bdf6 >PC $37 >S $2a >A $9c >X $d2 >Y $a2 >P $00a4 $d9 >M $bdf6 $67 >M $bdf7 $a4 >M $bdf8 $3f >M
T{ op PC S A X Y P -> $bdf8 $37 $2a $9c $d2 $a2 }T T{ $00a4 M $bdf6 M $bdf7 M $bdf8 M -> $99 $67 $a4 $3f }T
$a5c0 >PC $bc >S $c3 >A $4d >X $8e >Y $6e >P $0001 $0b >M $a5c0 $67 >M $a5c1 $01 >M $a5c2 $c6 >M
T{ op PC S A X Y P -> $a5c2 $bc $c3 $4d $8e $6e }T T{ $0001 M $a5c0 M $a5c1 M $a5c2 M -> $0b $67 $01 $c6 }T
$f94d >PC $b6 >S $f5 >A $af >X $74 >Y $66 >P $006f $d9 >M $f94d $67 >M $f94e $6f >M $f94f $59 >M
T{ op PC S A X Y P -> $f94f $b6 $f5 $af $74 $66 }T T{ $006f M $f94d M $f94e M $f94f M -> $99 $67 $6f $59 }T
$4abb >PC $3f >S $86 >A $53 >X $2c >Y $ea >P $00f7 $5f >M $4abb $67 >M $4abc $f7 >M $4abd $b2 >M
T{ op PC S A X Y P -> $4abd $3f $86 $53 $2c $ea }T T{ $00f7 M $4abb M $4abc M $4abd M -> $1f $67 $f7 $b2 }T
$617c >PC $90 >S $8c >A $a7 >X $79 >Y $2b >P $0069 $07 >M $617c $67 >M $617d $69 >M $617e $9f >M
T{ op PC S A X Y P -> $617e $90 $8c $a7 $79 $2b }T T{ $0069 M $617c M $617d M $617e M -> $07 $67 $69 $9f }T
$6ed4 >PC $bb >S $23 >A $5a >X $c5 >Y $63 >P $0067 $4c >M $6ed4 $67 >M $6ed5 $67 >M $6ed6 $74 >M
T{ op PC S A X Y P -> $6ed6 $bb $23 $5a $c5 $63 }T T{ $0067 M $6ed4 M $6ed5 M $6ed6 M -> $0c $67 $67 $74 }T
$dcc2 >PC $92 >S $2a >A $ad >X $40 >Y $66 >P $00d3 $fb >M $dcc2 $67 >M $dcc3 $d3 >M $dcc4 $89 >M
T{ op PC S A X Y P -> $dcc4 $92 $2a $ad $40 $66 }T T{ $00d3 M $dcc2 M $dcc3 M $dcc4 M -> $bb $67 $d3 $89 }T
$51ac >PC $02 >S $cb >A $5c >X $72 >Y $67 >P $00f6 $5d >M $51ac $67 >M $51ad $f6 >M $51ae $57 >M
T{ op PC S A X Y P -> $51ae $02 $cb $5c $72 $67 }T T{ $00f6 M $51ac M $51ad M $51ae M -> $1d $67 $f6 $57 }T
$e38c >PC $45 >S $66 >A $47 >X $0f >Y $a1 >P $0041 $7d >M $e38c $67 >M $e38d $41 >M $e38e $72 >M
T{ op PC S A X Y P -> $e38e $45 $66 $47 $0f $a1 }T T{ $0041 M $e38c M $e38d M $e38e M -> $3d $67 $41 $72 }T
$bd8a >PC $d8 >S $03 >A $39 >X $1d >Y $a8 >P $0027 $69 >M $bd8a $67 >M $bd8b $27 >M $bd8c $31 >M
T{ op PC S A X Y P -> $bd8c $d8 $03 $39 $1d $a8 }T T{ $0027 M $bd8a M $bd8b M $bd8c M -> $29 $67 $27 $31 }T
$aac6 >PC $fe >S $35 >A $e7 >X $9a >Y $eb >P $00d4 $f2 >M $aac6 $67 >M $aac7 $d4 >M $aac8 $b2 >M
T{ op PC S A X Y P -> $aac8 $fe $35 $e7 $9a $eb }T T{ $00d4 M $aac6 M $aac7 M $aac8 M -> $b2 $67 $d4 $b2 }T
$301f >PC $db >S $60 >A $a3 >X $b0 >Y $6e >P $005a $4a >M $301f $67 >M $3020 $5a >M $3021 $d6 >M
T{ op PC S A X Y P -> $3021 $db $60 $a3 $b0 $6e }T T{ $005a M $301f M $3020 M $3021 M -> $0a $67 $5a $d6 }T
$b35d >PC $5f >S $76 >A $21 >X $03 >Y $23 >P $00c4 $78 >M $b35d $67 >M $b35e $c4 >M $b35f $12 >M
T{ op PC S A X Y P -> $b35f $5f $76 $21 $03 $23 }T T{ $00c4 M $b35d M $b35e M $b35f M -> $38 $67 $c4 $12 }T
$4c2d >PC $71 >S $5b >A $89 >X $9a >Y $66 >P $0086 $ff >M $4c2d $67 >M $4c2e $86 >M $4c2f $bb >M
T{ op PC S A X Y P -> $4c2f $71 $5b $89 $9a $66 }T T{ $0086 M $4c2d M $4c2e M $4c2f M -> $bf $67 $86 $bb }T
$2b2c >PC $56 >S $d9 >A $64 >X $66 >Y $ae >P $0080 $eb >M $2b2c $67 >M $2b2d $80 >M $2b2e $06 >M
T{ op PC S A X Y P -> $2b2e $56 $d9 $64 $66 $ae }T T{ $0080 M $2b2c M $2b2d M $2b2e M -> $ab $67 $80 $06 }T
$d979 >PC $b6 >S $0b >A $0a >X $3f >Y $e2 >P $002c $69 >M $d979 $67 >M $d97a $2c >M $d97b $eb >M
T{ op PC S A X Y P -> $d97b $b6 $0b $0a $3f $e2 }T T{ $002c M $d979 M $d97a M $d97b M -> $29 $67 $2c $eb }T
( 68 )
$146a >PC $d5 >S $29 >A $af >X $6e >Y $a1 >P $01d5 $cb >M $01d6 $79 >M $146a $68 >M $146b $84 >M $146c $8f >M
T{ op PC S A X Y P -> $146b $d6 $79 $af $6e $21 }T T{ $01d5 M $01d6 M $146a M $146b M $146c M -> $cb $79 $68 $84 $8f }T
$37a2 >PC $81 >S $23 >A $1b >X $e7 >Y $6f >P $0181 $93 >M $0182 $ab >M $37a2 $68 >M $37a3 $91 >M $37a4 $b1 >M
T{ op PC S A X Y P -> $37a3 $82 $ab $1b $e7 $ed }T T{ $0181 M $0182 M $37a2 M $37a3 M $37a4 M -> $93 $ab $68 $91 $b1 }T
$6488 >PC $f1 >S $4c >A $87 >X $de >Y $68 >P $01f1 $ae >M $01f2 $00 >M $6488 $68 >M $6489 $24 >M $648a $fd >M
T{ op PC S A X Y P -> $6489 $f2 $00 $87 $de $6a }T T{ $01f1 M $01f2 M $6488 M $6489 M $648a M -> $ae $00 $68 $24 $fd }T
$e537 >PC $7a >S $f9 >A $bd >X $97 >Y $af >P $017a $ff >M $017b $ec >M $e537 $68 >M $e538 $b1 >M $e539 $af >M
T{ op PC S A X Y P -> $e538 $7b $ec $bd $97 $ad }T T{ $017a M $017b M $e537 M $e538 M $e539 M -> $ff $ec $68 $b1 $af }T
$df4a >PC $93 >S $99 >A $f0 >X $d8 >Y $6a >P $0193 $9d >M $0194 $9a >M $df4a $68 >M $df4b $66 >M $df4c $66 >M
T{ op PC S A X Y P -> $df4b $94 $9a $f0 $d8 $e8 }T T{ $0193 M $0194 M $df4a M $df4b M $df4c M -> $9d $9a $68 $66 $66 }T
$0667 >PC $6c >S $59 >A $bc >X $f2 >Y $6f >P $016c $74 >M $016d $f4 >M $0667 $68 >M $0668 $f6 >M $0669 $76 >M
T{ op PC S A X Y P -> $0668 $6d $f4 $bc $f2 $ed }T T{ $016c M $016d M $0667 M $0668 M $0669 M -> $74 $f4 $68 $f6 $76 }T
$1a16 >PC $fd >S $2e >A $91 >X $02 >Y $2d >P $01fd $55 >M $01fe $75 >M $1a16 $68 >M $1a17 $c1 >M $1a18 $5b >M
T{ op PC S A X Y P -> $1a17 $fe $75 $91 $02 $2d }T T{ $01fd M $01fe M $1a16 M $1a17 M $1a18 M -> $55 $75 $68 $c1 $5b }T
$92b1 >PC $40 >S $d9 >A $19 >X $8e >Y $24 >P $0140 $2c >M $0141 $c3 >M $92b1 $68 >M $92b2 $a3 >M $92b3 $1b >M
T{ op PC S A X Y P -> $92b2 $41 $c3 $19 $8e $a4 }T T{ $0140 M $0141 M $92b1 M $92b2 M $92b3 M -> $2c $c3 $68 $a3 $1b }T
$c354 >PC $3f >S $46 >A $84 >X $ae >Y $6f >P $013f $7d >M $0140 $e2 >M $c354 $68 >M $c355 $5b >M $c356 $0d >M
T{ op PC S A X Y P -> $c355 $40 $e2 $84 $ae $ed }T T{ $013f M $0140 M $c354 M $c355 M $c356 M -> $7d $e2 $68 $5b $0d }T
$b774 >PC $cb >S $5c >A $c0 >X $53 >Y $65 >P $01cb $95 >M $01cc $71 >M $b774 $68 >M $b775 $b7 >M $b776 $0d >M
T{ op PC S A X Y P -> $b775 $cc $71 $c0 $53 $65 }T T{ $01cb M $01cc M $b774 M $b775 M $b776 M -> $95 $71 $68 $b7 $0d }T
$34c0 >PC $96 >S $b2 >A $f5 >X $26 >Y $6d >P $0196 $20 >M $0197 $c7 >M $34c0 $68 >M $34c1 $9a >M $34c2 $7d >M
T{ op PC S A X Y P -> $34c1 $97 $c7 $f5 $26 $ed }T T{ $0196 M $0197 M $34c0 M $34c1 M $34c2 M -> $20 $c7 $68 $9a $7d }T
$814a >PC $a5 >S $32 >A $91 >X $98 >Y $26 >P $01a5 $f9 >M $01a6 $45 >M $814a $68 >M $814b $da >M $814c $1c >M
T{ op PC S A X Y P -> $814b $a6 $45 $91 $98 $24 }T T{ $01a5 M $01a6 M $814a M $814b M $814c M -> $f9 $45 $68 $da $1c }T
$8e7f >PC $9a >S $92 >A $5c >X $c2 >Y $ed >P $019a $af >M $019b $2b >M $8e7f $68 >M $8e80 $cf >M $8e81 $3e >M
T{ op PC S A X Y P -> $8e80 $9b $2b $5c $c2 $6d }T T{ $019a M $019b M $8e7f M $8e80 M $8e81 M -> $af $2b $68 $cf $3e }T
$4600 >PC $86 >S $81 >A $a9 >X $05 >Y $eb >P $0186 $3d >M $0187 $47 >M $4600 $68 >M $4601 $16 >M $4602 $ce >M
T{ op PC S A X Y P -> $4601 $87 $47 $a9 $05 $69 }T T{ $0186 M $0187 M $4600 M $4601 M $4602 M -> $3d $47 $68 $16 $ce }T
$113e >PC $36 >S $d1 >A $f8 >X $08 >Y $a0 >P $0136 $24 >M $0137 $e6 >M $113e $68 >M $113f $38 >M $1140 $e3 >M
T{ op PC S A X Y P -> $113f $37 $e6 $f8 $08 $a0 }T T{ $0136 M $0137 M $113e M $113f M $1140 M -> $24 $e6 $68 $38 $e3 }T
$c6f5 >PC $a8 >S $9c >A $af >X $b3 >Y $6f >P $01a8 $ab >M $01a9 $69 >M $c6f5 $68 >M $c6f6 $2e >M $c6f7 $41 >M
T{ op PC S A X Y P -> $c6f6 $a9 $69 $af $b3 $6d }T T{ $01a8 M $01a9 M $c6f5 M $c6f6 M $c6f7 M -> $ab $69 $68 $2e $41 }T
( 69 )
$8395 >PC $b4 >S $55 >A $06 >X $c7 >Y $ae >P $007f $f0 >M $8395 $69 >M $8396 $52 >M $8397 $66 >M
T{ op PC S A X Y P -> $8397 $b4 $07 $06 $c7 $6d }T T{ $007f M $8395 M $8396 M $8397 M -> $f0 $69 $52 $66 }T
$559e >PC $06 >S $73 >A $9c >X $88 >Y $a6 >P $559e $69 >M $559f $e2 >M $55a0 $e4 >M
T{ op PC S A X Y P -> $55a0 $06 $55 $9c $88 $25 }T T{ $559e M $559f M $55a0 M -> $69 $e2 $e4 }T
$d05b >PC $8d >S $67 >A $d2 >X $0f >Y $68 >P $007f $43 >M $d05b $69 >M $d05c $fb >M $d05d $8f >M
T{ op PC S A X Y P -> $d05d $8d $c8 $d2 $0f $a9 }T T{ $007f M $d05b M $d05c M $d05d M -> $43 $69 $fb $8f }T
$cc25 >PC $72 >S $d6 >A $51 >X $0b >Y $e0 >P $cc25 $69 >M $cc26 $2e >M $cc27 $68 >M
T{ op PC S A X Y P -> $cc27 $72 $04 $51 $0b $21 }T T{ $cc25 M $cc26 M $cc27 M -> $69 $2e $68 }T
$c3b8 >PC $ad >S $3a >A $66 >X $f8 >Y $61 >P $c3b8 $69 >M $c3b9 $65 >M $c3ba $a2 >M
T{ op PC S A X Y P -> $c3ba $ad $a0 $66 $f8 $e0 }T T{ $c3b8 M $c3b9 M $c3ba M -> $69 $65 $a2 }T
$72db >PC $71 >S $83 >A $98 >X $0b >Y $24 >P $72db $69 >M $72dc $08 >M $72dd $16 >M
T{ op PC S A X Y P -> $72dd $71 $8b $98 $0b $a4 }T T{ $72db M $72dc M $72dd M -> $69 $08 $16 }T
$0ec1 >PC $5b >S $eb >A $25 >X $61 >Y $27 >P $0ec1 $69 >M $0ec2 $6b >M $0ec3 $19 >M
T{ op PC S A X Y P -> $0ec3 $5b $57 $25 $61 $25 }T T{ $0ec1 M $0ec2 M $0ec3 M -> $69 $6b $19 }T
$d5bb >PC $1c >S $46 >A $33 >X $47 >Y $ae >P $007f $93 >M $d5bb $69 >M $d5bc $62 >M $d5bd $42 >M
T{ op PC S A X Y P -> $d5bd $1c $08 $33 $47 $6d }T T{ $007f M $d5bb M $d5bc M $d5bd M -> $93 $69 $62 $42 }T
$eb75 >PC $5d >S $61 >A $7f >X $41 >Y $2c >P $007f $71 >M $eb75 $69 >M $eb76 $a3 >M $eb77 $97 >M
T{ op PC S A X Y P -> $eb77 $5d $64 $7f $41 $2d }T T{ $007f M $eb75 M $eb76 M $eb77 M -> $71 $69 $a3 $97 }T
$c5d3 >PC $be >S $52 >A $e5 >X $4f >Y $65 >P $c5d3 $69 >M $c5d4 $51 >M $c5d5 $6d >M
T{ op PC S A X Y P -> $c5d5 $be $a4 $e5 $4f $e4 }T T{ $c5d3 M $c5d4 M $c5d5 M -> $69 $51 $6d }T
$e5d6 >PC $f8 >S $da >A $9e >X $ff >Y $23 >P $e5d6 $69 >M $e5d7 $e8 >M $e5d8 $96 >M
T{ op PC S A X Y P -> $e5d8 $f8 $c3 $9e $ff $a1 }T T{ $e5d6 M $e5d7 M $e5d8 M -> $69 $e8 $96 }T
$cd8a >PC $41 >S $dc >A $28 >X $70 >Y $21 >P $cd8a $69 >M $cd8b $50 >M $cd8c $99 >M
T{ op PC S A X Y P -> $cd8c $41 $2d $28 $70 $21 }T T{ $cd8a M $cd8b M $cd8c M -> $69 $50 $99 }T
$4ab3 >PC $56 >S $24 >A $75 >X $39 >Y $26 >P $4ab3 $69 >M $4ab4 $2c >M $4ab5 $c0 >M
T{ op PC S A X Y P -> $4ab5 $56 $50 $75 $39 $24 }T T{ $4ab3 M $4ab4 M $4ab5 M -> $69 $2c $c0 }T
$2311 >PC $45 >S $6e >A $98 >X $04 >Y $a7 >P $2311 $69 >M $2312 $af >M $2313 $7c >M
T{ op PC S A X Y P -> $2313 $45 $1e $98 $04 $25 }T T{ $2311 M $2312 M $2313 M -> $69 $af $7c }T
$ef5e >PC $b3 >S $dc >A $1e >X $50 >Y $ef >P $007f $5b >M $ef5e $69 >M $ef5f $c1 >M $ef60 $18 >M
T{ op PC S A X Y P -> $ef60 $b3 $04 $1e $50 $2d }T T{ $007f M $ef5e M $ef5f M $ef60 M -> $5b $69 $c1 $18 }T
$2298 >PC $4a >S $9d >A $a6 >X $1c >Y $a7 >P $2298 $69 >M $2299 $0b >M $229a $bf >M
T{ op PC S A X Y P -> $229a $4a $a9 $a6 $1c $a4 }T T{ $2298 M $2299 M $229a M -> $69 $0b $bf }T
( 6a )
$159e >PC $c6 >S $de >A $f0 >X $cc >Y $67 >P $159e $6a >M $159f $91 >M $15a0 $53 >M
T{ op PC S A X Y P -> $159f $c6 $ef $f0 $cc $e4 }T T{ $159e M $159f M $15a0 M -> $6a $91 $53 }T
$38bd >PC $5b >S $3a >A $c4 >X $8a >Y $a5 >P $38bd $6a >M $38be $19 >M $38bf $d4 >M
T{ op PC S A X Y P -> $38be $5b $9d $c4 $8a $a4 }T T{ $38bd M $38be M $38bf M -> $6a $19 $d4 }T
$8712 >PC $75 >S $1a >A $c9 >X $ea >Y $6d >P $8712 $6a >M $8713 $64 >M $8714 $02 >M
T{ op PC S A X Y P -> $8713 $75 $8d $c9 $ea $ec }T T{ $8712 M $8713 M $8714 M -> $6a $64 $02 }T
$4255 >PC $a3 >S $98 >A $74 >X $af >Y $27 >P $4255 $6a >M $4256 $08 >M $4257 $12 >M
T{ op PC S A X Y P -> $4256 $a3 $cc $74 $af $a4 }T T{ $4255 M $4256 M $4257 M -> $6a $08 $12 }T
$803e >PC $16 >S $89 >A $46 >X $32 >Y $6f >P $803e $6a >M $803f $de >M $8040 $59 >M
T{ op PC S A X Y P -> $803f $16 $c4 $46 $32 $ed }T T{ $803e M $803f M $8040 M -> $6a $de $59 }T
$3f8b >PC $21 >S $77 >A $1e >X $9b >Y $2d >P $3f8b $6a >M $3f8c $b6 >M $3f8d $83 >M
T{ op PC S A X Y P -> $3f8c $21 $bb $1e $9b $ad }T T{ $3f8b M $3f8c M $3f8d M -> $6a $b6 $83 }T
$8585 >PC $38 >S $f9 >A $4a >X $37 >Y $ad >P $8585 $6a >M $8586 $78 >M $8587 $df >M
T{ op PC S A X Y P -> $8586 $38 $fc $4a $37 $ad }T T{ $8585 M $8586 M $8587 M -> $6a $78 $df }T
$5de0 >PC $bd >S $22 >A $05 >X $b9 >Y $23 >P $5de0 $6a >M $5de1 $36 >M $5de2 $8a >M
T{ op PC S A X Y P -> $5de1 $bd $91 $05 $b9 $a0 }T T{ $5de0 M $5de1 M $5de2 M -> $6a $36 $8a }T
$e5b4 >PC $cf >S $6b >A $2b >X $0d >Y $e6 >P $e5b4 $6a >M $e5b5 $fe >M $e5b6 $ec >M
T{ op PC S A X Y P -> $e5b5 $cf $35 $2b $0d $65 }T T{ $e5b4 M $e5b5 M $e5b6 M -> $6a $fe $ec }T
$5e22 >PC $5f >S $6d >A $02 >X $9d >Y $e0 >P $5e22 $6a >M $5e23 $73 >M $5e24 $6f >M
T{ op PC S A X Y P -> $5e23 $5f $36 $02 $9d $61 }T T{ $5e22 M $5e23 M $5e24 M -> $6a $73 $6f }T
$2ecc >PC $f8 >S $61 >A $a3 >X $51 >Y $aa >P $2ecc $6a >M $2ecd $ef >M $2ece $70 >M
T{ op PC S A X Y P -> $2ecd $f8 $30 $a3 $51 $29 }T T{ $2ecc M $2ecd M $2ece M -> $6a $ef $70 }T
$438d >PC $3a >S $8a >A $6f >X $b5 >Y $26 >P $438d $6a >M $438e $84 >M $438f $5d >M
T{ op PC S A X Y P -> $438e $3a $45 $6f $b5 $24 }T T{ $438d M $438e M $438f M -> $6a $84 $5d }T
$5f6a >PC $e0 >S $ec >A $e9 >X $b3 >Y $26 >P $5f6a $6a >M $5f6b $c4 >M $5f6c $56 >M
T{ op PC S A X Y P -> $5f6b $e0 $76 $e9 $b3 $24 }T T{ $5f6a M $5f6b M $5f6c M -> $6a $c4 $56 }T
$7230 >PC $8f >S $f0 >A $b3 >X $e1 >Y $2f >P $7230 $6a >M $7231 $c7 >M $7232 $0d >M
T{ op PC S A X Y P -> $7231 $8f $f8 $b3 $e1 $ac }T T{ $7230 M $7231 M $7232 M -> $6a $c7 $0d }T
$aef1 >PC $f9 >S $39 >A $08 >X $7b >Y $61 >P $aef1 $6a >M $aef2 $35 >M $aef3 $87 >M
T{ op PC S A X Y P -> $aef2 $f9 $9c $08 $7b $e1 }T T{ $aef1 M $aef2 M $aef3 M -> $6a $35 $87 }T
$99ec >PC $6a >S $4f >A $06 >X $3f >Y $24 >P $99ec $6a >M $99ed $8c >M $99ee $2d >M
T{ op PC S A X Y P -> $99ed $6a $27 $06 $3f $25 }T T{ $99ec M $99ed M $99ee M -> $6a $8c $2d }T
( 6b )
$ba6f >PC $26 >S $aa >A $3f >X $2a >Y $24 >P $ba6f $6b >M $ba70 $f4 >M $ba71 $66 >M
T{ op PC S A X Y P -> $ba70 $26 $aa $3f $2a $24 }T T{ $ba6f M $ba70 M $ba71 M -> $6b $f4 $66 }T
$802c >PC $4c >S $cf >A $4f >X $e5 >Y $e1 >P $802c $6b >M $802d $50 >M $802e $1d >M
T{ op PC S A X Y P -> $802d $4c $cf $4f $e5 $e1 }T T{ $802c M $802d M $802e M -> $6b $50 $1d }T
$8392 >PC $85 >S $ad >A $a1 >X $df >Y $63 >P $8392 $6b >M $8393 $d3 >M $8394 $44 >M
T{ op PC S A X Y P -> $8393 $85 $ad $a1 $df $63 }T T{ $8392 M $8393 M $8394 M -> $6b $d3 $44 }T
$611b >PC $0d >S $10 >A $f3 >X $47 >Y $24 >P $611b $6b >M $611c $ab >M $611d $0e >M
T{ op PC S A X Y P -> $611c $0d $10 $f3 $47 $24 }T T{ $611b M $611c M $611d M -> $6b $ab $0e }T
$d7f5 >PC $eb >S $4d >A $0c >X $65 >Y $21 >P $d7f5 $6b >M $d7f6 $ac >M $d7f7 $f0 >M
T{ op PC S A X Y P -> $d7f6 $eb $4d $0c $65 $21 }T T{ $d7f5 M $d7f6 M $d7f7 M -> $6b $ac $f0 }T
$83fb >PC $52 >S $f7 >A $f2 >X $0b >Y $ad >P $83fb $6b >M $83fc $4e >M $83fd $ee >M
T{ op PC S A X Y P -> $83fc $52 $f7 $f2 $0b $ad }T T{ $83fb M $83fc M $83fd M -> $6b $4e $ee }T
$9ca2 >PC $7b >S $1e >A $dd >X $25 >Y $68 >P $9ca2 $6b >M $9ca3 $41 >M $9ca4 $30 >M
T{ op PC S A X Y P -> $9ca3 $7b $1e $dd $25 $68 }T T{ $9ca2 M $9ca3 M $9ca4 M -> $6b $41 $30 }T
$385e >PC $bb >S $50 >A $89 >X $88 >Y $a5 >P $385e $6b >M $385f $f1 >M $3860 $e4 >M
T{ op PC S A X Y P -> $385f $bb $50 $89 $88 $a5 }T T{ $385e M $385f M $3860 M -> $6b $f1 $e4 }T
$d6b2 >PC $0a >S $2e >A $bf >X $1c >Y $6b >P $d6b2 $6b >M $d6b3 $43 >M $d6b4 $61 >M
T{ op PC S A X Y P -> $d6b3 $0a $2e $bf $1c $6b }T T{ $d6b2 M $d6b3 M $d6b4 M -> $6b $43 $61 }T
$4482 >PC $19 >S $6b >A $60 >X $c1 >Y $ad >P $4482 $6b >M $4483 $99 >M $4484 $51 >M
T{ op PC S A X Y P -> $4483 $19 $6b $60 $c1 $ad }T T{ $4482 M $4483 M $4484 M -> $6b $99 $51 }T
$265b >PC $a3 >S $84 >A $fa >X $90 >Y $eb >P $265b $6b >M $265c $35 >M $265d $ca >M
T{ op PC S A X Y P -> $265c $a3 $84 $fa $90 $eb }T T{ $265b M $265c M $265d M -> $6b $35 $ca }T
$9d1c >PC $72 >S $bd >A $28 >X $f4 >Y $a8 >P $9d1c $6b >M $9d1d $5d >M $9d1e $ab >M
T{ op PC S A X Y P -> $9d1d $72 $bd $28 $f4 $a8 }T T{ $9d1c M $9d1d M $9d1e M -> $6b $5d $ab }T
$167d >PC $0b >S $90 >A $99 >X $f3 >Y $6a >P $167d $6b >M $167e $2e >M $167f $e7 >M
T{ op PC S A X Y P -> $167e $0b $90 $99 $f3 $6a }T T{ $167d M $167e M $167f M -> $6b $2e $e7 }T
$bd17 >PC $c6 >S $2b >A $22 >X $35 >Y $61 >P $bd17 $6b >M $bd18 $a7 >M $bd19 $3a >M
T{ op PC S A X Y P -> $bd18 $c6 $2b $22 $35 $61 }T T{ $bd17 M $bd18 M $bd19 M -> $6b $a7 $3a }T
$e5c0 >PC $06 >S $93 >A $a4 >X $6a >Y $e1 >P $e5c0 $6b >M $e5c1 $1e >M $e5c2 $46 >M
T{ op PC S A X Y P -> $e5c1 $06 $93 $a4 $6a $e1 }T T{ $e5c0 M $e5c1 M $e5c2 M -> $6b $1e $46 }T
$ff21 >PC $d5 >S $aa >A $b0 >X $e3 >Y $ec >P $ff21 $6b >M $ff22 $8e >M $ff23 $0c >M
T{ op PC S A X Y P -> $ff22 $d5 $aa $b0 $e3 $ec }T T{ $ff21 M $ff22 M $ff23 M -> $6b $8e $0c }T
( 6c )
$6d9b >PC $23 >S $c6 >A $32 >X $db >Y $2a >P $3133 $84 >M $6d9b $6c >M $6d9c $0a >M $6d9d $97 >M $970a $33 >M $970b $31 >M
T{ op PC S A X Y P -> $3133 $23 $c6 $32 $db $2a }T T{ $3133 M $6d9b M $6d9c M $6d9d M $970a M $970b M -> $84 $6c $0a $97 $33 $31 }T
$91fd >PC $5c >S $a7 >A $87 >X $8e >Y $26 >P $0cb6 $a5 >M $7c66 $b6 >M $7c67 $0c >M $91fd $6c >M $91fe $66 >M $91ff $7c >M
T{ op PC S A X Y P -> $0cb6 $5c $a7 $87 $8e $26 }T T{ $0cb6 M $7c66 M $7c67 M $91fd M $91fe M $91ff M -> $a5 $b6 $0c $6c $66 $7c }T
$16e8 >PC $cd >S $e2 >A $f9 >X $27 >Y $ad >P $16e8 $6c >M $16e9 $3b >M $16ea $94 >M $943b $96 >M $943c $c3 >M $c396 $42 >M
T{ op PC S A X Y P -> $c396 $cd $e2 $f9 $27 $ad }T T{ $16e8 M $16e9 M $16ea M $943b M $943c M $c396 M -> $6c $3b $94 $96 $c3 $42 }T
$7503 >PC $85 >S $56 >A $6d >X $7d >Y $a6 >P $7503 $6c >M $7504 $04 >M $7505 $8a >M $8a04 $55 >M $8a05 $9a >M $9a55 $a3 >M
T{ op PC S A X Y P -> $9a55 $85 $56 $6d $7d $a6 }T T{ $7503 M $7504 M $7505 M $8a04 M $8a05 M $9a55 M -> $6c $04 $8a $55 $9a $a3 }T
$04a2 >PC $11 >S $67 >A $c1 >X $f0 >Y $a9 >P $04a2 $6c >M $04a3 $8a >M $04a4 $2d >M $2d8a $79 >M $2d8b $3d >M $3d79 $4e >M
T{ op PC S A X Y P -> $3d79 $11 $67 $c1 $f0 $a9 }T T{ $04a2 M $04a3 M $04a4 M $2d8a M $2d8b M $3d79 M -> $6c $8a $2d $79 $3d $4e }T
$3457 >PC $81 >S $e7 >A $64 >X $98 >Y $ec >P $2768 $6f >M $2769 $32 >M $326f $8e >M $3457 $6c >M $3458 $68 >M $3459 $27 >M
T{ op PC S A X Y P -> $326f $81 $e7 $64 $98 $ec }T T{ $2768 M $2769 M $326f M $3457 M $3458 M $3459 M -> $6f $32 $8e $6c $68 $27 }T
$ff3f >PC $12 >S $7c >A $e1 >X $04 >Y $e8 >P $1a93 $ec >M $e2e1 $93 >M $e2e2 $1a >M $ff3f $6c >M $ff40 $e1 >M $ff41 $e2 >M
T{ op PC S A X Y P -> $1a93 $12 $7c $e1 $04 $e8 }T T{ $1a93 M $e2e1 M $e2e2 M $ff3f M $ff40 M $ff41 M -> $ec $93 $1a $6c $e1 $e2 }T
$91b5 >PC $2b >S $54 >A $fb >X $8d >Y $a9 >P $22f9 $54 >M $22fa $91 >M $9154 $9e >M $91b5 $6c >M $91b6 $f9 >M $91b7 $22 >M
T{ op PC S A X Y P -> $9154 $2b $54 $fb $8d $a9 }T T{ $22f9 M $22fa M $9154 M $91b5 M $91b6 M $91b7 M -> $54 $91 $9e $6c $f9 $22 }T
$eaa4 >PC $cc >S $4f >A $9d >X $47 >Y $69 >P $1f40 $d4 >M $38e9 $40 >M $38ea $1f >M $eaa4 $6c >M $eaa5 $e9 >M $eaa6 $38 >M
T{ op PC S A X Y P -> $1f40 $cc $4f $9d $47 $69 }T T{ $1f40 M $38e9 M $38ea M $eaa4 M $eaa5 M $eaa6 M -> $d4 $40 $1f $6c $e9 $38 }T
$bec1 >PC $40 >S $a6 >A $94 >X $89 >Y $27 >P $4837 $e8 >M $4838 $c3 >M $bec1 $6c >M $bec2 $37 >M $bec3 $48 >M $c3e8 $e9 >M
T{ op PC S A X Y P -> $c3e8 $40 $a6 $94 $89 $27 }T T{ $4837 M $4838 M $bec1 M $bec2 M $bec3 M $c3e8 M -> $e8 $c3 $6c $37 $48 $e9 }T
$ade6 >PC $94 >S $de >A $05 >X $6a >Y $20 >P $ade6 $6c >M $ade7 $05 >M $ade8 $cc >M $cc05 $fb >M $cc06 $d3 >M $d3fb $c9 >M
T{ op PC S A X Y P -> $d3fb $94 $de $05 $6a $20 }T T{ $ade6 M $ade7 M $ade8 M $cc05 M $cc06 M $d3fb M -> $6c $05 $cc $fb $d3 $c9 }T
$e2d5 >PC $16 >S $bb >A $31 >X $29 >Y $ad >P $d7dd $74 >M $e2d5 $6c >M $e2d6 $f3 >M $e2d7 $fd >M $fdf3 $dd >M $fdf4 $d7 >M
T{ op PC S A X Y P -> $d7dd $16 $bb $31 $29 $ad }T T{ $d7dd M $e2d5 M $e2d6 M $e2d7 M $fdf3 M $fdf4 M -> $74 $6c $f3 $fd $dd $d7 }T
$602d >PC $44 >S $42 >A $51 >X $37 >Y $ef >P $602d $6c >M $602e $51 >M $602f $f4 >M $8d1a $50 >M $f451 $1a >M $f452 $8d >M
T{ op PC S A X Y P -> $8d1a $44 $42 $51 $37 $ef }T T{ $602d M $602e M $602f M $8d1a M $f451 M $f452 M -> $6c $51 $f4 $50 $1a $8d }T
$6732 >PC $ee >S $ba >A $1b >X $af >Y $e5 >P $6732 $6c >M $6733 $5b >M $6734 $8a >M $7807 $29 >M $8a5b $07 >M $8a5c $78 >M
T{ op PC S A X Y P -> $7807 $ee $ba $1b $af $e5 }T T{ $6732 M $6733 M $6734 M $7807 M $8a5b M $8a5c M -> $6c $5b $8a $29 $07 $78 }T
$9482 >PC $42 >S $ef >A $65 >X $9d >Y $af >P $3c61 $a9 >M $62d9 $61 >M $62da $3c >M $9482 $6c >M $9483 $d9 >M $9484 $62 >M
T{ op PC S A X Y P -> $3c61 $42 $ef $65 $9d $af }T T{ $3c61 M $62d9 M $62da M $9482 M $9483 M $9484 M -> $a9 $61 $3c $6c $d9 $62 }T
$4c33 >PC $e1 >S $82 >A $f2 >X $2b >Y $29 >P $176c $e7 >M $4c33 $6c >M $4c34 $b3 >M $4c35 $bf >M $bfb3 $6c >M $bfb4 $17 >M
T{ op PC S A X Y P -> $176c $e1 $82 $f2 $2b $29 }T T{ $176c M $4c33 M $4c34 M $4c35 M $bfb3 M $bfb4 M -> $e7 $6c $b3 $bf $6c $17 }T
( 6d )
$772f >PC $8c >S $ea >A $03 >X $ee >Y $e5 >P $772f $6d >M $7730 $12 >M $7731 $cd >M $7732 $5d >M $cd12 $12 >M
T{ op PC S A X Y P -> $7732 $8c $fd $03 $ee $a4 }T T{ $772f M $7730 M $7731 M $7732 M $cd12 M -> $6d $12 $cd $5d $12 }T
$ae90 >PC $03 >S $00 >A $ee >X $ee >Y $aa >P $7b8d $d3 >M $ae90 $6d >M $ae91 $8d >M $ae92 $7b >M $ae93 $9f >M
T{ op PC S A X Y P -> $ae93 $03 $33 $ee $ee $29 }T T{ $7b8d M $ae90 M $ae91 M $ae92 M $ae93 M -> $d3 $6d $8d $7b $9f }T
$c695 >PC $e5 >S $18 >A $b6 >X $b0 >Y $ed >P $5de0 $88 >M $c695 $6d >M $c696 $e0 >M $c697 $5d >M $c698 $9d >M
T{ op PC S A X Y P -> $c698 $e5 $07 $b6 $b0 $2d }T T{ $5de0 M $c695 M $c696 M $c697 M $c698 M -> $88 $6d $e0 $5d $9d }T
$5e67 >PC $bf >S $83 >A $97 >X $11 >Y $a7 >P $5e67 $6d >M $5e68 $2a >M $5e69 $60 >M $5e6a $67 >M $602a $bd >M
T{ op PC S A X Y P -> $5e6a $bf $41 $97 $11 $65 }T T{ $5e67 M $5e68 M $5e69 M $5e6a M $602a M -> $6d $2a $60 $67 $bd }T
$3a5d >PC $16 >S $36 >A $da >X $e8 >Y $24 >P $3a5d $6d >M $3a5e $73 >M $3a5f $3d >M $3a60 $49 >M $3d73 $2b >M
T{ op PC S A X Y P -> $3a60 $16 $61 $da $e8 $24 }T T{ $3a5d M $3a5e M $3a5f M $3a60 M $3d73 M -> $6d $73 $3d $49 $2b }T
$df47 >PC $7c >S $3c >A $ff >X $d4 >Y $24 >P $60a0 $c2 >M $df47 $6d >M $df48 $a0 >M $df49 $60 >M $df4a $60 >M
T{ op PC S A X Y P -> $df4a $7c $fe $ff $d4 $a4 }T T{ $60a0 M $df47 M $df48 M $df49 M $df4a M -> $c2 $6d $a0 $60 $60 }T
$3b3c >PC $ae >S $0c >A $de >X $db >Y $25 >P $3b3c $6d >M $3b3d $ea >M $3b3e $81 >M $3b3f $d9 >M $81ea $34 >M
T{ op PC S A X Y P -> $3b3f $ae $41 $de $db $24 }T T{ $3b3c M $3b3d M $3b3e M $3b3f M $81ea M -> $6d $ea $81 $d9 $34 }T
$8bf1 >PC $64 >S $5b >A $38 >X $2e >Y $63 >P $1735 $a3 >M $8bf1 $6d >M $8bf2 $35 >M $8bf3 $17 >M $8bf4 $f2 >M
T{ op PC S A X Y P -> $8bf4 $64 $ff $38 $2e $a0 }T T{ $1735 M $8bf1 M $8bf2 M $8bf3 M $8bf4 M -> $a3 $6d $35 $17 $f2 }T
$7462 >PC $2d >S $dd >A $08 >X $1a >Y $aa >P $7462 $6d >M $7463 $cc >M $7464 $d9 >M $7465 $15 >M $d9cc $58 >M
T{ op PC S A X Y P -> $7465 $2d $9b $08 $1a $a9 }T T{ $7462 M $7463 M $7464 M $7465 M $d9cc M -> $6d $cc $d9 $15 $58 }T
$07d6 >PC $2e >S $c1 >A $cc >X $e9 >Y $ab >P $07d6 $6d >M $07d7 $49 >M $07d8 $8b >M $07d9 $9b >M $8b49 $52 >M
T{ op PC S A X Y P -> $07d9 $2e $74 $cc $e9 $29 }T T{ $07d6 M $07d7 M $07d8 M $07d9 M $8b49 M -> $6d $49 $8b $9b $52 }T
$fc71 >PC $54 >S $93 >A $a9 >X $e5 >Y $64 >P $0e1d $28 >M $fc71 $6d >M $fc72 $1d >M $fc73 $0e >M $fc74 $83 >M
T{ op PC S A X Y P -> $fc74 $54 $bb $a9 $e5 $a4 }T T{ $0e1d M $fc71 M $fc72 M $fc73 M $fc74 M -> $28 $6d $1d $0e $83 }T
$8cdd >PC $13 >S $8c >A $4c >X $2f >Y $23 >P $8cdd $6d >M $8cde $2e >M $8cdf $cb >M $8ce0 $d0 >M $cb2e $28 >M
T{ op PC S A X Y P -> $8ce0 $13 $b5 $4c $2f $a0 }T T{ $8cdd M $8cde M $8cdf M $8ce0 M $cb2e M -> $6d $2e $cb $d0 $28 }T
$7b8f >PC $aa >S $f8 >A $a1 >X $30 >Y $21 >P $4273 $eb >M $7b8f $6d >M $7b90 $73 >M $7b91 $42 >M $7b92 $90 >M
T{ op PC S A X Y P -> $7b92 $aa $e4 $a1 $30 $a1 }T T{ $4273 M $7b8f M $7b90 M $7b91 M $7b92 M -> $eb $6d $73 $42 $90 }T
$7553 >PC $a2 >S $08 >A $52 >X $31 >Y $eb >P $7553 $6d >M $7554 $7e >M $7555 $a3 >M $7556 $00 >M $a37e $2d >M
T{ op PC S A X Y P -> $7556 $a2 $3c $52 $31 $28 }T T{ $7553 M $7554 M $7555 M $7556 M $a37e M -> $6d $7e $a3 $00 $2d }T
$30b5 >PC $70 >S $a7 >A $4f >X $c9 >Y $a2 >P $30b5 $6d >M $30b6 $ee >M $30b7 $33 >M $30b8 $ad >M $33ee $ab >M
T{ op PC S A X Y P -> $30b8 $70 $52 $4f $c9 $61 }T T{ $30b5 M $30b6 M $30b7 M $30b8 M $33ee M -> $6d $ee $33 $ad $ab }T
$33af >PC $1e >S $fb >A $3f >X $c0 >Y $2c >P $33af $6d >M $33b0 $d9 >M $33b1 $82 >M $33b2 $62 >M $82d9 $a6 >M
T{ op PC S A X Y P -> $33b2 $1e $07 $3f $c0 $2d }T T{ $33af M $33b0 M $33b1 M $33b2 M $82d9 M -> $6d $d9 $82 $62 $a6 }T
( 6e )
$5434 >PC $09 >S $42 >A $66 >X $3f >Y $2f >P $3838 $88 >M $5434 $6e >M $5435 $38 >M $5436 $38 >M $5437 $f1 >M
T{ op PC S A X Y P -> $5437 $09 $42 $66 $3f $ac }T T{ $3838 M $5434 M $5435 M $5436 M $5437 M -> $c4 $6e $38 $38 $f1 }T
$5925 >PC $e2 >S $0b >A $cb >X $e2 >Y $23 >P $5925 $6e >M $5926 $fd >M $5927 $9b >M $5928 $d9 >M $9bfd $16 >M
T{ op PC S A X Y P -> $5928 $e2 $0b $cb $e2 $a0 }T T{ $5925 M $5926 M $5927 M $5928 M $9bfd M -> $6e $fd $9b $d9 $8b }T
$1c52 >PC $46 >S $b5 >A $35 >X $85 >Y $a8 >P $1c52 $6e >M $1c53 $83 >M $1c54 $e6 >M $1c55 $d5 >M $e683 $2b >M
T{ op PC S A X Y P -> $1c55 $46 $b5 $35 $85 $29 }T T{ $1c52 M $1c53 M $1c54 M $1c55 M $e683 M -> $6e $83 $e6 $d5 $15 }T
$14c9 >PC $9d >S $b8 >A $30 >X $ae >Y $60 >P $14c9 $6e >M $14ca $e9 >M $14cb $54 >M $14cc $25 >M $54e9 $d0 >M
T{ op PC S A X Y P -> $14cc $9d $b8 $30 $ae $60 }T T{ $14c9 M $14ca M $14cb M $14cc M $54e9 M -> $6e $e9 $54 $25 $68 }T
$fd41 >PC $d1 >S $db >A $90 >X $db >Y $27 >P $5410 $13 >M $fd41 $6e >M $fd42 $10 >M $fd43 $54 >M $fd44 $8d >M
T{ op PC S A X Y P -> $fd44 $d1 $db $90 $db $a5 }T T{ $5410 M $fd41 M $fd42 M $fd43 M $fd44 M -> $89 $6e $10 $54 $8d }T
$e97b >PC $72 >S $b6 >A $19 >X $48 >Y $66 >P $611b $86 >M $e97b $6e >M $e97c $1b >M $e97d $61 >M $e97e $3a >M
T{ op PC S A X Y P -> $e97e $72 $b6 $19 $48 $64 }T T{ $611b M $e97b M $e97c M $e97d M $e97e M -> $43 $6e $1b $61 $3a }T
$fb18 >PC $f1 >S $38 >A $37 >X $36 >Y $af >P $32db $e6 >M $fb18 $6e >M $fb19 $db >M $fb1a $32 >M $fb1b $6e >M
T{ op PC S A X Y P -> $fb1b $f1 $38 $37 $36 $ac }T T{ $32db M $fb18 M $fb19 M $fb1a M $fb1b M -> $f3 $6e $db $32 $6e }T
$6f9c >PC $04 >S $92 >A $93 >X $3c >Y $e2 >P $1978 $12 >M $6f9c $6e >M $6f9d $78 >M $6f9e $19 >M $6f9f $0d >M
T{ op PC S A X Y P -> $6f9f $04 $92 $93 $3c $60 }T T{ $1978 M $6f9c M $6f9d M $6f9e M $6f9f M -> $09 $6e $78 $19 $0d }T
$8010 >PC $b0 >S $23 >A $d1 >X $5a >Y $a8 >P $8010 $6e >M $8011 $1e >M $8012 $b6 >M $8013 $12 >M $b61e $18 >M
T{ op PC S A X Y P -> $8013 $b0 $23 $d1 $5a $28 }T T{ $8010 M $8011 M $8012 M $8013 M $b61e M -> $6e $1e $b6 $12 $0c }T
$01b4 >PC $9e >S $9f >A $af >X $9a >Y $ab >P $01b4 $6e >M $01b5 $8c >M $01b6 $25 >M $01b7 $75 >M $258c $56 >M
T{ op PC S A X Y P -> $01b7 $9e $9f $af $9a $a8 }T T{ $01b4 M $01b5 M $01b6 M $01b7 M $258c M -> $6e $8c $25 $75 $ab }T
$c4f9 >PC $e0 >S $44 >A $eb >X $11 >Y $a8 >P $3165 $f7 >M $c4f9 $6e >M $c4fa $65 >M $c4fb $31 >M $c4fc $04 >M
T{ op PC S A X Y P -> $c4fc $e0 $44 $eb $11 $29 }T T{ $3165 M $c4f9 M $c4fa M $c4fb M $c4fc M -> $7b $6e $65 $31 $04 }T
$bd63 >PC $4b >S $71 >A $39 >X $c9 >Y $20 >P $58d5 $1f >M $bd63 $6e >M $bd64 $d5 >M $bd65 $58 >M $bd66 $3e >M
T{ op PC S A X Y P -> $bd66 $4b $71 $39 $c9 $21 }T T{ $58d5 M $bd63 M $bd64 M $bd65 M $bd66 M -> $0f $6e $d5 $58 $3e }T
$519a >PC $df >S $0d >A $93 >X $94 >Y $61 >P $3773 $ae >M $519a $6e >M $519b $73 >M $519c $37 >M $519d $06 >M
T{ op PC S A X Y P -> $519d $df $0d $93 $94 $e0 }T T{ $3773 M $519a M $519b M $519c M $519d M -> $d7 $6e $73 $37 $06 }T
$5989 >PC $03 >S $4b >A $f4 >X $e8 >Y $a6 >P $3bc8 $58 >M $5989 $6e >M $598a $c8 >M $598b $3b >M $598c $ed >M
T{ op PC S A X Y P -> $598c $03 $4b $f4 $e8 $24 }T T{ $3bc8 M $5989 M $598a M $598b M $598c M -> $2c $6e $c8 $3b $ed }T
$7b7e >PC $d3 >S $2b >A $ec >X $a2 >Y $a1 >P $7b7e $6e >M $7b7f $94 >M $7b80 $84 >M $7b81 $bd >M $8494 $45 >M
T{ op PC S A X Y P -> $7b81 $d3 $2b $ec $a2 $a1 }T T{ $7b7e M $7b7f M $7b80 M $7b81 M $8494 M -> $6e $94 $84 $bd $a2 }T
$2773 >PC $56 >S $1e >A $1f >X $e4 >Y $6b >P $2773 $6e >M $2774 $9f >M $2775 $ae >M $2776 $84 >M $ae9f $2f >M
T{ op PC S A X Y P -> $2776 $56 $1e $1f $e4 $e9 }T T{ $2773 M $2774 M $2775 M $2776 M $ae9f M -> $6e $9f $ae $84 $97 }T
( 6f )
$ecfe >PC $3b >S $10 >A $1f >X $93 >Y $66 >P $0086 $9f >M $ecc1 $fd >M $ecfe $6f >M $ecff $86 >M $ed00 $c0 >M $edc1 $6d >M
T{ op PC S A X Y P -> $ecc1 $3b $10 $1f $93 $66 }T T{ $0086 M $ecc1 M $ecfe M $ecff M $ed00 M $edc1 M -> $9f $fd $6f $86 $c0 $6d }T
$cdbb >PC $8f >S $71 >A $df >X $f6 >Y $e7 >P $00c6 $f3 >M $cd12 $a2 >M $cdbb $6f >M $cdbc $c6 >M $cdbd $54 >M $cdbe $bc >M
T{ op PC S A X Y P -> $cdbe $8f $71 $df $f6 $e7 }T T{ $00c6 M $cd12 M $cdbb M $cdbc M $cdbd M $cdbe M -> $f3 $a2 $6f $c6 $54 $bc }T
$78b9 >PC $1a >S $a5 >A $db >X $b1 >Y $a8 >P $007f $99 >M $786d $a4 >M $78b9 $6f >M $78ba $7f >M $78bb $b1 >M
T{ op PC S A X Y P -> $786d $1a $a5 $db $b1 $a8 }T T{ $007f M $786d M $78b9 M $78ba M $78bb M -> $99 $a4 $6f $7f $b1 }T
$9c52 >PC $d0 >S $6d >A $a3 >X $e6 >Y $67 >P $0066 $4f >M $9c52 $6f >M $9c53 $66 >M $9c54 $5d >M $9c55 $f5 >M $9cb2 $79 >M
T{ op PC S A X Y P -> $9c55 $d0 $6d $a3 $e6 $67 }T T{ $0066 M $9c52 M $9c53 M $9c54 M $9c55 M $9cb2 M -> $4f $6f $66 $5d $f5 $79 }T
$4c2a >PC $ea >S $de >A $5b >X $ad >Y $6d >P $00fb $c7 >M $4c2a $6f >M $4c2b $fb >M $4c2c $a5 >M $4c2d $61 >M $4cd2 $95 >M
T{ op PC S A X Y P -> $4c2d $ea $de $5b $ad $6d }T T{ $00fb M $4c2a M $4c2b M $4c2c M $4c2d M $4cd2 M -> $c7 $6f $fb $a5 $61 $95 }T
$cc2d >PC $6b >S $1b >A $2c >X $23 >Y $68 >P $000d $57 >M $cc2d $6f >M $cc2e $0d >M $cc2f $7d >M $cc30 $18 >M $ccad $f3 >M
T{ op PC S A X Y P -> $cc30 $6b $1b $2c $23 $68 }T T{ $000d M $cc2d M $cc2e M $cc2f M $cc30 M $ccad M -> $57 $6f $0d $7d $18 $f3 }T
$c99d >PC $a9 >S $d5 >A $d5 >X $0d >Y $af >P $005b $10 >M $c99d $6f >M $c99e $5b >M $c99f $46 >M $c9e6 $f4 >M
T{ op PC S A X Y P -> $c9e6 $a9 $d5 $d5 $0d $af }T T{ $005b M $c99d M $c99e M $c99f M $c9e6 M -> $10 $6f $5b $46 $f4 }T
$a41f >PC $ef >S $13 >A $93 >X $33 >Y $20 >P $0012 $bb >M $a3f8 $9d >M $a41f $6f >M $a420 $12 >M $a421 $d6 >M $a4f8 $b0 >M
T{ op PC S A X Y P -> $a3f8 $ef $13 $93 $33 $20 }T T{ $0012 M $a3f8 M $a41f M $a420 M $a421 M $a4f8 M -> $bb $9d $6f $12 $d6 $b0 }T
$b909 >PC $27 >S $dc >A $ca >X $42 >Y $28 >P $0051 $ad >M $b909 $6f >M $b90a $51 >M $b90b $40 >M $b94c $88 >M
T{ op PC S A X Y P -> $b94c $27 $dc $ca $42 $28 }T T{ $0051 M $b909 M $b90a M $b90b M $b94c M -> $ad $6f $51 $40 $88 }T
$b3e2 >PC $48 >S $ee >A $59 >X $6d >Y $a8 >P $00f5 $e3 >M $b321 $90 >M $b3e2 $6f >M $b3e3 $f5 >M $b3e4 $3c >M $b3e5 $c8 >M
T{ op PC S A X Y P -> $b3e5 $48 $ee $59 $6d $a8 }T T{ $00f5 M $b321 M $b3e2 M $b3e3 M $b3e4 M $b3e5 M -> $e3 $90 $6f $f5 $3c $c8 }T
$be91 >PC $70 >S $c0 >A $f9 >X $4d >Y $a1 >P $004c $19 >M $be50 $ef >M $be91 $6f >M $be92 $4c >M $be93 $bc >M
T{ op PC S A X Y P -> $be50 $70 $c0 $f9 $4d $a1 }T T{ $004c M $be50 M $be91 M $be92 M $be93 M -> $19 $ef $6f $4c $bc }T
$cff3 >PC $fc >S $65 >A $e8 >X $d7 >Y $a7 >P $00fb $6a >M $cf47 $72 >M $cff3 $6f >M $cff4 $fb >M $cff5 $51 >M $cff6 $e7 >M
T{ op PC S A X Y P -> $cff6 $fc $65 $e8 $d7 $a7 }T T{ $00fb M $cf47 M $cff3 M $cff4 M $cff5 M $cff6 M -> $6a $72 $6f $fb $51 $e7 }T
$b725 >PC $63 >S $d3 >A $ff >X $85 >Y $ea >P $00a3 $32 >M $b6f4 $24 >M $b725 $6f >M $b726 $a3 >M $b727 $cc >M $b7f4 $d6 >M
T{ op PC S A X Y P -> $b6f4 $63 $d3 $ff $85 $ea }T T{ $00a3 M $b6f4 M $b725 M $b726 M $b727 M $b7f4 M -> $32 $24 $6f $a3 $cc $d6 }T
$95ff >PC $0e >S $d4 >A $36 >X $b5 >Y $2c >P $0019 $ee >M $95ff $6f >M $9600 $19 >M $9601 $ad >M $9602 $50 >M $96af $2b >M
T{ op PC S A X Y P -> $9602 $0e $d4 $36 $b5 $2c }T T{ $0019 M $95ff M $9600 M $9601 M $9602 M $96af M -> $ee $6f $19 $ad $50 $2b }T
$f960 >PC $a5 >S $02 >A $2c >X $9e >Y $62 >P $00ac $0f >M $f960 $6f >M $f961 $ac >M $f962 $2d >M $f990 $53 >M
T{ op PC S A X Y P -> $f990 $a5 $02 $2c $9e $62 }T T{ $00ac M $f960 M $f961 M $f962 M $f990 M -> $0f $6f $ac $2d $53 }T
$3843 >PC $42 >S $24 >A $03 >X $4c >Y $ef >P $00fd $5d >M $381f $3b >M $3843 $6f >M $3844 $fd >M $3845 $d9 >M $3846 $4d >M
T{ op PC S A X Y P -> $3846 $42 $24 $03 $4c $ef }T T{ $00fd M $381f M $3843 M $3844 M $3845 M $3846 M -> $5d $3b $6f $fd $d9 $4d }T
( 70 )
$c376 >PC $d0 >S $bf >A $22 >X $29 >Y $a8 >P $c376 $70 >M $c377 $59 >M $c378 $bd >M
T{ op PC S A X Y P -> $c378 $d0 $bf $22 $29 $a8 }T T{ $c376 M $c377 M $c378 M -> $70 $59 $bd }T
$5acd >PC $d3 >S $5c >A $bc >X $48 >Y $64 >P $5a50 $68 >M $5acd $70 >M $5ace $81 >M $5acf $7b >M
T{ op PC S A X Y P -> $5a50 $d3 $5c $bc $48 $64 }T T{ $5a50 M $5acd M $5ace M $5acf M -> $68 $70 $81 $7b }T
$ebf8 >PC $7e >S $e1 >A $1e >X $00 >Y $26 >P $ebf8 $70 >M $ebf9 $f0 >M $ebfa $36 >M
T{ op PC S A X Y P -> $ebfa $7e $e1 $1e $00 $26 }T T{ $ebf8 M $ebf9 M $ebfa M -> $70 $f0 $36 }T
$7b7d >PC $e2 >S $f6 >A $d5 >X $3e >Y $e9 >P $7b7d $70 >M $7b7e $46 >M $7b7f $57 >M $7bc5 $4e >M
T{ op PC S A X Y P -> $7bc5 $e2 $f6 $d5 $3e $e9 }T T{ $7b7d M $7b7e M $7b7f M $7bc5 M -> $70 $46 $57 $4e }T
$9288 >PC $2b >S $0f >A $c6 >X $98 >Y $64 >P $9288 $70 >M $9289 $70 >M $928a $d6 >M $92fa $39 >M
T{ op PC S A X Y P -> $92fa $2b $0f $c6 $98 $64 }T T{ $9288 M $9289 M $928a M $92fa M -> $70 $70 $d6 $39 }T
$6d6d >PC $8f >S $66 >A $07 >X $6d >Y $29 >P $6d6d $70 >M $6d6e $ce >M $6d6f $e4 >M
T{ op PC S A X Y P -> $6d6f $8f $66 $07 $6d $29 }T T{ $6d6d M $6d6e M $6d6f M -> $70 $ce $e4 }T
$07e7 >PC $a3 >S $da >A $65 >X $33 >Y $a1 >P $07e7 $70 >M $07e8 $80 >M $07e9 $da >M
T{ op PC S A X Y P -> $07e9 $a3 $da $65 $33 $a1 }T T{ $07e7 M $07e8 M $07e9 M -> $70 $80 $da }T
$1255 >PC $79 >S $be >A $1a >X $57 >Y $aa >P $1255 $70 >M $1256 $64 >M $1257 $31 >M
T{ op PC S A X Y P -> $1257 $79 $be $1a $57 $aa }T T{ $1255 M $1256 M $1257 M -> $70 $64 $31 }T
$602a >PC $7a >S $95 >A $ca >X $04 >Y $27 >P $602a $70 >M $602b $af >M $602c $a2 >M
T{ op PC S A X Y P -> $602c $7a $95 $ca $04 $27 }T T{ $602a M $602b M $602c M -> $70 $af $a2 }T
$7a6a >PC $46 >S $37 >A $a6 >X $3f >Y $ad >P $7a6a $70 >M $7a6b $93 >M $7a6c $64 >M
T{ op PC S A X Y P -> $7a6c $46 $37 $a6 $3f $ad }T T{ $7a6a M $7a6b M $7a6c M -> $70 $93 $64 }T
$b262 >PC $46 >S $df >A $93 >X $b0 >Y $e4 >P $b262 $70 >M $b263 $3d >M $b264 $f0 >M $b2a1 $c9 >M
T{ op PC S A X Y P -> $b2a1 $46 $df $93 $b0 $e4 }T T{ $b262 M $b263 M $b264 M $b2a1 M -> $70 $3d $f0 $c9 }T
$6067 >PC $d8 >S $28 >A $c9 >X $33 >Y $eb >P $6053 $c4 >M $6067 $70 >M $6068 $ea >M $6069 $4f >M
T{ op PC S A X Y P -> $6053 $d8 $28 $c9 $33 $eb }T T{ $6053 M $6067 M $6068 M $6069 M -> $c4 $70 $ea $4f }T
$c870 >PC $35 >S $63 >A $d2 >X $de >Y $aa >P $c870 $70 >M $c871 $f5 >M $c872 $62 >M
T{ op PC S A X Y P -> $c872 $35 $63 $d2 $de $aa }T T{ $c870 M $c871 M $c872 M -> $70 $f5 $62 }T
$b340 >PC $99 >S $f8 >A $b1 >X $ea >Y $20 >P $b340 $70 >M $b341 $e6 >M $b342 $89 >M
T{ op PC S A X Y P -> $b342 $99 $f8 $b1 $ea $20 }T T{ $b340 M $b341 M $b342 M -> $70 $e6 $89 }T
$6f05 >PC $f8 >S $05 >A $45 >X $c4 >Y $66 >P $6f05 $70 >M $6f06 $3d >M $6f07 $20 >M $6f44 $cb >M
T{ op PC S A X Y P -> $6f44 $f8 $05 $45 $c4 $66 }T T{ $6f05 M $6f06 M $6f07 M $6f44 M -> $70 $3d $20 $cb }T
$70bb >PC $e5 >S $6b >A $44 >X $d5 >Y $a3 >P $70bb $70 >M $70bc $c7 >M $70bd $5e >M
T{ op PC S A X Y P -> $70bd $e5 $6b $44 $d5 $a3 }T T{ $70bb M $70bc M $70bd M -> $70 $c7 $5e }T
( 71 )
$2d5a >PC $7b >S $4e >A $e4 >X $8c >Y $69 >P $0045 $cc >M $0046 $ed >M $2d5a $71 >M $2d5b $45 >M $2d5c $f7 >M $ee58 $b0 >M
T{ op PC S A X Y P -> $2d5c $7b $65 $e4 $8c $29 }T T{ $0045 M $0046 M $2d5a M $2d5b M $2d5c M $ee58 M -> $cc $ed $71 $45 $f7 $b0 }T
$7d17 >PC $5d >S $76 >A $ef >X $64 >Y $63 >P $006b $53 >M $006c $fb >M $7d17 $71 >M $7d18 $6b >M $7d19 $48 >M $fbb7 $57 >M
T{ op PC S A X Y P -> $7d19 $5d $ce $ef $64 $e0 }T T{ $006b M $006c M $7d17 M $7d18 M $7d19 M $fbb7 M -> $53 $fb $71 $6b $48 $57 }T
$9897 >PC $7e >S $30 >A $b3 >X $5f >Y $62 >P $00fb $c3 >M $00fc $45 >M $4622 $fc >M $9897 $71 >M $9898 $fb >M $9899 $ee >M
T{ op PC S A X Y P -> $9899 $7e $2c $b3 $5f $21 }T T{ $00fb M $00fc M $4622 M $9897 M $9898 M $9899 M -> $c3 $45 $fc $71 $fb $ee }T
$fc70 >PC $ac >S $59 >A $62 >X $97 >Y $a5 >P $00f6 $4e >M $00f7 $e9 >M $e9e5 $ed >M $fc70 $71 >M $fc71 $f6 >M $fc72 $09 >M
T{ op PC S A X Y P -> $fc72 $ac $47 $62 $97 $25 }T T{ $00f6 M $00f7 M $e9e5 M $fc70 M $fc71 M $fc72 M -> $4e $e9 $ed $71 $f6 $09 }T
$a694 >PC $bd >S $d5 >A $e5 >X $c1 >Y $a1 >P $006c $c2 >M $006d $98 >M $9983 $b4 >M $a694 $71 >M $a695 $6c >M $a696 $74 >M
T{ op PC S A X Y P -> $a696 $bd $8a $e5 $c1 $a1 }T T{ $006c M $006d M $9983 M $a694 M $a695 M $a696 M -> $c2 $98 $b4 $71 $6c $74 }T
$3378 >PC $09 >S $0f >A $85 >X $35 >Y $2b >P $0009 $f0 >M $000a $63 >M $3378 $71 >M $3379 $09 >M $337a $a5 >M $6425 $9f >M
T{ op PC S A X Y P -> $337a $09 $05 $85 $35 $29 }T T{ $0009 M $000a M $3378 M $3379 M $337a M $6425 M -> $f0 $63 $71 $09 $a5 $9f }T
$fec7 >PC $fe >S $db >A $34 >X $c7 >Y $2a >P $000d $9d >M $000e $11 >M $1264 $05 >M $fec7 $71 >M $fec8 $0d >M $fec9 $01 >M
T{ op PC S A X Y P -> $fec9 $fe $46 $34 $c7 $29 }T T{ $000d M $000e M $1264 M $fec7 M $fec8 M $fec9 M -> $9d $11 $05 $71 $0d $01 }T
$0689 >PC $51 >S $9f >A $ca >X $4b >Y $e0 >P $0047 $4e >M $0048 $9a >M $0689 $71 >M $068a $47 >M $068b $f6 >M $9a99 $a8 >M
T{ op PC S A X Y P -> $068b $51 $47 $ca $4b $61 }T T{ $0047 M $0048 M $0689 M $068a M $068b M $9a99 M -> $4e $9a $71 $47 $f6 $a8 }T
$0f33 >PC $06 >S $1f >A $9d >X $c5 >Y $aa >P $00b9 $6b >M $00ba $0a >M $0b30 $83 >M $0f33 $71 >M $0f34 $b9 >M $0f35 $02 >M
T{ op PC S A X Y P -> $0f35 $06 $08 $9d $c5 $29 }T T{ $00b9 M $00ba M $0b30 M $0f33 M $0f34 M $0f35 M -> $6b $0a $83 $71 $b9 $02 }T
$7359 >PC $19 >S $e6 >A $c5 >X $85 >Y $28 >P $00e0 $8a >M $00e1 $db >M $7359 $71 >M $735a $e0 >M $735b $92 >M $dc0f $f8 >M
T{ op PC S A X Y P -> $735b $19 $44 $c5 $85 $29 }T T{ $00e0 M $00e1 M $7359 M $735a M $735b M $dc0f M -> $8a $db $71 $e0 $92 $f8 }T
$2610 >PC $5d >S $61 >A $18 >X $f9 >Y $66 >P $00eb $7e >M $00ec $93 >M $2610 $71 >M $2611 $eb >M $2612 $5c >M $9477 $82 >M
T{ op PC S A X Y P -> $2612 $5d $e3 $18 $f9 $a4 }T T{ $00eb M $00ec M $2610 M $2611 M $2612 M $9477 M -> $7e $93 $71 $eb $5c $82 }T
$14b3 >PC $45 >S $8f >A $31 >X $e6 >Y $e0 >P $0090 $f4 >M $0091 $2b >M $14b3 $71 >M $14b4 $90 >M $14b5 $f4 >M $2cda $4f >M
T{ op PC S A X Y P -> $14b5 $45 $de $31 $e6 $a0 }T T{ $0090 M $0091 M $14b3 M $14b4 M $14b5 M $2cda M -> $f4 $2b $71 $90 $f4 $4f }T
$b01c >PC $05 >S $9d >A $69 >X $6d >Y $65 >P $005c $9d >M $005d $2c >M $2d0a $df >M $b01c $71 >M $b01d $5c >M $b01e $47 >M
T{ op PC S A X Y P -> $b01e $05 $7d $69 $6d $65 }T T{ $005c M $005d M $2d0a M $b01c M $b01d M $b01e M -> $9d $2c $df $71 $5c $47 }T
$b42f >PC $13 >S $2c >A $a4 >X $d3 >Y $28 >P $004d $2d >M $004e $bc >M $b42f $71 >M $b430 $4d >M $b431 $45 >M $bd00 $e8 >M
T{ op PC S A X Y P -> $b431 $13 $7a $a4 $d3 $29 }T T{ $004d M $004e M $b42f M $b430 M $b431 M $bd00 M -> $2d $bc $71 $4d $45 $e8 }T
$3363 >PC $a3 >S $93 >A $f2 >X $fd >Y $20 >P $002d $8f >M $002e $71 >M $3363 $71 >M $3364 $2d >M $3365 $7a >M $728c $07 >M
T{ op PC S A X Y P -> $3365 $a3 $9a $f2 $fd $a0 }T T{ $002d M $002e M $3363 M $3364 M $3365 M $728c M -> $8f $71 $71 $2d $7a $07 }T
$eb52 >PC $c4 >S $c2 >A $38 >X $d8 >Y $2e >P $00fe $a2 >M $00ff $3c >M $3d7a $03 >M $eb52 $71 >M $eb53 $fe >M $eb54 $c0 >M
T{ op PC S A X Y P -> $eb54 $c4 $25 $38 $d8 $2d }T T{ $00fe M $00ff M $3d7a M $eb52 M $eb53 M $eb54 M -> $a2 $3c $03 $71 $fe $c0 }T
( 72 )
$f0f8 >PC $f5 >S $80 >A $df >X $dc >Y $28 >P $0047 $9e >M $0048 $a9 >M $a99e $f6 >M $f0f8 $72 >M $f0f9 $47 >M $f0fa $5e >M
T{ op PC S A X Y P -> $f0fa $f5 $d6 $df $dc $e9 }T T{ $0047 M $0048 M $a99e M $f0f8 M $f0f9 M $f0fa M -> $9e $a9 $f6 $72 $47 $5e }T
$9dc9 >PC $09 >S $3a >A $63 >X $85 >Y $2d >P $0071 $27 >M $0072 $21 >M $2127 $38 >M $9dc9 $72 >M $9dca $71 >M $9dcb $74 >M
T{ op PC S A X Y P -> $9dcb $09 $79 $63 $85 $2c }T T{ $0071 M $0072 M $2127 M $9dc9 M $9dca M $9dcb M -> $27 $21 $38 $72 $71 $74 }T
$4c21 >PC $db >S $2d >A $b2 >X $d7 >Y $e6 >P $0082 $d1 >M $0083 $f7 >M $4c21 $72 >M $4c22 $82 >M $4c23 $a2 >M $f7d1 $8b >M
T{ op PC S A X Y P -> $4c23 $db $b8 $b2 $d7 $a4 }T T{ $0082 M $0083 M $4c21 M $4c22 M $4c23 M $f7d1 M -> $d1 $f7 $72 $82 $a2 $8b }T
$bb37 >PC $6c >S $a9 >A $7c >X $d4 >Y $66 >P $0017 $12 >M $0018 $f7 >M $bb37 $72 >M $bb38 $17 >M $bb39 $7f >M $f712 $fd >M
T{ op PC S A X Y P -> $bb39 $6c $a6 $7c $d4 $a5 }T T{ $0017 M $0018 M $bb37 M $bb38 M $bb39 M $f712 M -> $12 $f7 $72 $17 $7f $fd }T
$58f1 >PC $a9 >S $ba >A $15 >X $7f >Y $62 >P $00d2 $ce >M $00d3 $ab >M $58f1 $72 >M $58f2 $d2 >M $58f3 $a2 >M $abce $30 >M
T{ op PC S A X Y P -> $58f3 $a9 $ea $15 $7f $a0 }T T{ $00d2 M $00d3 M $58f1 M $58f2 M $58f3 M $abce M -> $ce $ab $72 $d2 $a2 $30 }T
$72b7 >PC $e6 >S $04 >A $f9 >X $f8 >Y $e7 >P $004c $35 >M $004d $12 >M $1235 $ff >M $72b7 $72 >M $72b8 $4c >M $72b9 $0f >M
T{ op PC S A X Y P -> $72b9 $e6 $04 $f9 $f8 $25 }T T{ $004c M $004d M $1235 M $72b7 M $72b8 M $72b9 M -> $35 $12 $ff $72 $4c $0f }T
$ed44 >PC $c7 >S $73 >A $cd >X $23 >Y $25 >P $0071 $6b >M $0072 $eb >M $eb6b $15 >M $ed44 $72 >M $ed45 $71 >M $ed46 $59 >M
T{ op PC S A X Y P -> $ed46 $c7 $89 $cd $23 $e4 }T T{ $0071 M $0072 M $eb6b M $ed44 M $ed45 M $ed46 M -> $6b $eb $15 $72 $71 $59 }T
$0b02 >PC $d0 >S $0a >A $0d >X $6f >Y $28 >P $0039 $0e >M $003a $3b >M $0b02 $72 >M $0b03 $39 >M $0b04 $90 >M $3b0e $4e >M
T{ op PC S A X Y P -> $0b04 $d0 $5e $0d $6f $28 }T T{ $0039 M $003a M $0b02 M $0b03 M $0b04 M $3b0e M -> $0e $3b $72 $39 $90 $4e }T
$5b5a >PC $8b >S $64 >A $96 >X $69 >Y $64 >P $00d7 $05 >M $00d8 $9a >M $5b5a $72 >M $5b5b $d7 >M $5b5c $ea >M $9a05 $19 >M
T{ op PC S A X Y P -> $5b5c $8b $7d $96 $69 $24 }T T{ $00d7 M $00d8 M $5b5a M $5b5b M $5b5c M $9a05 M -> $05 $9a $72 $d7 $ea $19 }T
$ca90 >PC $a0 >S $70 >A $58 >X $23 >Y $e0 >P $0021 $6e >M $0022 $60 >M $606e $2a >M $ca90 $72 >M $ca91 $21 >M $ca92 $16 >M
T{ op PC S A X Y P -> $ca92 $a0 $9a $58 $23 $e0 }T T{ $0021 M $0022 M $606e M $ca90 M $ca91 M $ca92 M -> $6e $60 $2a $72 $21 $16 }T
$77cf >PC $dc >S $86 >A $7d >X $62 >Y $23 >P $008b $4b >M $008c $4f >M $4f4b $e2 >M $77cf $72 >M $77d0 $8b >M $77d1 $9f >M
T{ op PC S A X Y P -> $77d1 $dc $69 $7d $62 $61 }T T{ $008b M $008c M $4f4b M $77cf M $77d0 M $77d1 M -> $4b $4f $e2 $72 $8b $9f }T
$4b96 >PC $5a >S $b1 >A $85 >X $4f >Y $6f >P $0025 $eb >M $0026 $15 >M $15eb $90 >M $4b96 $72 >M $4b97 $25 >M $4b98 $6f >M
T{ op PC S A X Y P -> $4b98 $5a $a2 $85 $4f $ed }T T{ $0025 M $0026 M $15eb M $4b96 M $4b97 M $4b98 M -> $eb $15 $90 $72 $25 $6f }T
$cd0b >PC $93 >S $6d >A $39 >X $c1 >Y $23 >P $00ee $7e >M $00ef $53 >M $537e $f8 >M $cd0b $72 >M $cd0c $ee >M $cd0d $1e >M
T{ op PC S A X Y P -> $cd0d $93 $66 $39 $c1 $21 }T T{ $00ee M $00ef M $537e M $cd0b M $cd0c M $cd0d M -> $7e $53 $f8 $72 $ee $1e }T
$7c96 >PC $ce >S $29 >A $a0 >X $ba >Y $2d >P $0013 $d0 >M $0014 $4a >M $4ad0 $60 >M $7c96 $72 >M $7c97 $13 >M $7c98 $48 >M
T{ op PC S A X Y P -> $7c98 $ce $90 $a0 $ba $ec }T T{ $0013 M $0014 M $4ad0 M $7c96 M $7c97 M $7c98 M -> $d0 $4a $60 $72 $13 $48 }T
$43c4 >PC $e6 >S $77 >A $b8 >X $79 >Y $25 >P $009e $cc >M $009f $d4 >M $43c4 $72 >M $43c5 $9e >M $43c6 $5c >M $d4cc $7b >M
T{ op PC S A X Y P -> $43c6 $e6 $f3 $b8 $79 $e4 }T T{ $009e M $009f M $43c4 M $43c5 M $43c6 M $d4cc M -> $cc $d4 $72 $9e $5c $7b }T
$865e >PC $3e >S $e9 >A $10 >X $88 >Y $ab >P $009c $b3 >M $009d $b1 >M $865e $72 >M $865f $9c >M $8660 $81 >M $b1b3 $19 >M
T{ op PC S A X Y P -> $8660 $3e $69 $10 $88 $29 }T T{ $009c M $009d M $865e M $865f M $8660 M $b1b3 M -> $b3 $b1 $72 $9c $81 $19 }T
( 73 )
$78ac >PC $34 >S $91 >A $ed >X $e1 >Y $29 >P $78ac $73 >M $78ad $0d >M $78ae $70 >M
T{ op PC S A X Y P -> $78ad $34 $91 $ed $e1 $29 }T T{ $78ac M $78ad M $78ae M -> $73 $0d $70 }T
$3783 >PC $d1 >S $63 >A $a0 >X $65 >Y $a3 >P $3783 $73 >M $3784 $b3 >M $3785 $e6 >M
T{ op PC S A X Y P -> $3784 $d1 $63 $a0 $65 $a3 }T T{ $3783 M $3784 M $3785 M -> $73 $b3 $e6 }T
$a43d >PC $a1 >S $2d >A $b7 >X $77 >Y $67 >P $a43d $73 >M $a43e $d7 >M $a43f $c6 >M
T{ op PC S A X Y P -> $a43e $a1 $2d $b7 $77 $67 }T T{ $a43d M $a43e M $a43f M -> $73 $d7 $c6 }T
$bc73 >PC $bc >S $52 >A $0e >X $1d >Y $a4 >P $bc73 $73 >M $bc74 $d9 >M $bc75 $9f >M
T{ op PC S A X Y P -> $bc74 $bc $52 $0e $1d $a4 }T T{ $bc73 M $bc74 M $bc75 M -> $73 $d9 $9f }T
$b2d8 >PC $ab >S $1c >A $a8 >X $42 >Y $e9 >P $b2d8 $73 >M $b2d9 $1c >M $b2da $fe >M
T{ op PC S A X Y P -> $b2d9 $ab $1c $a8 $42 $e9 }T T{ $b2d8 M $b2d9 M $b2da M -> $73 $1c $fe }T
$c13b >PC $fb >S $e2 >A $df >X $2b >Y $21 >P $c13b $73 >M $c13c $74 >M $c13d $fc >M
T{ op PC S A X Y P -> $c13c $fb $e2 $df $2b $21 }T T{ $c13b M $c13c M $c13d M -> $73 $74 $fc }T
$a34c >PC $23 >S $d0 >A $92 >X $9e >Y $a2 >P $a34c $73 >M $a34d $6f >M $a34e $bc >M
T{ op PC S A X Y P -> $a34d $23 $d0 $92 $9e $a2 }T T{ $a34c M $a34d M $a34e M -> $73 $6f $bc }T
$3c8b >PC $0b >S $a0 >A $f2 >X $45 >Y $e4 >P $3c8b $73 >M $3c8c $6f >M $3c8d $0a >M
T{ op PC S A X Y P -> $3c8c $0b $a0 $f2 $45 $e4 }T T{ $3c8b M $3c8c M $3c8d M -> $73 $6f $0a }T
$61e1 >PC $07 >S $50 >A $13 >X $85 >Y $23 >P $61e1 $73 >M $61e2 $50 >M $61e3 $b1 >M
T{ op PC S A X Y P -> $61e2 $07 $50 $13 $85 $23 }T T{ $61e1 M $61e2 M $61e3 M -> $73 $50 $b1 }T
$924e >PC $fc >S $62 >A $0c >X $b6 >Y $a7 >P $924e $73 >M $924f $f6 >M $9250 $00 >M
T{ op PC S A X Y P -> $924f $fc $62 $0c $b6 $a7 }T T{ $924e M $924f M $9250 M -> $73 $f6 $00 }T
$4373 >PC $f8 >S $4b >A $23 >X $9b >Y $e2 >P $4373 $73 >M $4374 $9a >M $4375 $4c >M
T{ op PC S A X Y P -> $4374 $f8 $4b $23 $9b $e2 }T T{ $4373 M $4374 M $4375 M -> $73 $9a $4c }T
$e141 >PC $b2 >S $8f >A $0a >X $f2 >Y $27 >P $e141 $73 >M $e142 $09 >M $e143 $fc >M
T{ op PC S A X Y P -> $e142 $b2 $8f $0a $f2 $27 }T T{ $e141 M $e142 M $e143 M -> $73 $09 $fc }T
$7b20 >PC $27 >S $93 >A $da >X $77 >Y $2a >P $7b20 $73 >M $7b21 $01 >M $7b22 $99 >M
T{ op PC S A X Y P -> $7b21 $27 $93 $da $77 $2a }T T{ $7b20 M $7b21 M $7b22 M -> $73 $01 $99 }T
$9130 >PC $f2 >S $3b >A $05 >X $ba >Y $66 >P $9130 $73 >M $9131 $ec >M $9132 $5d >M
T{ op PC S A X Y P -> $9131 $f2 $3b $05 $ba $66 }T T{ $9130 M $9131 M $9132 M -> $73 $ec $5d }T
$1e20 >PC $ae >S $81 >A $31 >X $63 >Y $ec >P $1e20 $73 >M $1e21 $7f >M $1e22 $6f >M
T{ op PC S A X Y P -> $1e21 $ae $81 $31 $63 $ec }T T{ $1e20 M $1e21 M $1e22 M -> $73 $7f $6f }T
$18b6 >PC $90 >S $6f >A $eb >X $af >Y $62 >P $18b6 $73 >M $18b7 $7c >M $18b8 $0d >M
T{ op PC S A X Y P -> $18b7 $90 $6f $eb $af $62 }T T{ $18b6 M $18b7 M $18b8 M -> $73 $7c $0d }T
( 74 )
$7680 >PC $81 >S $be >A $dc >X $00 >Y $21 >P $0012 $04 >M $7680 $74 >M $7681 $12 >M $7682 $ba >M
T{ op PC S A X Y P -> $7682 $81 $be $dc $00 $21 }T T{ $0012 M $00ee M $7680 M $7681 M $7682 M -> $04 $00 $74 $12 $ba }T
$8c36 >PC $f1 >S $fd >A $b7 >X $f4 >Y $a6 >P $00f7 $4e >M $8c36 $74 >M $8c37 $f7 >M $8c38 $8a >M
T{ op PC S A X Y P -> $8c38 $f1 $fd $b7 $f4 $a6 }T T{ $00ae M $00f7 M $8c36 M $8c37 M $8c38 M -> $00 $4e $74 $f7 $8a }T
$9b2c >PC $30 >S $29 >A $ec >X $e2 >Y $a1 >P $009e $99 >M $9b2c $74 >M $9b2d $9e >M $9b2e $41 >M
T{ op PC S A X Y P -> $9b2e $30 $29 $ec $e2 $a1 }T T{ $008a M $009e M $9b2c M $9b2d M $9b2e M -> $00 $99 $74 $9e $41 }T
$d1e0 >PC $63 >S $b8 >A $72 >X $fe >Y $23 >P $00e3 $c9 >M $d1e0 $74 >M $d1e1 $e3 >M $d1e2 $3e >M
T{ op PC S A X Y P -> $d1e2 $63 $b8 $72 $fe $23 }T T{ $0055 M $00e3 M $d1e0 M $d1e1 M $d1e2 M -> $00 $c9 $74 $e3 $3e }T
$d661 >PC $a6 >S $45 >A $74 >X $cd >Y $20 >P $00eb $4b >M $d661 $74 >M $d662 $eb >M $d663 $be >M
T{ op PC S A X Y P -> $d663 $a6 $45 $74 $cd $20 }T T{ $005f M $00eb M $d661 M $d662 M $d663 M -> $00 $4b $74 $eb $be }T
$2d94 >PC $63 >S $7e >A $7e >X $36 >Y $eb >P $0017 $c0 >M $2d94 $74 >M $2d95 $17 >M $2d96 $61 >M
T{ op PC S A X Y P -> $2d96 $63 $7e $7e $36 $eb }T T{ $0017 M $0095 M $2d94 M $2d95 M $2d96 M -> $c0 $00 $74 $17 $61 }T
$8c81 >PC $29 >S $d8 >A $db >X $e1 >Y $af >P $006a $34 >M $8c81 $74 >M $8c82 $6a >M $8c83 $78 >M
T{ op PC S A X Y P -> $8c83 $29 $d8 $db $e1 $af }T T{ $0045 M $006a M $8c81 M $8c82 M $8c83 M -> $00 $34 $74 $6a $78 }T
$9764 >PC $e8 >S $0b >A $4f >X $e3 >Y $ef >P $006f $c2 >M $9764 $74 >M $9765 $6f >M $9766 $ae >M
T{ op PC S A X Y P -> $9766 $e8 $0b $4f $e3 $ef }T T{ $006f M $00be M $9764 M $9765 M $9766 M -> $c2 $00 $74 $6f $ae }T
$985d >PC $1d >S $23 >A $45 >X $10 >Y $aa >P $0019 $7d >M $985d $74 >M $985e $19 >M $985f $cc >M
T{ op PC S A X Y P -> $985f $1d $23 $45 $10 $aa }T T{ $0019 M $005e M $985d M $985e M $985f M -> $7d $00 $74 $19 $cc }T
$f158 >PC $ba >S $71 >A $b8 >X $fb >Y $64 >P $000d $86 >M $f158 $74 >M $f159 $0d >M $f15a $32 >M
T{ op PC S A X Y P -> $f15a $ba $71 $b8 $fb $64 }T T{ $000d M $00c5 M $f158 M $f159 M $f15a M -> $86 $00 $74 $0d $32 }T
$0ac7 >PC $c7 >S $fd >A $13 >X $8d >Y $a6 >P $00b5 $37 >M $0ac7 $74 >M $0ac8 $b5 >M $0ac9 $42 >M
T{ op PC S A X Y P -> $0ac9 $c7 $fd $13 $8d $a6 }T T{ $00b5 M $00c8 M $0ac7 M $0ac8 M $0ac9 M -> $37 $00 $74 $b5 $42 }T
$c0b1 >PC $18 >S $57 >A $08 >X $b7 >Y $ae >P $00d4 $9b >M $c0b1 $74 >M $c0b2 $d4 >M $c0b3 $91 >M
T{ op PC S A X Y P -> $c0b3 $18 $57 $08 $b7 $ae }T T{ $00d4 M $00dc M $c0b1 M $c0b2 M $c0b3 M -> $9b $00 $74 $d4 $91 }T
$dee6 >PC $29 >S $bf >A $ba >X $e4 >Y $a4 >P $0023 $dd >M $dee6 $74 >M $dee7 $23 >M $dee8 $b1 >M
T{ op PC S A X Y P -> $dee8 $29 $bf $ba $e4 $a4 }T T{ $0023 M $00dd M $dee6 M $dee7 M $dee8 M -> $dd $00 $74 $23 $b1 }T
$0814 >PC $78 >S $74 >A $4c >X $54 >Y $a9 >P $00a0 $56 >M $0814 $74 >M $0815 $a0 >M $0816 $23 >M
T{ op PC S A X Y P -> $0816 $78 $74 $4c $54 $a9 }T T{ $00a0 M $00ec M $0814 M $0815 M $0816 M -> $56 $00 $74 $a0 $23 }T
$db18 >PC $a6 >S $54 >A $57 >X $bf >Y $ae >P $0051 $6c >M $db18 $74 >M $db19 $51 >M $db1a $67 >M
T{ op PC S A X Y P -> $db1a $a6 $54 $57 $bf $ae }T T{ $0051 M $00a8 M $db18 M $db19 M $db1a M -> $6c $00 $74 $51 $67 }T
$20dd >PC $6b >S $9e >A $f2 >X $50 >Y $21 >P $0075 $bf >M $20dd $74 >M $20de $75 >M $20df $ec >M
T{ op PC S A X Y P -> $20df $6b $9e $f2 $50 $21 }T T{ $0067 M $0075 M $20dd M $20de M $20df M -> $00 $bf $74 $75 $ec }T
( 75 )
$422d >PC $28 >S $43 >A $7e >X $ec >Y $e6 >P $006d $19 >M $00eb $c6 >M $422d $75 >M $422e $6d >M $422f $26 >M
T{ op PC S A X Y P -> $422f $28 $09 $7e $ec $25 }T T{ $006d M $00eb M $422d M $422e M $422f M -> $19 $c6 $75 $6d $26 }T
$5307 >PC $b6 >S $22 >A $06 >X $f9 >Y $29 >P $00f8 $f6 >M $00fe $ea >M $5307 $75 >M $5308 $f8 >M $5309 $52 >M
T{ op PC S A X Y P -> $5309 $b6 $73 $06 $f9 $29 }T T{ $00f8 M $00fe M $5307 M $5308 M $5309 M -> $f6 $ea $75 $f8 $52 }T
$e460 >PC $37 >S $c5 >A $12 >X $f3 >Y $ac >P $0096 $cd >M $00a8 $39 >M $e460 $75 >M $e461 $96 >M $e462 $03 >M
T{ op PC S A X Y P -> $e462 $37 $64 $12 $f3 $2d }T T{ $0096 M $00a8 M $e460 M $e461 M $e462 M -> $cd $39 $75 $96 $03 }T
$e6e1 >PC $e6 >S $fe >A $8e >X $40 >Y $e8 >P $0065 $25 >M $00f3 $47 >M $e6e1 $75 >M $e6e2 $65 >M $e6e3 $a3 >M
T{ op PC S A X Y P -> $e6e3 $e6 $ab $8e $40 $a9 }T T{ $0065 M $00f3 M $e6e1 M $e6e2 M $e6e3 M -> $25 $47 $75 $65 $a3 }T
$9ee8 >PC $0f >S $cf >A $a7 >X $87 >Y $a5 >P $008e $c6 >M $00e7 $79 >M $9ee8 $75 >M $9ee9 $e7 >M $9eea $9e >M
T{ op PC S A X Y P -> $9eea $0f $96 $a7 $87 $a5 }T T{ $008e M $00e7 M $9ee8 M $9ee9 M $9eea M -> $c6 $79 $75 $e7 $9e }T
$864b >PC $74 >S $e2 >A $5c >X $1b >Y $af >P $0064 $94 >M $00c0 $ca >M $864b $75 >M $864c $64 >M $864d $42 >M
T{ op PC S A X Y P -> $864d $74 $13 $5c $1b $2d }T T{ $0064 M $00c0 M $864b M $864c M $864d M -> $94 $ca $75 $64 $42 }T
$e032 >PC $9b >S $f2 >A $83 >X $97 >Y $63 >P $003e $cf >M $00bb $f2 >M $e032 $75 >M $e033 $bb >M $e034 $3e >M
T{ op PC S A X Y P -> $e034 $9b $c2 $83 $97 $a1 }T T{ $003e M $00bb M $e032 M $e033 M $e034 M -> $cf $f2 $75 $bb $3e }T
$1d68 >PC $0c >S $b1 >A $88 >X $ff >Y $65 >P $0015 $ed >M $008d $7c >M $1d68 $75 >M $1d69 $8d >M $1d6a $66 >M
T{ op PC S A X Y P -> $1d6a $0c $9f $88 $ff $a5 }T T{ $0015 M $008d M $1d68 M $1d69 M $1d6a M -> $ed $7c $75 $8d $66 }T
$f949 >PC $00 >S $aa >A $ae >X $10 >Y $26 >P $0081 $4d >M $00d3 $69 >M $f949 $75 >M $f94a $d3 >M $f94b $6a >M
T{ op PC S A X Y P -> $f94b $00 $f7 $ae $10 $a4 }T T{ $0081 M $00d3 M $f949 M $f94a M $f94b M -> $4d $69 $75 $d3 $6a }T
$8f8b >PC $fb >S $74 >A $c6 >X $63 >Y $e4 >P $0036 $a0 >M $00fc $3b >M $8f8b $75 >M $8f8c $36 >M $8f8d $2f >M
T{ op PC S A X Y P -> $8f8d $fb $af $c6 $63 $e4 }T T{ $0036 M $00fc M $8f8b M $8f8c M $8f8d M -> $a0 $3b $75 $36 $2f }T
$e60c >PC $d3 >S $2f >A $83 >X $eb >Y $ee >P $0067 $a7 >M $00ea $6d >M $e60c $75 >M $e60d $67 >M $e60e $66 >M
T{ op PC S A X Y P -> $e60e $d3 $92 $83 $eb $ec }T T{ $0067 M $00ea M $e60c M $e60d M $e60e M -> $a7 $6d $75 $67 $66 }T
$b159 >PC $8a >S $70 >A $78 >X $6d >Y $a5 >P $001f $90 >M $0097 $20 >M $b159 $75 >M $b15a $1f >M $b15b $be >M
T{ op PC S A X Y P -> $b15b $8a $91 $78 $6d $e4 }T T{ $001f M $0097 M $b159 M $b15a M $b15b M -> $90 $20 $75 $1f $be }T
$7a6b >PC $cd >S $8e >A $eb >X $54 >Y $24 >P $0071 $54 >M $0086 $c8 >M $7a6b $75 >M $7a6c $86 >M $7a6d $d0 >M
T{ op PC S A X Y P -> $7a6d $cd $e2 $eb $54 $a4 }T T{ $0071 M $0086 M $7a6b M $7a6c M $7a6d M -> $54 $c8 $75 $86 $d0 }T
$762c >PC $20 >S $c4 >A $60 >X $5a >Y $ec >P $0056 $a5 >M $00f6 $b6 >M $762c $75 >M $762d $f6 >M $762e $02 >M
T{ op PC S A X Y P -> $762e $20 $c9 $60 $5a $ed }T T{ $0056 M $00f6 M $762c M $762d M $762e M -> $a5 $b6 $75 $f6 $02 }T
$1f3e >PC $5c >S $d5 >A $cb >X $c5 >Y $23 >P $0094 $cc >M $00c9 $87 >M $1f3e $75 >M $1f3f $c9 >M $1f40 $97 >M
T{ op PC S A X Y P -> $1f40 $5c $a2 $cb $c5 $a1 }T T{ $0094 M $00c9 M $1f3e M $1f3f M $1f40 M -> $cc $87 $75 $c9 $97 }T
$1f7e >PC $98 >S $69 >A $55 >X $fd >Y $2b >P $0034 $b6 >M $0089 $7f >M $1f7e $75 >M $1f7f $34 >M $1f80 $25 >M
T{ op PC S A X Y P -> $1f80 $98 $4f $55 $fd $69 }T T{ $0034 M $0089 M $1f7e M $1f7f M $1f80 M -> $b6 $7f $75 $34 $25 }T
( 76 )
$41c4 >PC $32 >S $81 >A $d1 >X $74 >Y $e8 >P $000b $0b >M $003a $8c >M $41c4 $76 >M $41c5 $3a >M $41c6 $58 >M
T{ op PC S A X Y P -> $41c6 $32 $81 $d1 $74 $69 }T T{ $000b M $003a M $41c4 M $41c5 M $41c6 M -> $05 $8c $76 $3a $58 }T
$d7bf >PC $25 >S $f1 >A $99 >X $37 >Y $2e >P $0061 $44 >M $00fa $f0 >M $d7bf $76 >M $d7c0 $61 >M $d7c1 $73 >M
T{ op PC S A X Y P -> $d7c1 $25 $f1 $99 $37 $2c }T T{ $0061 M $00fa M $d7bf M $d7c0 M $d7c1 M -> $44 $78 $76 $61 $73 }T
$81e3 >PC $53 >S $73 >A $84 >X $53 >Y $a4 >P $0020 $0a >M $00a4 $86 >M $81e3 $76 >M $81e4 $20 >M $81e5 $99 >M
T{ op PC S A X Y P -> $81e5 $53 $73 $84 $53 $24 }T T{ $0020 M $00a4 M $81e3 M $81e4 M $81e5 M -> $0a $43 $76 $20 $99 }T
$fdea >PC $0a >S $61 >A $16 >X $32 >Y $61 >P $0075 $74 >M $008b $b9 >M $fdea $76 >M $fdeb $75 >M $fdec $aa >M
T{ op PC S A X Y P -> $fdec $0a $61 $16 $32 $e1 }T T{ $0075 M $008b M $fdea M $fdeb M $fdec M -> $74 $dc $76 $75 $aa }T
$df7e >PC $be >S $db >A $40 >X $d8 >Y $ad >P $002e $f7 >M $00ee $16 >M $df7e $76 >M $df7f $ee >M $df80 $d2 >M
T{ op PC S A X Y P -> $df80 $be $db $40 $d8 $ad }T T{ $002e M $00ee M $df7e M $df7f M $df80 M -> $fb $16 $76 $ee $d2 }T
$4b11 >PC $fa >S $0e >A $d0 >X $5b >Y $ac >P $006d $11 >M $009d $8f >M $4b11 $76 >M $4b12 $9d >M $4b13 $e2 >M
T{ op PC S A X Y P -> $4b13 $fa $0e $d0 $5b $2d }T T{ $006d M $009d M $4b11 M $4b12 M $4b13 M -> $08 $8f $76 $9d $e2 }T
$7025 >PC $cb >S $18 >A $1a >X $ca >Y $67 >P $001f $69 >M $0039 $54 >M $7025 $76 >M $7026 $1f >M $7027 $58 >M
T{ op PC S A X Y P -> $7027 $cb $18 $1a $ca $e4 }T T{ $001f M $0039 M $7025 M $7026 M $7027 M -> $69 $aa $76 $1f $58 }T
$2328 >PC $fa >S $b0 >A $e7 >X $d5 >Y $af >P $000c $f0 >M $00f3 $c0 >M $2328 $76 >M $2329 $0c >M $232a $b6 >M
T{ op PC S A X Y P -> $232a $fa $b0 $e7 $d5 $ac }T T{ $000c M $00f3 M $2328 M $2329 M $232a M -> $f0 $e0 $76 $0c $b6 }T
$446e >PC $06 >S $fc >A $45 >X $d1 >Y $e7 >P $0031 $bb >M $00ec $9e >M $446e $76 >M $446f $ec >M $4470 $ec >M
T{ op PC S A X Y P -> $4470 $06 $fc $45 $d1 $e5 }T T{ $0031 M $00ec M $446e M $446f M $4470 M -> $dd $9e $76 $ec $ec }T
$b770 >PC $06 >S $df >A $ee >X $e7 >Y $ed >P $0008 $98 >M $001a $f5 >M $b770 $76 >M $b771 $1a >M $b772 $da >M
T{ op PC S A X Y P -> $b772 $06 $df $ee $e7 $ec }T T{ $0008 M $001a M $b770 M $b771 M $b772 M -> $cc $f5 $76 $1a $da }T
$b73b >PC $28 >S $04 >A $bc >X $d6 >Y $66 >P $002a $88 >M $00e6 $50 >M $b73b $76 >M $b73c $2a >M $b73d $48 >M
T{ op PC S A X Y P -> $b73d $28 $04 $bc $d6 $64 }T T{ $002a M $00e6 M $b73b M $b73c M $b73d M -> $88 $28 $76 $2a $48 }T
$7168 >PC $04 >S $a1 >A $f1 >X $89 >Y $29 >P $0096 $7e >M $00a5 $93 >M $7168 $76 >M $7169 $a5 >M $716a $71 >M
T{ op PC S A X Y P -> $716a $04 $a1 $f1 $89 $a8 }T T{ $0096 M $00a5 M $7168 M $7169 M $716a M -> $bf $93 $76 $a5 $71 }T
$02cd >PC $9b >S $ab >A $82 >X $fa >Y $24 >P $0038 $01 >M $00ba $f8 >M $02cd $76 >M $02ce $38 >M $02cf $a7 >M
T{ op PC S A X Y P -> $02cf $9b $ab $82 $fa $24 }T T{ $0038 M $00ba M $02cd M $02ce M $02cf M -> $01 $7c $76 $38 $a7 }T
$d4a0 >PC $38 >S $76 >A $5f >X $47 >Y $2a >P $005f $fb >M $00be $dc >M $d4a0 $76 >M $d4a1 $5f >M $d4a2 $ce >M
T{ op PC S A X Y P -> $d4a2 $38 $76 $5f $47 $28 }T T{ $005f M $00be M $d4a0 M $d4a1 M $d4a2 M -> $fb $6e $76 $5f $ce }T
$21dd >PC $94 >S $24 >A $75 >X $c1 >Y $e5 >P $0057 $ba >M $00cc $2f >M $21dd $76 >M $21de $57 >M $21df $e2 >M
T{ op PC S A X Y P -> $21df $94 $24 $75 $c1 $e5 }T T{ $0057 M $00cc M $21dd M $21de M $21df M -> $ba $97 $76 $57 $e2 }T
$11a6 >PC $67 >S $22 >A $72 >X $ac >Y $60 >P $005a $17 >M $00e8 $17 >M $11a6 $76 >M $11a7 $e8 >M $11a8 $b3 >M
T{ op PC S A X Y P -> $11a8 $67 $22 $72 $ac $61 }T T{ $005a M $00e8 M $11a6 M $11a7 M $11a8 M -> $0b $17 $76 $e8 $b3 }T
( 77 )
$38a3 >PC $17 >S $f3 >A $89 >X $cc >Y $af >P $0081 $1c >M $38a3 $77 >M $38a4 $81 >M $38a5 $8e >M
T{ op PC S A X Y P -> $38a5 $17 $f3 $89 $cc $af }T T{ $0081 M $38a3 M $38a4 M $38a5 M -> $1c $77 $81 $8e }T
$1183 >PC $90 >S $c6 >A $54 >X $ec >Y $ab >P $00bf $e1 >M $1183 $77 >M $1184 $bf >M $1185 $95 >M
T{ op PC S A X Y P -> $1185 $90 $c6 $54 $ec $ab }T T{ $00bf M $1183 M $1184 M $1185 M -> $61 $77 $bf $95 }T
$036d >PC $8f >S $71 >A $2e >X $76 >Y $a8 >P $00ee $3c >M $036d $77 >M $036e $ee >M $036f $9a >M
T{ op PC S A X Y P -> $036f $8f $71 $2e $76 $a8 }T T{ $00ee M $036d M $036e M $036f M -> $3c $77 $ee $9a }T
$4eee >PC $2f >S $76 >A $1d >X $04 >Y $23 >P $0067 $36 >M $4eee $77 >M $4eef $67 >M $4ef0 $b4 >M
T{ op PC S A X Y P -> $4ef0 $2f $76 $1d $04 $23 }T T{ $0067 M $4eee M $4eef M $4ef0 M -> $36 $77 $67 $b4 }T
$6802 >PC $24 >S $6f >A $49 >X $d1 >Y $64 >P $0089 $d2 >M $6802 $77 >M $6803 $89 >M $6804 $da >M
T{ op PC S A X Y P -> $6804 $24 $6f $49 $d1 $64 }T T{ $0089 M $6802 M $6803 M $6804 M -> $52 $77 $89 $da }T
$c54d >PC $9e >S $f7 >A $51 >X $9e >Y $6c >P $0073 $60 >M $c54d $77 >M $c54e $73 >M $c54f $c2 >M
T{ op PC S A X Y P -> $c54f $9e $f7 $51 $9e $6c }T T{ $0073 M $c54d M $c54e M $c54f M -> $60 $77 $73 $c2 }T
$7ec2 >PC $7b >S $a2 >A $50 >X $5f >Y $a4 >P $006e $f2 >M $7ec2 $77 >M $7ec3 $6e >M $7ec4 $0c >M
T{ op PC S A X Y P -> $7ec4 $7b $a2 $50 $5f $a4 }T T{ $006e M $7ec2 M $7ec3 M $7ec4 M -> $72 $77 $6e $0c }T
$3016 >PC $40 >S $a6 >A $cc >X $b7 >Y $6b >P $002c $cd >M $3016 $77 >M $3017 $2c >M $3018 $0f >M
T{ op PC S A X Y P -> $3018 $40 $a6 $cc $b7 $6b }T T{ $002c M $3016 M $3017 M $3018 M -> $4d $77 $2c $0f }T
$0dd6 >PC $f3 >S $94 >A $94 >X $91 >Y $ae >P $0033 $3d >M $0dd6 $77 >M $0dd7 $33 >M $0dd8 $0e >M
T{ op PC S A X Y P -> $0dd8 $f3 $94 $94 $91 $ae }T T{ $0033 M $0dd6 M $0dd7 M $0dd8 M -> $3d $77 $33 $0e }T
$4954 >PC $ed >S $a0 >A $a8 >X $17 >Y $a9 >P $00d8 $f5 >M $4954 $77 >M $4955 $d8 >M $4956 $89 >M
T{ op PC S A X Y P -> $4956 $ed $a0 $a8 $17 $a9 }T T{ $00d8 M $4954 M $4955 M $4956 M -> $75 $77 $d8 $89 }T
$b73a >PC $b9 >S $01 >A $b4 >X $d4 >Y $6f >P $0023 $27 >M $b73a $77 >M $b73b $23 >M $b73c $37 >M
T{ op PC S A X Y P -> $b73c $b9 $01 $b4 $d4 $6f }T T{ $0023 M $b73a M $b73b M $b73c M -> $27 $77 $23 $37 }T
$6e69 >PC $68 >S $67 >A $7a >X $cc >Y $64 >P $00dd $5c >M $6e69 $77 >M $6e6a $dd >M $6e6b $23 >M
T{ op PC S A X Y P -> $6e6b $68 $67 $7a $cc $64 }T T{ $00dd M $6e69 M $6e6a M $6e6b M -> $5c $77 $dd $23 }T
$cb7a >PC $88 >S $42 >A $16 >X $c1 >Y $24 >P $00ae $52 >M $cb7a $77 >M $cb7b $ae >M $cb7c $a5 >M
T{ op PC S A X Y P -> $cb7c $88 $42 $16 $c1 $24 }T T{ $00ae M $cb7a M $cb7b M $cb7c M -> $52 $77 $ae $a5 }T
$7d3f >PC $4d >S $c3 >A $84 >X $e3 >Y $65 >P $0080 $a5 >M $7d3f $77 >M $7d40 $80 >M $7d41 $6b >M
T{ op PC S A X Y P -> $7d41 $4d $c3 $84 $e3 $65 }T T{ $0080 M $7d3f M $7d40 M $7d41 M -> $25 $77 $80 $6b }T
$e6c0 >PC $6e >S $b4 >A $13 >X $ec >Y $66 >P $0071 $97 >M $e6c0 $77 >M $e6c1 $71 >M $e6c2 $33 >M
T{ op PC S A X Y P -> $e6c2 $6e $b4 $13 $ec $66 }T T{ $0071 M $e6c0 M $e6c1 M $e6c2 M -> $17 $77 $71 $33 }T
$b4c9 >PC $ce >S $21 >A $22 >X $6b >Y $af >P $0071 $6c >M $b4c9 $77 >M $b4ca $71 >M $b4cb $89 >M
T{ op PC S A X Y P -> $b4cb $ce $21 $22 $6b $af }T T{ $0071 M $b4c9 M $b4ca M $b4cb M -> $6c $77 $71 $89 }T
( 78 )
$8f19 >PC $32 >S $16 >A $76 >X $cc >Y $ab >P $8f19 $78 >M $8f1a $44 >M $8f1b $71 >M
T{ op PC S A X Y P -> $8f1a $32 $16 $76 $cc $af }T T{ $8f19 M $8f1a M $8f1b M -> $78 $44 $71 }T
$4856 >PC $2c >S $df >A $a4 >X $88 >Y $ab >P $4856 $78 >M $4857 $9f >M $4858 $2f >M
T{ op PC S A X Y P -> $4857 $2c $df $a4 $88 $af }T T{ $4856 M $4857 M $4858 M -> $78 $9f $2f }T
$8a7b >PC $ed >S $7f >A $4b >X $b2 >Y $2f >P $8a7b $78 >M $8a7c $7e >M $8a7d $7d >M
T{ op PC S A X Y P -> $8a7c $ed $7f $4b $b2 $2f }T T{ $8a7b M $8a7c M $8a7d M -> $78 $7e $7d }T
$97a8 >PC $ec >S $f2 >A $d9 >X $9f >Y $6a >P $97a8 $78 >M $97a9 $be >M $97aa $93 >M
T{ op PC S A X Y P -> $97a9 $ec $f2 $d9 $9f $6e }T T{ $97a8 M $97a9 M $97aa M -> $78 $be $93 }T
$0761 >PC $c1 >S $98 >A $f4 >X $9e >Y $6e >P $0761 $78 >M $0762 $29 >M $0763 $83 >M
T{ op PC S A X Y P -> $0762 $c1 $98 $f4 $9e $6e }T T{ $0761 M $0762 M $0763 M -> $78 $29 $83 }T
$6c44 >PC $9c >S $e7 >A $0e >X $d2 >Y $6c >P $6c44 $78 >M $6c45 $ca >M $6c46 $fa >M
T{ op PC S A X Y P -> $6c45 $9c $e7 $0e $d2 $6c }T T{ $6c44 M $6c45 M $6c46 M -> $78 $ca $fa }T
$6753 >PC $50 >S $aa >A $9a >X $43 >Y $69 >P $6753 $78 >M $6754 $a7 >M $6755 $f7 >M
T{ op PC S A X Y P -> $6754 $50 $aa $9a $43 $6d }T T{ $6753 M $6754 M $6755 M -> $78 $a7 $f7 }T
$2e91 >PC $b6 >S $62 >A $b8 >X $1f >Y $eb >P $2e91 $78 >M $2e92 $ab >M $2e93 $d3 >M
T{ op PC S A X Y P -> $2e92 $b6 $62 $b8 $1f $ef }T T{ $2e91 M $2e92 M $2e93 M -> $78 $ab $d3 }T
$24da >PC $41 >S $84 >A $aa >X $a9 >Y $28 >P $24da $78 >M $24db $22 >M $24dc $6e >M
T{ op PC S A X Y P -> $24db $41 $84 $aa $a9 $2c }T T{ $24da M $24db M $24dc M -> $78 $22 $6e }T
$1e44 >PC $03 >S $07 >A $c6 >X $19 >Y $27 >P $1e44 $78 >M $1e45 $36 >M $1e46 $71 >M
T{ op PC S A X Y P -> $1e45 $03 $07 $c6 $19 $27 }T T{ $1e44 M $1e45 M $1e46 M -> $78 $36 $71 }T
$1623 >PC $19 >S $9d >A $07 >X $da >Y $ec >P $1623 $78 >M $1624 $88 >M $1625 $64 >M
T{ op PC S A X Y P -> $1624 $19 $9d $07 $da $ec }T T{ $1623 M $1624 M $1625 M -> $78 $88 $64 }T
$a7e2 >PC $9f >S $8a >A $4d >X $b0 >Y $a2 >P $a7e2 $78 >M $a7e3 $62 >M $a7e4 $c0 >M
T{ op PC S A X Y P -> $a7e3 $9f $8a $4d $b0 $a6 }T T{ $a7e2 M $a7e3 M $a7e4 M -> $78 $62 $c0 }T
$ed96 >PC $cf >S $41 >A $67 >X $c6 >Y $64 >P $ed96 $78 >M $ed97 $9c >M $ed98 $19 >M
T{ op PC S A X Y P -> $ed97 $cf $41 $67 $c6 $64 }T T{ $ed96 M $ed97 M $ed98 M -> $78 $9c $19 }T
$aad4 >PC $e6 >S $33 >A $b2 >X $dd >Y $22 >P $aad4 $78 >M $aad5 $b5 >M $aad6 $50 >M
T{ op PC S A X Y P -> $aad5 $e6 $33 $b2 $dd $26 }T T{ $aad4 M $aad5 M $aad6 M -> $78 $b5 $50 }T
$d72d >PC $49 >S $60 >A $bd >X $ce >Y $e1 >P $d72d $78 >M $d72e $e0 >M $d72f $89 >M
T{ op PC S A X Y P -> $d72e $49 $60 $bd $ce $e5 }T T{ $d72d M $d72e M $d72f M -> $78 $e0 $89 }T
$e2aa >PC $6b >S $06 >A $42 >X $eb >Y $2d >P $e2aa $78 >M $e2ab $96 >M $e2ac $04 >M
T{ op PC S A X Y P -> $e2ab $6b $06 $42 $eb $2d }T T{ $e2aa M $e2ab M $e2ac M -> $78 $96 $04 }T
( 79 )
$5ed0 >PC $46 >S $8f >A $52 >X $fa >Y $64 >P $5ed0 $79 >M $5ed1 $5c >M $5ed2 $c3 >M $5ed3 $a4 >M $c456 $5b >M
T{ op PC S A X Y P -> $5ed3 $46 $ea $52 $fa $a4 }T T{ $5ed0 M $5ed1 M $5ed2 M $5ed3 M $c456 M -> $79 $5c $c3 $a4 $5b }T
$1aef >PC $5f >S $b7 >A $13 >X $0b >Y $a6 >P $1aef $79 >M $1af0 $6a >M $1af1 $7a >M $1af2 $b1 >M $7a75 $04 >M
T{ op PC S A X Y P -> $1af2 $5f $bb $13 $0b $a4 }T T{ $1aef M $1af0 M $1af1 M $1af2 M $7a75 M -> $79 $6a $7a $b1 $04 }T
$0840 >PC $89 >S $ba >A $42 >X $b3 >Y $65 >P $0840 $79 >M $0841 $d2 >M $0842 $e0 >M $0843 $be >M $e185 $1f >M
T{ op PC S A X Y P -> $0843 $89 $da $42 $b3 $a4 }T T{ $0840 M $0841 M $0842 M $0843 M $e185 M -> $79 $d2 $e0 $be $1f }T
$cd84 >PC $d5 >S $d8 >A $67 >X $c3 >Y $ee >P $41ad $44 >M $cd84 $79 >M $cd85 $ea >M $cd86 $40 >M $cd87 $0e >M
T{ op PC S A X Y P -> $cd87 $d5 $82 $67 $c3 $ad }T T{ $41ad M $cd84 M $cd85 M $cd86 M $cd87 M -> $44 $79 $ea $40 $0e }T
$66d5 >PC $86 >S $36 >A $63 >X $6d >Y $2e >P $66d5 $79 >M $66d6 $f3 >M $66d7 $72 >M $66d8 $eb >M $7360 $b8 >M
T{ op PC S A X Y P -> $66d8 $86 $54 $63 $6d $2d }T T{ $66d5 M $66d6 M $66d7 M $66d8 M $7360 M -> $79 $f3 $72 $eb $b8 }T
$1414 >PC $35 >S $e4 >A $a5 >X $30 >Y $e0 >P $1414 $79 >M $1415 $45 >M $1416 $6f >M $1417 $1b >M $6f75 $71 >M
T{ op PC S A X Y P -> $1417 $35 $55 $a5 $30 $21 }T T{ $1414 M $1415 M $1416 M $1417 M $6f75 M -> $79 $45 $6f $1b $71 }T
$a597 >PC $4f >S $34 >A $0c >X $63 >Y $e9 >P $966d $15 >M $a597 $79 >M $a598 $0a >M $a599 $96 >M $a59a $f9 >M
T{ op PC S A X Y P -> $a59a $4f $50 $0c $63 $28 }T T{ $966d M $a597 M $a598 M $a599 M $a59a M -> $15 $79 $0a $96 $f9 }T
$dfa5 >PC $7a >S $0c >A $ea >X $05 >Y $ee >P $6b65 $2c >M $dfa5 $79 >M $dfa6 $60 >M $dfa7 $6b >M $dfa8 $53 >M
T{ op PC S A X Y P -> $dfa8 $7a $3e $ea $05 $2c }T T{ $6b65 M $dfa5 M $dfa6 M $dfa7 M $dfa8 M -> $2c $79 $60 $6b $53 }T
$5f89 >PC $34 >S $c6 >A $a3 >X $31 >Y $ab >P $0a70 $26 >M $5f89 $79 >M $5f8a $3f >M $5f8b $0a >M $5f8c $57 >M
T{ op PC S A X Y P -> $5f8c $34 $53 $a3 $31 $29 }T T{ $0a70 M $5f89 M $5f8a M $5f8b M $5f8c M -> $26 $79 $3f $0a $57 }T
$ff37 >PC $85 >S $e8 >A $05 >X $81 >Y $e1 >P $ee2a $74 >M $ff37 $79 >M $ff38 $a9 >M $ff39 $ed >M $ff3a $c8 >M
T{ op PC S A X Y P -> $ff3a $85 $5d $05 $81 $21 }T T{ $ee2a M $ff37 M $ff38 M $ff39 M $ff3a M -> $74 $79 $a9 $ed $c8 }T
$a1ee >PC $ad >S $58 >A $8d >X $f5 >Y $ef >P $a1ee $79 >M $a1ef $56 >M $a1f0 $e5 >M $a1f1 $fa >M $e64b $52 >M
T{ op PC S A X Y P -> $a1f1 $ad $11 $8d $f5 $6d }T T{ $a1ee M $a1ef M $a1f0 M $a1f1 M $e64b M -> $79 $56 $e5 $fa $52 }T
$c5ce >PC $6a >S $18 >A $f5 >X $91 >Y $a8 >P $2872 $fb >M $c5ce $79 >M $c5cf $e1 >M $c5d0 $27 >M $c5d1 $f2 >M
T{ op PC S A X Y P -> $c5d1 $6a $79 $f5 $91 $29 }T T{ $2872 M $c5ce M $c5cf M $c5d0 M $c5d1 M -> $fb $79 $e1 $27 $f2 }T
$4b7c >PC $00 >S $c5 >A $d8 >X $ff >Y $ab >P $1af3 $f3 >M $4b7c $79 >M $4b7d $f4 >M $4b7e $19 >M $4b7f $c8 >M
T{ op PC S A X Y P -> $4b7f $00 $19 $d8 $ff $29 }T T{ $1af3 M $4b7c M $4b7d M $4b7e M $4b7f M -> $f3 $79 $f4 $19 $c8 }T
$c3df >PC $e4 >S $5d >A $fc >X $e4 >Y $af >P $c244 $bd >M $c3df $79 >M $c3e0 $60 >M $c3e1 $c1 >M $c3e2 $9c >M
T{ op PC S A X Y P -> $c3e2 $e4 $71 $fc $e4 $2d }T T{ $c244 M $c3df M $c3e0 M $c3e1 M $c3e2 M -> $bd $79 $60 $c1 $9c }T
$5c31 >PC $25 >S $ab >A $c9 >X $43 >Y $e4 >P $0275 $c2 >M $5c31 $79 >M $5c32 $32 >M $5c33 $02 >M $5c34 $c5 >M
T{ op PC S A X Y P -> $5c34 $25 $6d $c9 $43 $65 }T T{ $0275 M $5c31 M $5c32 M $5c33 M $5c34 M -> $c2 $79 $32 $02 $c5 }T
$ee0e >PC $a6 >S $21 >A $33 >X $7e >Y $a8 >P $966f $71 >M $ee0e $79 >M $ee0f $f1 >M $ee10 $95 >M $ee11 $40 >M
T{ op PC S A X Y P -> $ee11 $a6 $92 $33 $7e $e8 }T T{ $966f M $ee0e M $ee0f M $ee10 M $ee11 M -> $71 $79 $f1 $95 $40 }T
( 7a )
$6e87 >PC $fc >S $2d >A $1f >X $27 >Y $6f >P $01fc $7f >M $01fd $cd >M $6e87 $7a >M $6e88 $ce >M $6e89 $f1 >M
T{ op PC S A X Y P -> $6e88 $fd $2d $1f $cd $ed }T T{ $01fc M $01fd M $6e87 M $6e88 M $6e89 M -> $7f $cd $7a $ce $f1 }T
$f0ef >PC $34 >S $e1 >A $23 >X $89 >Y $af >P $0134 $64 >M $0135 $f5 >M $f0ef $7a >M $f0f0 $38 >M $f0f1 $0e >M
T{ op PC S A X Y P -> $f0f0 $35 $e1 $23 $f5 $ad }T T{ $0134 M $0135 M $f0ef M $f0f0 M $f0f1 M -> $64 $f5 $7a $38 $0e }T
$5d32 >PC $e6 >S $ee >A $83 >X $b5 >Y $e3 >P $01e6 $e5 >M $01e7 $92 >M $5d32 $7a >M $5d33 $05 >M $5d34 $c5 >M
T{ op PC S A X Y P -> $5d33 $e7 $ee $83 $92 $e1 }T T{ $01e6 M $01e7 M $5d32 M $5d33 M $5d34 M -> $e5 $92 $7a $05 $c5 }T
$90a0 >PC $05 >S $5b >A $d6 >X $d1 >Y $2a >P $0105 $ed >M $0106 $fa >M $90a0 $7a >M $90a1 $87 >M $90a2 $23 >M
T{ op PC S A X Y P -> $90a1 $06 $5b $d6 $fa $a8 }T T{ $0105 M $0106 M $90a0 M $90a1 M $90a2 M -> $ed $fa $7a $87 $23 }T
$233f >PC $d9 >S $a2 >A $49 >X $05 >Y $e8 >P $01d9 $88 >M $01da $85 >M $233f $7a >M $2340 $23 >M $2341 $95 >M
T{ op PC S A X Y P -> $2340 $da $a2 $49 $85 $e8 }T T{ $01d9 M $01da M $233f M $2340 M $2341 M -> $88 $85 $7a $23 $95 }T
$3094 >PC $cd >S $e5 >A $0d >X $50 >Y $2d >P $01cd $8c >M $01ce $41 >M $3094 $7a >M $3095 $3f >M $3096 $0c >M
T{ op PC S A X Y P -> $3095 $ce $e5 $0d $41 $2d }T T{ $01cd M $01ce M $3094 M $3095 M $3096 M -> $8c $41 $7a $3f $0c }T
$be5d >PC $16 >S $f2 >A $42 >X $ce >Y $2a >P $0116 $9c >M $0117 $9a >M $be5d $7a >M $be5e $81 >M $be5f $40 >M
T{ op PC S A X Y P -> $be5e $17 $f2 $42 $9a $a8 }T T{ $0116 M $0117 M $be5d M $be5e M $be5f M -> $9c $9a $7a $81 $40 }T
$e4f0 >PC $66 >S $99 >A $65 >X $73 >Y $23 >P $0166 $48 >M $0167 $87 >M $e4f0 $7a >M $e4f1 $1d >M $e4f2 $4b >M
T{ op PC S A X Y P -> $e4f1 $67 $99 $65 $87 $a1 }T T{ $0166 M $0167 M $e4f0 M $e4f1 M $e4f2 M -> $48 $87 $7a $1d $4b }T
$cda2 >PC $08 >S $60 >A $e5 >X $7d >Y $a4 >P $0108 $35 >M $0109 $c1 >M $cda2 $7a >M $cda3 $73 >M $cda4 $74 >M
T{ op PC S A X Y P -> $cda3 $09 $60 $e5 $c1 $a4 }T T{ $0108 M $0109 M $cda2 M $cda3 M $cda4 M -> $35 $c1 $7a $73 $74 }T
$5596 >PC $26 >S $d9 >A $c4 >X $26 >Y $6a >P $0126 $af >M $0127 $cf >M $5596 $7a >M $5597 $14 >M $5598 $a8 >M
T{ op PC S A X Y P -> $5597 $27 $d9 $c4 $cf $e8 }T T{ $0126 M $0127 M $5596 M $5597 M $5598 M -> $af $cf $7a $14 $a8 }T
$2525 >PC $83 >S $40 >A $0e >X $66 >Y $e0 >P $0183 $64 >M $0184 $2a >M $2525 $7a >M $2526 $ef >M $2527 $69 >M
T{ op PC S A X Y P -> $2526 $84 $40 $0e $2a $60 }T T{ $0183 M $0184 M $2525 M $2526 M $2527 M -> $64 $2a $7a $ef $69 }T
$e87c >PC $15 >S $75 >A $12 >X $28 >Y $2c >P $0115 $aa >M $0116 $eb >M $e87c $7a >M $e87d $a3 >M $e87e $e0 >M
T{ op PC S A X Y P -> $e87d $16 $75 $12 $eb $ac }T T{ $0115 M $0116 M $e87c M $e87d M $e87e M -> $aa $eb $7a $a3 $e0 }T
$9131 >PC $5b >S $ad >A $b1 >X $94 >Y $af >P $015b $10 >M $015c $bc >M $9131 $7a >M $9132 $24 >M $9133 $7d >M
T{ op PC S A X Y P -> $9132 $5c $ad $b1 $bc $ad }T T{ $015b M $015c M $9131 M $9132 M $9133 M -> $10 $bc $7a $24 $7d }T
$20e9 >PC $d4 >S $72 >A $0a >X $d0 >Y $ac >P $01d4 $4b >M $01d5 $61 >M $20e9 $7a >M $20ea $a0 >M $20eb $84 >M
T{ op PC S A X Y P -> $20ea $d5 $72 $0a $61 $2c }T T{ $01d4 M $01d5 M $20e9 M $20ea M $20eb M -> $4b $61 $7a $a0 $84 }T
$59b4 >PC $f2 >S $78 >A $84 >X $28 >Y $69 >P $01f2 $2b >M $01f3 $81 >M $59b4 $7a >M $59b5 $ff >M $59b6 $7b >M
T{ op PC S A X Y P -> $59b5 $f3 $78 $84 $81 $e9 }T T{ $01f2 M $01f3 M $59b4 M $59b5 M $59b6 M -> $2b $81 $7a $ff $7b }T
$d0f0 >PC $b7 >S $31 >A $aa >X $40 >Y $a0 >P $01b7 $22 >M $01b8 $fd >M $d0f0 $7a >M $d0f1 $cd >M $d0f2 $c9 >M
T{ op PC S A X Y P -> $d0f1 $b8 $31 $aa $fd $a0 }T T{ $01b7 M $01b8 M $d0f0 M $d0f1 M $d0f2 M -> $22 $fd $7a $cd $c9 }T
( 7b )
$a060 >PC $60 >S $93 >A $9d >X $51 >Y $24 >P $a060 $7b >M $a061 $13 >M $a062 $72 >M
T{ op PC S A X Y P -> $a061 $60 $93 $9d $51 $24 }T T{ $a060 M $a061 M $a062 M -> $7b $13 $72 }T
$235c >PC $43 >S $71 >A $de >X $5f >Y $2e >P $235c $7b >M $235d $01 >M $235e $51 >M
T{ op PC S A X Y P -> $235d $43 $71 $de $5f $2e }T T{ $235c M $235d M $235e M -> $7b $01 $51 }T
$5c28 >PC $da >S $24 >A $73 >X $e9 >Y $ec >P $5c28 $7b >M $5c29 $86 >M $5c2a $66 >M
T{ op PC S A X Y P -> $5c29 $da $24 $73 $e9 $ec }T T{ $5c28 M $5c29 M $5c2a M -> $7b $86 $66 }T
$3d6e >PC $31 >S $ed >A $49 >X $fa >Y $ee >P $3d6e $7b >M $3d6f $98 >M $3d70 $a8 >M
T{ op PC S A X Y P -> $3d6f $31 $ed $49 $fa $ee }T T{ $3d6e M $3d6f M $3d70 M -> $7b $98 $a8 }T
$9003 >PC $45 >S $22 >A $13 >X $ba >Y $ed >P $9003 $7b >M $9004 $69 >M $9005 $16 >M
T{ op PC S A X Y P -> $9004 $45 $22 $13 $ba $ed }T T{ $9003 M $9004 M $9005 M -> $7b $69 $16 }T
$d6ed >PC $b7 >S $c4 >A $69 >X $6f >Y $23 >P $d6ed $7b >M $d6ee $aa >M $d6ef $f3 >M
T{ op PC S A X Y P -> $d6ee $b7 $c4 $69 $6f $23 }T T{ $d6ed M $d6ee M $d6ef M -> $7b $aa $f3 }T
$a1cc >PC $5a >S $38 >A $73 >X $d3 >Y $6d >P $a1cc $7b >M $a1cd $eb >M $a1ce $5c >M
T{ op PC S A X Y P -> $a1cd $5a $38 $73 $d3 $6d }T T{ $a1cc M $a1cd M $a1ce M -> $7b $eb $5c }T
$e3f9 >PC $13 >S $10 >A $12 >X $d7 >Y $6e >P $e3f9 $7b >M $e3fa $68 >M $e3fb $42 >M
T{ op PC S A X Y P -> $e3fa $13 $10 $12 $d7 $6e }T T{ $e3f9 M $e3fa M $e3fb M -> $7b $68 $42 }T
$948d >PC $54 >S $72 >A $c2 >X $ff >Y $6e >P $948d $7b >M $948e $ca >M $948f $d2 >M
T{ op PC S A X Y P -> $948e $54 $72 $c2 $ff $6e }T T{ $948d M $948e M $948f M -> $7b $ca $d2 }T
$c001 >PC $00 >S $4e >A $2a >X $bb >Y $2d >P $c001 $7b >M $c002 $cc >M $c003 $0c >M
T{ op PC S A X Y P -> $c002 $00 $4e $2a $bb $2d }T T{ $c001 M $c002 M $c003 M -> $7b $cc $0c }T
$5d03 >PC $0c >S $5e >A $74 >X $d2 >Y $20 >P $5d03 $7b >M $5d04 $a2 >M $5d05 $0d >M
T{ op PC S A X Y P -> $5d04 $0c $5e $74 $d2 $20 }T T{ $5d03 M $5d04 M $5d05 M -> $7b $a2 $0d }T
$38d5 >PC $f1 >S $64 >A $20 >X $cf >Y $a7 >P $38d5 $7b >M $38d6 $25 >M $38d7 $58 >M
T{ op PC S A X Y P -> $38d6 $f1 $64 $20 $cf $a7 }T T{ $38d5 M $38d6 M $38d7 M -> $7b $25 $58 }T
$da1a >PC $2b >S $af >A $b4 >X $24 >Y $23 >P $da1a $7b >M $da1b $2e >M $da1c $d3 >M
T{ op PC S A X Y P -> $da1b $2b $af $b4 $24 $23 }T T{ $da1a M $da1b M $da1c M -> $7b $2e $d3 }T
$9a18 >PC $e9 >S $0d >A $45 >X $71 >Y $6e >P $9a18 $7b >M $9a19 $43 >M $9a1a $17 >M
T{ op PC S A X Y P -> $9a19 $e9 $0d $45 $71 $6e }T T{ $9a18 M $9a19 M $9a1a M -> $7b $43 $17 }T
$a5bd >PC $db >S $b1 >A $0a >X $de >Y $6f >P $a5bd $7b >M $a5be $5b >M $a5bf $0a >M
T{ op PC S A X Y P -> $a5be $db $b1 $0a $de $6f }T T{ $a5bd M $a5be M $a5bf M -> $7b $5b $0a }T
$0839 >PC $a1 >S $e1 >A $c7 >X $58 >Y $a7 >P $0839 $7b >M $083a $f4 >M $083b $12 >M
T{ op PC S A X Y P -> $083a $a1 $e1 $c7 $58 $a7 }T T{ $0839 M $083a M $083b M -> $7b $f4 $12 }T
( 7c )
$c53b >PC $44 >S $0c >A $5a >X $84 >Y $a0 >P $9cb5 $2e >M $9cb6 $a2 >M $a22e $69 >M $c53b $7c >M $c53c $5b >M $c53d $9c >M
T{ op PC S A X Y P -> $a22e $44 $0c $5a $84 $a0 }T T{ $9cb5 M $9cb6 M $a22e M $c53b M $c53c M $c53d M -> $2e $a2 $69 $7c $5b $9c }T
$1872 >PC $e1 >S $0f >A $6e >X $9a >Y $ae >P $1872 $7c >M $1873 $63 >M $1874 $9d >M $9dd1 $b5 >M $9dd2 $c7 >M $c7b5 $96 >M
T{ op PC S A X Y P -> $c7b5 $e1 $0f $6e $9a $ae }T T{ $1872 M $1873 M $1874 M $9dd1 M $9dd2 M $c7b5 M -> $7c $63 $9d $b5 $c7 $96 }T
$38a0 >PC $d4 >S $8a >A $b4 >X $12 >Y $20 >P $196f $e9 >M $38a0 $7c >M $38a1 $c9 >M $38a2 $57 >M $587d $6f >M $587e $19 >M
T{ op PC S A X Y P -> $196f $d4 $8a $b4 $12 $20 }T T{ $196f M $38a0 M $38a1 M $38a2 M $587d M $587e M -> $e9 $7c $c9 $57 $6f $19 }T
$c4a0 >PC $72 >S $a2 >A $04 >X $86 >Y $2a >P $409a $88 >M $409b $4e >M $4e88 $52 >M $c4a0 $7c >M $c4a1 $96 >M $c4a2 $40 >M
T{ op PC S A X Y P -> $4e88 $72 $a2 $04 $86 $2a }T T{ $409a M $409b M $4e88 M $c4a0 M $c4a1 M $c4a2 M -> $88 $4e $52 $7c $96 $40 }T
$971d >PC $30 >S $9c >A $cb >X $f7 >Y $a1 >P $1cf8 $a1 >M $1cf9 $86 >M $86a1 $b8 >M $971d $7c >M $971e $2d >M $971f $1c >M
T{ op PC S A X Y P -> $86a1 $30 $9c $cb $f7 $a1 }T T{ $1cf8 M $1cf9 M $86a1 M $971d M $971e M $971f M -> $a1 $86 $b8 $7c $2d $1c }T
$12fc >PC $44 >S $73 >A $c9 >X $c0 >Y $ab >P $12fc $7c >M $12fd $63 >M $12fe $1f >M $202c $84 >M $202d $a1 >M $a184 $e2 >M
T{ op PC S A X Y P -> $a184 $44 $73 $c9 $c0 $ab }T T{ $12fc M $12fd M $12fe M $202c M $202d M $a184 M -> $7c $63 $1f $84 $a1 $e2 }T
$7173 >PC $20 >S $72 >A $66 >X $24 >Y $e7 >P $449f $92 >M $7173 $7c >M $7174 $61 >M $7175 $f7 >M $f7c7 $9f >M $f7c8 $44 >M
T{ op PC S A X Y P -> $449f $20 $72 $66 $24 $e7 }T T{ $449f M $7173 M $7174 M $7175 M $f7c7 M $f7c8 M -> $92 $7c $61 $f7 $9f $44 }T
$345b >PC $9c >S $ef >A $36 >X $f7 >Y $2d >P $345b $7c >M $345c $ef >M $345d $bb >M $3ab1 $e1 >M $bc25 $b1 >M $bc26 $3a >M
T{ op PC S A X Y P -> $3ab1 $9c $ef $36 $f7 $2d }T T{ $345b M $345c M $345d M $3ab1 M $bc25 M $bc26 M -> $7c $ef $bb $e1 $b1 $3a }T
$a2a0 >PC $51 >S $52 >A $9b >X $1c >Y $aa >P $4278 $30 >M $62c9 $78 >M $62ca $42 >M $a2a0 $7c >M $a2a1 $2e >M $a2a2 $62 >M
T{ op PC S A X Y P -> $4278 $51 $52 $9b $1c $aa }T T{ $4278 M $62c9 M $62ca M $a2a0 M $a2a1 M $a2a2 M -> $30 $78 $42 $7c $2e $62 }T
$04fe >PC $d1 >S $92 >A $56 >X $7e >Y $e6 >P $04fe $7c >M $04ff $66 >M $0500 $d0 >M $4e53 $37 >M $d0bc $53 >M $d0bd $4e >M
T{ op PC S A X Y P -> $4e53 $d1 $92 $56 $7e $e6 }T T{ $04fe M $04ff M $0500 M $4e53 M $d0bc M $d0bd M -> $7c $66 $d0 $37 $53 $4e }T
$4cb1 >PC $6c >S $92 >A $34 >X $3a >Y $28 >P $22b4 $42 >M $22b5 $e4 >M $4cb1 $7c >M $4cb2 $80 >M $4cb3 $22 >M $e442 $0f >M
T{ op PC S A X Y P -> $e442 $6c $92 $34 $3a $28 }T T{ $22b4 M $22b5 M $4cb1 M $4cb2 M $4cb3 M $e442 M -> $42 $e4 $7c $80 $22 $0f }T
$4c68 >PC $51 >S $5a >A $0f >X $c3 >Y $26 >P $4c68 $7c >M $4c69 $6e >M $4c6a $d4 >M $d47d $21 >M $d47e $f0 >M $f021 $b6 >M
T{ op PC S A X Y P -> $f021 $51 $5a $0f $c3 $26 }T T{ $4c68 M $4c69 M $4c6a M $d47d M $d47e M $f021 M -> $7c $6e $d4 $21 $f0 $b6 }T
$b07e >PC $fe >S $cd >A $e9 >X $8e >Y $ea >P $7bae $ee >M $7baf $f3 >M $b07e $7c >M $b07f $c5 >M $b080 $7a >M $f3ee $12 >M
T{ op PC S A X Y P -> $f3ee $fe $cd $e9 $8e $ea }T T{ $7bae M $7baf M $b07e M $b07f M $b080 M $f3ee M -> $ee $f3 $7c $c5 $7a $12 }T
$119b >PC $d7 >S $98 >A $f2 >X $ec >Y $69 >P $119b $7c >M $119c $68 >M $119d $40 >M $415a $de >M $415b $c8 >M $c8de $69 >M
T{ op PC S A X Y P -> $c8de $d7 $98 $f2 $ec $69 }T T{ $119b M $119c M $119d M $415a M $415b M $c8de M -> $7c $68 $40 $de $c8 $69 }T
$7760 >PC $c2 >S $9e >A $68 >X $21 >Y $26 >P $04d6 $e3 >M $305b $d6 >M $305c $04 >M $7760 $7c >M $7761 $f3 >M $7762 $2f >M
T{ op PC S A X Y P -> $04d6 $c2 $9e $68 $21 $26 }T T{ $04d6 M $305b M $305c M $7760 M $7761 M $7762 M -> $e3 $d6 $04 $7c $f3 $2f }T
$858f >PC $cb >S $44 >A $74 >X $8b >Y $ab >P $4b0b $25 >M $858f $7c >M $8590 $e2 >M $8591 $89 >M $8a56 $0b >M $8a57 $4b >M
T{ op PC S A X Y P -> $4b0b $cb $44 $74 $8b $ab }T T{ $4b0b M $858f M $8590 M $8591 M $8a56 M $8a57 M -> $25 $7c $e2 $89 $0b $4b }T
( 7d )
$efbe >PC $46 >S $01 >A $67 >X $e6 >Y $e6 >P $11eb $0f >M $efbe $7d >M $efbf $84 >M $efc0 $11 >M $efc1 $13 >M
T{ op PC S A X Y P -> $efc1 $46 $10 $67 $e6 $24 }T T{ $11eb M $efbe M $efbf M $efc0 M $efc1 M -> $0f $7d $84 $11 $13 }T
$8205 >PC $22 >S $c7 >A $ab >X $e0 >Y $ad >P $8205 $7d >M $8206 $11 >M $8207 $cf >M $8208 $bd >M $cfbc $f1 >M
T{ op PC S A X Y P -> $8208 $22 $19 $ab $e0 $2d }T T{ $8205 M $8206 M $8207 M $8208 M $cfbc M -> $7d $11 $cf $bd $f1 }T
$0ba5 >PC $9a >S $dc >A $88 >X $b6 >Y $a4 >P $0ba5 $7d >M $0ba6 $18 >M $0ba7 $37 >M $0ba8 $78 >M $37a0 $2e >M
T{ op PC S A X Y P -> $0ba8 $9a $0a $88 $b6 $25 }T T{ $0ba5 M $0ba6 M $0ba7 M $0ba8 M $37a0 M -> $7d $18 $37 $78 $2e }T
$d067 >PC $d7 >S $72 >A $13 >X $ce >Y $6a >P $d067 $7d >M $d068 $92 >M $d069 $f2 >M $d06a $5b >M $f2a5 $27 >M
T{ op PC S A X Y P -> $d06a $d7 $99 $13 $ce $e8 }T T{ $d067 M $d068 M $d069 M $d06a M $f2a5 M -> $7d $92 $f2 $5b $27 }T
$1bc6 >PC $45 >S $1f >A $0f >X $28 >Y $22 >P $1bc6 $7d >M $1bc7 $ee >M $1bc8 $a8 >M $1bc9 $25 >M $a8fd $48 >M
T{ op PC S A X Y P -> $1bc9 $45 $67 $0f $28 $20 }T T{ $1bc6 M $1bc7 M $1bc8 M $1bc9 M $a8fd M -> $7d $ee $a8 $25 $48 }T
$8a1d >PC $cb >S $fa >A $25 >X $5b >Y $e4 >P $20d8 $3c >M $8a1d $7d >M $8a1e $b3 >M $8a1f $20 >M $8a20 $7f >M
T{ op PC S A X Y P -> $8a20 $cb $36 $25 $5b $25 }T T{ $20d8 M $8a1d M $8a1e M $8a1f M $8a20 M -> $3c $7d $b3 $20 $7f }T
$56ac >PC $e7 >S $6e >A $d7 >X $63 >Y $2b >P $56ac $7d >M $56ad $4e >M $56ae $b9 >M $56af $bb >M $ba25 $00 >M
T{ op PC S A X Y P -> $56af $e7 $75 $d7 $63 $28 }T T{ $56ac M $56ad M $56ae M $56af M $ba25 M -> $7d $4e $b9 $bb $00 }T
$43a5 >PC $af >S $51 >A $0f >X $3a >Y $ae >P $43a5 $7d >M $43a6 $0a >M $43a7 $e1 >M $43a8 $c8 >M $e119 $f9 >M
T{ op PC S A X Y P -> $43a8 $af $b0 $0f $3a $ad }T T{ $43a5 M $43a6 M $43a7 M $43a8 M $e119 M -> $7d $0a $e1 $c8 $f9 }T
$182a >PC $20 >S $56 >A $32 >X $55 >Y $a4 >P $182a $7d >M $182b $d9 >M $182c $3b >M $182d $4b >M $3c0b $7e >M
T{ op PC S A X Y P -> $182d $20 $d4 $32 $55 $e4 }T T{ $182a M $182b M $182c M $182d M $3c0b M -> $7d $d9 $3b $4b $7e }T
$57a5 >PC $31 >S $7d >A $48 >X $16 >Y $2e >P $57a5 $7d >M $57a6 $01 >M $57a7 $9a >M $57a8 $d0 >M $9a49 $5f >M
T{ op PC S A X Y P -> $57a8 $31 $32 $48 $16 $6d }T T{ $57a5 M $57a6 M $57a7 M $57a8 M $9a49 M -> $7d $01 $9a $d0 $5f }T
$8068 >PC $c5 >S $f8 >A $89 >X $f1 >Y $66 >P $78b3 $ca >M $8068 $7d >M $8069 $2a >M $806a $78 >M $806b $39 >M
T{ op PC S A X Y P -> $806b $c5 $c2 $89 $f1 $a5 }T T{ $78b3 M $8068 M $8069 M $806a M $806b M -> $ca $7d $2a $78 $39 }T
$98c9 >PC $dc >S $4e >A $d0 >X $c9 >Y $2f >P $0478 $b5 >M $98c9 $7d >M $98ca $a8 >M $98cb $03 >M $98cc $a9 >M
T{ op PC S A X Y P -> $98cc $dc $6a $d0 $c9 $2d }T T{ $0478 M $98c9 M $98ca M $98cb M $98cc M -> $b5 $7d $a8 $03 $a9 }T
$6b40 >PC $62 >S $5b >A $93 >X $bd >Y $e4 >P $6b40 $7d >M $6b41 $98 >M $6b42 $de >M $6b43 $27 >M $df2b $99 >M
T{ op PC S A X Y P -> $6b43 $62 $f4 $93 $bd $a4 }T T{ $6b40 M $6b41 M $6b42 M $6b43 M $df2b M -> $7d $98 $de $27 $99 }T
$6c76 >PC $9c >S $b7 >A $23 >X $d2 >Y $eb >P $4c09 $1f >M $6c76 $7d >M $6c77 $e6 >M $6c78 $4b >M $6c79 $e1 >M
T{ op PC S A X Y P -> $6c79 $9c $3d $23 $d2 $29 }T T{ $4c09 M $6c76 M $6c77 M $6c78 M $6c79 M -> $1f $7d $e6 $4b $e1 }T
$69cd >PC $ef >S $47 >A $0f >X $6d >Y $6c >P $59b6 $80 >M $69cd $7d >M $69ce $a7 >M $69cf $59 >M $69d0 $60 >M
T{ op PC S A X Y P -> $69d0 $ef $27 $0f $6d $2d }T T{ $59b6 M $69cd M $69ce M $69cf M $69d0 M -> $80 $7d $a7 $59 $60 }T
$d0f4 >PC $6d >S $15 >A $21 >X $a8 >Y $a0 >P $759e $a6 >M $d0f4 $7d >M $d0f5 $7d >M $d0f6 $75 >M $d0f7 $41 >M
T{ op PC S A X Y P -> $d0f7 $6d $bb $21 $a8 $a0 }T T{ $759e M $d0f4 M $d0f5 M $d0f6 M $d0f7 M -> $a6 $7d $7d $75 $41 }T
( 7e )
$afc1 >PC $66 >S $9e >A $0a >X $ee >Y $a3 >P $3760 $05 >M $afc1 $7e >M $afc2 $56 >M $afc3 $37 >M $afc4 $6b >M
T{ op PC S A X Y P -> $afc4 $66 $9e $0a $ee $a1 }T T{ $3760 M $afc1 M $afc2 M $afc3 M $afc4 M -> $82 $7e $56 $37 $6b }T
$24c3 >PC $71 >S $6f >A $aa >X $d3 >Y $61 >P $1642 $13 >M $24c3 $7e >M $24c4 $98 >M $24c5 $15 >M $24c6 $80 >M
T{ op PC S A X Y P -> $24c6 $71 $6f $aa $d3 $e1 }T T{ $1642 M $24c3 M $24c4 M $24c5 M $24c6 M -> $89 $7e $98 $15 $80 }T
$f883 >PC $fb >S $bf >A $29 >X $75 >Y $6f >P $198a $d0 >M $f883 $7e >M $f884 $61 >M $f885 $19 >M $f886 $44 >M
T{ op PC S A X Y P -> $f886 $fb $bf $29 $75 $ec }T T{ $198a M $f883 M $f884 M $f885 M $f886 M -> $e8 $7e $61 $19 $44 }T
$c8f6 >PC $89 >S $0f >A $f0 >X $8d >Y $a3 >P $8896 $e1 >M $c8f6 $7e >M $c8f7 $a6 >M $c8f8 $87 >M $c8f9 $9c >M
T{ op PC S A X Y P -> $c8f9 $89 $0f $f0 $8d $a1 }T T{ $8896 M $c8f6 M $c8f7 M $c8f8 M $c8f9 M -> $f0 $7e $a6 $87 $9c }T
$07c4 >PC $12 >S $4e >A $1d >X $37 >Y $65 >P $07c4 $7e >M $07c5 $bb >M $07c6 $ef >M $07c7 $3d >M $efd8 $fb >M
T{ op PC S A X Y P -> $07c7 $12 $4e $1d $37 $e5 }T T{ $07c4 M $07c5 M $07c6 M $07c7 M $efd8 M -> $7e $bb $ef $3d $fd }T
$4b22 >PC $29 >S $54 >A $2e >X $d4 >Y $e7 >P $4b22 $7e >M $4b23 $a1 >M $4b24 $4d >M $4b25 $d1 >M $4dcf $ec >M
T{ op PC S A X Y P -> $4b25 $29 $54 $2e $d4 $e4 }T T{ $4b22 M $4b23 M $4b24 M $4b25 M $4dcf M -> $7e $a1 $4d $d1 $f6 }T
$e284 >PC $b5 >S $ab >A $2a >X $b9 >Y $25 >P $a691 $bd >M $e284 $7e >M $e285 $67 >M $e286 $a6 >M $e287 $57 >M
T{ op PC S A X Y P -> $e287 $b5 $ab $2a $b9 $a5 }T T{ $a691 M $e284 M $e285 M $e286 M $e287 M -> $de $7e $67 $a6 $57 }T
$e66e >PC $86 >S $b2 >A $0d >X $4e >Y $ea >P $b6fa $bf >M $e66e $7e >M $e66f $ed >M $e670 $b6 >M $e671 $a7 >M
T{ op PC S A X Y P -> $e671 $86 $b2 $0d $4e $69 }T T{ $b6fa M $e66e M $e66f M $e670 M $e671 M -> $5f $7e $ed $b6 $a7 }T
$141d >PC $84 >S $79 >A $0a >X $90 >Y $af >P $141d $7e >M $141e $ee >M $141f $c4 >M $1420 $37 >M $c4f8 $86 >M
T{ op PC S A X Y P -> $1420 $84 $79 $0a $90 $ac }T T{ $141d M $141e M $141f M $1420 M $c4f8 M -> $7e $ee $c4 $37 $c3 }T
$9a01 >PC $1a >S $41 >A $cb >X $51 >Y $2c >P $95b4 $69 >M $9a01 $7e >M $9a02 $e9 >M $9a03 $94 >M $9a04 $d2 >M
T{ op PC S A X Y P -> $9a04 $1a $41 $cb $51 $2d }T T{ $95b4 M $9a01 M $9a02 M $9a03 M $9a04 M -> $34 $7e $e9 $94 $d2 }T
$9808 >PC $ca >S $6a >A $b9 >X $96 >Y $2c >P $1b58 $99 >M $9808 $7e >M $9809 $9f >M $980a $1a >M $980b $2d >M
T{ op PC S A X Y P -> $980b $ca $6a $b9 $96 $2d }T T{ $1b58 M $9808 M $9809 M $980a M $980b M -> $4c $7e $9f $1a $2d }T
$ffef >PC $16 >S $7f >A $69 >X $24 >Y $6f >P $c631 $a1 >M $ffef $7e >M $fff0 $c8 >M $fff1 $c5 >M $fff2 $a5 >M
T{ op PC S A X Y P -> $fff2 $16 $7f $69 $24 $ed }T T{ $c631 M $ffef M $fff0 M $fff1 M $fff2 M -> $d0 $7e $c8 $c5 $a5 }T
$37f6 >PC $41 >S $a1 >A $aa >X $2d >Y $2d >P $37f6 $7e >M $37f7 $54 >M $37f8 $47 >M $37f9 $9a >M $47fe $e9 >M
T{ op PC S A X Y P -> $37f9 $41 $a1 $aa $2d $ad }T T{ $37f6 M $37f7 M $37f8 M $37f9 M $47fe M -> $7e $54 $47 $9a $f4 }T
$5353 >PC $ba >S $58 >A $bb >X $80 >Y $a6 >P $4d6b $a0 >M $5353 $7e >M $5354 $b0 >M $5355 $4c >M $5356 $f4 >M
T{ op PC S A X Y P -> $5356 $ba $58 $bb $80 $24 }T T{ $4d6b M $5353 M $5354 M $5355 M $5356 M -> $50 $7e $b0 $4c $f4 }T
$4f57 >PC $e5 >S $c7 >A $0e >X $be >Y $6f >P $4f57 $7e >M $4f58 $1c >M $4f59 $74 >M $4f5a $9a >M $742a $a8 >M
T{ op PC S A X Y P -> $4f5a $e5 $c7 $0e $be $ec }T T{ $4f57 M $4f58 M $4f59 M $4f5a M $742a M -> $7e $1c $74 $9a $d4 }T
$cf4a >PC $e3 >S $6f >A $31 >X $71 >Y $ac >P $01dc $91 >M $cf4a $7e >M $cf4b $ab >M $cf4c $01 >M $cf4d $5a >M
T{ op PC S A X Y P -> $cf4d $e3 $6f $31 $71 $2d }T T{ $01dc M $cf4a M $cf4b M $cf4c M $cf4d M -> $48 $7e $ab $01 $5a }T
( 7f )
$eaf7 >PC $d6 >S $d9 >A $58 >X $be >Y $65 >P $005d $f2 >M $ea17 $97 >M $eaf7 $7f >M $eaf8 $5d >M $eaf9 $1d >M $eafa $66 >M
T{ op PC S A X Y P -> $eafa $d6 $d9 $58 $be $65 }T T{ $005d M $ea17 M $eaf7 M $eaf8 M $eaf9 M $eafa M -> $f2 $97 $7f $5d $1d $66 }T
$c772 >PC $19 >S $25 >A $06 >X $85 >Y $6a >P $004f $f9 >M $c772 $7f >M $c773 $4f >M $c774 $6f >M $c775 $ed >M $c7e4 $a6 >M
T{ op PC S A X Y P -> $c775 $19 $25 $06 $85 $6a }T T{ $004f M $c772 M $c773 M $c774 M $c775 M $c7e4 M -> $f9 $7f $4f $6f $ed $a6 }T
$7c9a >PC $27 >S $78 >A $7c >X $94 >Y $e5 >P $00b2 $d1 >M $7c9a $7f >M $7c9b $b2 >M $7c9c $21 >M $7c9d $02 >M $7cbe $44 >M
T{ op PC S A X Y P -> $7c9d $27 $78 $7c $94 $e5 }T T{ $00b2 M $7c9a M $7c9b M $7c9c M $7c9d M $7cbe M -> $d1 $7f $b2 $21 $02 $44 }T
$8243 >PC $f5 >S $d0 >A $88 >X $da >Y $2f >P $0096 $65 >M $8202 $83 >M $8243 $7f >M $8244 $96 >M $8245 $bc >M
T{ op PC S A X Y P -> $8202 $f5 $d0 $88 $da $2f }T T{ $0096 M $8202 M $8243 M $8244 M $8245 M -> $65 $83 $7f $96 $bc }T
$8bd8 >PC $45 >S $f6 >A $ee >X $f7 >Y $ac >P $007a $69 >M $8b75 $dc >M $8bd8 $7f >M $8bd9 $7a >M $8bda $9a >M
T{ op PC S A X Y P -> $8b75 $45 $f6 $ee $f7 $ac }T T{ $007a M $8b75 M $8bd8 M $8bd9 M $8bda M -> $69 $dc $7f $7a $9a }T
$4ca3 >PC $c7 >S $8a >A $28 >X $13 >Y $23 >P $0041 $ef >M $4ca3 $7f >M $4ca4 $41 >M $4ca5 $37 >M $4ca6 $2a >M $4cdd $e0 >M
T{ op PC S A X Y P -> $4ca6 $c7 $8a $28 $13 $23 }T T{ $0041 M $4ca3 M $4ca4 M $4ca5 M $4ca6 M $4cdd M -> $ef $7f $41 $37 $2a $e0 }T
$52ee >PC $26 >S $ea >A $78 >X $44 >Y $65 >P $0043 $83 >M $5218 $c2 >M $52ee $7f >M $52ef $43 >M $52f0 $27 >M $52f1 $d5 >M
T{ op PC S A X Y P -> $52f1 $26 $ea $78 $44 $65 }T T{ $0043 M $5218 M $52ee M $52ef M $52f0 M $52f1 M -> $83 $c2 $7f $43 $27 $d5 }T
$2692 >PC $ea >S $e0 >A $c2 >X $82 >Y $28 >P $00d8 $13 >M $264a $b6 >M $2692 $7f >M $2693 $d8 >M $2694 $b5 >M
T{ op PC S A X Y P -> $264a $ea $e0 $c2 $82 $28 }T T{ $00d8 M $264a M $2692 M $2693 M $2694 M -> $13 $b6 $7f $d8 $b5 }T
$f51a >PC $84 >S $87 >A $5e >X $ca >Y $a7 >P $0097 $07 >M $f515 $90 >M $f51a $7f >M $f51b $97 >M $f51c $f8 >M
T{ op PC S A X Y P -> $f515 $84 $87 $5e $ca $a7 }T T{ $0097 M $f515 M $f51a M $f51b M $f51c M -> $07 $90 $7f $97 $f8 }T
$415a >PC $45 >S $97 >A $6c >X $41 >Y $27 >P $0062 $e6 >M $415a $7f >M $415b $62 >M $415c $4b >M $415d $42 >M $41a8 $3a >M
T{ op PC S A X Y P -> $415d $45 $97 $6c $41 $27 }T T{ $0062 M $415a M $415b M $415c M $415d M $41a8 M -> $e6 $7f $62 $4b $42 $3a }T
$7735 >PC $ab >S $5a >A $db >X $92 >Y $21 >P $0078 $1f >M $7735 $7f >M $7736 $78 >M $7737 $6a >M $77a2 $bb >M
T{ op PC S A X Y P -> $77a2 $ab $5a $db $92 $21 }T T{ $0078 M $7735 M $7736 M $7737 M $77a2 M -> $1f $7f $78 $6a $bb }T
$0ce6 >PC $2c >S $ef >A $57 >X $a9 >Y $67 >P $0031 $da >M $0cd2 $bf >M $0ce6 $7f >M $0ce7 $31 >M $0ce8 $e9 >M $0ce9 $bc >M
T{ op PC S A X Y P -> $0ce9 $2c $ef $57 $a9 $67 }T T{ $0031 M $0cd2 M $0ce6 M $0ce7 M $0ce8 M $0ce9 M -> $da $bf $7f $31 $e9 $bc }T
$b6ec >PC $28 >S $03 >A $18 >X $af >Y $a7 >P $00a6 $7d >M $b63f $1e >M $b6ec $7f >M $b6ed $a6 >M $b6ee $50 >M $b73f $16 >M
T{ op PC S A X Y P -> $b73f $28 $03 $18 $af $a7 }T T{ $00a6 M $b63f M $b6ec M $b6ed M $b6ee M $b73f M -> $7d $1e $7f $a6 $50 $16 }T
$baa6 >PC $26 >S $62 >A $ec >X $1d >Y $6f >P $0022 $3e >M $ba1a $c3 >M $baa6 $7f >M $baa7 $22 >M $baa8 $71 >M $bb1a $9c >M
T{ op PC S A X Y P -> $bb1a $26 $62 $ec $1d $6f }T T{ $0022 M $ba1a M $baa6 M $baa7 M $baa8 M $bb1a M -> $3e $c3 $7f $22 $71 $9c }T
$af52 >PC $34 >S $b2 >A $01 >X $b8 >Y $20 >P $00d7 $48 >M $af16 $87 >M $af52 $7f >M $af53 $d7 >M $af54 $c1 >M
T{ op PC S A X Y P -> $af16 $34 $b2 $01 $b8 $20 }T T{ $00d7 M $af16 M $af52 M $af53 M $af54 M -> $48 $87 $7f $d7 $c1 }T
$2ee5 >PC $fc >S $d5 >A $c3 >X $12 >Y $68 >P $004a $8b >M $2e6c $c6 >M $2ee5 $7f >M $2ee6 $4a >M $2ee7 $84 >M $2ee8 $b0 >M
T{ op PC S A X Y P -> $2ee8 $fc $d5 $c3 $12 $68 }T T{ $004a M $2e6c M $2ee5 M $2ee6 M $2ee7 M $2ee8 M -> $8b $c6 $7f $4a $84 $b0 }T
( 80 )
$654f >PC $f5 >S $c1 >A $58 >X $5d >Y $2f >P $64fa $c4 >M $654f $80 >M $6550 $a9 >M $6551 $62 >M $65fa $3d >M
T{ op PC S A X Y P -> $64fa $f5 $c1 $58 $5d $2f }T T{ $64fa M $654f M $6550 M $6551 M $65fa M -> $c4 $80 $a9 $62 $3d }T
$85f6 >PC $e4 >S $02 >A $f3 >X $cf >Y $6a >P $8551 $21 >M $85f6 $80 >M $85f7 $59 >M $85f8 $e8 >M $8651 $b4 >M
T{ op PC S A X Y P -> $8651 $e4 $02 $f3 $cf $6a }T T{ $8551 M $85f6 M $85f7 M $85f8 M $8651 M -> $21 $80 $59 $e8 $b4 }T
$8494 >PC $ab >S $c6 >A $04 >X $87 >Y $64 >P $8462 $e0 >M $8494 $80 >M $8495 $cc >M $8496 $ce >M
T{ op PC S A X Y P -> $8462 $ab $c6 $04 $87 $64 }T T{ $8462 M $8494 M $8495 M $8496 M -> $e0 $80 $cc $ce }T
$a057 >PC $13 >S $cb >A $7f >X $bf >Y $e4 >P $a057 $80 >M $a058 $11 >M $a059 $c0 >M $a06a $26 >M
T{ op PC S A X Y P -> $a06a $13 $cb $7f $bf $e4 }T T{ $a057 M $a058 M $a059 M $a06a M -> $80 $11 $c0 $26 }T
$cfaa >PC $60 >S $91 >A $58 >X $8d >Y $ec >P $cfaa $80 >M $cfab $49 >M $cfac $4a >M $cff5 $5f >M
T{ op PC S A X Y P -> $cff5 $60 $91 $58 $8d $ec }T T{ $cfaa M $cfab M $cfac M $cff5 M -> $80 $49 $4a $5f }T
$2d1a >PC $70 >S $cc >A $6c >X $72 >Y $2a >P $2ce9 $6b >M $2d1a $80 >M $2d1b $cd >M $2d1c $2a >M $2de9 $4b >M
T{ op PC S A X Y P -> $2ce9 $70 $cc $6c $72 $2a }T T{ $2ce9 M $2d1a M $2d1b M $2d1c M $2de9 M -> $6b $80 $cd $2a $4b }T
$5186 >PC $e1 >S $f5 >A $cd >X $2f >Y $e3 >P $5161 $df >M $5186 $80 >M $5187 $d9 >M $5188 $d5 >M
T{ op PC S A X Y P -> $5161 $e1 $f5 $cd $2f $e3 }T T{ $5161 M $5186 M $5187 M $5188 M -> $df $80 $d9 $d5 }T
$c3a9 >PC $eb >S $4d >A $b9 >X $73 >Y $2c >P $c31e $56 >M $c3a9 $80 >M $c3aa $73 >M $c3ab $c0 >M $c41e $11 >M
T{ op PC S A X Y P -> $c41e $eb $4d $b9 $73 $2c }T T{ $c31e M $c3a9 M $c3aa M $c3ab M $c41e M -> $56 $80 $73 $c0 $11 }T
$659a >PC $e0 >S $e6 >A $54 >X $a8 >Y $6b >P $6527 $4f >M $659a $80 >M $659b $8b >M $659c $e3 >M
T{ op PC S A X Y P -> $6527 $e0 $e6 $54 $a8 $6b }T T{ $6527 M $659a M $659b M $659c M -> $4f $80 $8b $e3 }T
$812b >PC $c2 >S $39 >A $73 >X $32 >Y $27 >P $80d9 $c9 >M $812b $80 >M $812c $ac >M $812d $8d >M $81d9 $78 >M
T{ op PC S A X Y P -> $80d9 $c2 $39 $73 $32 $27 }T T{ $80d9 M $812b M $812c M $812d M $81d9 M -> $c9 $80 $ac $8d $78 }T
$bed2 >PC $20 >S $92 >A $f0 >X $ab >Y $2e >P $be87 $72 >M $bed2 $80 >M $bed3 $b3 >M $bed4 $f1 >M
T{ op PC S A X Y P -> $be87 $20 $92 $f0 $ab $2e }T T{ $be87 M $bed2 M $bed3 M $bed4 M -> $72 $80 $b3 $f1 }T
$2cc9 >PC $57 >S $ed >A $e6 >X $59 >Y $60 >P $2cc9 $80 >M $2cca $27 >M $2ccb $f1 >M $2cf2 $a0 >M
T{ op PC S A X Y P -> $2cf2 $57 $ed $e6 $59 $60 }T T{ $2cc9 M $2cca M $2ccb M $2cf2 M -> $80 $27 $f1 $a0 }T
$3a98 >PC $ac >S $0b >A $fb >X $d6 >Y $ac >P $3a98 $80 >M $3a99 $3d >M $3a9a $51 >M $3ad7 $b4 >M
T{ op PC S A X Y P -> $3ad7 $ac $0b $fb $d6 $ac }T T{ $3a98 M $3a99 M $3a9a M $3ad7 M -> $80 $3d $51 $b4 }T
$70bf >PC $b7 >S $63 >A $d6 >X $e9 >Y $a7 >P $7037 $b5 >M $70bf $80 >M $70c0 $76 >M $70c1 $a0 >M $7137 $63 >M
T{ op PC S A X Y P -> $7137 $b7 $63 $d6 $e9 $a7 }T T{ $7037 M $70bf M $70c0 M $70c1 M $7137 M -> $b5 $80 $76 $a0 $63 }T
$3a46 >PC $bd >S $15 >A $76 >X $e1 >Y $62 >P $3a1a $8a >M $3a46 $80 >M $3a47 $d2 >M $3a48 $ec >M
T{ op PC S A X Y P -> $3a1a $bd $15 $76 $e1 $62 }T T{ $3a1a M $3a46 M $3a47 M $3a48 M -> $8a $80 $d2 $ec }T
$b75c >PC $17 >S $73 >A $d0 >X $70 >Y $2d >P $b72d $41 >M $b75c $80 >M $b75d $cf >M $b75e $ce >M
T{ op PC S A X Y P -> $b72d $17 $73 $d0 $70 $2d }T T{ $b72d M $b75c M $b75d M $b75e M -> $41 $80 $cf $ce }T
( 81 )
$ba3e >PC $b2 >S $99 >A $de >X $a2 >Y $60 >P $0065 $82 >M $0066 $b7 >M $0087 $ec >M $ba3e $81 >M $ba3f $87 >M $ba40 $c5 >M
T{ op PC S A X Y P -> $ba40 $b2 $99 $de $a2 $60 }T T{ $0065 M $0066 M $0087 M $b782 M $ba3e M $ba3f M $ba40 M -> $82 $b7 $ec $99 $81 $87 $c5 }T
$c22d >PC $62 >S $b4 >A $4e >X $b1 >Y $6d >P $00a2 $08 >M $00f0 $28 >M $00f1 $09 >M $c22d $81 >M $c22e $a2 >M $c22f $e2 >M
T{ op PC S A X Y P -> $c22f $62 $b4 $4e $b1 $6d }T T{ $00a2 M $00f0 M $00f1 M $0928 M $c22d M $c22e M $c22f M -> $08 $28 $09 $b4 $81 $a2 $e2 }T
$904b >PC $62 >S $9d >A $03 >X $b5 >Y $69 >P $0027 $4d >M $002a $56 >M $002b $7b >M $904b $81 >M $904c $27 >M $904d $23 >M
T{ op PC S A X Y P -> $904d $62 $9d $03 $b5 $69 }T T{ $0027 M $002a M $002b M $7b56 M $904b M $904c M $904d M -> $4d $56 $7b $9d $81 $27 $23 }T
$abea >PC $9b >S $f1 >A $1c >X $f0 >Y $63 >P $0011 $a5 >M $002d $df >M $002e $7b >M $abea $81 >M $abeb $11 >M $abec $44 >M
T{ op PC S A X Y P -> $abec $9b $f1 $1c $f0 $63 }T T{ $0011 M $002d M $002e M $7bdf M $abea M $abeb M $abec M -> $a5 $df $7b $f1 $81 $11 $44 }T
$164a >PC $fd >S $50 >A $9b >X $3d >Y $69 >P $000d $61 >M $000e $70 >M $0072 $5b >M $164a $81 >M $164b $72 >M $164c $43 >M
T{ op PC S A X Y P -> $164c $fd $50 $9b $3d $69 }T T{ $000d M $000e M $0072 M $164a M $164b M $164c M $7061 M -> $61 $70 $5b $81 $72 $43 $50 }T
$9026 >PC $c6 >S $c1 >A $8b >X $d7 >Y $6c >P $0073 $4f >M $00fe $20 >M $00ff $9d >M $9026 $81 >M $9027 $73 >M $9028 $57 >M
T{ op PC S A X Y P -> $9028 $c6 $c1 $8b $d7 $6c }T T{ $0073 M $00fe M $00ff M $9026 M $9027 M $9028 M $9d20 M -> $4f $20 $9d $81 $73 $57 $c1 }T
$f217 >PC $e7 >S $2c >A $d4 >X $82 >Y $ec >P $009e $4c >M $009f $4e >M $00ca $16 >M $f217 $81 >M $f218 $ca >M $f219 $ce >M
T{ op PC S A X Y P -> $f219 $e7 $2c $d4 $82 $ec }T T{ $009e M $009f M $00ca M $4e4c M $f217 M $f218 M $f219 M -> $4c $4e $16 $2c $81 $ca $ce }T
$4dd1 >PC $b4 >S $f1 >A $d6 >X $86 >Y $21 >P $0081 $4e >M $0082 $75 >M $00ab $36 >M $4dd1 $81 >M $4dd2 $ab >M $4dd3 $d0 >M
T{ op PC S A X Y P -> $4dd3 $b4 $f1 $d6 $86 $21 }T T{ $0081 M $0082 M $00ab M $4dd1 M $4dd2 M $4dd3 M $754e M -> $4e $75 $36 $81 $ab $d0 $f1 }T
$3954 >PC $f2 >S $40 >A $40 >X $bc >Y $21 >P $002f $62 >M $006f $13 >M $0070 $4e >M $3954 $81 >M $3955 $2f >M $3956 $bd >M
T{ op PC S A X Y P -> $3956 $f2 $40 $40 $bc $21 }T T{ $002f M $006f M $0070 M $3954 M $3955 M $3956 M $4e13 M -> $62 $13 $4e $81 $2f $bd $40 }T
$e238 >PC $56 >S $1d >A $bf >X $98 >Y $2c >P $0009 $06 >M $000a $38 >M $004a $07 >M $e238 $81 >M $e239 $4a >M $e23a $7d >M
T{ op PC S A X Y P -> $e23a $56 $1d $bf $98 $2c }T T{ $0009 M $000a M $004a M $3806 M $e238 M $e239 M $e23a M -> $06 $38 $07 $1d $81 $4a $7d }T
$f4ff >PC $c0 >S $e0 >A $a2 >X $b7 >Y $a2 >P $002d $8f >M $00cf $0c >M $00d0 $a0 >M $f4ff $81 >M $f500 $2d >M $f501 $c0 >M
T{ op PC S A X Y P -> $f501 $c0 $e0 $a2 $b7 $a2 }T T{ $002d M $00cf M $00d0 M $a00c M $f4ff M $f500 M $f501 M -> $8f $0c $a0 $e0 $81 $2d $c0 }T
$1476 >PC $7e >S $ff >A $37 >X $03 >Y $ab >P $004f $16 >M $0086 $93 >M $0087 $cf >M $1476 $81 >M $1477 $4f >M $1478 $98 >M
T{ op PC S A X Y P -> $1478 $7e $ff $37 $03 $ab }T T{ $004f M $0086 M $0087 M $1476 M $1477 M $1478 M $cf93 M -> $16 $93 $cf $81 $4f $98 $ff }T
$cc38 >PC $8b >S $a6 >A $76 >X $a7 >Y $af >P $005c $8a >M $00d2 $76 >M $00d3 $00 >M $cc38 $81 >M $cc39 $5c >M $cc3a $b5 >M
T{ op PC S A X Y P -> $cc3a $8b $a6 $76 $a7 $af }T T{ $005c M $0076 M $00d2 M $00d3 M $cc38 M $cc39 M $cc3a M -> $8a $a6 $76 $00 $81 $5c $b5 }T
$1a02 >PC $91 >S $80 >A $36 >X $33 >Y $27 >P $0054 $c2 >M $008a $f4 >M $008b $06 >M $1a02 $81 >M $1a03 $54 >M $1a04 $80 >M
T{ op PC S A X Y P -> $1a04 $91 $80 $36 $33 $27 }T T{ $0054 M $008a M $008b M $06f4 M $1a02 M $1a03 M $1a04 M -> $c2 $f4 $06 $80 $81 $54 $80 }T
$06b6 >PC $d5 >S $98 >A $43 >X $c1 >Y $a1 >P $002a $ee >M $002b $91 >M $00e7 $64 >M $06b6 $81 >M $06b7 $e7 >M $06b8 $ca >M
T{ op PC S A X Y P -> $06b8 $d5 $98 $43 $c1 $a1 }T T{ $002a M $002b M $00e7 M $06b6 M $06b7 M $06b8 M $91ee M -> $ee $91 $64 $81 $e7 $ca $98 }T
$38be >PC $84 >S $db >A $1a >X $2a >Y $2e >P $00da $06 >M $00f4 $92 >M $00f5 $f9 >M $38be $81 >M $38bf $da >M $38c0 $4f >M
T{ op PC S A X Y P -> $38c0 $84 $db $1a $2a $2e }T T{ $00da M $00f4 M $00f5 M $38be M $38bf M $38c0 M $f992 M -> $06 $92 $f9 $81 $da $4f $db }T
( 82 )
$7124 >PC $41 >S $8f >A $b1 >X $b2 >Y $a8 >P $7124 $82 >M $7125 $65 >M $7126 $04 >M
T{ op PC S A X Y P -> $7126 $41 $8f $b1 $b2 $a8 }T T{ $7124 M $7125 M $7126 M -> $82 $65 $04 }T
$3ba7 >PC $01 >S $7c >A $b1 >X $05 >Y $6b >P $3ba7 $82 >M $3ba8 $3d >M $3ba9 $4b >M
T{ op PC S A X Y P -> $3ba9 $01 $7c $b1 $05 $6b }T T{ $3ba7 M $3ba8 M $3ba9 M -> $82 $3d $4b }T
$ee7b >PC $70 >S $22 >A $17 >X $21 >Y $e3 >P $ee7b $82 >M $ee7c $73 >M $ee7d $d3 >M
T{ op PC S A X Y P -> $ee7d $70 $22 $17 $21 $e3 }T T{ $ee7b M $ee7c M $ee7d M -> $82 $73 $d3 }T
$1f51 >PC $e8 >S $57 >A $c4 >X $b3 >Y $a2 >P $1f51 $82 >M $1f52 $35 >M $1f53 $b9 >M
T{ op PC S A X Y P -> $1f53 $e8 $57 $c4 $b3 $a2 }T T{ $1f51 M $1f52 M $1f53 M -> $82 $35 $b9 }T
$948a >PC $39 >S $a8 >A $f1 >X $82 >Y $ee >P $948a $82 >M $948b $e5 >M $948c $9d >M
T{ op PC S A X Y P -> $948c $39 $a8 $f1 $82 $ee }T T{ $948a M $948b M $948c M -> $82 $e5 $9d }T
$a5c6 >PC $c0 >S $79 >A $37 >X $0a >Y $68 >P $a5c6 $82 >M $a5c7 $a0 >M $a5c8 $ab >M
T{ op PC S A X Y P -> $a5c8 $c0 $79 $37 $0a $68 }T T{ $a5c6 M $a5c7 M $a5c8 M -> $82 $a0 $ab }T
$b901 >PC $16 >S $99 >A $f2 >X $38 >Y $e4 >P $b901 $82 >M $b902 $fe >M $b903 $78 >M
T{ op PC S A X Y P -> $b903 $16 $99 $f2 $38 $e4 }T T{ $b901 M $b902 M $b903 M -> $82 $fe $78 }T
$368b >PC $11 >S $7a >A $2d >X $6c >Y $6b >P $368b $82 >M $368c $40 >M $368d $2b >M
T{ op PC S A X Y P -> $368d $11 $7a $2d $6c $6b }T T{ $368b M $368c M $368d M -> $82 $40 $2b }T
$6395 >PC $fe >S $ce >A $ab >X $72 >Y $a6 >P $6395 $82 >M $6396 $37 >M $6397 $7d >M
T{ op PC S A X Y P -> $6397 $fe $ce $ab $72 $a6 }T T{ $6395 M $6396 M $6397 M -> $82 $37 $7d }T
$391c >PC $16 >S $b3 >A $73 >X $b5 >Y $e8 >P $391c $82 >M $391d $39 >M $391e $0a >M
T{ op PC S A X Y P -> $391e $16 $b3 $73 $b5 $e8 }T T{ $391c M $391d M $391e M -> $82 $39 $0a }T
$e1d7 >PC $2b >S $a5 >A $b8 >X $61 >Y $aa >P $e1d7 $82 >M $e1d8 $e7 >M $e1d9 $5c >M
T{ op PC S A X Y P -> $e1d9 $2b $a5 $b8 $61 $aa }T T{ $e1d7 M $e1d8 M $e1d9 M -> $82 $e7 $5c }T
$e668 >PC $b3 >S $e0 >A $34 >X $27 >Y $ed >P $e668 $82 >M $e669 $85 >M $e66a $ae >M
T{ op PC S A X Y P -> $e66a $b3 $e0 $34 $27 $ed }T T{ $e668 M $e669 M $e66a M -> $82 $85 $ae }T
$715c >PC $0a >S $b1 >A $75 >X $cf >Y $26 >P $715c $82 >M $715d $ba >M $715e $c9 >M
T{ op PC S A X Y P -> $715e $0a $b1 $75 $cf $26 }T T{ $715c M $715d M $715e M -> $82 $ba $c9 }T
$3ddb >PC $b1 >S $02 >A $4b >X $ae >Y $ed >P $3ddb $82 >M $3ddc $2d >M $3ddd $14 >M
T{ op PC S A X Y P -> $3ddd $b1 $02 $4b $ae $ed }T T{ $3ddb M $3ddc M $3ddd M -> $82 $2d $14 }T
$fbdf >PC $b2 >S $20 >A $5b >X $ec >Y $6d >P $fbdf $82 >M $fbe0 $28 >M $fbe1 $7b >M
T{ op PC S A X Y P -> $fbe1 $b2 $20 $5b $ec $6d }T T{ $fbdf M $fbe0 M $fbe1 M -> $82 $28 $7b }T
$4d6a >PC $3b >S $8c >A $f3 >X $fc >Y $64 >P $4d6a $82 >M $4d6b $95 >M $4d6c $7d >M
T{ op PC S A X Y P -> $4d6c $3b $8c $f3 $fc $64 }T T{ $4d6a M $4d6b M $4d6c M -> $82 $95 $7d }T
( 83 )
$3ab9 >PC $94 >S $83 >A $36 >X $bd >Y $e7 >P $3ab9 $83 >M $3aba $72 >M $3abb $9d >M
T{ op PC S A X Y P -> $3aba $94 $83 $36 $bd $e7 }T T{ $3ab9 M $3aba M $3abb M -> $83 $72 $9d }T
$7372 >PC $60 >S $02 >A $a8 >X $48 >Y $a1 >P $7372 $83 >M $7373 $23 >M $7374 $06 >M
T{ op PC S A X Y P -> $7373 $60 $02 $a8 $48 $a1 }T T{ $7372 M $7373 M $7374 M -> $83 $23 $06 }T
$83f9 >PC $59 >S $42 >A $4f >X $e2 >Y $a7 >P $83f9 $83 >M $83fa $7b >M $83fb $4f >M
T{ op PC S A X Y P -> $83fa $59 $42 $4f $e2 $a7 }T T{ $83f9 M $83fa M $83fb M -> $83 $7b $4f }T
$d885 >PC $6b >S $59 >A $19 >X $fc >Y $a0 >P $d885 $83 >M $d886 $6e >M $d887 $34 >M
T{ op PC S A X Y P -> $d886 $6b $59 $19 $fc $a0 }T T{ $d885 M $d886 M $d887 M -> $83 $6e $34 }T
$16bb >PC $de >S $c2 >A $5b >X $bd >Y $66 >P $16bb $83 >M $16bc $75 >M $16bd $0f >M
T{ op PC S A X Y P -> $16bc $de $c2 $5b $bd $66 }T T{ $16bb M $16bc M $16bd M -> $83 $75 $0f }T
$dcd4 >PC $87 >S $d2 >A $5b >X $7e >Y $2f >P $dcd4 $83 >M $dcd5 $5b >M $dcd6 $90 >M
T{ op PC S A X Y P -> $dcd5 $87 $d2 $5b $7e $2f }T T{ $dcd4 M $dcd5 M $dcd6 M -> $83 $5b $90 }T
$907c >PC $56 >S $79 >A $77 >X $e9 >Y $e8 >P $907c $83 >M $907d $ab >M $907e $87 >M
T{ op PC S A X Y P -> $907d $56 $79 $77 $e9 $e8 }T T{ $907c M $907d M $907e M -> $83 $ab $87 }T
$30f4 >PC $34 >S $3f >A $80 >X $e2 >Y $24 >P $30f4 $83 >M $30f5 $f4 >M $30f6 $87 >M
T{ op PC S A X Y P -> $30f5 $34 $3f $80 $e2 $24 }T T{ $30f4 M $30f5 M $30f6 M -> $83 $f4 $87 }T
$5ab8 >PC $8b >S $e3 >A $06 >X $57 >Y $ed >P $5ab8 $83 >M $5ab9 $85 >M $5aba $fc >M
T{ op PC S A X Y P -> $5ab9 $8b $e3 $06 $57 $ed }T T{ $5ab8 M $5ab9 M $5aba M -> $83 $85 $fc }T
$d256 >PC $82 >S $8f >A $a1 >X $40 >Y $26 >P $d256 $83 >M $d257 $84 >M $d258 $4d >M
T{ op PC S A X Y P -> $d257 $82 $8f $a1 $40 $26 }T T{ $d256 M $d257 M $d258 M -> $83 $84 $4d }T
$25c5 >PC $12 >S $30 >A $ca >X $90 >Y $a1 >P $25c5 $83 >M $25c6 $7b >M $25c7 $04 >M
T{ op PC S A X Y P -> $25c6 $12 $30 $ca $90 $a1 }T T{ $25c5 M $25c6 M $25c7 M -> $83 $7b $04 }T
$036c >PC $71 >S $6e >A $99 >X $85 >Y $65 >P $036c $83 >M $036d $50 >M $036e $2f >M
T{ op PC S A X Y P -> $036d $71 $6e $99 $85 $65 }T T{ $036c M $036d M $036e M -> $83 $50 $2f }T
$0eda >PC $05 >S $bd >A $6d >X $fa >Y $ea >P $0eda $83 >M $0edb $32 >M $0edc $8c >M
T{ op PC S A X Y P -> $0edb $05 $bd $6d $fa $ea }T T{ $0eda M $0edb M $0edc M -> $83 $32 $8c }T
$b7b4 >PC $76 >S $fd >A $d3 >X $3b >Y $69 >P $b7b4 $83 >M $b7b5 $ea >M $b7b6 $18 >M
T{ op PC S A X Y P -> $b7b5 $76 $fd $d3 $3b $69 }T T{ $b7b4 M $b7b5 M $b7b6 M -> $83 $ea $18 }T
$80d9 >PC $4b >S $88 >A $04 >X $34 >Y $24 >P $80d9 $83 >M $80da $0b >M $80db $ae >M
T{ op PC S A X Y P -> $80da $4b $88 $04 $34 $24 }T T{ $80d9 M $80da M $80db M -> $83 $0b $ae }T
$3ae0 >PC $ce >S $0f >A $df >X $aa >Y $61 >P $3ae0 $83 >M $3ae1 $a0 >M $3ae2 $65 >M
T{ op PC S A X Y P -> $3ae1 $ce $0f $df $aa $61 }T T{ $3ae0 M $3ae1 M $3ae2 M -> $83 $a0 $65 }T
( 84 )
$c359 >PC $59 >S $7e >A $4a >X $49 >Y $ab >P $c359 $84 >M $c35a $45 >M $c35b $83 >M
T{ op PC S A X Y P -> $c35b $59 $7e $4a $49 $ab }T T{ $0045 M $c359 M $c35a M $c35b M -> $49 $84 $45 $83 }T
$8ff4 >PC $83 >S $29 >A $76 >X $72 >Y $24 >P $8ff4 $84 >M $8ff5 $f4 >M $8ff6 $6a >M
T{ op PC S A X Y P -> $8ff6 $83 $29 $76 $72 $24 }T T{ $00f4 M $8ff4 M $8ff5 M $8ff6 M -> $72 $84 $f4 $6a }T
$2e49 >PC $0a >S $42 >A $ff >X $01 >Y $e0 >P $2e49 $84 >M $2e4a $7f >M $2e4b $75 >M
T{ op PC S A X Y P -> $2e4b $0a $42 $ff $01 $e0 }T T{ $007f M $2e49 M $2e4a M $2e4b M -> $01 $84 $7f $75 }T
$ae76 >PC $90 >S $03 >A $7c >X $f8 >Y $28 >P $ae76 $84 >M $ae77 $1b >M $ae78 $f7 >M
T{ op PC S A X Y P -> $ae78 $90 $03 $7c $f8 $28 }T T{ $001b M $ae76 M $ae77 M $ae78 M -> $f8 $84 $1b $f7 }T
$a60b >PC $96 >S $48 >A $31 >X $d1 >Y $aa >P $a60b $84 >M $a60c $58 >M $a60d $57 >M
T{ op PC S A X Y P -> $a60d $96 $48 $31 $d1 $aa }T T{ $0058 M $a60b M $a60c M $a60d M -> $d1 $84 $58 $57 }T
$8dca >PC $b7 >S $11 >A $b6 >X $42 >Y $6d >P $8dca $84 >M $8dcb $95 >M $8dcc $19 >M
T{ op PC S A X Y P -> $8dcc $b7 $11 $b6 $42 $6d }T T{ $0095 M $8dca M $8dcb M $8dcc M -> $42 $84 $95 $19 }T
$269a >PC $9e >S $35 >A $7d >X $91 >Y $ec >P $269a $84 >M $269b $43 >M $269c $63 >M
T{ op PC S A X Y P -> $269c $9e $35 $7d $91 $ec }T T{ $0043 M $269a M $269b M $269c M -> $91 $84 $43 $63 }T
$54bd >PC $a8 >S $ac >A $7e >X $dc >Y $69 >P $54bd $84 >M $54be $ab >M $54bf $08 >M
T{ op PC S A X Y P -> $54bf $a8 $ac $7e $dc $69 }T T{ $00ab M $54bd M $54be M $54bf M -> $dc $84 $ab $08 }T
$6bae >PC $13 >S $b4 >A $9e >X $a8 >Y $61 >P $6bae $84 >M $6baf $3c >M $6bb0 $c9 >M
T{ op PC S A X Y P -> $6bb0 $13 $b4 $9e $a8 $61 }T T{ $003c M $6bae M $6baf M $6bb0 M -> $a8 $84 $3c $c9 }T
$3f51 >PC $d4 >S $7c >A $9e >X $17 >Y $20 >P $3f51 $84 >M $3f52 $c1 >M $3f53 $26 >M
T{ op PC S A X Y P -> $3f53 $d4 $7c $9e $17 $20 }T T{ $00c1 M $3f51 M $3f52 M $3f53 M -> $17 $84 $c1 $26 }T
$b06e >PC $72 >S $66 >A $bf >X $73 >Y $68 >P $b06e $84 >M $b06f $d7 >M $b070 $73 >M
T{ op PC S A X Y P -> $b070 $72 $66 $bf $73 $68 }T T{ $00d7 M $b06e M $b06f M $b070 M -> $73 $84 $d7 $73 }T
$7d4b >PC $f4 >S $d7 >A $6c >X $f0 >Y $2f >P $7d4b $84 >M $7d4c $ce >M $7d4d $13 >M
T{ op PC S A X Y P -> $7d4d $f4 $d7 $6c $f0 $2f }T T{ $00ce M $7d4b M $7d4c M $7d4d M -> $f0 $84 $ce $13 }T
$d753 >PC $b0 >S $f6 >A $bc >X $57 >Y $e9 >P $d753 $84 >M $d754 $1d >M $d755 $d6 >M
T{ op PC S A X Y P -> $d755 $b0 $f6 $bc $57 $e9 }T T{ $001d M $d753 M $d754 M $d755 M -> $57 $84 $1d $d6 }T
$ad48 >PC $3d >S $24 >A $87 >X $22 >Y $e2 >P $ad48 $84 >M $ad49 $09 >M $ad4a $43 >M
T{ op PC S A X Y P -> $ad4a $3d $24 $87 $22 $e2 }T T{ $0009 M $ad48 M $ad49 M $ad4a M -> $22 $84 $09 $43 }T
$51bc >PC $17 >S $75 >A $80 >X $bf >Y $ac >P $51bc $84 >M $51bd $3e >M $51be $6c >M
T{ op PC S A X Y P -> $51be $17 $75 $80 $bf $ac }T T{ $003e M $51bc M $51bd M $51be M -> $bf $84 $3e $6c }T
$f970 >PC $53 >S $20 >A $f4 >X $bc >Y $ea >P $f970 $84 >M $f971 $ec >M $f972 $e1 >M
T{ op PC S A X Y P -> $f972 $53 $20 $f4 $bc $ea }T T{ $00ec M $f970 M $f971 M $f972 M -> $bc $84 $ec $e1 }T
( 85 )
$f9c6 >PC $07 >S $7e >A $a4 >X $99 >Y $e2 >P $f9c6 $85 >M $f9c7 $6d >M $f9c8 $ed >M
T{ op PC S A X Y P -> $f9c8 $07 $7e $a4 $99 $e2 }T T{ $006d M $f9c6 M $f9c7 M $f9c8 M -> $7e $85 $6d $ed }T
$2589 >PC $8a >S $cc >A $fc >X $2b >Y $ee >P $2589 $85 >M $258a $fb >M $258b $ab >M
T{ op PC S A X Y P -> $258b $8a $cc $fc $2b $ee }T T{ $00fb M $2589 M $258a M $258b M -> $cc $85 $fb $ab }T
$14d8 >PC $3e >S $b4 >A $89 >X $57 >Y $e8 >P $14d8 $85 >M $14d9 $bd >M $14da $7b >M
T{ op PC S A X Y P -> $14da $3e $b4 $89 $57 $e8 }T T{ $00bd M $14d8 M $14d9 M $14da M -> $b4 $85 $bd $7b }T
$a42a >PC $47 >S $65 >A $42 >X $9e >Y $e7 >P $a42a $85 >M $a42b $ca >M $a42c $cb >M
T{ op PC S A X Y P -> $a42c $47 $65 $42 $9e $e7 }T T{ $00ca M $a42a M $a42b M $a42c M -> $65 $85 $ca $cb }T
$1298 >PC $9e >S $99 >A $5f >X $c7 >Y $6b >P $1298 $85 >M $1299 $25 >M $129a $42 >M
T{ op PC S A X Y P -> $129a $9e $99 $5f $c7 $6b }T T{ $0025 M $1298 M $1299 M $129a M -> $99 $85 $25 $42 }T
$d660 >PC $73 >S $e9 >A $6b >X $2c >Y $65 >P $d660 $85 >M $d661 $30 >M $d662 $8c >M
T{ op PC S A X Y P -> $d662 $73 $e9 $6b $2c $65 }T T{ $0030 M $d660 M $d661 M $d662 M -> $e9 $85 $30 $8c }T
$5422 >PC $7f >S $01 >A $d4 >X $c9 >Y $a1 >P $5422 $85 >M $5423 $61 >M $5424 $82 >M
T{ op PC S A X Y P -> $5424 $7f $01 $d4 $c9 $a1 }T T{ $0061 M $5422 M $5423 M $5424 M -> $01 $85 $61 $82 }T
$7f0f >PC $19 >S $47 >A $e7 >X $23 >Y $a5 >P $7f0f $85 >M $7f10 $fa >M $7f11 $74 >M
T{ op PC S A X Y P -> $7f11 $19 $47 $e7 $23 $a5 }T T{ $00fa M $7f0f M $7f10 M $7f11 M -> $47 $85 $fa $74 }T
$87ac >PC $33 >S $0e >A $f4 >X $86 >Y $e2 >P $87ac $85 >M $87ad $8b >M $87ae $80 >M
T{ op PC S A X Y P -> $87ae $33 $0e $f4 $86 $e2 }T T{ $008b M $87ac M $87ad M $87ae M -> $0e $85 $8b $80 }T
$08cf >PC $06 >S $e4 >A $0a >X $ca >Y $66 >P $08cf $85 >M $08d0 $67 >M $08d1 $c0 >M
T{ op PC S A X Y P -> $08d1 $06 $e4 $0a $ca $66 }T T{ $0067 M $08cf M $08d0 M $08d1 M -> $e4 $85 $67 $c0 }T
$7295 >PC $53 >S $24 >A $aa >X $5f >Y $61 >P $7295 $85 >M $7296 $24 >M $7297 $18 >M
T{ op PC S A X Y P -> $7297 $53 $24 $aa $5f $61 }T T{ $0024 M $7295 M $7296 M $7297 M -> $24 $85 $24 $18 }T
$ce6e >PC $76 >S $10 >A $7b >X $1a >Y $e3 >P $ce6e $85 >M $ce6f $5b >M $ce70 $53 >M
T{ op PC S A X Y P -> $ce70 $76 $10 $7b $1a $e3 }T T{ $005b M $ce6e M $ce6f M $ce70 M -> $10 $85 $5b $53 }T
$d07d >PC $e5 >S $03 >A $1f >X $96 >Y $65 >P $d07d $85 >M $d07e $6b >M $d07f $01 >M
T{ op PC S A X Y P -> $d07f $e5 $03 $1f $96 $65 }T T{ $006b M $d07d M $d07e M $d07f M -> $03 $85 $6b $01 }T
$d84f >PC $e6 >S $4f >A $7e >X $8a >Y $21 >P $d84f $85 >M $d850 $2a >M $d851 $34 >M
T{ op PC S A X Y P -> $d851 $e6 $4f $7e $8a $21 }T T{ $002a M $d84f M $d850 M $d851 M -> $4f $85 $2a $34 }T
$5ca5 >PC $9a >S $79 >A $bd >X $d6 >Y $a1 >P $5ca5 $85 >M $5ca6 $86 >M $5ca7 $aa >M
T{ op PC S A X Y P -> $5ca7 $9a $79 $bd $d6 $a1 }T T{ $0086 M $5ca5 M $5ca6 M $5ca7 M -> $79 $85 $86 $aa }T
$1f58 >PC $2b >S $aa >A $9c >X $f5 >Y $29 >P $1f58 $85 >M $1f59 $5e >M $1f5a $44 >M
T{ op PC S A X Y P -> $1f5a $2b $aa $9c $f5 $29 }T T{ $005e M $1f58 M $1f59 M $1f5a M -> $aa $85 $5e $44 }T
( 86 )
$7a57 >PC $af >S $92 >A $62 >X $b5 >Y $2f >P $7a57 $86 >M $7a58 $f9 >M $7a59 $e4 >M
T{ op PC S A X Y P -> $7a59 $af $92 $62 $b5 $2f }T T{ $00f9 M $7a57 M $7a58 M $7a59 M -> $62 $86 $f9 $e4 }T
$a591 >PC $ad >S $68 >A $5a >X $ba >Y $29 >P $a591 $86 >M $a592 $ae >M $a593 $e1 >M
T{ op PC S A X Y P -> $a593 $ad $68 $5a $ba $29 }T T{ $00ae M $a591 M $a592 M $a593 M -> $5a $86 $ae $e1 }T
$782c >PC $87 >S $02 >A $84 >X $65 >Y $2e >P $782c $86 >M $782d $e7 >M $782e $d1 >M
T{ op PC S A X Y P -> $782e $87 $02 $84 $65 $2e }T T{ $00e7 M $782c M $782d M $782e M -> $84 $86 $e7 $d1 }T
$931a >PC $fb >S $b5 >A $a1 >X $2d >Y $a8 >P $931a $86 >M $931b $f2 >M $931c $65 >M
T{ op PC S A X Y P -> $931c $fb $b5 $a1 $2d $a8 }T T{ $00f2 M $931a M $931b M $931c M -> $a1 $86 $f2 $65 }T
$fe6d >PC $0f >S $8b >A $2d >X $c5 >Y $21 >P $fe6d $86 >M $fe6e $6b >M $fe6f $f0 >M
T{ op PC S A X Y P -> $fe6f $0f $8b $2d $c5 $21 }T T{ $006b M $fe6d M $fe6e M $fe6f M -> $2d $86 $6b $f0 }T
$1013 >PC $8f >S $6e >A $9a >X $d3 >Y $e8 >P $1013 $86 >M $1014 $0b >M $1015 $e1 >M
T{ op PC S A X Y P -> $1015 $8f $6e $9a $d3 $e8 }T T{ $000b M $1013 M $1014 M $1015 M -> $9a $86 $0b $e1 }T
$4fb0 >PC $c0 >S $0b >A $29 >X $a4 >Y $aa >P $4fb0 $86 >M $4fb1 $0a >M $4fb2 $08 >M
T{ op PC S A X Y P -> $4fb2 $c0 $0b $29 $a4 $aa }T T{ $000a M $4fb0 M $4fb1 M $4fb2 M -> $29 $86 $0a $08 }T
$948e >PC $40 >S $ab >A $f4 >X $6a >Y $67 >P $948e $86 >M $948f $58 >M $9490 $68 >M
T{ op PC S A X Y P -> $9490 $40 $ab $f4 $6a $67 }T T{ $0058 M $948e M $948f M $9490 M -> $f4 $86 $58 $68 }T
$436c >PC $8d >S $8f >A $64 >X $f9 >Y $af >P $436c $86 >M $436d $b8 >M $436e $f0 >M
T{ op PC S A X Y P -> $436e $8d $8f $64 $f9 $af }T T{ $00b8 M $436c M $436d M $436e M -> $64 $86 $b8 $f0 }T
$0322 >PC $f0 >S $6c >A $44 >X $cf >Y $22 >P $0322 $86 >M $0323 $df >M $0324 $a2 >M
T{ op PC S A X Y P -> $0324 $f0 $6c $44 $cf $22 }T T{ $00df M $0322 M $0323 M $0324 M -> $44 $86 $df $a2 }T
$c59e >PC $2f >S $38 >A $61 >X $76 >Y $6a >P $c59e $86 >M $c59f $9d >M $c5a0 $93 >M
T{ op PC S A X Y P -> $c5a0 $2f $38 $61 $76 $6a }T T{ $009d M $c59e M $c59f M $c5a0 M -> $61 $86 $9d $93 }T
$650c >PC $86 >S $7c >A $99 >X $82 >Y $67 >P $650c $86 >M $650d $57 >M $650e $42 >M
T{ op PC S A X Y P -> $650e $86 $7c $99 $82 $67 }T T{ $0057 M $650c M $650d M $650e M -> $99 $86 $57 $42 }T
$e9e8 >PC $61 >S $b0 >A $de >X $b5 >Y $60 >P $e9e8 $86 >M $e9e9 $d6 >M $e9ea $b0 >M
T{ op PC S A X Y P -> $e9ea $61 $b0 $de $b5 $60 }T T{ $00d6 M $e9e8 M $e9e9 M $e9ea M -> $de $86 $d6 $b0 }T
$4534 >PC $bf >S $25 >A $f6 >X $0d >Y $ab >P $4534 $86 >M $4535 $fb >M $4536 $73 >M
T{ op PC S A X Y P -> $4536 $bf $25 $f6 $0d $ab }T T{ $00fb M $4534 M $4535 M $4536 M -> $f6 $86 $fb $73 }T
$e6d9 >PC $d5 >S $84 >A $83 >X $43 >Y $66 >P $e6d9 $86 >M $e6da $bf >M $e6db $1a >M
T{ op PC S A X Y P -> $e6db $d5 $84 $83 $43 $66 }T T{ $00bf M $e6d9 M $e6da M $e6db M -> $83 $86 $bf $1a }T
$af49 >PC $82 >S $36 >A $f1 >X $7d >Y $a8 >P $af49 $86 >M $af4a $0c >M $af4b $9b >M
T{ op PC S A X Y P -> $af4b $82 $36 $f1 $7d $a8 }T T{ $000c M $af49 M $af4a M $af4b M -> $f1 $86 $0c $9b }T
( 87 )
$7c96 >PC $9b >S $91 >A $66 >X $56 >Y $29 >P $008c $b2 >M $7c96 $87 >M $7c97 $8c >M $7c98 $ff >M
T{ op PC S A X Y P -> $7c98 $9b $91 $66 $56 $29 }T T{ $008c M $7c96 M $7c97 M $7c98 M -> $b3 $87 $8c $ff }T
$5487 >PC $32 >S $28 >A $88 >X $7c >Y $a6 >P $002e $f4 >M $5487 $87 >M $5488 $2e >M $5489 $a0 >M
T{ op PC S A X Y P -> $5489 $32 $28 $88 $7c $a6 }T T{ $002e M $5487 M $5488 M $5489 M -> $f5 $87 $2e $a0 }T
$3c65 >PC $6f >S $c1 >A $10 >X $1e >Y $a7 >P $0083 $46 >M $3c65 $87 >M $3c66 $83 >M $3c67 $f1 >M
T{ op PC S A X Y P -> $3c67 $6f $c1 $10 $1e $a7 }T T{ $0083 M $3c65 M $3c66 M $3c67 M -> $47 $87 $83 $f1 }T
$9eba >PC $f8 >S $10 >A $87 >X $2b >Y $a2 >P $0052 $72 >M $9eba $87 >M $9ebb $52 >M $9ebc $f3 >M
T{ op PC S A X Y P -> $9ebc $f8 $10 $87 $2b $a2 }T T{ $0052 M $9eba M $9ebb M $9ebc M -> $73 $87 $52 $f3 }T
$d075 >PC $cf >S $a9 >A $b0 >X $df >Y $e4 >P $00b9 $3e >M $d075 $87 >M $d076 $b9 >M $d077 $aa >M
T{ op PC S A X Y P -> $d077 $cf $a9 $b0 $df $e4 }T T{ $00b9 M $d075 M $d076 M $d077 M -> $3f $87 $b9 $aa }T
$21ca >PC $90 >S $81 >A $10 >X $6e >Y $61 >P $00d8 $50 >M $21ca $87 >M $21cb $d8 >M $21cc $f9 >M
T{ op PC S A X Y P -> $21cc $90 $81 $10 $6e $61 }T T{ $00d8 M $21ca M $21cb M $21cc M -> $51 $87 $d8 $f9 }T
$cf82 >PC $c7 >S $bd >A $f6 >X $88 >Y $60 >P $00ce $15 >M $cf82 $87 >M $cf83 $ce >M $cf84 $3a >M
T{ op PC S A X Y P -> $cf84 $c7 $bd $f6 $88 $60 }T T{ $00ce M $cf82 M $cf83 M $cf84 M -> $15 $87 $ce $3a }T
$3950 >PC $7e >S $08 >A $63 >X $6c >Y $a0 >P $00d1 $95 >M $3950 $87 >M $3951 $d1 >M $3952 $05 >M
T{ op PC S A X Y P -> $3952 $7e $08 $63 $6c $a0 }T T{ $00d1 M $3950 M $3951 M $3952 M -> $95 $87 $d1 $05 }T
$9900 >PC $fa >S $60 >A $52 >X $19 >Y $ab >P $0073 $45 >M $9900 $87 >M $9901 $73 >M $9902 $df >M
T{ op PC S A X Y P -> $9902 $fa $60 $52 $19 $ab }T T{ $0073 M $9900 M $9901 M $9902 M -> $45 $87 $73 $df }T
$c2be >PC $85 >S $00 >A $34 >X $ad >Y $6e >P $0038 $bf >M $c2be $87 >M $c2bf $38 >M $c2c0 $11 >M
T{ op PC S A X Y P -> $c2c0 $85 $00 $34 $ad $6e }T T{ $0038 M $c2be M $c2bf M $c2c0 M -> $bf $87 $38 $11 }T
$5f21 >PC $c1 >S $dd >A $38 >X $32 >Y $23 >P $00fb $08 >M $5f21 $87 >M $5f22 $fb >M $5f23 $4c >M
T{ op PC S A X Y P -> $5f23 $c1 $dd $38 $32 $23 }T T{ $00fb M $5f21 M $5f22 M $5f23 M -> $09 $87 $fb $4c }T
$efb7 >PC $c4 >S $6c >A $c3 >X $7b >Y $2c >P $00e9 $41 >M $efb7 $87 >M $efb8 $e9 >M $efb9 $d2 >M
T{ op PC S A X Y P -> $efb9 $c4 $6c $c3 $7b $2c }T T{ $00e9 M $efb7 M $efb8 M $efb9 M -> $41 $87 $e9 $d2 }T
$e809 >PC $58 >S $27 >A $96 >X $83 >Y $2f >P $002d $f8 >M $e809 $87 >M $e80a $2d >M $e80b $45 >M
T{ op PC S A X Y P -> $e80b $58 $27 $96 $83 $2f }T T{ $002d M $e809 M $e80a M $e80b M -> $f9 $87 $2d $45 }T
$ca3c >PC $3a >S $5b >A $17 >X $8c >Y $61 >P $001e $d1 >M $ca3c $87 >M $ca3d $1e >M $ca3e $4b >M
T{ op PC S A X Y P -> $ca3e $3a $5b $17 $8c $61 }T T{ $001e M $ca3c M $ca3d M $ca3e M -> $d1 $87 $1e $4b }T
$4efc >PC $44 >S $26 >A $2c >X $3a >Y $20 >P $009b $4f >M $4efc $87 >M $4efd $9b >M $4efe $8e >M
T{ op PC S A X Y P -> $4efe $44 $26 $2c $3a $20 }T T{ $009b M $4efc M $4efd M $4efe M -> $4f $87 $9b $8e }T
$3eb0 >PC $f5 >S $bc >A $a7 >X $31 >Y $e9 >P $0064 $0c >M $3eb0 $87 >M $3eb1 $64 >M $3eb2 $c1 >M
T{ op PC S A X Y P -> $3eb2 $f5 $bc $a7 $31 $e9 }T T{ $0064 M $3eb0 M $3eb1 M $3eb2 M -> $0d $87 $64 $c1 }T
( 88 )
$69d2 >PC $f2 >S $f7 >A $ce >X $21 >Y $ac >P $69d2 $88 >M $69d3 $ad >M $69d4 $77 >M
T{ op PC S A X Y P -> $69d3 $f2 $f7 $ce $20 $2c }T T{ $69d2 M $69d3 M $69d4 M -> $88 $ad $77 }T
$715d >PC $75 >S $d2 >A $51 >X $21 >Y $a4 >P $715d $88 >M $715e $32 >M $715f $61 >M
T{ op PC S A X Y P -> $715e $75 $d2 $51 $20 $24 }T T{ $715d M $715e M $715f M -> $88 $32 $61 }T
$87ff >PC $d3 >S $22 >A $fc >X $29 >Y $28 >P $87ff $88 >M $8800 $5f >M $8801 $b0 >M
T{ op PC S A X Y P -> $8800 $d3 $22 $fc $28 $28 }T T{ $87ff M $8800 M $8801 M -> $88 $5f $b0 }T
$1021 >PC $a3 >S $b7 >A $7c >X $f8 >Y $27 >P $1021 $88 >M $1022 $30 >M $1023 $59 >M
T{ op PC S A X Y P -> $1022 $a3 $b7 $7c $f7 $a5 }T T{ $1021 M $1022 M $1023 M -> $88 $30 $59 }T
$0589 >PC $10 >S $22 >A $f8 >X $dd >Y $eb >P $0589 $88 >M $058a $d4 >M $058b $f5 >M
T{ op PC S A X Y P -> $058a $10 $22 $f8 $dc $e9 }T T{ $0589 M $058a M $058b M -> $88 $d4 $f5 }T
$7112 >PC $8b >S $e6 >A $db >X $d3 >Y $66 >P $7112 $88 >M $7113 $79 >M $7114 $23 >M
T{ op PC S A X Y P -> $7113 $8b $e6 $db $d2 $e4 }T T{ $7112 M $7113 M $7114 M -> $88 $79 $23 }T
$4908 >PC $24 >S $28 >A $47 >X $0d >Y $a9 >P $4908 $88 >M $4909 $df >M $490a $25 >M
T{ op PC S A X Y P -> $4909 $24 $28 $47 $0c $29 }T T{ $4908 M $4909 M $490a M -> $88 $df $25 }T
$1922 >PC $b2 >S $9f >A $f7 >X $a8 >Y $a4 >P $1922 $88 >M $1923 $b6 >M $1924 $b1 >M
T{ op PC S A X Y P -> $1923 $b2 $9f $f7 $a7 $a4 }T T{ $1922 M $1923 M $1924 M -> $88 $b6 $b1 }T
$a25f >PC $83 >S $5a >A $2b >X $00 >Y $64 >P $a25f $88 >M $a260 $67 >M $a261 $95 >M
T{ op PC S A X Y P -> $a260 $83 $5a $2b $ff $e4 }T T{ $a25f M $a260 M $a261 M -> $88 $67 $95 }T
$22db >PC $31 >S $53 >A $49 >X $11 >Y $a3 >P $22db $88 >M $22dc $90 >M $22dd $ce >M
T{ op PC S A X Y P -> $22dc $31 $53 $49 $10 $21 }T T{ $22db M $22dc M $22dd M -> $88 $90 $ce }T
$b98f >PC $93 >S $b2 >A $aa >X $c1 >Y $6e >P $b98f $88 >M $b990 $0b >M $b991 $46 >M
T{ op PC S A X Y P -> $b990 $93 $b2 $aa $c0 $ec }T T{ $b98f M $b990 M $b991 M -> $88 $0b $46 }T
$b570 >PC $ba >S $44 >A $47 >X $99 >Y $68 >P $b570 $88 >M $b571 $67 >M $b572 $43 >M
T{ op PC S A X Y P -> $b571 $ba $44 $47 $98 $e8 }T T{ $b570 M $b571 M $b572 M -> $88 $67 $43 }T
$b7ff >PC $7d >S $63 >A $47 >X $ee >Y $a6 >P $b7ff $88 >M $b800 $cf >M $b801 $16 >M
T{ op PC S A X Y P -> $b800 $7d $63 $47 $ed $a4 }T T{ $b7ff M $b800 M $b801 M -> $88 $cf $16 }T
$7cf2 >PC $5a >S $cd >A $ba >X $02 >Y $a2 >P $7cf2 $88 >M $7cf3 $90 >M $7cf4 $f5 >M
T{ op PC S A X Y P -> $7cf3 $5a $cd $ba $01 $20 }T T{ $7cf2 M $7cf3 M $7cf4 M -> $88 $90 $f5 }T
$5aaf >PC $01 >S $75 >A $a5 >X $e6 >Y $e9 >P $5aaf $88 >M $5ab0 $90 >M $5ab1 $65 >M
T{ op PC S A X Y P -> $5ab0 $01 $75 $a5 $e5 $e9 }T T{ $5aaf M $5ab0 M $5ab1 M -> $88 $90 $65 }T
$1adc >PC $a9 >S $6d >A $ff >X $b3 >Y $eb >P $1adc $88 >M $1add $51 >M $1ade $54 >M
T{ op PC S A X Y P -> $1add $a9 $6d $ff $b2 $e9 }T T{ $1adc M $1add M $1ade M -> $88 $51 $54 }T
( 89 )
$59ae >PC $66 >S $45 >A $0b >X $07 >Y $a6 >P $59ae $89 >M $59af $cf >M $59b0 $1c >M
T{ op PC S A X Y P -> $59b0 $66 $45 $0b $07 $a4 }T T{ $59ae M $59af M $59b0 M -> $89 $cf $1c }T
$9188 >PC $d1 >S $e4 >A $65 >X $f5 >Y $a5 >P $9188 $89 >M $9189 $e9 >M $918a $71 >M
T{ op PC S A X Y P -> $918a $d1 $e4 $65 $f5 $a5 }T T{ $9188 M $9189 M $918a M -> $89 $e9 $71 }T
$d2ca >PC $5d >S $5f >A $08 >X $05 >Y $e2 >P $d2ca $89 >M $d2cb $7e >M $d2cc $9d >M
T{ op PC S A X Y P -> $d2cc $5d $5f $08 $05 $e0 }T T{ $d2ca M $d2cb M $d2cc M -> $89 $7e $9d }T
$cc94 >PC $08 >S $20 >A $49 >X $16 >Y $e6 >P $cc94 $89 >M $cc95 $8a >M $cc96 $e0 >M
T{ op PC S A X Y P -> $cc96 $08 $20 $49 $16 $e6 }T T{ $cc94 M $cc95 M $cc96 M -> $89 $8a $e0 }T
$6491 >PC $dd >S $e8 >A $7b >X $79 >Y $68 >P $6491 $89 >M $6492 $ba >M $6493 $ed >M
T{ op PC S A X Y P -> $6493 $dd $e8 $7b $79 $68 }T T{ $6491 M $6492 M $6493 M -> $89 $ba $ed }T
$42f0 >PC $1b >S $81 >A $e1 >X $a1 >Y $a9 >P $42f0 $89 >M $42f1 $96 >M $42f2 $1c >M
T{ op PC S A X Y P -> $42f2 $1b $81 $e1 $a1 $a9 }T T{ $42f0 M $42f1 M $42f2 M -> $89 $96 $1c }T
$c189 >PC $fe >S $e4 >A $5e >X $d4 >Y $20 >P $c189 $89 >M $c18a $1a >M $c18b $6a >M
T{ op PC S A X Y P -> $c18b $fe $e4 $5e $d4 $22 }T T{ $c189 M $c18a M $c18b M -> $89 $1a $6a }T
$0221 >PC $91 >S $37 >A $a7 >X $16 >Y $2d >P $0221 $89 >M $0222 $aa >M $0223 $30 >M
T{ op PC S A X Y P -> $0223 $91 $37 $a7 $16 $2d }T T{ $0221 M $0222 M $0223 M -> $89 $aa $30 }T
$87d8 >PC $42 >S $fd >A $62 >X $cb >Y $ed >P $87d8 $89 >M $87d9 $98 >M $87da $b8 >M
T{ op PC S A X Y P -> $87da $42 $fd $62 $cb $ed }T T{ $87d8 M $87d9 M $87da M -> $89 $98 $b8 }T
$4b92 >PC $47 >S $a1 >A $43 >X $06 >Y $27 >P $4b92 $89 >M $4b93 $92 >M $4b94 $46 >M
T{ op PC S A X Y P -> $4b94 $47 $a1 $43 $06 $25 }T T{ $4b92 M $4b93 M $4b94 M -> $89 $92 $46 }T
$b02e >PC $09 >S $3e >A $2e >X $49 >Y $e0 >P $b02e $89 >M $b02f $93 >M $b030 $56 >M
T{ op PC S A X Y P -> $b030 $09 $3e $2e $49 $e0 }T T{ $b02e M $b02f M $b030 M -> $89 $93 $56 }T
$e671 >PC $91 >S $67 >A $f9 >X $86 >Y $69 >P $e671 $89 >M $e672 $27 >M $e673 $8e >M
T{ op PC S A X Y P -> $e673 $91 $67 $f9 $86 $69 }T T{ $e671 M $e672 M $e673 M -> $89 $27 $8e }T
$f580 >PC $8d >S $ae >A $1e >X $af >Y $e9 >P $f580 $89 >M $f581 $ba >M $f582 $0d >M
T{ op PC S A X Y P -> $f582 $8d $ae $1e $af $e9 }T T{ $f580 M $f581 M $f582 M -> $89 $ba $0d }T
$3e8e >PC $8b >S $68 >A $97 >X $5b >Y $a9 >P $3e8e $89 >M $3e8f $f1 >M $3e90 $df >M
T{ op PC S A X Y P -> $3e90 $8b $68 $97 $5b $a9 }T T{ $3e8e M $3e8f M $3e90 M -> $89 $f1 $df }T
$0085 >PC $d0 >S $20 >A $5f >X $a4 >Y $ee >P $0085 $89 >M $0086 $9d >M $0087 $87 >M
T{ op PC S A X Y P -> $0087 $d0 $20 $5f $a4 $ee }T T{ $0085 M $0086 M $0087 M -> $89 $9d $87 }T
$a709 >PC $2e >S $3d >A $cf >X $44 >Y $e9 >P $a709 $89 >M $a70a $a5 >M $a70b $f9 >M
T{ op PC S A X Y P -> $a70b $2e $3d $cf $44 $e9 }T T{ $a709 M $a70a M $a70b M -> $89 $a5 $f9 }T
( 8a )
$2f85 >PC $cb >S $7a >A $f9 >X $63 >Y $2d >P $2f85 $8a >M $2f86 $9d >M $2f87 $79 >M
T{ op PC S A X Y P -> $2f86 $cb $f9 $f9 $63 $ad }T T{ $2f85 M $2f86 M $2f87 M -> $8a $9d $79 }T
$100d >PC $ec >S $e9 >A $67 >X $ed >Y $27 >P $100d $8a >M $100e $8b >M $100f $52 >M
T{ op PC S A X Y P -> $100e $ec $67 $67 $ed $25 }T T{ $100d M $100e M $100f M -> $8a $8b $52 }T
$d0ee >PC $bb >S $59 >A $08 >X $e5 >Y $a8 >P $d0ee $8a >M $d0ef $e8 >M $d0f0 $df >M
T{ op PC S A X Y P -> $d0ef $bb $08 $08 $e5 $28 }T T{ $d0ee M $d0ef M $d0f0 M -> $8a $e8 $df }T
$f79b >PC $36 >S $5d >A $fd >X $ff >Y $a5 >P $f79b $8a >M $f79c $27 >M $f79d $39 >M
T{ op PC S A X Y P -> $f79c $36 $fd $fd $ff $a5 }T T{ $f79b M $f79c M $f79d M -> $8a $27 $39 }T
$fdac >PC $4f >S $72 >A $47 >X $ba >Y $2a >P $fdac $8a >M $fdad $6f >M $fdae $37 >M
T{ op PC S A X Y P -> $fdad $4f $47 $47 $ba $28 }T T{ $fdac M $fdad M $fdae M -> $8a $6f $37 }T
$1ff0 >PC $6d >S $41 >A $87 >X $2c >Y $64 >P $1ff0 $8a >M $1ff1 $9a >M $1ff2 $65 >M
T{ op PC S A X Y P -> $1ff1 $6d $87 $87 $2c $e4 }T T{ $1ff0 M $1ff1 M $1ff2 M -> $8a $9a $65 }T
$1c31 >PC $39 >S $10 >A $05 >X $f8 >Y $a8 >P $1c31 $8a >M $1c32 $59 >M $1c33 $19 >M
T{ op PC S A X Y P -> $1c32 $39 $05 $05 $f8 $28 }T T{ $1c31 M $1c32 M $1c33 M -> $8a $59 $19 }T
$1e23 >PC $2c >S $36 >A $f7 >X $b5 >Y $ee >P $1e23 $8a >M $1e24 $40 >M $1e25 $1e >M
T{ op PC S A X Y P -> $1e24 $2c $f7 $f7 $b5 $ec }T T{ $1e23 M $1e24 M $1e25 M -> $8a $40 $1e }T
$6bd9 >PC $0c >S $c0 >A $62 >X $c0 >Y $21 >P $6bd9 $8a >M $6bda $6d >M $6bdb $80 >M
T{ op PC S A X Y P -> $6bda $0c $62 $62 $c0 $21 }T T{ $6bd9 M $6bda M $6bdb M -> $8a $6d $80 }T
$5f93 >PC $0c >S $82 >A $21 >X $8f >Y $25 >P $5f93 $8a >M $5f94 $5c >M $5f95 $1f >M
T{ op PC S A X Y P -> $5f94 $0c $21 $21 $8f $25 }T T{ $5f93 M $5f94 M $5f95 M -> $8a $5c $1f }T
$287b >PC $c4 >S $be >A $f9 >X $bf >Y $af >P $287b $8a >M $287c $c6 >M $287d $94 >M
T{ op PC S A X Y P -> $287c $c4 $f9 $f9 $bf $ad }T T{ $287b M $287c M $287d M -> $8a $c6 $94 }T
$ddf1 >PC $dc >S $91 >A $ca >X $e1 >Y $66 >P $ddf1 $8a >M $ddf2 $79 >M $ddf3 $a7 >M
T{ op PC S A X Y P -> $ddf2 $dc $ca $ca $e1 $e4 }T T{ $ddf1 M $ddf2 M $ddf3 M -> $8a $79 $a7 }T
$6e38 >PC $df >S $f8 >A $08 >X $d9 >Y $20 >P $6e38 $8a >M $6e39 $5a >M $6e3a $30 >M
T{ op PC S A X Y P -> $6e39 $df $08 $08 $d9 $20 }T T{ $6e38 M $6e39 M $6e3a M -> $8a $5a $30 }T
$4c9c >PC $a9 >S $d2 >A $16 >X $51 >Y $ec >P $4c9c $8a >M $4c9d $1a >M $4c9e $4e >M
T{ op PC S A X Y P -> $4c9d $a9 $16 $16 $51 $6c }T T{ $4c9c M $4c9d M $4c9e M -> $8a $1a $4e }T
$929e >PC $9f >S $ae >A $6c >X $d0 >Y $e1 >P $929e $8a >M $929f $8f >M $92a0 $ae >M
T{ op PC S A X Y P -> $929f $9f $6c $6c $d0 $61 }T T{ $929e M $929f M $92a0 M -> $8a $8f $ae }T
$5f28 >PC $06 >S $d7 >A $5a >X $a4 >Y $64 >P $5f28 $8a >M $5f29 $23 >M $5f2a $33 >M
T{ op PC S A X Y P -> $5f29 $06 $5a $5a $a4 $64 }T T{ $5f28 M $5f29 M $5f2a M -> $8a $23 $33 }T
( 8b )
$181c >PC $e2 >S $43 >A $be >X $9d >Y $6f >P $181c $8b >M $181d $95 >M $181e $ba >M
T{ op PC S A X Y P -> $181d $e2 $43 $be $9d $6f }T T{ $181c M $181d M $181e M -> $8b $95 $ba }T
$dc84 >PC $12 >S $03 >A $84 >X $4e >Y $a9 >P $dc84 $8b >M $dc85 $4d >M $dc86 $78 >M
T{ op PC S A X Y P -> $dc85 $12 $03 $84 $4e $a9 }T T{ $dc84 M $dc85 M $dc86 M -> $8b $4d $78 }T
$8653 >PC $d1 >S $26 >A $1d >X $be >Y $e5 >P $8653 $8b >M $8654 $3d >M $8655 $ca >M
T{ op PC S A X Y P -> $8654 $d1 $26 $1d $be $e5 }T T{ $8653 M $8654 M $8655 M -> $8b $3d $ca }T
$d3c3 >PC $1e >S $f2 >A $12 >X $4d >Y $28 >P $d3c3 $8b >M $d3c4 $e6 >M $d3c5 $cb >M
T{ op PC S A X Y P -> $d3c4 $1e $f2 $12 $4d $28 }T T{ $d3c3 M $d3c4 M $d3c5 M -> $8b $e6 $cb }T
$f351 >PC $a7 >S $dd >A $89 >X $b5 >Y $23 >P $f351 $8b >M $f352 $f3 >M $f353 $1c >M
T{ op PC S A X Y P -> $f352 $a7 $dd $89 $b5 $23 }T T{ $f351 M $f352 M $f353 M -> $8b $f3 $1c }T
$f5a7 >PC $ef >S $b3 >A $66 >X $de >Y $e5 >P $f5a7 $8b >M $f5a8 $67 >M $f5a9 $3d >M
T{ op PC S A X Y P -> $f5a8 $ef $b3 $66 $de $e5 }T T{ $f5a7 M $f5a8 M $f5a9 M -> $8b $67 $3d }T
$d157 >PC $58 >S $d8 >A $66 >X $c0 >Y $e8 >P $d157 $8b >M $d158 $3c >M $d159 $37 >M
T{ op PC S A X Y P -> $d158 $58 $d8 $66 $c0 $e8 }T T{ $d157 M $d158 M $d159 M -> $8b $3c $37 }T
$e662 >PC $68 >S $2b >A $ab >X $10 >Y $28 >P $e662 $8b >M $e663 $02 >M $e664 $89 >M
T{ op PC S A X Y P -> $e663 $68 $2b $ab $10 $28 }T T{ $e662 M $e663 M $e664 M -> $8b $02 $89 }T
$4f9c >PC $f4 >S $ba >A $97 >X $1e >Y $e1 >P $4f9c $8b >M $4f9d $31 >M $4f9e $79 >M
T{ op PC S A X Y P -> $4f9d $f4 $ba $97 $1e $e1 }T T{ $4f9c M $4f9d M $4f9e M -> $8b $31 $79 }T
$ed03 >PC $5e >S $7f >A $8b >X $9d >Y $eb >P $ed03 $8b >M $ed04 $39 >M $ed05 $69 >M
T{ op PC S A X Y P -> $ed04 $5e $7f $8b $9d $eb }T T{ $ed03 M $ed04 M $ed05 M -> $8b $39 $69 }T
$cedf >PC $95 >S $1c >A $22 >X $f5 >Y $65 >P $cedf $8b >M $cee0 $ef >M $cee1 $b8 >M
T{ op PC S A X Y P -> $cee0 $95 $1c $22 $f5 $65 }T T{ $cedf M $cee0 M $cee1 M -> $8b $ef $b8 }T
$96c1 >PC $3f >S $f5 >A $95 >X $b6 >Y $24 >P $96c1 $8b >M $96c2 $b9 >M $96c3 $8c >M
T{ op PC S A X Y P -> $96c2 $3f $f5 $95 $b6 $24 }T T{ $96c1 M $96c2 M $96c3 M -> $8b $b9 $8c }T
$5c9d >PC $be >S $85 >A $49 >X $d7 >Y $aa >P $5c9d $8b >M $5c9e $58 >M $5c9f $ff >M
T{ op PC S A X Y P -> $5c9e $be $85 $49 $d7 $aa }T T{ $5c9d M $5c9e M $5c9f M -> $8b $58 $ff }T
$910a >PC $8e >S $b3 >A $08 >X $3f >Y $2b >P $910a $8b >M $910b $6e >M $910c $50 >M
T{ op PC S A X Y P -> $910b $8e $b3 $08 $3f $2b }T T{ $910a M $910b M $910c M -> $8b $6e $50 }T
$159e >PC $84 >S $e7 >A $af >X $4a >Y $ab >P $159e $8b >M $159f $de >M $15a0 $36 >M
T{ op PC S A X Y P -> $159f $84 $e7 $af $4a $ab }T T{ $159e M $159f M $15a0 M -> $8b $de $36 }T
$762f >PC $b6 >S $1d >A $59 >X $39 >Y $e2 >P $762f $8b >M $7630 $e1 >M $7631 $64 >M
T{ op PC S A X Y P -> $7630 $b6 $1d $59 $39 $e2 }T T{ $762f M $7630 M $7631 M -> $8b $e1 $64 }T
( 8c )
$f1d6 >PC $7d >S $c9 >A $f4 >X $32 >Y $eb >P $f1d6 $8c >M $f1d7 $0f >M $f1d8 $fa >M $f1d9 $9b >M
T{ op PC S A X Y P -> $f1d9 $7d $c9 $f4 $32 $eb }T T{ $f1d6 M $f1d7 M $f1d8 M $f1d9 M $fa0f M -> $8c $0f $fa $9b $32 }T
$ec8a >PC $3f >S $03 >A $ae >X $42 >Y $6f >P $ec8a $8c >M $ec8b $3a >M $ec8c $b3 >M $ec8d $f6 >M
T{ op PC S A X Y P -> $ec8d $3f $03 $ae $42 $6f }T T{ $b33a M $ec8a M $ec8b M $ec8c M $ec8d M -> $42 $8c $3a $b3 $f6 }T
$649e >PC $5a >S $a6 >A $60 >X $5f >Y $af >P $649e $8c >M $649f $90 >M $64a0 $f7 >M $64a1 $1a >M
T{ op PC S A X Y P -> $64a1 $5a $a6 $60 $5f $af }T T{ $649e M $649f M $64a0 M $64a1 M $f790 M -> $8c $90 $f7 $1a $5f }T
$2699 >PC $ec >S $64 >A $21 >X $e5 >Y $2f >P $2699 $8c >M $269a $b0 >M $269b $aa >M $269c $8b >M
T{ op PC S A X Y P -> $269c $ec $64 $21 $e5 $2f }T T{ $2699 M $269a M $269b M $269c M $aab0 M -> $8c $b0 $aa $8b $e5 }T
$e69c >PC $08 >S $d0 >A $96 >X $82 >Y $e7 >P $e69c $8c >M $e69d $2c >M $e69e $f2 >M $e69f $c5 >M
T{ op PC S A X Y P -> $e69f $08 $d0 $96 $82 $e7 }T T{ $e69c M $e69d M $e69e M $e69f M $f22c M -> $8c $2c $f2 $c5 $82 }T
$15df >PC $d0 >S $80 >A $39 >X $7c >Y $28 >P $15df $8c >M $15e0 $df >M $15e1 $c9 >M $15e2 $8f >M
T{ op PC S A X Y P -> $15e2 $d0 $80 $39 $7c $28 }T T{ $15df M $15e0 M $15e1 M $15e2 M $c9df M -> $8c $df $c9 $8f $7c }T
$6a22 >PC $af >S $74 >A $9f >X $aa >Y $ae >P $6a22 $8c >M $6a23 $d1 >M $6a24 $87 >M $6a25 $81 >M
T{ op PC S A X Y P -> $6a25 $af $74 $9f $aa $ae }T T{ $6a22 M $6a23 M $6a24 M $6a25 M $87d1 M -> $8c $d1 $87 $81 $aa }T
$10dd >PC $35 >S $ec >A $f2 >X $5a >Y $20 >P $10dd $8c >M $10de $2f >M $10df $94 >M $10e0 $3d >M
T{ op PC S A X Y P -> $10e0 $35 $ec $f2 $5a $20 }T T{ $10dd M $10de M $10df M $10e0 M $942f M -> $8c $2f $94 $3d $5a }T
$fee3 >PC $1c >S $cd >A $73 >X $20 >Y $a1 >P $fee3 $8c >M $fee4 $13 >M $fee5 $35 >M $fee6 $92 >M
T{ op PC S A X Y P -> $fee6 $1c $cd $73 $20 $a1 }T T{ $3513 M $fee3 M $fee4 M $fee5 M $fee6 M -> $20 $8c $13 $35 $92 }T
$2fc9 >PC $13 >S $7c >A $0c >X $02 >Y $a6 >P $2fc9 $8c >M $2fca $8c >M $2fcb $b3 >M $2fcc $95 >M
T{ op PC S A X Y P -> $2fcc $13 $7c $0c $02 $a6 }T T{ $2fc9 M $2fca M $2fcb M $2fcc M $b38c M -> $8c $8c $b3 $95 $02 }T
$c593 >PC $76 >S $64 >A $c7 >X $0b >Y $a0 >P $c593 $8c >M $c594 $da >M $c595 $d6 >M $c596 $ed >M
T{ op PC S A X Y P -> $c596 $76 $64 $c7 $0b $a0 }T T{ $c593 M $c594 M $c595 M $c596 M $d6da M -> $8c $da $d6 $ed $0b }T
$c717 >PC $e4 >S $9a >A $17 >X $18 >Y $e2 >P $c717 $8c >M $c718 $57 >M $c719 $55 >M $c71a $27 >M
T{ op PC S A X Y P -> $c71a $e4 $9a $17 $18 $e2 }T T{ $5557 M $c717 M $c718 M $c719 M $c71a M -> $18 $8c $57 $55 $27 }T
$9200 >PC $84 >S $9c >A $55 >X $fd >Y $2c >P $9200 $8c >M $9201 $94 >M $9202 $60 >M $9203 $c5 >M
T{ op PC S A X Y P -> $9203 $84 $9c $55 $fd $2c }T T{ $6094 M $9200 M $9201 M $9202 M $9203 M -> $fd $8c $94 $60 $c5 }T
$9840 >PC $c2 >S $18 >A $f5 >X $de >Y $af >P $9840 $8c >M $9841 $79 >M $9842 $84 >M $9843 $dd >M
T{ op PC S A X Y P -> $9843 $c2 $18 $f5 $de $af }T T{ $8479 M $9840 M $9841 M $9842 M $9843 M -> $de $8c $79 $84 $dd }T
$27d5 >PC $4a >S $d3 >A $89 >X $5a >Y $ec >P $27d5 $8c >M $27d6 $2b >M $27d7 $ea >M $27d8 $93 >M
T{ op PC S A X Y P -> $27d8 $4a $d3 $89 $5a $ec }T T{ $27d5 M $27d6 M $27d7 M $27d8 M $ea2b M -> $8c $2b $ea $93 $5a }T
$34c1 >PC $7d >S $6a >A $96 >X $ab >Y $a6 >P $34c1 $8c >M $34c2 $01 >M $34c3 $48 >M $34c4 $fc >M
T{ op PC S A X Y P -> $34c4 $7d $6a $96 $ab $a6 }T T{ $34c1 M $34c2 M $34c3 M $34c4 M $4801 M -> $8c $01 $48 $fc $ab }T
( 8d )
$08a2 >PC $77 >S $1c >A $16 >X $14 >Y $61 >P $08a2 $8d >M $08a3 $c0 >M $08a4 $2f >M $08a5 $b8 >M
T{ op PC S A X Y P -> $08a5 $77 $1c $16 $14 $61 }T T{ $08a2 M $08a3 M $08a4 M $08a5 M $2fc0 M -> $8d $c0 $2f $b8 $1c }T
$9901 >PC $0a >S $8a >A $24 >X $5e >Y $e3 >P $9901 $8d >M $9902 $a1 >M $9903 $8d >M $9904 $bf >M
T{ op PC S A X Y P -> $9904 $0a $8a $24 $5e $e3 }T T{ $8da1 M $9901 M $9902 M $9903 M $9904 M -> $8a $8d $a1 $8d $bf }T
$55fb >PC $47 >S $fb >A $00 >X $61 >Y $6b >P $55fb $8d >M $55fc $ea >M $55fd $87 >M $55fe $91 >M
T{ op PC S A X Y P -> $55fe $47 $fb $00 $61 $6b }T T{ $55fb M $55fc M $55fd M $55fe M $87ea M -> $8d $ea $87 $91 $fb }T
$fea6 >PC $6a >S $1a >A $46 >X $40 >Y $ec >P $fea6 $8d >M $fea7 $99 >M $fea8 $39 >M $fea9 $c5 >M
T{ op PC S A X Y P -> $fea9 $6a $1a $46 $40 $ec }T T{ $3999 M $fea6 M $fea7 M $fea8 M $fea9 M -> $1a $8d $99 $39 $c5 }T
$17c4 >PC $63 >S $97 >A $18 >X $7c >Y $67 >P $17c4 $8d >M $17c5 $fa >M $17c6 $30 >M $17c7 $61 >M
T{ op PC S A X Y P -> $17c7 $63 $97 $18 $7c $67 }T T{ $17c4 M $17c5 M $17c6 M $17c7 M $30fa M -> $8d $fa $30 $61 $97 }T
$6865 >PC $2c >S $a6 >A $c0 >X $f1 >Y $29 >P $6865 $8d >M $6866 $42 >M $6867 $fd >M $6868 $83 >M
T{ op PC S A X Y P -> $6868 $2c $a6 $c0 $f1 $29 }T T{ $6865 M $6866 M $6867 M $6868 M $fd42 M -> $8d $42 $fd $83 $a6 }T
$608b >PC $3b >S $d7 >A $45 >X $e9 >Y $27 >P $608b $8d >M $608c $4e >M $608d $eb >M $608e $aa >M
T{ op PC S A X Y P -> $608e $3b $d7 $45 $e9 $27 }T T{ $608b M $608c M $608d M $608e M $eb4e M -> $8d $4e $eb $aa $d7 }T
$ae0f >PC $b4 >S $88 >A $59 >X $98 >Y $26 >P $ae0f $8d >M $ae10 $d7 >M $ae11 $6c >M $ae12 $79 >M
T{ op PC S A X Y P -> $ae12 $b4 $88 $59 $98 $26 }T T{ $6cd7 M $ae0f M $ae10 M $ae11 M $ae12 M -> $88 $8d $d7 $6c $79 }T
$de3a >PC $e8 >S $ef >A $b6 >X $8d >Y $60 >P $de3a $8d >M $de3b $57 >M $de3c $48 >M $de3d $1a >M
T{ op PC S A X Y P -> $de3d $e8 $ef $b6 $8d $60 }T T{ $4857 M $de3a M $de3b M $de3c M $de3d M -> $ef $8d $57 $48 $1a }T
$cc60 >PC $0e >S $77 >A $f8 >X $38 >Y $a3 >P $cc60 $8d >M $cc61 $6f >M $cc62 $27 >M $cc63 $12 >M
T{ op PC S A X Y P -> $cc63 $0e $77 $f8 $38 $a3 }T T{ $276f M $cc60 M $cc61 M $cc62 M $cc63 M -> $77 $8d $6f $27 $12 }T
$368e >PC $4d >S $6b >A $59 >X $e5 >Y $6e >P $368e $8d >M $368f $f9 >M $3690 $ad >M $3691 $b3 >M
T{ op PC S A X Y P -> $3691 $4d $6b $59 $e5 $6e }T T{ $368e M $368f M $3690 M $3691 M $adf9 M -> $8d $f9 $ad $b3 $6b }T
$f5bd >PC $68 >S $55 >A $30 >X $4e >Y $27 >P $f5bd $8d >M $f5be $4c >M $f5bf $06 >M $f5c0 $e9 >M
T{ op PC S A X Y P -> $f5c0 $68 $55 $30 $4e $27 }T T{ $064c M $f5bd M $f5be M $f5bf M $f5c0 M -> $55 $8d $4c $06 $e9 }T
$24e6 >PC $69 >S $59 >A $ac >X $4b >Y $29 >P $24e6 $8d >M $24e7 $30 >M $24e8 $61 >M $24e9 $49 >M
T{ op PC S A X Y P -> $24e9 $69 $59 $ac $4b $29 }T T{ $24e6 M $24e7 M $24e8 M $24e9 M $6130 M -> $8d $30 $61 $49 $59 }T
$0027 >PC $d3 >S $b2 >A $0b >X $cc >Y $aa >P $0027 $8d >M $0028 $e1 >M $0029 $00 >M $002a $ca >M
T{ op PC S A X Y P -> $002a $d3 $b2 $0b $cc $aa }T T{ $0027 M $0028 M $0029 M $002a M $00e1 M -> $8d $e1 $00 $ca $b2 }T
$197f >PC $a6 >S $89 >A $d7 >X $44 >Y $a1 >P $197f $8d >M $1980 $0f >M $1981 $99 >M $1982 $4d >M
T{ op PC S A X Y P -> $1982 $a6 $89 $d7 $44 $a1 }T T{ $197f M $1980 M $1981 M $1982 M $990f M -> $8d $0f $99 $4d $89 }T
$b345 >PC $4b >S $a9 >A $e7 >X $32 >Y $68 >P $b345 $8d >M $b346 $53 >M $b347 $37 >M $b348 $d1 >M
T{ op PC S A X Y P -> $b348 $4b $a9 $e7 $32 $68 }T T{ $3753 M $b345 M $b346 M $b347 M $b348 M -> $a9 $8d $53 $37 $d1 }T
( 8e )
$b14e >PC $98 >S $de >A $58 >X $fb >Y $a5 >P $b14e $8e >M $b14f $15 >M $b150 $8f >M $b151 $15 >M
T{ op PC S A X Y P -> $b151 $98 $de $58 $fb $a5 }T T{ $8f15 M $b14e M $b14f M $b150 M $b151 M -> $58 $8e $15 $8f $15 }T
$7bdb >PC $9e >S $92 >A $68 >X $84 >Y $ee >P $7bdb $8e >M $7bdc $4b >M $7bdd $ff >M $7bde $63 >M
T{ op PC S A X Y P -> $7bde $9e $92 $68 $84 $ee }T T{ $7bdb M $7bdc M $7bdd M $7bde M $ff4b M -> $8e $4b $ff $63 $68 }T
$1f82 >PC $ac >S $e0 >A $f2 >X $15 >Y $e9 >P $1f82 $8e >M $1f83 $44 >M $1f84 $3d >M $1f85 $6d >M
T{ op PC S A X Y P -> $1f85 $ac $e0 $f2 $15 $e9 }T T{ $1f82 M $1f83 M $1f84 M $1f85 M $3d44 M -> $8e $44 $3d $6d $f2 }T
$5283 >PC $34 >S $f3 >A $6a >X $46 >Y $23 >P $5283 $8e >M $5284 $ac >M $5285 $43 >M $5286 $19 >M
T{ op PC S A X Y P -> $5286 $34 $f3 $6a $46 $23 }T T{ $43ac M $5283 M $5284 M $5285 M $5286 M -> $6a $8e $ac $43 $19 }T
$c441 >PC $e9 >S $89 >A $f4 >X $fb >Y $a9 >P $c441 $8e >M $c442 $3b >M $c443 $54 >M $c444 $8f >M
T{ op PC S A X Y P -> $c444 $e9 $89 $f4 $fb $a9 }T T{ $543b M $c441 M $c442 M $c443 M $c444 M -> $f4 $8e $3b $54 $8f }T
$c4de >PC $c0 >S $eb >A $cc >X $a4 >Y $2d >P $c4de $8e >M $c4df $02 >M $c4e0 $a8 >M $c4e1 $e7 >M
T{ op PC S A X Y P -> $c4e1 $c0 $eb $cc $a4 $2d }T T{ $a802 M $c4de M $c4df M $c4e0 M $c4e1 M -> $cc $8e $02 $a8 $e7 }T
$8b93 >PC $f9 >S $3c >A $74 >X $47 >Y $ae >P $8b93 $8e >M $8b94 $e1 >M $8b95 $c1 >M $8b96 $a7 >M
T{ op PC S A X Y P -> $8b96 $f9 $3c $74 $47 $ae }T T{ $8b93 M $8b94 M $8b95 M $8b96 M $c1e1 M -> $8e $e1 $c1 $a7 $74 }T
$0453 >PC $c3 >S $3c >A $f5 >X $2c >Y $21 >P $0453 $8e >M $0454 $6f >M $0455 $e4 >M $0456 $73 >M
T{ op PC S A X Y P -> $0456 $c3 $3c $f5 $2c $21 }T T{ $0453 M $0454 M $0455 M $0456 M $e46f M -> $8e $6f $e4 $73 $f5 }T
$a534 >PC $89 >S $af >A $ae >X $af >Y $2b >P $a534 $8e >M $a535 $78 >M $a536 $22 >M $a537 $01 >M
T{ op PC S A X Y P -> $a537 $89 $af $ae $af $2b }T T{ $2278 M $a534 M $a535 M $a536 M $a537 M -> $ae $8e $78 $22 $01 }T
$b1fe >PC $f9 >S $3c >A $0f >X $7d >Y $66 >P $b1fe $8e >M $b1ff $8a >M $b200 $90 >M $b201 $3a >M
T{ op PC S A X Y P -> $b201 $f9 $3c $0f $7d $66 }T T{ $908a M $b1fe M $b1ff M $b200 M $b201 M -> $0f $8e $8a $90 $3a }T
$9b56 >PC $c1 >S $42 >A $2d >X $33 >Y $a7 >P $9b56 $8e >M $9b57 $d3 >M $9b58 $66 >M $9b59 $d3 >M
T{ op PC S A X Y P -> $9b59 $c1 $42 $2d $33 $a7 }T T{ $66d3 M $9b56 M $9b57 M $9b58 M $9b59 M -> $2d $8e $d3 $66 $d3 }T
$2c2c >PC $fd >S $23 >A $ba >X $2c >Y $2b >P $2c2c $8e >M $2c2d $7f >M $2c2e $5a >M $2c2f $1e >M
T{ op PC S A X Y P -> $2c2f $fd $23 $ba $2c $2b }T T{ $2c2c M $2c2d M $2c2e M $2c2f M $5a7f M -> $8e $7f $5a $1e $ba }T
$9e10 >PC $af >S $a2 >A $b0 >X $73 >Y $28 >P $9e10 $8e >M $9e11 $6b >M $9e12 $2d >M $9e13 $d3 >M
T{ op PC S A X Y P -> $9e13 $af $a2 $b0 $73 $28 }T T{ $2d6b M $9e10 M $9e11 M $9e12 M $9e13 M -> $b0 $8e $6b $2d $d3 }T
$03ec >PC $ac >S $fd >A $fe >X $82 >Y $e7 >P $03ec $8e >M $03ed $b1 >M $03ee $c2 >M $03ef $f9 >M
T{ op PC S A X Y P -> $03ef $ac $fd $fe $82 $e7 }T T{ $03ec M $03ed M $03ee M $03ef M $c2b1 M -> $8e $b1 $c2 $f9 $fe }T
$e9b2 >PC $bf >S $3e >A $80 >X $6b >Y $a2 >P $e9b2 $8e >M $e9b3 $ac >M $e9b4 $1f >M $e9b5 $0a >M
T{ op PC S A X Y P -> $e9b5 $bf $3e $80 $6b $a2 }T T{ $1fac M $e9b2 M $e9b3 M $e9b4 M $e9b5 M -> $80 $8e $ac $1f $0a }T
$4de8 >PC $f2 >S $82 >A $61 >X $4a >Y $6b >P $4de8 $8e >M $4de9 $48 >M $4dea $07 >M $4deb $05 >M
T{ op PC S A X Y P -> $4deb $f2 $82 $61 $4a $6b }T T{ $0748 M $4de8 M $4de9 M $4dea M $4deb M -> $61 $8e $48 $07 $05 }T
( 8f )
$c244 >PC $a5 >S $32 >A $14 >X $f4 >Y $e9 >P $00b5 $8c >M $c244 $8f >M $c245 $b5 >M $c246 $9c >M $c247 $92 >M $c2e3 $5b >M
T{ op PC S A X Y P -> $c247 $a5 $32 $14 $f4 $e9 }T T{ $00b5 M $c244 M $c245 M $c246 M $c247 M $c2e3 M -> $8c $8f $b5 $9c $92 $5b }T
$7a77 >PC $e3 >S $7d >A $b5 >X $fb >Y $a7 >P $00dd $38 >M $7a5b $0c >M $7a77 $8f >M $7a78 $dd >M $7a79 $e1 >M $7a7a $e4 >M
T{ op PC S A X Y P -> $7a7a $e3 $7d $b5 $fb $a7 }T T{ $00dd M $7a5b M $7a77 M $7a78 M $7a79 M $7a7a M -> $38 $0c $8f $dd $e1 $e4 }T
$8323 >PC $25 >S $ba >A $4f >X $7d >Y $a3 >P $002f $4e >M $8323 $8f >M $8324 $2f >M $8325 $07 >M $8326 $2e >M $832d $7c >M
T{ op PC S A X Y P -> $8326 $25 $ba $4f $7d $a3 }T T{ $002f M $8323 M $8324 M $8325 M $8326 M $832d M -> $4e $8f $2f $07 $2e $7c }T
$ffe4 >PC $a3 >S $11 >A $2e >X $1d >Y $a5 >P $004d $a0 >M $ffe4 $8f >M $ffe5 $4d >M $ffe6 $0b >M $ffe7 $d1 >M $fff2 $29 >M
T{ op PC S A X Y P -> $ffe7 $a3 $11 $2e $1d $a5 }T T{ $004d M $ffe4 M $ffe5 M $ffe6 M $ffe7 M $fff2 M -> $a0 $8f $4d $0b $d1 $29 }T
$c7d0 >PC $33 >S $1e >A $cf >X $83 >Y $25 >P $006f $4a >M $c735 $91 >M $c7d0 $8f >M $c7d1 $6f >M $c7d2 $62 >M $c7d3 $12 >M
T{ op PC S A X Y P -> $c7d3 $33 $1e $cf $83 $25 }T T{ $006f M $c735 M $c7d0 M $c7d1 M $c7d2 M $c7d3 M -> $4a $91 $8f $6f $62 $12 }T
$af90 >PC $dd >S $52 >A $1e >X $01 >Y $67 >P $00b1 $3c >M $af84 $32 >M $af90 $8f >M $af91 $b1 >M $af92 $f1 >M $af93 $6f >M
T{ op PC S A X Y P -> $af93 $dd $52 $1e $01 $67 }T T{ $00b1 M $af84 M $af90 M $af91 M $af92 M $af93 M -> $3c $32 $8f $b1 $f1 $6f }T
$4aee >PC $61 >S $69 >A $f0 >X $fc >Y $ae >P $0072 $d2 >M $4a9e $ed >M $4aee $8f >M $4aef $72 >M $4af0 $ad >M $4af1 $c6 >M
T{ op PC S A X Y P -> $4af1 $61 $69 $f0 $fc $ae }T T{ $0072 M $4a9e M $4aee M $4aef M $4af0 M $4af1 M -> $d2 $ed $8f $72 $ad $c6 }T
$b56c >PC $83 >S $2a >A $fb >X $62 >Y $a3 >P $00fc $b1 >M $b546 $cb >M $b56c $8f >M $b56d $fc >M $b56e $d7 >M
T{ op PC S A X Y P -> $b546 $83 $2a $fb $62 $a3 }T T{ $00fc M $b546 M $b56c M $b56d M $b56e M -> $b1 $cb $8f $fc $d7 }T
$b1b3 >PC $e2 >S $e8 >A $0f >X $95 >Y $ef >P $0010 $ff >M $b150 $4c >M $b1b3 $8f >M $b1b4 $10 >M $b1b5 $9a >M
T{ op PC S A X Y P -> $b150 $e2 $e8 $0f $95 $ef }T T{ $0010 M $b150 M $b1b3 M $b1b4 M $b1b5 M -> $ff $4c $8f $10 $9a }T
$338a >PC $c6 >S $9b >A $5d >X $81 >Y $ad >P $00d9 $b9 >M $3357 $52 >M $338a $8f >M $338b $d9 >M $338c $ca >M
T{ op PC S A X Y P -> $3357 $c6 $9b $5d $81 $ad }T T{ $00d9 M $3357 M $338a M $338b M $338c M -> $b9 $52 $8f $d9 $ca }T
$9726 >PC $9b >S $37 >A $5f >X $8c >Y $e5 >P $000e $f4 >M $9726 $8f >M $9727 $0e >M $9728 $d1 >M $9729 $c5 >M $97fa $d3 >M
T{ op PC S A X Y P -> $9729 $9b $37 $5f $8c $e5 }T T{ $000e M $9726 M $9727 M $9728 M $9729 M $97fa M -> $f4 $8f $0e $d1 $c5 $d3 }T
$9a3b >PC $9a >S $30 >A $49 >X $d0 >Y $a0 >P $001a $e5 >M $9a3b $8f >M $9a3c $1a >M $9a3d $46 >M $9a84 $e4 >M
T{ op PC S A X Y P -> $9a84 $9a $30 $49 $d0 $a0 }T T{ $001a M $9a3b M $9a3c M $9a3d M $9a84 M -> $e5 $8f $1a $46 $e4 }T
$a4c1 >PC $cf >S $6d >A $74 >X $40 >Y $e2 >P $0003 $41 >M $a428 $ee >M $a4c1 $8f >M $a4c2 $03 >M $a4c3 $64 >M $a528 $d7 >M
T{ op PC S A X Y P -> $a528 $cf $6d $74 $40 $e2 }T T{ $0003 M $a428 M $a4c1 M $a4c2 M $a4c3 M $a528 M -> $41 $ee $8f $03 $64 $d7 }T
$5d68 >PC $00 >S $e8 >A $29 >X $f1 >Y $ee >P $00a3 $44 >M $5d68 $8f >M $5d69 $a3 >M $5d6a $85 >M $5d6b $15 >M $5df0 $98 >M
T{ op PC S A X Y P -> $5d6b $00 $e8 $29 $f1 $ee }T T{ $00a3 M $5d68 M $5d69 M $5d6a M $5d6b M $5df0 M -> $44 $8f $a3 $85 $15 $98 }T
$81fd >PC $e7 >S $9d >A $d9 >X $a4 >Y $66 >P $00f8 $52 >M $81fd $8f >M $81fe $f8 >M $81ff $42 >M $8200 $60 >M $8242 $d3 >M
T{ op PC S A X Y P -> $8200 $e7 $9d $d9 $a4 $66 }T T{ $00f8 M $81fd M $81fe M $81ff M $8200 M $8242 M -> $52 $8f $f8 $42 $60 $d3 }T
$f03a >PC $bf >S $18 >A $6f >X $04 >Y $21 >P $0018 $4c >M $f03a $8f >M $f03b $18 >M $f03c $3b >M $f03d $3b >M $f078 $3b >M
T{ op PC S A X Y P -> $f03d $bf $18 $6f $04 $21 }T T{ $0018 M $f03a M $f03b M $f03c M $f03d M $f078 M -> $4c $8f $18 $3b $3b $3b }T
( 90 )
$4375 >PC $bf >S $79 >A $e1 >X $56 >Y $2d >P $4375 $90 >M $4376 $77 >M $4377 $50 >M
T{ op PC S A X Y P -> $4377 $bf $79 $e1 $56 $2d }T T{ $4375 M $4376 M $4377 M -> $90 $77 $50 }T
$b6a0 >PC $36 >S $23 >A $48 >X $0d >Y $61 >P $b6a0 $90 >M $b6a1 $72 >M $b6a2 $d1 >M
T{ op PC S A X Y P -> $b6a2 $36 $23 $48 $0d $61 }T T{ $b6a0 M $b6a1 M $b6a2 M -> $90 $72 $d1 }T
$8098 >PC $0c >S $11 >A $fb >X $22 >Y $ac >P $8001 $79 >M $8098 $90 >M $8099 $67 >M $809a $b6 >M $8101 $cc >M
T{ op PC S A X Y P -> $8101 $0c $11 $fb $22 $ac }T T{ $8001 M $8098 M $8099 M $809a M $8101 M -> $79 $90 $67 $b6 $cc }T
$59d9 >PC $ed >S $7f >A $95 >X $ef >Y $e5 >P $59d9 $90 >M $59da $87 >M $59db $92 >M
T{ op PC S A X Y P -> $59db $ed $7f $95 $ef $e5 }T T{ $59d9 M $59da M $59db M -> $90 $87 $92 }T
$5299 >PC $c0 >S $e1 >A $c6 >X $14 >Y $ef >P $5299 $90 >M $529a $27 >M $529b $6e >M
T{ op PC S A X Y P -> $529b $c0 $e1 $c6 $14 $ef }T T{ $5299 M $529a M $529b M -> $90 $27 $6e }T
$fa3b >PC $4d >S $fc >A $69 >X $6e >Y $e5 >P $fa3b $90 >M $fa3c $fd >M $fa3d $6b >M
T{ op PC S A X Y P -> $fa3d $4d $fc $69 $6e $e5 }T T{ $fa3b M $fa3c M $fa3d M -> $90 $fd $6b }T
$e730 >PC $fe >S $a3 >A $63 >X $ad >Y $2b >P $e730 $90 >M $e731 $c7 >M $e732 $ef >M
T{ op PC S A X Y P -> $e732 $fe $a3 $63 $ad $2b }T T{ $e730 M $e731 M $e732 M -> $90 $c7 $ef }T
$2d26 >PC $a7 >S $bc >A $51 >X $36 >Y $aa >P $2d01 $12 >M $2d26 $90 >M $2d27 $d9 >M $2d28 $38 >M
T{ op PC S A X Y P -> $2d01 $a7 $bc $51 $36 $aa }T T{ $2d01 M $2d26 M $2d27 M $2d28 M -> $12 $90 $d9 $38 }T
$9090 >PC $bb >S $fe >A $38 >X $bf >Y $e4 >P $9090 $90 >M $9091 $01 >M $9092 $fe >M $9093 $54 >M
T{ op PC S A X Y P -> $9093 $bb $fe $38 $bf $e4 }T T{ $9090 M $9091 M $9092 M $9093 M -> $90 $01 $fe $54 }T
$fd66 >PC $e8 >S $e2 >A $e5 >X $21 >Y $a7 >P $fd66 $90 >M $fd67 $28 >M $fd68 $a6 >M
T{ op PC S A X Y P -> $fd68 $e8 $e2 $e5 $21 $a7 }T T{ $fd66 M $fd67 M $fd68 M -> $90 $28 $a6 }T
$6593 >PC $44 >S $e8 >A $4b >X $44 >Y $a9 >P $6593 $90 >M $6594 $ac >M $6595 $4d >M
T{ op PC S A X Y P -> $6595 $44 $e8 $4b $44 $a9 }T T{ $6593 M $6594 M $6595 M -> $90 $ac $4d }T
$b930 >PC $6e >S $de >A $6e >X $b0 >Y $2f >P $b930 $90 >M $b931 $f6 >M $b932 $f3 >M
T{ op PC S A X Y P -> $b932 $6e $de $6e $b0 $2f }T T{ $b930 M $b931 M $b932 M -> $90 $f6 $f3 }T
$4a39 >PC $df >S $5e >A $90 >X $c5 >Y $e9 >P $4a39 $90 >M $4a3a $37 >M $4a3b $08 >M
T{ op PC S A X Y P -> $4a3b $df $5e $90 $c5 $e9 }T T{ $4a39 M $4a3a M $4a3b M -> $90 $37 $08 }T
$8ad5 >PC $2c >S $b8 >A $e2 >X $de >Y $a1 >P $8ad5 $90 >M $8ad6 $ce >M $8ad7 $d8 >M
T{ op PC S A X Y P -> $8ad7 $2c $b8 $e2 $de $a1 }T T{ $8ad5 M $8ad6 M $8ad7 M -> $90 $ce $d8 }T
$45f8 >PC $9d >S $00 >A $8b >X $fd >Y $a7 >P $45f8 $90 >M $45f9 $6d >M $45fa $74 >M
T{ op PC S A X Y P -> $45fa $9d $00 $8b $fd $a7 }T T{ $45f8 M $45f9 M $45fa M -> $90 $6d $74 }T
$e0a0 >PC $2f >S $aa >A $76 >X $3d >Y $ef >P $e0a0 $90 >M $e0a1 $19 >M $e0a2 $87 >M
T{ op PC S A X Y P -> $e0a2 $2f $aa $76 $3d $ef }T T{ $e0a0 M $e0a1 M $e0a2 M -> $90 $19 $87 }T
( 91 )
$d800 >PC $4d >S $9c >A $dd >X $aa >Y $ed >P $00e0 $70 >M $00e1 $55 >M $d800 $91 >M $d801 $e0 >M $d802 $fd >M
T{ op PC S A X Y P -> $d802 $4d $9c $dd $aa $ed }T T{ $00e0 M $00e1 M $561a M $d800 M $d801 M $d802 M -> $70 $55 $9c $91 $e0 $fd }T
$e841 >PC $30 >S $07 >A $60 >X $be >Y $a7 >P $00e4 $79 >M $00e5 $11 >M $e841 $91 >M $e842 $e4 >M $e843 $df >M
T{ op PC S A X Y P -> $e843 $30 $07 $60 $be $a7 }T T{ $00e4 M $00e5 M $1237 M $e841 M $e842 M $e843 M -> $79 $11 $07 $91 $e4 $df }T
$132b >PC $3b >S $6a >A $bd >X $1a >Y $6e >P $0028 $6f >M $0029 $3a >M $132b $91 >M $132c $28 >M $132d $e8 >M
T{ op PC S A X Y P -> $132d $3b $6a $bd $1a $6e }T T{ $0028 M $0029 M $132b M $132c M $132d M $3a89 M -> $6f $3a $91 $28 $e8 $6a }T
$8686 >PC $8c >S $cc >A $8f >X $30 >Y $64 >P $0068 $2b >M $0069 $a1 >M $8686 $91 >M $8687 $68 >M $8688 $8a >M
T{ op PC S A X Y P -> $8688 $8c $cc $8f $30 $64 }T T{ $0068 M $0069 M $8686 M $8687 M $8688 M $a15b M -> $2b $a1 $91 $68 $8a $cc }T
$b371 >PC $a4 >S $a2 >A $2c >X $a1 >Y $e7 >P $00f7 $18 >M $00f8 $a1 >M $b371 $91 >M $b372 $f7 >M $b373 $09 >M
T{ op PC S A X Y P -> $b373 $a4 $a2 $2c $a1 $e7 }T T{ $00f7 M $00f8 M $a1b9 M $b371 M $b372 M $b373 M -> $18 $a1 $a2 $91 $f7 $09 }T
$88e0 >PC $36 >S $bc >A $53 >X $28 >Y $eb >P $0058 $7d >M $0059 $b6 >M $88e0 $91 >M $88e1 $58 >M $88e2 $59 >M
T{ op PC S A X Y P -> $88e2 $36 $bc $53 $28 $eb }T T{ $0058 M $0059 M $88e0 M $88e1 M $88e2 M $b6a5 M -> $7d $b6 $91 $58 $59 $bc }T
$70fb >PC $32 >S $2a >A $cc >X $e5 >Y $a1 >P $008c $66 >M $008d $03 >M $70fb $91 >M $70fc $8c >M $70fd $aa >M
T{ op PC S A X Y P -> $70fd $32 $2a $cc $e5 $a1 }T T{ $008c M $008d M $044b M $70fb M $70fc M $70fd M -> $66 $03 $2a $91 $8c $aa }T
$52ad >PC $67 >S $df >A $05 >X $aa >Y $2f >P $00ea $00 >M $00eb $e3 >M $52ad $91 >M $52ae $ea >M $52af $4c >M
T{ op PC S A X Y P -> $52af $67 $df $05 $aa $2f }T T{ $00ea M $00eb M $52ad M $52ae M $52af M $e3aa M -> $00 $e3 $91 $ea $4c $df }T
$437b >PC $a6 >S $91 >A $98 >X $d5 >Y $6c >P $00bb $0b >M $00bc $2b >M $437b $91 >M $437c $bb >M $437d $69 >M
T{ op PC S A X Y P -> $437d $a6 $91 $98 $d5 $6c }T T{ $00bb M $00bc M $2be0 M $437b M $437c M $437d M -> $0b $2b $91 $91 $bb $69 }T
$289e >PC $88 >S $fe >A $bd >X $b4 >Y $2f >P $00cc $1d >M $00cd $1e >M $289e $91 >M $289f $cc >M $28a0 $a5 >M
T{ op PC S A X Y P -> $28a0 $88 $fe $bd $b4 $2f }T T{ $00cc M $00cd M $1ed1 M $289e M $289f M $28a0 M -> $1d $1e $fe $91 $cc $a5 }T
$76fa >PC $23 >S $05 >A $42 >X $94 >Y $63 >P $0082 $dd >M $0083 $b7 >M $76fa $91 >M $76fb $82 >M $76fc $a9 >M
T{ op PC S A X Y P -> $76fc $23 $05 $42 $94 $63 }T T{ $0082 M $0083 M $76fa M $76fb M $76fc M $b871 M -> $dd $b7 $91 $82 $a9 $05 }T
$e25f >PC $79 >S $8d >A $bb >X $fe >Y $ef >P $00fa $52 >M $00fb $88 >M $e25f $91 >M $e260 $fa >M $e261 $6b >M
T{ op PC S A X Y P -> $e261 $79 $8d $bb $fe $ef }T T{ $00fa M $00fb M $8950 M $e25f M $e260 M $e261 M -> $52 $88 $8d $91 $fa $6b }T
$3a43 >PC $73 >S $ba >A $bc >X $68 >Y $2a >P $00f8 $b8 >M $00f9 $3d >M $3a43 $91 >M $3a44 $f8 >M $3a45 $c9 >M
T{ op PC S A X Y P -> $3a45 $73 $ba $bc $68 $2a }T T{ $00f8 M $00f9 M $3a43 M $3a44 M $3a45 M $3e20 M -> $b8 $3d $91 $f8 $c9 $ba }T
$9100 >PC $c9 >S $5f >A $b3 >X $d4 >Y $ee >P $001a $2f >M $001b $97 >M $9100 $91 >M $9101 $1a >M $9102 $5c >M
T{ op PC S A X Y P -> $9102 $c9 $5f $b3 $d4 $ee }T T{ $001a M $001b M $9100 M $9101 M $9102 M $9803 M -> $2f $97 $91 $1a $5c $5f }T
$93f4 >PC $79 >S $5d >A $15 >X $62 >Y $2e >P $0098 $0b >M $0099 $44 >M $93f4 $91 >M $93f5 $98 >M $93f6 $91 >M
T{ op PC S A X Y P -> $93f6 $79 $5d $15 $62 $2e }T T{ $0098 M $0099 M $446d M $93f4 M $93f5 M $93f6 M -> $0b $44 $5d $91 $98 $91 }T
$e446 >PC $67 >S $92 >A $7b >X $a9 >Y $67 >P $0018 $46 >M $0019 $69 >M $e446 $91 >M $e447 $18 >M $e448 $a2 >M
T{ op PC S A X Y P -> $e448 $67 $92 $7b $a9 $67 }T T{ $0018 M $0019 M $69ef M $e446 M $e447 M $e448 M -> $46 $69 $92 $91 $18 $a2 }T
( 92 )
$59d1 >PC $67 >S $58 >A $f6 >X $e8 >Y $e9 >P $00e7 $cc >M $00e8 $56 >M $59d1 $92 >M $59d2 $e7 >M $59d3 $e8 >M
T{ op PC S A X Y P -> $59d3 $67 $58 $f6 $e8 $e9 }T T{ $00e7 M $00e8 M $56cc M $59d1 M $59d2 M $59d3 M -> $cc $56 $58 $92 $e7 $e8 }T
$c42d >PC $ef >S $cc >A $15 >X $31 >Y $2e >P $0081 $a8 >M $0082 $88 >M $c42d $92 >M $c42e $81 >M $c42f $a1 >M
T{ op PC S A X Y P -> $c42f $ef $cc $15 $31 $2e }T T{ $0081 M $0082 M $88a8 M $c42d M $c42e M $c42f M -> $a8 $88 $cc $92 $81 $a1 }T
$dd30 >PC $d6 >S $6d >A $70 >X $ca >Y $e9 >P $0031 $27 >M $0032 $40 >M $dd30 $92 >M $dd31 $31 >M $dd32 $61 >M
T{ op PC S A X Y P -> $dd32 $d6 $6d $70 $ca $e9 }T T{ $0031 M $0032 M $4027 M $dd30 M $dd31 M $dd32 M -> $27 $40 $6d $92 $31 $61 }T
$6289 >PC $a3 >S $80 >A $2d >X $d2 >Y $a4 >P $009e $50 >M $009f $ab >M $6289 $92 >M $628a $9e >M $628b $f8 >M
T{ op PC S A X Y P -> $628b $a3 $80 $2d $d2 $a4 }T T{ $009e M $009f M $6289 M $628a M $628b M $ab50 M -> $50 $ab $92 $9e $f8 $80 }T
$c039 >PC $61 >S $5d >A $34 >X $b4 >Y $6d >P $00d9 $63 >M $00da $63 >M $c039 $92 >M $c03a $d9 >M $c03b $80 >M
T{ op PC S A X Y P -> $c03b $61 $5d $34 $b4 $6d }T T{ $00d9 M $00da M $6363 M $c039 M $c03a M $c03b M -> $63 $63 $5d $92 $d9 $80 }T
$bd77 >PC $89 >S $45 >A $82 >X $39 >Y $28 >P $002c $41 >M $002d $f5 >M $bd77 $92 >M $bd78 $2c >M $bd79 $ab >M
T{ op PC S A X Y P -> $bd79 $89 $45 $82 $39 $28 }T T{ $002c M $002d M $bd77 M $bd78 M $bd79 M $f541 M -> $41 $f5 $92 $2c $ab $45 }T
$4e10 >PC $a7 >S $84 >A $b2 >X $08 >Y $68 >P $00b2 $c2 >M $00b3 $83 >M $4e10 $92 >M $4e11 $b2 >M $4e12 $0f >M
T{ op PC S A X Y P -> $4e12 $a7 $84 $b2 $08 $68 }T T{ $00b2 M $00b3 M $4e10 M $4e11 M $4e12 M $83c2 M -> $c2 $83 $92 $b2 $0f $84 }T
$dc2a >PC $b2 >S $25 >A $7e >X $ec >Y $27 >P $0089 $0a >M $008a $1a >M $dc2a $92 >M $dc2b $89 >M $dc2c $e3 >M
T{ op PC S A X Y P -> $dc2c $b2 $25 $7e $ec $27 }T T{ $0089 M $008a M $1a0a M $dc2a M $dc2b M $dc2c M -> $0a $1a $25 $92 $89 $e3 }T
$65db >PC $f2 >S $32 >A $53 >X $37 >Y $ec >P $0061 $05 >M $0062 $3e >M $65db $92 >M $65dc $61 >M $65dd $1c >M
T{ op PC S A X Y P -> $65dd $f2 $32 $53 $37 $ec }T T{ $0061 M $0062 M $3e05 M $65db M $65dc M $65dd M -> $05 $3e $32 $92 $61 $1c }T
$9d20 >PC $67 >S $2b >A $0d >X $7f >Y $6a >P $00ec $31 >M $00ed $6a >M $9d20 $92 >M $9d21 $ec >M $9d22 $23 >M
T{ op PC S A X Y P -> $9d22 $67 $2b $0d $7f $6a }T T{ $00ec M $00ed M $6a31 M $9d20 M $9d21 M $9d22 M -> $31 $6a $2b $92 $ec $23 }T
$16d1 >PC $b3 >S $3b >A $cd >X $d7 >Y $e3 >P $00fe $02 >M $00ff $a2 >M $16d1 $92 >M $16d2 $fe >M $16d3 $f1 >M
T{ op PC S A X Y P -> $16d3 $b3 $3b $cd $d7 $e3 }T T{ $00fe M $00ff M $16d1 M $16d2 M $16d3 M $a202 M -> $02 $a2 $92 $fe $f1 $3b }T
$9898 >PC $23 >S $ff >A $34 >X $ac >Y $e7 >P $0047 $aa >M $0048 $84 >M $9898 $92 >M $9899 $47 >M $989a $1c >M
T{ op PC S A X Y P -> $989a $23 $ff $34 $ac $e7 }T T{ $0047 M $0048 M $84aa M $9898 M $9899 M $989a M -> $aa $84 $ff $92 $47 $1c }T
$9024 >PC $68 >S $38 >A $3a >X $15 >Y $ea >P $0027 $77 >M $0028 $04 >M $9024 $92 >M $9025 $27 >M $9026 $66 >M
T{ op PC S A X Y P -> $9026 $68 $38 $3a $15 $ea }T T{ $0027 M $0028 M $0477 M $9024 M $9025 M $9026 M -> $77 $04 $38 $92 $27 $66 }T
$e3b3 >PC $97 >S $22 >A $37 >X $80 >Y $e3 >P $0085 $2f >M $0086 $fc >M $e3b3 $92 >M $e3b4 $85 >M $e3b5 $6e >M
T{ op PC S A X Y P -> $e3b5 $97 $22 $37 $80 $e3 }T T{ $0085 M $0086 M $e3b3 M $e3b4 M $e3b5 M $fc2f M -> $2f $fc $92 $85 $6e $22 }T
$d32b >PC $1c >S $03 >A $02 >X $4f >Y $20 >P $0052 $f0 >M $0053 $9b >M $d32b $92 >M $d32c $52 >M $d32d $5d >M
T{ op PC S A X Y P -> $d32d $1c $03 $02 $4f $20 }T T{ $0052 M $0053 M $9bf0 M $d32b M $d32c M $d32d M -> $f0 $9b $03 $92 $52 $5d }T
$e7cb >PC $2a >S $8d >A $c7 >X $90 >Y $a6 >P $00cd $90 >M $00ce $42 >M $e7cb $92 >M $e7cc $cd >M $e7cd $10 >M
T{ op PC S A X Y P -> $e7cd $2a $8d $c7 $90 $a6 }T T{ $00cd M $00ce M $4290 M $e7cb M $e7cc M $e7cd M -> $90 $42 $8d $92 $cd $10 }T
( 93 )
$a128 >PC $0d >S $2a >A $5f >X $32 >Y $e7 >P $a128 $93 >M $a129 $0b >M $a12a $d7 >M
T{ op PC S A X Y P -> $a129 $0d $2a $5f $32 $e7 }T T{ $a128 M $a129 M $a12a M -> $93 $0b $d7 }T
$da1d >PC $21 >S $ad >A $91 >X $f1 >Y $ab >P $da1d $93 >M $da1e $22 >M $da1f $81 >M
T{ op PC S A X Y P -> $da1e $21 $ad $91 $f1 $ab }T T{ $da1d M $da1e M $da1f M -> $93 $22 $81 }T
$4be7 >PC $36 >S $66 >A $ba >X $ee >Y $e2 >P $4be7 $93 >M $4be8 $74 >M $4be9 $13 >M
T{ op PC S A X Y P -> $4be8 $36 $66 $ba $ee $e2 }T T{ $4be7 M $4be8 M $4be9 M -> $93 $74 $13 }T
$7d1b >PC $db >S $a9 >A $94 >X $7d >Y $29 >P $7d1b $93 >M $7d1c $54 >M $7d1d $b4 >M
T{ op PC S A X Y P -> $7d1c $db $a9 $94 $7d $29 }T T{ $7d1b M $7d1c M $7d1d M -> $93 $54 $b4 }T
$5c95 >PC $9b >S $07 >A $6e >X $31 >Y $ea >P $5c95 $93 >M $5c96 $3f >M $5c97 $06 >M
T{ op PC S A X Y P -> $5c96 $9b $07 $6e $31 $ea }T T{ $5c95 M $5c96 M $5c97 M -> $93 $3f $06 }T
$40b2 >PC $55 >S $bd >A $a8 >X $e8 >Y $26 >P $40b2 $93 >M $40b3 $ff >M $40b4 $8d >M
T{ op PC S A X Y P -> $40b3 $55 $bd $a8 $e8 $26 }T T{ $40b2 M $40b3 M $40b4 M -> $93 $ff $8d }T
$70fb >PC $b1 >S $6d >A $a9 >X $5c >Y $6a >P $70fb $93 >M $70fc $a7 >M $70fd $87 >M
T{ op PC S A X Y P -> $70fc $b1 $6d $a9 $5c $6a }T T{ $70fb M $70fc M $70fd M -> $93 $a7 $87 }T
$f36f >PC $1b >S $13 >A $c9 >X $1b >Y $a3 >P $f36f $93 >M $f370 $51 >M $f371 $f2 >M
T{ op PC S A X Y P -> $f370 $1b $13 $c9 $1b $a3 }T T{ $f36f M $f370 M $f371 M -> $93 $51 $f2 }T
$84e6 >PC $e9 >S $ca >A $cb >X $75 >Y $69 >P $84e6 $93 >M $84e7 $89 >M $84e8 $a9 >M
T{ op PC S A X Y P -> $84e7 $e9 $ca $cb $75 $69 }T T{ $84e6 M $84e7 M $84e8 M -> $93 $89 $a9 }T
$d479 >PC $84 >S $9d >A $a7 >X $61 >Y $ed >P $d479 $93 >M $d47a $96 >M $d47b $da >M
T{ op PC S A X Y P -> $d47a $84 $9d $a7 $61 $ed }T T{ $d479 M $d47a M $d47b M -> $93 $96 $da }T
$0405 >PC $b1 >S $40 >A $f7 >X $49 >Y $68 >P $0405 $93 >M $0406 $ed >M $0407 $d4 >M
T{ op PC S A X Y P -> $0406 $b1 $40 $f7 $49 $68 }T T{ $0405 M $0406 M $0407 M -> $93 $ed $d4 }T
$0dfb >PC $da >S $7d >A $22 >X $56 >Y $6a >P $0dfb $93 >M $0dfc $31 >M $0dfd $25 >M
T{ op PC S A X Y P -> $0dfc $da $7d $22 $56 $6a }T T{ $0dfb M $0dfc M $0dfd M -> $93 $31 $25 }T
$8b80 >PC $41 >S $a7 >A $1e >X $82 >Y $a5 >P $8b80 $93 >M $8b81 $80 >M $8b82 $57 >M
T{ op PC S A X Y P -> $8b81 $41 $a7 $1e $82 $a5 }T T{ $8b80 M $8b81 M $8b82 M -> $93 $80 $57 }T
$10b9 >PC $3e >S $7c >A $58 >X $b8 >Y $ec >P $10b9 $93 >M $10ba $75 >M $10bb $5a >M
T{ op PC S A X Y P -> $10ba $3e $7c $58 $b8 $ec }T T{ $10b9 M $10ba M $10bb M -> $93 $75 $5a }T
$62ec >PC $f4 >S $d6 >A $8f >X $b6 >Y $aa >P $62ec $93 >M $62ed $c8 >M $62ee $e7 >M
T{ op PC S A X Y P -> $62ed $f4 $d6 $8f $b6 $aa }T T{ $62ec M $62ed M $62ee M -> $93 $c8 $e7 }T
$4e47 >PC $c8 >S $0d >A $be >X $74 >Y $2f >P $4e47 $93 >M $4e48 $2a >M $4e49 $de >M
T{ op PC S A X Y P -> $4e48 $c8 $0d $be $74 $2f }T T{ $4e47 M $4e48 M $4e49 M -> $93 $2a $de }T
( 94 )
$e309 >PC $1f >S $ef >A $d9 >X $7f >Y $2c >P $0094 $a9 >M $e309 $94 >M $e30a $94 >M $e30b $df >M
T{ op PC S A X Y P -> $e30b $1f $ef $d9 $7f $2c }T T{ $006d M $0094 M $e309 M $e30a M $e30b M -> $7f $a9 $94 $94 $df }T
$5210 >PC $aa >S $fa >A $0d >X $9e >Y $a5 >P $0076 $9a >M $5210 $94 >M $5211 $76 >M $5212 $6a >M
T{ op PC S A X Y P -> $5212 $aa $fa $0d $9e $a5 }T T{ $0076 M $0083 M $5210 M $5211 M $5212 M -> $9a $9e $94 $76 $6a }T
$c2b5 >PC $06 >S $4e >A $ca >X $78 >Y $67 >P $00d8 $ee >M $c2b5 $94 >M $c2b6 $d8 >M $c2b7 $3a >M
T{ op PC S A X Y P -> $c2b7 $06 $4e $ca $78 $67 }T T{ $00a2 M $00d8 M $c2b5 M $c2b6 M $c2b7 M -> $78 $ee $94 $d8 $3a }T
$3865 >PC $f1 >S $54 >A $83 >X $85 >Y $69 >P $0069 $90 >M $3865 $94 >M $3866 $69 >M $3867 $18 >M
T{ op PC S A X Y P -> $3867 $f1 $54 $83 $85 $69 }T T{ $0069 M $00ec M $3865 M $3866 M $3867 M -> $90 $85 $94 $69 $18 }T
$cd24 >PC $89 >S $b0 >A $b6 >X $b6 >Y $a3 >P $00e7 $91 >M $cd24 $94 >M $cd25 $e7 >M $cd26 $11 >M
T{ op PC S A X Y P -> $cd26 $89 $b0 $b6 $b6 $a3 }T T{ $009d M $00e7 M $cd24 M $cd25 M $cd26 M -> $b6 $91 $94 $e7 $11 }T
$b895 >PC $20 >S $d0 >A $91 >X $9c >Y $a8 >P $008e $7d >M $b895 $94 >M $b896 $8e >M $b897 $fd >M
T{ op PC S A X Y P -> $b897 $20 $d0 $91 $9c $a8 }T T{ $001f M $008e M $b895 M $b896 M $b897 M -> $9c $7d $94 $8e $fd }T
$9d4b >PC $58 >S $cf >A $aa >X $f6 >Y $24 >P $003b $29 >M $9d4b $94 >M $9d4c $3b >M $9d4d $c7 >M
T{ op PC S A X Y P -> $9d4d $58 $cf $aa $f6 $24 }T T{ $003b M $00e5 M $9d4b M $9d4c M $9d4d M -> $29 $f6 $94 $3b $c7 }T
$e185 >PC $ca >S $48 >A $ed >X $37 >Y $af >P $00e3 $af >M $e185 $94 >M $e186 $e3 >M $e187 $5b >M
T{ op PC S A X Y P -> $e187 $ca $48 $ed $37 $af }T T{ $00d0 M $00e3 M $e185 M $e186 M $e187 M -> $37 $af $94 $e3 $5b }T
$92ef >PC $97 >S $6f >A $1c >X $c4 >Y $69 >P $00cb $f7 >M $92ef $94 >M $92f0 $cb >M $92f1 $f1 >M
T{ op PC S A X Y P -> $92f1 $97 $6f $1c $c4 $69 }T T{ $00cb M $00e7 M $92ef M $92f0 M $92f1 M -> $f7 $c4 $94 $cb $f1 }T
$f52b >PC $ff >S $4f >A $3c >X $48 >Y $e1 >P $0088 $0c >M $f52b $94 >M $f52c $88 >M $f52d $7e >M
T{ op PC S A X Y P -> $f52d $ff $4f $3c $48 $e1 }T T{ $0088 M $00c4 M $f52b M $f52c M $f52d M -> $0c $48 $94 $88 $7e }T
$f8ff >PC $2a >S $87 >A $99 >X $09 >Y $e8 >P $007b $4d >M $f8ff $94 >M $f900 $7b >M $f901 $fd >M
T{ op PC S A X Y P -> $f901 $2a $87 $99 $09 $e8 }T T{ $0014 M $007b M $f8ff M $f900 M $f901 M -> $09 $4d $94 $7b $fd }T
$584a >PC $7d >S $a4 >A $3a >X $86 >Y $2d >P $006d $2b >M $584a $94 >M $584b $6d >M $584c $de >M
T{ op PC S A X Y P -> $584c $7d $a4 $3a $86 $2d }T T{ $006d M $00a7 M $584a M $584b M $584c M -> $2b $86 $94 $6d $de }T
$03eb >PC $02 >S $42 >A $a3 >X $24 >Y $ed >P $00a9 $d0 >M $03eb $94 >M $03ec $a9 >M $03ed $f1 >M
T{ op PC S A X Y P -> $03ed $02 $42 $a3 $24 $ed }T T{ $004c M $00a9 M $03eb M $03ec M $03ed M -> $24 $d0 $94 $a9 $f1 }T
$5a97 >PC $4b >S $4b >A $e5 >X $08 >Y $ab >P $0064 $b5 >M $5a97 $94 >M $5a98 $64 >M $5a99 $f4 >M
T{ op PC S A X Y P -> $5a99 $4b $4b $e5 $08 $ab }T T{ $0049 M $0064 M $5a97 M $5a98 M $5a99 M -> $08 $b5 $94 $64 $f4 }T
$485f >PC $33 >S $4a >A $da >X $18 >Y $64 >P $00c6 $fe >M $485f $94 >M $4860 $c6 >M $4861 $d8 >M
T{ op PC S A X Y P -> $4861 $33 $4a $da $18 $64 }T T{ $00a0 M $00c6 M $485f M $4860 M $4861 M -> $18 $fe $94 $c6 $d8 }T
$5ecb >PC $35 >S $a5 >A $77 >X $c6 >Y $ee >P $00a3 $51 >M $5ecb $94 >M $5ecc $a3 >M $5ecd $c0 >M
T{ op PC S A X Y P -> $5ecd $35 $a5 $77 $c6 $ee }T T{ $001a M $00a3 M $5ecb M $5ecc M $5ecd M -> $c6 $51 $94 $a3 $c0 }T
( 95 )
$63e4 >PC $83 >S $f0 >A $32 >X $e3 >Y $e1 >P $00c9 $ed >M $63e4 $95 >M $63e5 $c9 >M $63e6 $e4 >M
T{ op PC S A X Y P -> $63e6 $83 $f0 $32 $e3 $e1 }T T{ $00c9 M $00fb M $63e4 M $63e5 M $63e6 M -> $ed $f0 $95 $c9 $e4 }T
$5b58 >PC $24 >S $42 >A $6c >X $82 >Y $69 >P $00c5 $55 >M $5b58 $95 >M $5b59 $c5 >M $5b5a $73 >M
T{ op PC S A X Y P -> $5b5a $24 $42 $6c $82 $69 }T T{ $0031 M $00c5 M $5b58 M $5b59 M $5b5a M -> $42 $55 $95 $c5 $73 }T
$3b9c >PC $3f >S $14 >A $2e >X $67 >Y $e0 >P $00d9 $31 >M $3b9c $95 >M $3b9d $d9 >M $3b9e $3f >M
T{ op PC S A X Y P -> $3b9e $3f $14 $2e $67 $e0 }T T{ $0007 M $00d9 M $3b9c M $3b9d M $3b9e M -> $14 $31 $95 $d9 $3f }T
$c411 >PC $da >S $90 >A $a7 >X $2f >Y $21 >P $0009 $dc >M $c411 $95 >M $c412 $09 >M $c413 $57 >M
T{ op PC S A X Y P -> $c413 $da $90 $a7 $2f $21 }T T{ $0009 M $00b0 M $c411 M $c412 M $c413 M -> $dc $90 $95 $09 $57 }T
$5417 >PC $f8 >S $11 >A $f7 >X $b4 >Y $aa >P $0064 $9b >M $5417 $95 >M $5418 $64 >M $5419 $0b >M
T{ op PC S A X Y P -> $5419 $f8 $11 $f7 $b4 $aa }T T{ $005b M $0064 M $5417 M $5418 M $5419 M -> $11 $9b $95 $64 $0b }T
$63d4 >PC $82 >S $8f >A $9d >X $c8 >Y $af >P $0014 $69 >M $63d4 $95 >M $63d5 $14 >M $63d6 $de >M
T{ op PC S A X Y P -> $63d6 $82 $8f $9d $c8 $af }T T{ $0014 M $00b1 M $63d4 M $63d5 M $63d6 M -> $69 $8f $95 $14 $de }T
$2ca2 >PC $2b >S $8e >A $85 >X $c2 >Y $61 >P $0006 $8d >M $2ca2 $95 >M $2ca3 $06 >M $2ca4 $d6 >M
T{ op PC S A X Y P -> $2ca4 $2b $8e $85 $c2 $61 }T T{ $0006 M $008b M $2ca2 M $2ca3 M $2ca4 M -> $8d $8e $95 $06 $d6 }T
$dd78 >PC $7f >S $a1 >A $da >X $f0 >Y $a4 >P $0031 $c2 >M $dd78 $95 >M $dd79 $31 >M $dd7a $56 >M
T{ op PC S A X Y P -> $dd7a $7f $a1 $da $f0 $a4 }T T{ $000b M $0031 M $dd78 M $dd79 M $dd7a M -> $a1 $c2 $95 $31 $56 }T
$5b8a >PC $68 >S $62 >A $4c >X $6c >Y $e5 >P $00ab $35 >M $5b8a $95 >M $5b8b $ab >M $5b8c $88 >M
T{ op PC S A X Y P -> $5b8c $68 $62 $4c $6c $e5 }T T{ $00ab M $00f7 M $5b8a M $5b8b M $5b8c M -> $35 $62 $95 $ab $88 }T
$19cb >PC $a9 >S $5d >A $ae >X $fb >Y $ee >P $008f $95 >M $19cb $95 >M $19cc $8f >M $19cd $d7 >M
T{ op PC S A X Y P -> $19cd $a9 $5d $ae $fb $ee }T T{ $003d M $008f M $19cb M $19cc M $19cd M -> $5d $95 $95 $8f $d7 }T
$3d60 >PC $79 >S $41 >A $72 >X $7b >Y $a2 >P $0022 $87 >M $3d60 $95 >M $3d61 $22 >M $3d62 $bf >M
T{ op PC S A X Y P -> $3d62 $79 $41 $72 $7b $a2 }T T{ $0022 M $0094 M $3d60 M $3d61 M $3d62 M -> $87 $41 $95 $22 $bf }T
$b8cd >PC $e9 >S $6a >A $cf >X $d5 >Y $aa >P $005f $9a >M $b8cd $95 >M $b8ce $5f >M $b8cf $83 >M
T{ op PC S A X Y P -> $b8cf $e9 $6a $cf $d5 $aa }T T{ $002e M $005f M $b8cd M $b8ce M $b8cf M -> $6a $9a $95 $5f $83 }T
$6634 >PC $5b >S $16 >A $1f >X $2c >Y $ec >P $0003 $da >M $6634 $95 >M $6635 $03 >M $6636 $4c >M
T{ op PC S A X Y P -> $6636 $5b $16 $1f $2c $ec }T T{ $0003 M $0022 M $6634 M $6635 M $6636 M -> $da $16 $95 $03 $4c }T
$ab58 >PC $a9 >S $4b >A $10 >X $84 >Y $25 >P $00ad $de >M $ab58 $95 >M $ab59 $ad >M $ab5a $be >M
T{ op PC S A X Y P -> $ab5a $a9 $4b $10 $84 $25 }T T{ $00ad M $00bd M $ab58 M $ab59 M $ab5a M -> $de $4b $95 $ad $be }T
$80c7 >PC $ec >S $65 >A $5d >X $0a >Y $24 >P $00ca $23 >M $80c7 $95 >M $80c8 $ca >M $80c9 $30 >M
T{ op PC S A X Y P -> $80c9 $ec $65 $5d $0a $24 }T T{ $0027 M $00ca M $80c7 M $80c8 M $80c9 M -> $65 $23 $95 $ca $30 }T
$a1e9 >PC $a4 >S $87 >A $1e >X $0e >Y $6b >P $00ab $6b >M $a1e9 $95 >M $a1ea $ab >M $a1eb $96 >M
T{ op PC S A X Y P -> $a1eb $a4 $87 $1e $0e $6b }T T{ $00ab M $00c9 M $a1e9 M $a1ea M $a1eb M -> $6b $87 $95 $ab $96 }T
( 96 )
$d588 >PC $6c >S $da >A $c1 >X $c8 >Y $af >P $00e3 $a1 >M $d588 $96 >M $d589 $e3 >M $d58a $6b >M
T{ op PC S A X Y P -> $d58a $6c $da $c1 $c8 $af }T T{ $00ab M $00e3 M $d588 M $d589 M $d58a M -> $c1 $a1 $96 $e3 $6b }T
$857e >PC $d5 >S $06 >A $39 >X $c2 >Y $e5 >P $0079 $80 >M $857e $96 >M $857f $79 >M $8580 $7e >M
T{ op PC S A X Y P -> $8580 $d5 $06 $39 $c2 $e5 }T T{ $003b M $0079 M $857e M $857f M $8580 M -> $39 $80 $96 $79 $7e }T
$4d00 >PC $d7 >S $4e >A $2b >X $be >Y $a7 >P $0025 $73 >M $4d00 $96 >M $4d01 $25 >M $4d02 $80 >M
T{ op PC S A X Y P -> $4d02 $d7 $4e $2b $be $a7 }T T{ $0025 M $00e3 M $4d00 M $4d01 M $4d02 M -> $73 $2b $96 $25 $80 }T
$8b75 >PC $e5 >S $4c >A $26 >X $f8 >Y $eb >P $00d1 $03 >M $8b75 $96 >M $8b76 $d1 >M $8b77 $6c >M
T{ op PC S A X Y P -> $8b77 $e5 $4c $26 $f8 $eb }T T{ $00c9 M $00d1 M $8b75 M $8b76 M $8b77 M -> $26 $03 $96 $d1 $6c }T
$b54c >PC $e9 >S $4b >A $a3 >X $37 >Y $ab >P $00b4 $64 >M $b54c $96 >M $b54d $b4 >M $b54e $77 >M
T{ op PC S A X Y P -> $b54e $e9 $4b $a3 $37 $ab }T T{ $00b4 M $00eb M $b54c M $b54d M $b54e M -> $64 $a3 $96 $b4 $77 }T
$b2ff >PC $2c >S $5d >A $30 >X $a8 >Y $29 >P $00d2 $88 >M $b2ff $96 >M $b300 $d2 >M $b301 $e9 >M
T{ op PC S A X Y P -> $b301 $2c $5d $30 $a8 $29 }T T{ $007a M $00d2 M $b2ff M $b300 M $b301 M -> $30 $88 $96 $d2 $e9 }T
$dbac >PC $ff >S $2e >A $44 >X $8b >Y $a2 >P $00c8 $e5 >M $dbac $96 >M $dbad $c8 >M $dbae $47 >M
T{ op PC S A X Y P -> $dbae $ff $2e $44 $8b $a2 }T T{ $0053 M $00c8 M $dbac M $dbad M $dbae M -> $44 $e5 $96 $c8 $47 }T
$fbe8 >PC $b4 >S $08 >A $8e >X $81 >Y $61 >P $00ab $5e >M $fbe8 $96 >M $fbe9 $ab >M $fbea $72 >M
T{ op PC S A X Y P -> $fbea $b4 $08 $8e $81 $61 }T T{ $002c M $00ab M $fbe8 M $fbe9 M $fbea M -> $8e $5e $96 $ab $72 }T
$12fb >PC $d2 >S $5a >A $fe >X $8d >Y $a0 >P $0025 $75 >M $12fb $96 >M $12fc $25 >M $12fd $b3 >M
T{ op PC S A X Y P -> $12fd $d2 $5a $fe $8d $a0 }T T{ $0025 M $00b2 M $12fb M $12fc M $12fd M -> $75 $fe $96 $25 $b3 }T
$ef82 >PC $bd >S $d5 >A $be >X $07 >Y $6b >P $006a $09 >M $ef82 $96 >M $ef83 $6a >M $ef84 $34 >M
T{ op PC S A X Y P -> $ef84 $bd $d5 $be $07 $6b }T T{ $006a M $0071 M $ef82 M $ef83 M $ef84 M -> $09 $be $96 $6a $34 }T
$b407 >PC $09 >S $09 >A $cc >X $4e >Y $e3 >P $00bd $c7 >M $b407 $96 >M $b408 $bd >M $b409 $5f >M
T{ op PC S A X Y P -> $b409 $09 $09 $cc $4e $e3 }T T{ $000b M $00bd M $b407 M $b408 M $b409 M -> $cc $c7 $96 $bd $5f }T
$9534 >PC $32 >S $cf >A $3f >X $dd >Y $e1 >P $00bd $57 >M $9534 $96 >M $9535 $bd >M $9536 $57 >M
T{ op PC S A X Y P -> $9536 $32 $cf $3f $dd $e1 }T T{ $009a M $00bd M $9534 M $9535 M $9536 M -> $3f $57 $96 $bd $57 }T
$0d13 >PC $3d >S $c3 >A $7e >X $7c >Y $63 >P $00e6 $76 >M $0d13 $96 >M $0d14 $e6 >M $0d15 $80 >M
T{ op PC S A X Y P -> $0d15 $3d $c3 $7e $7c $63 }T T{ $0062 M $00e6 M $0d13 M $0d14 M $0d15 M -> $7e $76 $96 $e6 $80 }T
$0cde >PC $48 >S $2d >A $62 >X $64 >Y $ef >P $009c $96 >M $0cde $96 >M $0cdf $9c >M $0ce0 $b1 >M
T{ op PC S A X Y P -> $0ce0 $48 $2d $62 $64 $ef }T T{ $0000 M $009c M $0cde M $0cdf M $0ce0 M -> $62 $96 $96 $9c $b1 }T
$461e >PC $33 >S $6f >A $23 >X $c3 >Y $20 >P $0013 $df >M $461e $96 >M $461f $13 >M $4620 $e5 >M
T{ op PC S A X Y P -> $4620 $33 $6f $23 $c3 $20 }T T{ $0013 M $00d6 M $461e M $461f M $4620 M -> $df $23 $96 $13 $e5 }T
$539a >PC $a0 >S $6c >A $9c >X $e8 >Y $a4 >P $009e $c7 >M $539a $96 >M $539b $9e >M $539c $9e >M
T{ op PC S A X Y P -> $539c $a0 $6c $9c $e8 $a4 }T T{ $0086 M $009e M $539a M $539b M $539c M -> $9c $c7 $96 $9e $9e }T
( 97 )
$54b7 >PC $46 >S $e3 >A $b5 >X $24 >Y $22 >P $008a $9d >M $54b7 $97 >M $54b8 $8a >M $54b9 $d1 >M
T{ op PC S A X Y P -> $54b9 $46 $e3 $b5 $24 $22 }T T{ $008a M $54b7 M $54b8 M $54b9 M -> $9f $97 $8a $d1 }T
$85c5 >PC $ec >S $8f >A $73 >X $ad >Y $65 >P $004d $b3 >M $85c5 $97 >M $85c6 $4d >M $85c7 $43 >M
T{ op PC S A X Y P -> $85c7 $ec $8f $73 $ad $65 }T T{ $004d M $85c5 M $85c6 M $85c7 M -> $b3 $97 $4d $43 }T
$8164 >PC $10 >S $93 >A $42 >X $b5 >Y $e3 >P $00c4 $e9 >M $8164 $97 >M $8165 $c4 >M $8166 $36 >M
T{ op PC S A X Y P -> $8166 $10 $93 $42 $b5 $e3 }T T{ $00c4 M $8164 M $8165 M $8166 M -> $eb $97 $c4 $36 }T
$ac55 >PC $bf >S $a7 >A $9e >X $57 >Y $25 >P $0056 $cb >M $ac55 $97 >M $ac56 $56 >M $ac57 $3f >M
T{ op PC S A X Y P -> $ac57 $bf $a7 $9e $57 $25 }T T{ $0056 M $ac55 M $ac56 M $ac57 M -> $cb $97 $56 $3f }T
$8b21 >PC $ac >S $53 >A $26 >X $77 >Y $e5 >P $00b7 $18 >M $8b21 $97 >M $8b22 $b7 >M $8b23 $b7 >M
T{ op PC S A X Y P -> $8b23 $ac $53 $26 $77 $e5 }T T{ $00b7 M $8b21 M $8b22 M $8b23 M -> $1a $97 $b7 $b7 }T
$99d0 >PC $c7 >S $d4 >A $8e >X $04 >Y $67 >P $00c6 $07 >M $99d0 $97 >M $99d1 $c6 >M $99d2 $05 >M
T{ op PC S A X Y P -> $99d2 $c7 $d4 $8e $04 $67 }T T{ $00c6 M $99d0 M $99d1 M $99d2 M -> $07 $97 $c6 $05 }T
$47df >PC $42 >S $b4 >A $2e >X $af >Y $a6 >P $003d $0d >M $47df $97 >M $47e0 $3d >M $47e1 $17 >M
T{ op PC S A X Y P -> $47e1 $42 $b4 $2e $af $a6 }T T{ $003d M $47df M $47e0 M $47e1 M -> $0f $97 $3d $17 }T
$5702 >PC $9b >S $60 >A $04 >X $75 >Y $60 >P $00f0 $c0 >M $5702 $97 >M $5703 $f0 >M $5704 $e8 >M
T{ op PC S A X Y P -> $5704 $9b $60 $04 $75 $60 }T T{ $00f0 M $5702 M $5703 M $5704 M -> $c2 $97 $f0 $e8 }T
$76e6 >PC $22 >S $37 >A $7c >X $17 >Y $28 >P $0027 $fd >M $76e6 $97 >M $76e7 $27 >M $76e8 $df >M
T{ op PC S A X Y P -> $76e8 $22 $37 $7c $17 $28 }T T{ $0027 M $76e6 M $76e7 M $76e8 M -> $ff $97 $27 $df }T
$9c26 >PC $75 >S $25 >A $d3 >X $f7 >Y $e8 >P $009d $04 >M $9c26 $97 >M $9c27 $9d >M $9c28 $4b >M
T{ op PC S A X Y P -> $9c28 $75 $25 $d3 $f7 $e8 }T T{ $009d M $9c26 M $9c27 M $9c28 M -> $06 $97 $9d $4b }T
$9570 >PC $25 >S $1e >A $59 >X $ee >Y $22 >P $003c $06 >M $9570 $97 >M $9571 $3c >M $9572 $65 >M
T{ op PC S A X Y P -> $9572 $25 $1e $59 $ee $22 }T T{ $003c M $9570 M $9571 M $9572 M -> $06 $97 $3c $65 }T
$8943 >PC $0b >S $9a >A $bf >X $10 >Y $a4 >P $0094 $75 >M $8943 $97 >M $8944 $94 >M $8945 $e2 >M
T{ op PC S A X Y P -> $8945 $0b $9a $bf $10 $a4 }T T{ $0094 M $8943 M $8944 M $8945 M -> $77 $97 $94 $e2 }T
$0917 >PC $e7 >S $c0 >A $3c >X $5b >Y $e9 >P $0094 $5e >M $0917 $97 >M $0918 $94 >M $0919 $22 >M
T{ op PC S A X Y P -> $0919 $e7 $c0 $3c $5b $e9 }T T{ $0094 M $0917 M $0918 M $0919 M -> $5e $97 $94 $22 }T
$f741 >PC $3e >S $bc >A $fe >X $9b >Y $65 >P $00b5 $28 >M $f741 $97 >M $f742 $b5 >M $f743 $08 >M
T{ op PC S A X Y P -> $f743 $3e $bc $fe $9b $65 }T T{ $00b5 M $f741 M $f742 M $f743 M -> $2a $97 $b5 $08 }T
$756c >PC $6d >S $48 >A $29 >X $c5 >Y $65 >P $0024 $22 >M $756c $97 >M $756d $24 >M $756e $d7 >M
T{ op PC S A X Y P -> $756e $6d $48 $29 $c5 $65 }T T{ $0024 M $756c M $756d M $756e M -> $22 $97 $24 $d7 }T
$5b30 >PC $27 >S $5e >A $c8 >X $77 >Y $26 >P $0019 $97 >M $5b30 $97 >M $5b31 $19 >M $5b32 $35 >M
T{ op PC S A X Y P -> $5b32 $27 $5e $c8 $77 $26 }T T{ $0019 M $5b30 M $5b31 M $5b32 M -> $97 $97 $19 $35 }T
( 98 )
$de22 >PC $fb >S $29 >A $86 >X $c7 >Y $69 >P $de22 $98 >M $de23 $3d >M $de24 $df >M
T{ op PC S A X Y P -> $de23 $fb $c7 $86 $c7 $e9 }T T{ $de22 M $de23 M $de24 M -> $98 $3d $df }T
$eb39 >PC $bf >S $d3 >A $ad >X $f0 >Y $e6 >P $eb39 $98 >M $eb3a $f3 >M $eb3b $b9 >M
T{ op PC S A X Y P -> $eb3a $bf $f0 $ad $f0 $e4 }T T{ $eb39 M $eb3a M $eb3b M -> $98 $f3 $b9 }T
$a11a >PC $95 >S $d3 >A $33 >X $5e >Y $22 >P $a11a $98 >M $a11b $bd >M $a11c $18 >M
T{ op PC S A X Y P -> $a11b $95 $5e $33 $5e $20 }T T{ $a11a M $a11b M $a11c M -> $98 $bd $18 }T
$5fc5 >PC $ce >S $d7 >A $99 >X $54 >Y $2f >P $5fc5 $98 >M $5fc6 $c8 >M $5fc7 $d3 >M
T{ op PC S A X Y P -> $5fc6 $ce $54 $99 $54 $2d }T T{ $5fc5 M $5fc6 M $5fc7 M -> $98 $c8 $d3 }T
$2bfe >PC $26 >S $eb >A $cb >X $a4 >Y $6c >P $2bfe $98 >M $2bff $f9 >M $2c00 $f0 >M
T{ op PC S A X Y P -> $2bff $26 $a4 $cb $a4 $ec }T T{ $2bfe M $2bff M $2c00 M -> $98 $f9 $f0 }T
$fe6b >PC $97 >S $ed >A $6a >X $03 >Y $ac >P $fe6b $98 >M $fe6c $b5 >M $fe6d $7a >M
T{ op PC S A X Y P -> $fe6c $97 $03 $6a $03 $2c }T T{ $fe6b M $fe6c M $fe6d M -> $98 $b5 $7a }T
$b0a0 >PC $76 >S $95 >A $70 >X $b6 >Y $29 >P $b0a0 $98 >M $b0a1 $8c >M $b0a2 $fb >M
T{ op PC S A X Y P -> $b0a1 $76 $b6 $70 $b6 $a9 }T T{ $b0a0 M $b0a1 M $b0a2 M -> $98 $8c $fb }T
$8f43 >PC $59 >S $c8 >A $bd >X $67 >Y $af >P $8f43 $98 >M $8f44 $8d >M $8f45 $0a >M
T{ op PC S A X Y P -> $8f44 $59 $67 $bd $67 $2d }T T{ $8f43 M $8f44 M $8f45 M -> $98 $8d $0a }T
$88e2 >PC $61 >S $d5 >A $85 >X $5e >Y $e5 >P $88e2 $98 >M $88e3 $3f >M $88e4 $9c >M
T{ op PC S A X Y P -> $88e3 $61 $5e $85 $5e $65 }T T{ $88e2 M $88e3 M $88e4 M -> $98 $3f $9c }T
$6cd8 >PC $38 >S $ed >A $66 >X $fb >Y $25 >P $6cd8 $98 >M $6cd9 $c4 >M $6cda $99 >M
T{ op PC S A X Y P -> $6cd9 $38 $fb $66 $fb $a5 }T T{ $6cd8 M $6cd9 M $6cda M -> $98 $c4 $99 }T
$4f2e >PC $58 >S $a5 >A $69 >X $67 >Y $60 >P $4f2e $98 >M $4f2f $02 >M $4f30 $f2 >M
T{ op PC S A X Y P -> $4f2f $58 $67 $69 $67 $60 }T T{ $4f2e M $4f2f M $4f30 M -> $98 $02 $f2 }T
$f5ac >PC $59 >S $ab >A $c9 >X $22 >Y $ea >P $f5ac $98 >M $f5ad $f5 >M $f5ae $3f >M
T{ op PC S A X Y P -> $f5ad $59 $22 $c9 $22 $68 }T T{ $f5ac M $f5ad M $f5ae M -> $98 $f5 $3f }T
$93b4 >PC $91 >S $47 >A $3a >X $cb >Y $ed >P $93b4 $98 >M $93b5 $b1 >M $93b6 $94 >M
T{ op PC S A X Y P -> $93b5 $91 $cb $3a $cb $ed }T T{ $93b4 M $93b5 M $93b6 M -> $98 $b1 $94 }T
$0181 >PC $7f >S $63 >A $17 >X $35 >Y $a3 >P $0181 $98 >M $0182 $46 >M $0183 $67 >M
T{ op PC S A X Y P -> $0182 $7f $35 $17 $35 $21 }T T{ $0181 M $0182 M $0183 M -> $98 $46 $67 }T
$f76d >PC $fd >S $bf >A $da >X $5b >Y $60 >P $f76d $98 >M $f76e $09 >M $f76f $38 >M
T{ op PC S A X Y P -> $f76e $fd $5b $da $5b $60 }T T{ $f76d M $f76e M $f76f M -> $98 $09 $38 }T
$d4cb >PC $05 >S $9a >A $b0 >X $8a >Y $27 >P $d4cb $98 >M $d4cc $8b >M $d4cd $ea >M
T{ op PC S A X Y P -> $d4cc $05 $8a $b0 $8a $a5 }T T{ $d4cb M $d4cc M $d4cd M -> $98 $8b $ea }T
( 99 )
$4fec >PC $56 >S $4a >A $c7 >X $4b >Y $24 >P $4fec $99 >M $4fed $06 >M $4fee $1c >M $4fef $64 >M
T{ op PC S A X Y P -> $4fef $56 $4a $c7 $4b $24 }T T{ $1c51 M $4fec M $4fed M $4fee M $4fef M -> $4a $99 $06 $1c $64 }T
$b47f >PC $c2 >S $81 >A $f3 >X $7c >Y $65 >P $b47f $99 >M $b480 $4d >M $b481 $de >M $b482 $14 >M
T{ op PC S A X Y P -> $b482 $c2 $81 $f3 $7c $65 }T T{ $b47f M $b480 M $b481 M $b482 M $dec9 M -> $99 $4d $de $14 $81 }T
$725a >PC $5a >S $66 >A $da >X $d1 >Y $22 >P $725a $99 >M $725b $8b >M $725c $d4 >M $725d $13 >M
T{ op PC S A X Y P -> $725d $5a $66 $da $d1 $22 }T T{ $725a M $725b M $725c M $725d M $d55c M -> $99 $8b $d4 $13 $66 }T
$50c8 >PC $29 >S $c2 >A $c0 >X $07 >Y $a0 >P $50c8 $99 >M $50c9 $2d >M $50ca $1d >M $50cb $82 >M
T{ op PC S A X Y P -> $50cb $29 $c2 $c0 $07 $a0 }T T{ $1d34 M $50c8 M $50c9 M $50ca M $50cb M -> $c2 $99 $2d $1d $82 }T
$abf7 >PC $37 >S $79 >A $49 >X $40 >Y $a0 >P $abf7 $99 >M $abf8 $cb >M $abf9 $c1 >M $abfa $cf >M
T{ op PC S A X Y P -> $abfa $37 $79 $49 $40 $a0 }T T{ $abf7 M $abf8 M $abf9 M $abfa M $c20b M -> $99 $cb $c1 $cf $79 }T
$7b85 >PC $13 >S $89 >A $b5 >X $5f >Y $ae >P $7b85 $99 >M $7b86 $5e >M $7b87 $6a >M $7b88 $06 >M
T{ op PC S A X Y P -> $7b88 $13 $89 $b5 $5f $ae }T T{ $6abd M $7b85 M $7b86 M $7b87 M $7b88 M -> $89 $99 $5e $6a $06 }T
$377d >PC $a4 >S $34 >A $19 >X $91 >Y $e4 >P $377d $99 >M $377e $76 >M $377f $26 >M $3780 $a6 >M
T{ op PC S A X Y P -> $3780 $a4 $34 $19 $91 $e4 }T T{ $2707 M $377d M $377e M $377f M $3780 M -> $34 $99 $76 $26 $a6 }T
$3805 >PC $1b >S $96 >A $51 >X $ae >Y $29 >P $3805 $99 >M $3806 $09 >M $3807 $b2 >M $3808 $a9 >M
T{ op PC S A X Y P -> $3808 $1b $96 $51 $ae $29 }T T{ $3805 M $3806 M $3807 M $3808 M $b2b7 M -> $99 $09 $b2 $a9 $96 }T
$f892 >PC $e9 >S $ec >A $12 >X $a4 >Y $6a >P $f892 $99 >M $f893 $f8 >M $f894 $47 >M $f895 $8c >M
T{ op PC S A X Y P -> $f895 $e9 $ec $12 $a4 $6a }T T{ $489c M $f892 M $f893 M $f894 M $f895 M -> $ec $99 $f8 $47 $8c }T
$9baa >PC $e9 >S $be >A $46 >X $7c >Y $66 >P $9baa $99 >M $9bab $f5 >M $9bac $17 >M $9bad $64 >M
T{ op PC S A X Y P -> $9bad $e9 $be $46 $7c $66 }T T{ $1871 M $9baa M $9bab M $9bac M $9bad M -> $be $99 $f5 $17 $64 }T
$5dc1 >PC $4c >S $b4 >A $43 >X $6f >Y $6f >P $5dc1 $99 >M $5dc2 $67 >M $5dc3 $75 >M $5dc4 $9a >M
T{ op PC S A X Y P -> $5dc4 $4c $b4 $43 $6f $6f }T T{ $5dc1 M $5dc2 M $5dc3 M $5dc4 M $75d6 M -> $99 $67 $75 $9a $b4 }T
$bd66 >PC $71 >S $70 >A $51 >X $15 >Y $e0 >P $bd66 $99 >M $bd67 $fb >M $bd68 $87 >M $bd69 $49 >M
T{ op PC S A X Y P -> $bd69 $71 $70 $51 $15 $e0 }T T{ $8810 M $bd66 M $bd67 M $bd68 M $bd69 M -> $70 $99 $fb $87 $49 }T
$aa7a >PC $48 >S $fd >A $e6 >X $3c >Y $63 >P $aa7a $99 >M $aa7b $19 >M $aa7c $b5 >M $aa7d $84 >M
T{ op PC S A X Y P -> $aa7d $48 $fd $e6 $3c $63 }T T{ $aa7a M $aa7b M $aa7c M $aa7d M $b555 M -> $99 $19 $b5 $84 $fd }T
$1049 >PC $c0 >S $31 >A $f3 >X $97 >Y $22 >P $1049 $99 >M $104a $40 >M $104b $d4 >M $104c $e5 >M
T{ op PC S A X Y P -> $104c $c0 $31 $f3 $97 $22 }T T{ $1049 M $104a M $104b M $104c M $d4d7 M -> $99 $40 $d4 $e5 $31 }T
$0300 >PC $08 >S $8a >A $ec >X $26 >Y $6c >P $0300 $99 >M $0301 $78 >M $0302 $0f >M $0303 $fb >M
T{ op PC S A X Y P -> $0303 $08 $8a $ec $26 $6c }T T{ $0300 M $0301 M $0302 M $0303 M $0f9e M -> $99 $78 $0f $fb $8a }T
$f4a6 >PC $21 >S $f5 >A $d4 >X $98 >Y $2c >P $f4a6 $99 >M $f4a7 $83 >M $f4a8 $0b >M $f4a9 $fd >M
T{ op PC S A X Y P -> $f4a9 $21 $f5 $d4 $98 $2c }T T{ $0c1b M $f4a6 M $f4a7 M $f4a8 M $f4a9 M -> $f5 $99 $83 $0b $fd }T
( 9a )
$a3e8 >PC $5f >S $0d >A $da >X $7a >Y $2c >P $a3e8 $9a >M $a3e9 $43 >M $a3ea $96 >M
T{ op PC S A X Y P -> $a3e9 $da $0d $da $7a $2c }T T{ $a3e8 M $a3e9 M $a3ea M -> $9a $43 $96 }T
$d4a3 >PC $bf >S $26 >A $92 >X $61 >Y $2e >P $d4a3 $9a >M $d4a4 $af >M $d4a5 $10 >M
T{ op PC S A X Y P -> $d4a4 $92 $26 $92 $61 $2e }T T{ $d4a3 M $d4a4 M $d4a5 M -> $9a $af $10 }T
$1147 >PC $55 >S $79 >A $a0 >X $8b >Y $6b >P $1147 $9a >M $1148 $09 >M $1149 $57 >M
T{ op PC S A X Y P -> $1148 $a0 $79 $a0 $8b $6b }T T{ $1147 M $1148 M $1149 M -> $9a $09 $57 }T
$2a22 >PC $83 >S $20 >A $7e >X $9b >Y $a9 >P $2a22 $9a >M $2a23 $f3 >M $2a24 $ec >M
T{ op PC S A X Y P -> $2a23 $7e $20 $7e $9b $a9 }T T{ $2a22 M $2a23 M $2a24 M -> $9a $f3 $ec }T
$e055 >PC $34 >S $cd >A $6e >X $05 >Y $29 >P $e055 $9a >M $e056 $d1 >M $e057 $81 >M
T{ op PC S A X Y P -> $e056 $6e $cd $6e $05 $29 }T T{ $e055 M $e056 M $e057 M -> $9a $d1 $81 }T
$2283 >PC $7f >S $3b >A $5b >X $fa >Y $66 >P $2283 $9a >M $2284 $87 >M $2285 $a5 >M
T{ op PC S A X Y P -> $2284 $5b $3b $5b $fa $66 }T T{ $2283 M $2284 M $2285 M -> $9a $87 $a5 }T
$086f >PC $e3 >S $c3 >A $b1 >X $fe >Y $ec >P $086f $9a >M $0870 $1a >M $0871 $39 >M
T{ op PC S A X Y P -> $0870 $b1 $c3 $b1 $fe $ec }T T{ $086f M $0870 M $0871 M -> $9a $1a $39 }T
$6ddd >PC $2d >S $cb >A $90 >X $05 >Y $ee >P $6ddd $9a >M $6dde $57 >M $6ddf $8c >M
T{ op PC S A X Y P -> $6dde $90 $cb $90 $05 $ee }T T{ $6ddd M $6dde M $6ddf M -> $9a $57 $8c }T
$5d14 >PC $a7 >S $dc >A $33 >X $61 >Y $6a >P $5d14 $9a >M $5d15 $a2 >M $5d16 $ac >M
T{ op PC S A X Y P -> $5d15 $33 $dc $33 $61 $6a }T T{ $5d14 M $5d15 M $5d16 M -> $9a $a2 $ac }T
$2e45 >PC $18 >S $cd >A $68 >X $45 >Y $eb >P $2e45 $9a >M $2e46 $b8 >M $2e47 $b4 >M
T{ op PC S A X Y P -> $2e46 $68 $cd $68 $45 $eb }T T{ $2e45 M $2e46 M $2e47 M -> $9a $b8 $b4 }T
$349c >PC $17 >S $8d >A $f9 >X $e4 >Y $a7 >P $349c $9a >M $349d $ba >M $349e $84 >M
T{ op PC S A X Y P -> $349d $f9 $8d $f9 $e4 $a7 }T T{ $349c M $349d M $349e M -> $9a $ba $84 }T
$1c21 >PC $b6 >S $15 >A $2c >X $34 >Y $e6 >P $1c21 $9a >M $1c22 $08 >M $1c23 $5e >M
T{ op PC S A X Y P -> $1c22 $2c $15 $2c $34 $e6 }T T{ $1c21 M $1c22 M $1c23 M -> $9a $08 $5e }T
$d1f8 >PC $81 >S $c7 >A $54 >X $08 >Y $26 >P $d1f8 $9a >M $d1f9 $a2 >M $d1fa $94 >M
T{ op PC S A X Y P -> $d1f9 $54 $c7 $54 $08 $26 }T T{ $d1f8 M $d1f9 M $d1fa M -> $9a $a2 $94 }T
$9fe9 >PC $ce >S $32 >A $55 >X $04 >Y $6c >P $9fe9 $9a >M $9fea $d0 >M $9feb $26 >M
T{ op PC S A X Y P -> $9fea $55 $32 $55 $04 $6c }T T{ $9fe9 M $9fea M $9feb M -> $9a $d0 $26 }T
$14b9 >PC $cc >S $e1 >A $99 >X $b0 >Y $a0 >P $14b9 $9a >M $14ba $e1 >M $14bb $f3 >M
T{ op PC S A X Y P -> $14ba $99 $e1 $99 $b0 $a0 }T T{ $14b9 M $14ba M $14bb M -> $9a $e1 $f3 }T
$5169 >PC $5d >S $3c >A $10 >X $5b >Y $2f >P $5169 $9a >M $516a $c8 >M $516b $58 >M
T{ op PC S A X Y P -> $516a $10 $3c $10 $5b $2f }T T{ $5169 M $516a M $516b M -> $9a $c8 $58 }T
( 9b )
$0719 >PC $6b >S $3c >A $73 >X $c4 >Y $a3 >P $0719 $9b >M $071a $f0 >M $071b $d4 >M
T{ op PC S A X Y P -> $071a $6b $3c $73 $c4 $a3 }T T{ $0719 M $071a M $071b M -> $9b $f0 $d4 }T
$9631 >PC $71 >S $c2 >A $2f >X $fd >Y $a8 >P $9631 $9b >M $9632 $36 >M $9633 $03 >M
T{ op PC S A X Y P -> $9632 $71 $c2 $2f $fd $a8 }T T{ $9631 M $9632 M $9633 M -> $9b $36 $03 }T
$2d91 >PC $e0 >S $3a >A $14 >X $83 >Y $64 >P $2d91 $9b >M $2d92 $a7 >M $2d93 $08 >M
T{ op PC S A X Y P -> $2d92 $e0 $3a $14 $83 $64 }T T{ $2d91 M $2d92 M $2d93 M -> $9b $a7 $08 }T
$fe7c >PC $d8 >S $7d >A $38 >X $0f >Y $ef >P $fe7c $9b >M $fe7d $f0 >M $fe7e $25 >M
T{ op PC S A X Y P -> $fe7d $d8 $7d $38 $0f $ef }T T{ $fe7c M $fe7d M $fe7e M -> $9b $f0 $25 }T
$4bdd >PC $33 >S $bc >A $d7 >X $56 >Y $e6 >P $4bdd $9b >M $4bde $d9 >M $4bdf $b1 >M
T{ op PC S A X Y P -> $4bde $33 $bc $d7 $56 $e6 }T T{ $4bdd M $4bde M $4bdf M -> $9b $d9 $b1 }T
$c7d3 >PC $ce >S $bf >A $18 >X $c6 >Y $e4 >P $c7d3 $9b >M $c7d4 $66 >M $c7d5 $09 >M
T{ op PC S A X Y P -> $c7d4 $ce $bf $18 $c6 $e4 }T T{ $c7d3 M $c7d4 M $c7d5 M -> $9b $66 $09 }T
$0aad >PC $49 >S $eb >A $bb >X $b9 >Y $6d >P $0aad $9b >M $0aae $23 >M $0aaf $62 >M
T{ op PC S A X Y P -> $0aae $49 $eb $bb $b9 $6d }T T{ $0aad M $0aae M $0aaf M -> $9b $23 $62 }T
$8489 >PC $8c >S $33 >A $87 >X $d9 >Y $a6 >P $8489 $9b >M $848a $78 >M $848b $56 >M
T{ op PC S A X Y P -> $848a $8c $33 $87 $d9 $a6 }T T{ $8489 M $848a M $848b M -> $9b $78 $56 }T
$5482 >PC $58 >S $db >A $a1 >X $0b >Y $6d >P $5482 $9b >M $5483 $8e >M $5484 $e2 >M
T{ op PC S A X Y P -> $5483 $58 $db $a1 $0b $6d }T T{ $5482 M $5483 M $5484 M -> $9b $8e $e2 }T
$dcbd >PC $e4 >S $ec >A $fe >X $15 >Y $65 >P $dcbd $9b >M $dcbe $3a >M $dcbf $18 >M
T{ op PC S A X Y P -> $dcbe $e4 $ec $fe $15 $65 }T T{ $dcbd M $dcbe M $dcbf M -> $9b $3a $18 }T
$048d >PC $5b >S $50 >A $b2 >X $c6 >Y $2c >P $048d $9b >M $048e $28 >M $048f $9f >M
T{ op PC S A X Y P -> $048e $5b $50 $b2 $c6 $2c }T T{ $048d M $048e M $048f M -> $9b $28 $9f }T
$02b3 >PC $fd >S $df >A $4b >X $10 >Y $2e >P $02b3 $9b >M $02b4 $83 >M $02b5 $cd >M
T{ op PC S A X Y P -> $02b4 $fd $df $4b $10 $2e }T T{ $02b3 M $02b4 M $02b5 M -> $9b $83 $cd }T
$a4fe >PC $eb >S $2d >A $8e >X $b3 >Y $6f >P $a4fe $9b >M $a4ff $f8 >M $a500 $cf >M
T{ op PC S A X Y P -> $a4ff $eb $2d $8e $b3 $6f }T T{ $a4fe M $a4ff M $a500 M -> $9b $f8 $cf }T
$5536 >PC $6f >S $5b >A $8b >X $11 >Y $a1 >P $5536 $9b >M $5537 $ce >M $5538 $3c >M
T{ op PC S A X Y P -> $5537 $6f $5b $8b $11 $a1 }T T{ $5536 M $5537 M $5538 M -> $9b $ce $3c }T
$8aa2 >PC $7a >S $cb >A $86 >X $31 >Y $ae >P $8aa2 $9b >M $8aa3 $80 >M $8aa4 $d1 >M
T{ op PC S A X Y P -> $8aa3 $7a $cb $86 $31 $ae }T T{ $8aa2 M $8aa3 M $8aa4 M -> $9b $80 $d1 }T
$d865 >PC $46 >S $85 >A $5d >X $85 >Y $2d >P $d865 $9b >M $d866 $a2 >M $d867 $79 >M
T{ op PC S A X Y P -> $d866 $46 $85 $5d $85 $2d }T T{ $d865 M $d866 M $d867 M -> $9b $a2 $79 }T
( 9c )
$93b4 >PC $17 >S $69 >A $b0 >X $28 >Y $ae >P $93b4 $9c >M $93b5 $42 >M $93b6 $20 >M $93b7 $34 >M
T{ op PC S A X Y P -> $93b7 $17 $69 $b0 $28 $ae }T T{ $2042 M $93b4 M $93b5 M $93b6 M $93b7 M -> $00 $9c $42 $20 $34 }T
$1157 >PC $70 >S $44 >A $da >X $3f >Y $21 >P $1157 $9c >M $1158 $5d >M $1159 $cd >M $115a $3e >M
T{ op PC S A X Y P -> $115a $70 $44 $da $3f $21 }T T{ $1157 M $1158 M $1159 M $115a M $cd5d M -> $9c $5d $cd $3e $00 }T
$7d6e >PC $ed >S $92 >A $17 >X $13 >Y $e5 >P $7d6e $9c >M $7d6f $e7 >M $7d70 $04 >M $7d71 $f8 >M
T{ op PC S A X Y P -> $7d71 $ed $92 $17 $13 $e5 }T T{ $04e7 M $7d6e M $7d6f M $7d70 M $7d71 M -> $00 $9c $e7 $04 $f8 }T
$f7dd >PC $b8 >S $5d >A $6d >X $63 >Y $63 >P $f7dd $9c >M $f7de $41 >M $f7df $ae >M $f7e0 $01 >M
T{ op PC S A X Y P -> $f7e0 $b8 $5d $6d $63 $63 }T T{ $ae41 M $f7dd M $f7de M $f7df M $f7e0 M -> $00 $9c $41 $ae $01 }T
$4efa >PC $03 >S $8a >A $c5 >X $87 >Y $64 >P $4efa $9c >M $4efb $23 >M $4efc $37 >M $4efd $1c >M
T{ op PC S A X Y P -> $4efd $03 $8a $c5 $87 $64 }T T{ $3723 M $4efa M $4efb M $4efc M $4efd M -> $00 $9c $23 $37 $1c }T
$052d >PC $12 >S $93 >A $a9 >X $51 >Y $e1 >P $052d $9c >M $052e $5e >M $052f $3c >M $0530 $ba >M
T{ op PC S A X Y P -> $0530 $12 $93 $a9 $51 $e1 }T T{ $052d M $052e M $052f M $0530 M $3c5e M -> $9c $5e $3c $ba $00 }T
$6f10 >PC $83 >S $06 >A $3e >X $0f >Y $a1 >P $6f10 $9c >M $6f11 $f4 >M $6f12 $2b >M $6f13 $93 >M
T{ op PC S A X Y P -> $6f13 $83 $06 $3e $0f $a1 }T T{ $2bf4 M $6f10 M $6f11 M $6f12 M $6f13 M -> $00 $9c $f4 $2b $93 }T
$6c9b >PC $2f >S $6a >A $40 >X $7d >Y $2a >P $6c9b $9c >M $6c9c $d5 >M $6c9d $2d >M $6c9e $63 >M
T{ op PC S A X Y P -> $6c9e $2f $6a $40 $7d $2a }T T{ $2dd5 M $6c9b M $6c9c M $6c9d M $6c9e M -> $00 $9c $d5 $2d $63 }T
$a907 >PC $b8 >S $36 >A $5e >X $90 >Y $2f >P $a907 $9c >M $a908 $83 >M $a909 $26 >M $a90a $c1 >M
T{ op PC S A X Y P -> $a90a $b8 $36 $5e $90 $2f }T T{ $2683 M $a907 M $a908 M $a909 M $a90a M -> $00 $9c $83 $26 $c1 }T
$7c51 >PC $b3 >S $4b >A $74 >X $c0 >Y $e6 >P $7c51 $9c >M $7c52 $0a >M $7c53 $12 >M $7c54 $a7 >M
T{ op PC S A X Y P -> $7c54 $b3 $4b $74 $c0 $e6 }T T{ $120a M $7c51 M $7c52 M $7c53 M $7c54 M -> $00 $9c $0a $12 $a7 }T
$93fe >PC $a1 >S $ac >A $94 >X $5f >Y $27 >P $93fe $9c >M $93ff $54 >M $9400 $f9 >M $9401 $05 >M
T{ op PC S A X Y P -> $9401 $a1 $ac $94 $5f $27 }T T{ $93fe M $93ff M $9400 M $9401 M $f954 M -> $9c $54 $f9 $05 $00 }T
$eb37 >PC $81 >S $10 >A $3d >X $a4 >Y $26 >P $eb37 $9c >M $eb38 $55 >M $eb39 $db >M $eb3a $8d >M
T{ op PC S A X Y P -> $eb3a $81 $10 $3d $a4 $26 }T T{ $db55 M $eb37 M $eb38 M $eb39 M $eb3a M -> $00 $9c $55 $db $8d }T
$8a67 >PC $bc >S $82 >A $3e >X $f0 >Y $6b >P $8a67 $9c >M $8a68 $cb >M $8a69 $ce >M $8a6a $70 >M
T{ op PC S A X Y P -> $8a6a $bc $82 $3e $f0 $6b }T T{ $8a67 M $8a68 M $8a69 M $8a6a M $cecb M -> $9c $cb $ce $70 $00 }T
$7ef0 >PC $83 >S $33 >A $9e >X $ec >Y $6f >P $7ef0 $9c >M $7ef1 $9a >M $7ef2 $ab >M $7ef3 $a9 >M
T{ op PC S A X Y P -> $7ef3 $83 $33 $9e $ec $6f }T T{ $7ef0 M $7ef1 M $7ef2 M $7ef3 M $ab9a M -> $9c $9a $ab $a9 $00 }T
$60a1 >PC $11 >S $5d >A $59 >X $ce >Y $ad >P $60a1 $9c >M $60a2 $a1 >M $60a3 $8a >M $60a4 $5f >M
T{ op PC S A X Y P -> $60a4 $11 $5d $59 $ce $ad }T T{ $60a1 M $60a2 M $60a3 M $60a4 M $8aa1 M -> $9c $a1 $8a $5f $00 }T
$4029 >PC $3e >S $03 >A $36 >X $82 >Y $6d >P $4029 $9c >M $402a $47 >M $402b $b2 >M $402c $0e >M
T{ op PC S A X Y P -> $402c $3e $03 $36 $82 $6d }T T{ $4029 M $402a M $402b M $402c M $b247 M -> $9c $47 $b2 $0e $00 }T
( 9d )
$73f6 >PC $35 >S $c8 >A $7f >X $6d >Y $6d >P $73f6 $9d >M $73f7 $07 >M $73f8 $4d >M $73f9 $24 >M
T{ op PC S A X Y P -> $73f9 $35 $c8 $7f $6d $6d }T T{ $4d86 M $73f6 M $73f7 M $73f8 M $73f9 M -> $c8 $9d $07 $4d $24 }T
$3326 >PC $d6 >S $a4 >A $64 >X $36 >Y $e0 >P $3326 $9d >M $3327 $60 >M $3328 $68 >M $3329 $21 >M
T{ op PC S A X Y P -> $3329 $d6 $a4 $64 $36 $e0 }T T{ $3326 M $3327 M $3328 M $3329 M $68c4 M -> $9d $60 $68 $21 $a4 }T
$5213 >PC $c6 >S $39 >A $f7 >X $4d >Y $ec >P $5213 $9d >M $5214 $1a >M $5215 $9f >M $5216 $b8 >M
T{ op PC S A X Y P -> $5216 $c6 $39 $f7 $4d $ec }T T{ $5213 M $5214 M $5215 M $5216 M $a011 M -> $9d $1a $9f $b8 $39 }T
$b480 >PC $72 >S $13 >A $05 >X $f6 >Y $20 >P $b480 $9d >M $b481 $b7 >M $b482 $ee >M $b483 $83 >M
T{ op PC S A X Y P -> $b483 $72 $13 $05 $f6 $20 }T T{ $b480 M $b481 M $b482 M $b483 M $eebc M -> $9d $b7 $ee $83 $13 }T
$3f15 >PC $9c >S $24 >A $b6 >X $cf >Y $65 >P $3f15 $9d >M $3f16 $6d >M $3f17 $8e >M $3f18 $53 >M
T{ op PC S A X Y P -> $3f18 $9c $24 $b6 $cf $65 }T T{ $3f15 M $3f16 M $3f17 M $3f18 M $8f23 M -> $9d $6d $8e $53 $24 }T
$fac8 >PC $50 >S $80 >A $12 >X $d1 >Y $eb >P $fac8 $9d >M $fac9 $25 >M $faca $6e >M $facb $48 >M
T{ op PC S A X Y P -> $facb $50 $80 $12 $d1 $eb }T T{ $6e37 M $fac8 M $fac9 M $faca M $facb M -> $80 $9d $25 $6e $48 }T
$dce0 >PC $ca >S $c1 >A $7d >X $90 >Y $e9 >P $dce0 $9d >M $dce1 $af >M $dce2 $c8 >M $dce3 $36 >M
T{ op PC S A X Y P -> $dce3 $ca $c1 $7d $90 $e9 }T T{ $c92c M $dce0 M $dce1 M $dce2 M $dce3 M -> $c1 $9d $af $c8 $36 }T
$759c >PC $0b >S $b9 >A $b4 >X $27 >Y $61 >P $759c $9d >M $759d $8e >M $759e $6b >M $759f $43 >M
T{ op PC S A X Y P -> $759f $0b $b9 $b4 $27 $61 }T T{ $6c42 M $759c M $759d M $759e M $759f M -> $b9 $9d $8e $6b $43 }T
$f90b >PC $6d >S $85 >A $2e >X $52 >Y $60 >P $f90b $9d >M $f90c $8c >M $f90d $93 >M $f90e $e4 >M
T{ op PC S A X Y P -> $f90e $6d $85 $2e $52 $60 }T T{ $93ba M $f90b M $f90c M $f90d M $f90e M -> $85 $9d $8c $93 $e4 }T
$6f07 >PC $45 >S $eb >A $f5 >X $0f >Y $e6 >P $6f07 $9d >M $6f08 $b3 >M $6f09 $89 >M $6f0a $31 >M
T{ op PC S A X Y P -> $6f0a $45 $eb $f5 $0f $e6 }T T{ $6f07 M $6f08 M $6f09 M $6f0a M $8aa8 M -> $9d $b3 $89 $31 $eb }T
$51ce >PC $4c >S $d2 >A $da >X $90 >Y $20 >P $51ce $9d >M $51cf $60 >M $51d0 $18 >M $51d1 $7b >M
T{ op PC S A X Y P -> $51d1 $4c $d2 $da $90 $20 }T T{ $193a M $51ce M $51cf M $51d0 M $51d1 M -> $d2 $9d $60 $18 $7b }T
$4aee >PC $95 >S $8f >A $c5 >X $a3 >Y $e2 >P $4aee $9d >M $4aef $1d >M $4af0 $6a >M $4af1 $8a >M
T{ op PC S A X Y P -> $4af1 $95 $8f $c5 $a3 $e2 }T T{ $4aee M $4aef M $4af0 M $4af1 M $6ae2 M -> $9d $1d $6a $8a $8f }T
$49b6 >PC $26 >S $c5 >A $7e >X $17 >Y $61 >P $49b6 $9d >M $49b7 $54 >M $49b8 $c1 >M $49b9 $c9 >M
T{ op PC S A X Y P -> $49b9 $26 $c5 $7e $17 $61 }T T{ $49b6 M $49b7 M $49b8 M $49b9 M $c1d2 M -> $9d $54 $c1 $c9 $c5 }T
$6022 >PC $0f >S $cd >A $b9 >X $07 >Y $a8 >P $6022 $9d >M $6023 $c2 >M $6024 $af >M $6025 $8a >M
T{ op PC S A X Y P -> $6025 $0f $cd $b9 $07 $a8 }T T{ $6022 M $6023 M $6024 M $6025 M $b07b M -> $9d $c2 $af $8a $cd }T
$1038 >PC $c9 >S $e9 >A $f4 >X $06 >Y $24 >P $1038 $9d >M $1039 $fa >M $103a $48 >M $103b $31 >M
T{ op PC S A X Y P -> $103b $c9 $e9 $f4 $06 $24 }T T{ $1038 M $1039 M $103a M $103b M $49ee M -> $9d $fa $48 $31 $e9 }T
$b291 >PC $95 >S $fd >A $e5 >X $da >Y $24 >P $b291 $9d >M $b292 $74 >M $b293 $5a >M $b294 $cd >M
T{ op PC S A X Y P -> $b294 $95 $fd $e5 $da $24 }T T{ $5b59 M $b291 M $b292 M $b293 M $b294 M -> $fd $9d $74 $5a $cd }T
( 9e )
$3928 >PC $22 >S $8f >A $74 >X $e5 >Y $a7 >P $3928 $9e >M $3929 $b7 >M $392a $0a >M $392b $41 >M
T{ op PC S A X Y P -> $392b $22 $8f $74 $e5 $a7 }T T{ $0b2b M $3928 M $3929 M $392a M $392b M -> $00 $9e $b7 $0a $41 }T
$f638 >PC $09 >S $2f >A $e1 >X $ca >Y $ec >P $f638 $9e >M $f639 $f4 >M $f63a $64 >M $f63b $f8 >M
T{ op PC S A X Y P -> $f63b $09 $2f $e1 $ca $ec }T T{ $65d5 M $f638 M $f639 M $f63a M $f63b M -> $00 $9e $f4 $64 $f8 }T
$5e4c >PC $2c >S $3e >A $d9 >X $33 >Y $e0 >P $5e4c $9e >M $5e4d $2e >M $5e4e $40 >M $5e4f $43 >M
T{ op PC S A X Y P -> $5e4f $2c $3e $d9 $33 $e0 }T T{ $4107 M $5e4c M $5e4d M $5e4e M $5e4f M -> $00 $9e $2e $40 $43 }T
$c112 >PC $96 >S $bf >A $0a >X $ed >Y $ad >P $c112 $9e >M $c113 $c9 >M $c114 $ae >M $c115 $aa >M
T{ op PC S A X Y P -> $c115 $96 $bf $0a $ed $ad }T T{ $aed3 M $c112 M $c113 M $c114 M $c115 M -> $00 $9e $c9 $ae $aa }T
$1c5a >PC $b3 >S $71 >A $2e >X $a7 >Y $6b >P $1c5a $9e >M $1c5b $18 >M $1c5c $61 >M $1c5d $98 >M
T{ op PC S A X Y P -> $1c5d $b3 $71 $2e $a7 $6b }T T{ $1c5a M $1c5b M $1c5c M $1c5d M $6146 M -> $9e $18 $61 $98 $00 }T
$62cb >PC $2f >S $5b >A $76 >X $bb >Y $61 >P $62cb $9e >M $62cc $6b >M $62cd $84 >M $62ce $f3 >M
T{ op PC S A X Y P -> $62ce $2f $5b $76 $bb $61 }T T{ $62cb M $62cc M $62cd M $62ce M $84e1 M -> $9e $6b $84 $f3 $00 }T
$0dd4 >PC $a3 >S $7c >A $ae >X $5d >Y $27 >P $0dd4 $9e >M $0dd5 $46 >M $0dd6 $0f >M $0dd7 $22 >M
T{ op PC S A X Y P -> $0dd7 $a3 $7c $ae $5d $27 }T T{ $0dd4 M $0dd5 M $0dd6 M $0dd7 M $0ff4 M -> $9e $46 $0f $22 $00 }T
$fca4 >PC $8b >S $76 >A $05 >X $36 >Y $2f >P $fca4 $9e >M $fca5 $1a >M $fca6 $02 >M $fca7 $85 >M
T{ op PC S A X Y P -> $fca7 $8b $76 $05 $36 $2f }T T{ $021f M $fca4 M $fca5 M $fca6 M $fca7 M -> $00 $9e $1a $02 $85 }T
$a70d >PC $5e >S $c7 >A $74 >X $4e >Y $63 >P $a70d $9e >M $a70e $aa >M $a70f $26 >M $a710 $44 >M
T{ op PC S A X Y P -> $a710 $5e $c7 $74 $4e $63 }T T{ $271e M $a70d M $a70e M $a70f M $a710 M -> $00 $9e $aa $26 $44 }T
$3286 >PC $10 >S $21 >A $8c >X $e7 >Y $a8 >P $3286 $9e >M $3287 $37 >M $3288 $5b >M $3289 $2a >M
T{ op PC S A X Y P -> $3289 $10 $21 $8c $e7 $a8 }T T{ $3286 M $3287 M $3288 M $3289 M $5bc3 M -> $9e $37 $5b $2a $00 }T
$8105 >PC $eb >S $9d >A $2f >X $64 >Y $6a >P $8105 $9e >M $8106 $44 >M $8107 $00 >M $8108 $e8 >M
T{ op PC S A X Y P -> $8108 $eb $9d $2f $64 $6a }T T{ $0073 M $8105 M $8106 M $8107 M $8108 M -> $00 $9e $44 $00 $e8 }T
$1b9a >PC $8a >S $5b >A $77 >X $46 >Y $2a >P $1b9a $9e >M $1b9b $d2 >M $1b9c $33 >M $1b9d $64 >M
T{ op PC S A X Y P -> $1b9d $8a $5b $77 $46 $2a }T T{ $1b9a M $1b9b M $1b9c M $1b9d M $3449 M -> $9e $d2 $33 $64 $00 }T
$85bb >PC $75 >S $eb >A $4e >X $bb >Y $24 >P $85bb $9e >M $85bc $a9 >M $85bd $96 >M $85be $37 >M
T{ op PC S A X Y P -> $85be $75 $eb $4e $bb $24 }T T{ $85bb M $85bc M $85bd M $85be M $96f7 M -> $9e $a9 $96 $37 $00 }T
$e960 >PC $91 >S $88 >A $23 >X $28 >Y $22 >P $e960 $9e >M $e961 $38 >M $e962 $4f >M $e963 $ed >M
T{ op PC S A X Y P -> $e963 $91 $88 $23 $28 $22 }T T{ $4f5b M $e960 M $e961 M $e962 M $e963 M -> $00 $9e $38 $4f $ed }T
$b7ed >PC $f0 >S $a8 >A $7e >X $4f >Y $6d >P $b7ed $9e >M $b7ee $e6 >M $b7ef $09 >M $b7f0 $0b >M
T{ op PC S A X Y P -> $b7f0 $f0 $a8 $7e $4f $6d }T T{ $0a64 M $b7ed M $b7ee M $b7ef M $b7f0 M -> $00 $9e $e6 $09 $0b }T
$b767 >PC $4f >S $99 >A $f3 >X $7a >Y $69 >P $b767 $9e >M $b768 $07 >M $b769 $16 >M $b76a $bd >M
T{ op PC S A X Y P -> $b76a $4f $99 $f3 $7a $69 }T T{ $16fa M $b767 M $b768 M $b769 M $b76a M -> $00 $9e $07 $16 $bd }T
( 9f )
$b83f >PC $eb >S $5f >A $ae >X $4e >Y $6b >P $0096 $00 >M $b83f $9f >M $b840 $96 >M $b841 $1d >M $b842 $04 >M $b85f $1a >M
T{ op PC S A X Y P -> $b842 $eb $5f $ae $4e $6b }T T{ $0096 M $b83f M $b840 M $b841 M $b842 M $b85f M -> $00 $9f $96 $1d $04 $1a }T
$f789 >PC $ea >S $dd >A $9f >X $06 >Y $2e >P $00ba $9c >M $f789 $9f >M $f78a $ba >M $f78b $fd >M $f78c $9d >M
T{ op PC S A X Y P -> $f78c $ea $dd $9f $06 $2e }T T{ $00ba M $f789 M $f78a M $f78b M $f78c M -> $9c $9f $ba $fd $9d }T
$6af7 >PC $84 >S $0a >A $e2 >X $87 >Y $ef >P $00dc $d8 >M $6aa1 $93 >M $6af7 $9f >M $6af8 $dc >M $6af9 $a7 >M $6afa $fe >M
T{ op PC S A X Y P -> $6afa $84 $0a $e2 $87 $ef }T T{ $00dc M $6aa1 M $6af7 M $6af8 M $6af9 M $6afa M -> $d8 $93 $9f $dc $a7 $fe }T
$784c >PC $17 >S $2c >A $d1 >X $d8 >Y $27 >P $005a $d4 >M $784c $9f >M $784d $5a >M $784e $37 >M $784f $11 >M $7886 $6b >M
T{ op PC S A X Y P -> $784f $17 $2c $d1 $d8 $27 }T T{ $005a M $784c M $784d M $784e M $784f M $7886 M -> $d4 $9f $5a $37 $11 $6b }T
$eb96 >PC $cf >S $4a >A $9a >X $68 >Y $22 >P $0089 $6d >M $eb96 $9f >M $eb97 $89 >M $eb98 $fe >M $eb99 $60 >M
T{ op PC S A X Y P -> $eb99 $cf $4a $9a $68 $22 }T T{ $0089 M $eb96 M $eb97 M $eb98 M $eb99 M -> $6d $9f $89 $fe $60 }T
$9425 >PC $fe >S $bf >A $73 >X $a7 >Y $22 >P $00a9 $66 >M $9414 $6e >M $9425 $9f >M $9426 $a9 >M $9427 $ec >M
T{ op PC S A X Y P -> $9414 $fe $bf $73 $a7 $22 }T T{ $00a9 M $9414 M $9425 M $9426 M $9427 M -> $66 $6e $9f $a9 $ec }T
$8e5a >PC $65 >S $6c >A $dc >X $8a >Y $60 >P $00db $b8 >M $8e5a $9f >M $8e5b $db >M $8e5c $16 >M $8e5d $3c >M $8e73 $02 >M
T{ op PC S A X Y P -> $8e5d $65 $6c $dc $8a $60 }T T{ $00db M $8e5a M $8e5b M $8e5c M $8e5d M $8e73 M -> $b8 $9f $db $16 $3c $02 }T
$805a >PC $b0 >S $2e >A $43 >X $e7 >Y $2e >P $0093 $5e >M $8058 $fa >M $805a $9f >M $805b $93 >M $805c $fb >M
T{ op PC S A X Y P -> $8058 $b0 $2e $43 $e7 $2e }T T{ $0093 M $8058 M $805a M $805b M $805c M -> $5e $fa $9f $93 $fb }T
$2f03 >PC $0b >S $93 >A $85 >X $4a >Y $24 >P $00d2 $66 >M $2f03 $9f >M $2f04 $d2 >M $2f05 $59 >M $2f5f $79 >M
T{ op PC S A X Y P -> $2f5f $0b $93 $85 $4a $24 }T T{ $00d2 M $2f03 M $2f04 M $2f05 M $2f5f M -> $66 $9f $d2 $59 $79 }T
$ad20 >PC $f2 >S $09 >A $2f >X $c4 >Y $e8 >P $005c $66 >M $acb7 $f0 >M $ad20 $9f >M $ad21 $5c >M $ad22 $94 >M $adb7 $bf >M
T{ op PC S A X Y P -> $acb7 $f2 $09 $2f $c4 $e8 }T T{ $005c M $acb7 M $ad20 M $ad21 M $ad22 M $adb7 M -> $66 $f0 $9f $5c $94 $bf }T
$52b3 >PC $b6 >S $b6 >A $f2 >X $f4 >Y $22 >P $00eb $7e >M $5210 $18 >M $52b3 $9f >M $52b4 $eb >M $52b5 $5a >M $5310 $9c >M
T{ op PC S A X Y P -> $5310 $b6 $b6 $f2 $f4 $22 }T T{ $00eb M $5210 M $52b3 M $52b4 M $52b5 M $5310 M -> $7e $18 $9f $eb $5a $9c }T
$04f7 >PC $2b >S $f2 >A $f5 >X $7e >Y $ec >P $00d9 $2f >M $0411 $19 >M $04f7 $9f >M $04f8 $d9 >M $04f9 $17 >M $0511 $2c >M
T{ op PC S A X Y P -> $0511 $2b $f2 $f5 $7e $ec }T T{ $00d9 M $0411 M $04f7 M $04f8 M $04f9 M $0511 M -> $2f $19 $9f $d9 $17 $2c }T
$3a4a >PC $91 >S $37 >A $a2 >X $b1 >Y $20 >P $0005 $be >M $3a4a $9f >M $3a4b $05 >M $3a4c $05 >M $3a52 $3b >M
T{ op PC S A X Y P -> $3a52 $91 $37 $a2 $b1 $20 }T T{ $0005 M $3a4a M $3a4b M $3a4c M $3a52 M -> $be $9f $05 $05 $3b }T
$c9e2 >PC $e6 >S $ba >A $05 >X $8f >Y $aa >P $000d $b7 >M $c996 $72 >M $c9e2 $9f >M $c9e3 $0d >M $c9e4 $b1 >M
T{ op PC S A X Y P -> $c996 $e6 $ba $05 $8f $aa }T T{ $000d M $c996 M $c9e2 M $c9e3 M $c9e4 M -> $b7 $72 $9f $0d $b1 }T
$b4a3 >PC $1c >S $fd >A $28 >X $84 >Y $2e >P $0089 $5b >M $b49e $ab >M $b4a3 $9f >M $b4a4 $89 >M $b4a5 $f8 >M
T{ op PC S A X Y P -> $b49e $1c $fd $28 $84 $2e }T T{ $0089 M $b49e M $b4a3 M $b4a4 M $b4a5 M -> $5b $ab $9f $89 $f8 }T
$74de >PC $34 >S $9e >A $59 >X $10 >Y $e9 >P $0036 $d3 >M $7470 $d5 >M $74de $9f >M $74df $36 >M $74e0 $8f >M
T{ op PC S A X Y P -> $7470 $34 $9e $59 $10 $e9 }T T{ $0036 M $7470 M $74de M $74df M $74e0 M -> $d3 $d5 $9f $36 $8f }T
( a0 )
$95aa >PC $84 >S $ed >A $7a >X $ed >Y $20 >P $95aa $a0 >M $95ab $f9 >M $95ac $d4 >M
T{ op PC S A X Y P -> $95ac $84 $ed $7a $f9 $a0 }T T{ $95aa M $95ab M $95ac M -> $a0 $f9 $d4 }T
$a17d >PC $2e >S $67 >A $55 >X $b7 >Y $e2 >P $a17d $a0 >M $a17e $12 >M $a17f $7f >M
T{ op PC S A X Y P -> $a17f $2e $67 $55 $12 $60 }T T{ $a17d M $a17e M $a17f M -> $a0 $12 $7f }T
$e190 >PC $c7 >S $5a >A $79 >X $84 >Y $aa >P $e190 $a0 >M $e191 $c7 >M $e192 $e1 >M
T{ op PC S A X Y P -> $e192 $c7 $5a $79 $c7 $a8 }T T{ $e190 M $e191 M $e192 M -> $a0 $c7 $e1 }T
$b8be >PC $60 >S $95 >A $39 >X $53 >Y $a2 >P $b8be $a0 >M $b8bf $55 >M $b8c0 $b7 >M
T{ op PC S A X Y P -> $b8c0 $60 $95 $39 $55 $20 }T T{ $b8be M $b8bf M $b8c0 M -> $a0 $55 $b7 }T
$3064 >PC $e2 >S $f5 >A $0e >X $1f >Y $e9 >P $3064 $a0 >M $3065 $56 >M $3066 $cf >M
T{ op PC S A X Y P -> $3066 $e2 $f5 $0e $56 $69 }T T{ $3064 M $3065 M $3066 M -> $a0 $56 $cf }T
$4e8e >PC $57 >S $35 >A $69 >X $ba >Y $e8 >P $4e8e $a0 >M $4e8f $35 >M $4e90 $72 >M
T{ op PC S A X Y P -> $4e90 $57 $35 $69 $35 $68 }T T{ $4e8e M $4e8f M $4e90 M -> $a0 $35 $72 }T
$3a27 >PC $58 >S $be >A $85 >X $17 >Y $2c >P $3a27 $a0 >M $3a28 $5c >M $3a29 $ae >M
T{ op PC S A X Y P -> $3a29 $58 $be $85 $5c $2c }T T{ $3a27 M $3a28 M $3a29 M -> $a0 $5c $ae }T
$16c1 >PC $40 >S $59 >A $4a >X $ad >Y $21 >P $16c1 $a0 >M $16c2 $52 >M $16c3 $31 >M
T{ op PC S A X Y P -> $16c3 $40 $59 $4a $52 $21 }T T{ $16c1 M $16c2 M $16c3 M -> $a0 $52 $31 }T
$a1d5 >PC $b9 >S $1d >A $21 >X $9f >Y $6d >P $a1d5 $a0 >M $a1d6 $df >M $a1d7 $25 >M
T{ op PC S A X Y P -> $a1d7 $b9 $1d $21 $df $ed }T T{ $a1d5 M $a1d6 M $a1d7 M -> $a0 $df $25 }T
$b776 >PC $9c >S $5f >A $19 >X $c4 >Y $62 >P $b776 $a0 >M $b777 $69 >M $b778 $12 >M
T{ op PC S A X Y P -> $b778 $9c $5f $19 $69 $60 }T T{ $b776 M $b777 M $b778 M -> $a0 $69 $12 }T
$5022 >PC $be >S $d5 >A $4a >X $ff >Y $ac >P $5022 $a0 >M $5023 $d9 >M $5024 $60 >M
T{ op PC S A X Y P -> $5024 $be $d5 $4a $d9 $ac }T T{ $5022 M $5023 M $5024 M -> $a0 $d9 $60 }T
$4fdb >PC $79 >S $b6 >A $80 >X $2b >Y $6c >P $4fdb $a0 >M $4fdc $73 >M $4fdd $24 >M
T{ op PC S A X Y P -> $4fdd $79 $b6 $80 $73 $6c }T T{ $4fdb M $4fdc M $4fdd M -> $a0 $73 $24 }T
$05b7 >PC $db >S $70 >A $62 >X $77 >Y $63 >P $05b7 $a0 >M $05b8 $45 >M $05b9 $95 >M
T{ op PC S A X Y P -> $05b9 $db $70 $62 $45 $61 }T T{ $05b7 M $05b8 M $05b9 M -> $a0 $45 $95 }T
$3a7e >PC $3d >S $04 >A $3a >X $33 >Y $2f >P $3a7e $a0 >M $3a7f $b4 >M $3a80 $5a >M
T{ op PC S A X Y P -> $3a80 $3d $04 $3a $b4 $ad }T T{ $3a7e M $3a7f M $3a80 M -> $a0 $b4 $5a }T
$c168 >PC $f1 >S $50 >A $b7 >X $73 >Y $23 >P $c168 $a0 >M $c169 $af >M $c16a $72 >M
T{ op PC S A X Y P -> $c16a $f1 $50 $b7 $af $a1 }T T{ $c168 M $c169 M $c16a M -> $a0 $af $72 }T
$4167 >PC $f3 >S $6c >A $d0 >X $fb >Y $2a >P $4167 $a0 >M $4168 $34 >M $4169 $79 >M
T{ op PC S A X Y P -> $4169 $f3 $6c $d0 $34 $28 }T T{ $4167 M $4168 M $4169 M -> $a0 $34 $79 }T
( a1 )
$52b3 >PC $37 >S $f1 >A $fd >X $0d >Y $e9 >P $00f0 $7c >M $00f1 $4c >M $00f3 $ed >M $4c7c $ec >M $52b3 $a1 >M $52b4 $f3 >M $52b5 $b0 >M
T{ op PC S A X Y P -> $52b5 $37 $ec $fd $0d $e9 }T T{ $00f0 M $00f1 M $00f3 M $4c7c M $52b3 M $52b4 M $52b5 M -> $7c $4c $ed $ec $a1 $f3 $b0 }T
$cdf9 >PC $d8 >S $71 >A $16 >X $05 >Y $ec >P $0065 $29 >M $007b $87 >M $007c $05 >M $0587 $40 >M $cdf9 $a1 >M $cdfa $65 >M $cdfb $c0 >M
T{ op PC S A X Y P -> $cdfb $d8 $40 $16 $05 $6c }T T{ $0065 M $007b M $007c M $0587 M $cdf9 M $cdfa M $cdfb M -> $29 $87 $05 $40 $a1 $65 $c0 }T
$4f1d >PC $a4 >S $80 >A $95 >X $cc >Y $e4 >P $007f $3f >M $0080 $e8 >M $00ea $31 >M $4f1d $a1 >M $4f1e $ea >M $4f1f $12 >M $e83f $8c >M
T{ op PC S A X Y P -> $4f1f $a4 $8c $95 $cc $e4 }T T{ $007f M $0080 M $00ea M $4f1d M $4f1e M $4f1f M $e83f M -> $3f $e8 $31 $a1 $ea $12 $8c }T
$75c9 >PC $85 >S $3b >A $43 >X $f0 >Y $e0 >P $003c $89 >M $007f $92 >M $0080 $ef >M $75c9 $a1 >M $75ca $3c >M $75cb $ca >M $ef92 $86 >M
T{ op PC S A X Y P -> $75cb $85 $86 $43 $f0 $e0 }T T{ $003c M $007f M $0080 M $75c9 M $75ca M $75cb M $ef92 M -> $89 $92 $ef $a1 $3c $ca $86 }T
$fa4b >PC $43 >S $5b >A $40 >X $8f >Y $ed >P $0076 $5e >M $00b6 $7b >M $00b7 $36 >M $367b $4c >M $fa4b $a1 >M $fa4c $76 >M $fa4d $45 >M
T{ op PC S A X Y P -> $fa4d $43 $4c $40 $8f $6d }T T{ $0076 M $00b6 M $00b7 M $367b M $fa4b M $fa4c M $fa4d M -> $5e $7b $36 $4c $a1 $76 $45 }T
$e27e >PC $fa >S $86 >A $ed >X $02 >Y $27 >P $0011 $61 >M $00fe $ad >M $00ff $9f >M $9fad $c2 >M $e27e $a1 >M $e27f $11 >M $e280 $63 >M
T{ op PC S A X Y P -> $e280 $fa $c2 $ed $02 $a5 }T T{ $0011 M $00fe M $00ff M $9fad M $e27e M $e27f M $e280 M -> $61 $ad $9f $c2 $a1 $11 $63 }T
$b259 >PC $0a >S $ee >A $65 >X $07 >Y $a8 >P $001b $1c >M $0080 $d7 >M $0081 $29 >M $29d7 $af >M $b259 $a1 >M $b25a $1b >M $b25b $19 >M
T{ op PC S A X Y P -> $b25b $0a $af $65 $07 $a8 }T T{ $001b M $0080 M $0081 M $29d7 M $b259 M $b25a M $b25b M -> $1c $d7 $29 $af $a1 $1b $19 }T
$9a70 >PC $95 >S $b0 >A $49 >X $8b >Y $6a >P $0022 $0b >M $006b $ee >M $006c $d9 >M $9a70 $a1 >M $9a71 $22 >M $9a72 $79 >M $d9ee $78 >M
T{ op PC S A X Y P -> $9a72 $95 $78 $49 $8b $68 }T T{ $0022 M $006b M $006c M $9a70 M $9a71 M $9a72 M $d9ee M -> $0b $ee $d9 $a1 $22 $79 $78 }T
$474c >PC $5f >S $95 >A $c9 >X $c3 >Y $ef >P $0093 $36 >M $0094 $6b >M $00ca $76 >M $474c $a1 >M $474d $ca >M $474e $5c >M $6b36 $c8 >M
T{ op PC S A X Y P -> $474e $5f $c8 $c9 $c3 $ed }T T{ $0093 M $0094 M $00ca M $474c M $474d M $474e M $6b36 M -> $36 $6b $76 $a1 $ca $5c $c8 }T
$58d4 >PC $5c >S $2e >A $46 >X $ca >Y $a4 >P $00a9 $a1 >M $00ef $a0 >M $00f0 $6f >M $58d4 $a1 >M $58d5 $a9 >M $58d6 $db >M $6fa0 $1c >M
T{ op PC S A X Y P -> $58d6 $5c $1c $46 $ca $24 }T T{ $00a9 M $00ef M $00f0 M $58d4 M $58d5 M $58d6 M $6fa0 M -> $a1 $a0 $6f $a1 $a9 $db $1c }T
$9eaa >PC $e7 >S $7d >A $71 >X $b2 >Y $24 >P $0051 $d9 >M $0052 $f3 >M $00e0 $fe >M $9eaa $a1 >M $9eab $e0 >M $9eac $e8 >M $f3d9 $78 >M
T{ op PC S A X Y P -> $9eac $e7 $78 $71 $b2 $24 }T T{ $0051 M $0052 M $00e0 M $9eaa M $9eab M $9eac M $f3d9 M -> $d9 $f3 $fe $a1 $e0 $e8 $78 }T
$ae87 >PC $78 >S $1f >A $e1 >X $71 >Y $6d >P $0079 $a3 >M $007a $ad >M $0098 $4c >M $ada3 $b4 >M $ae87 $a1 >M $ae88 $98 >M $ae89 $b0 >M
T{ op PC S A X Y P -> $ae89 $78 $b4 $e1 $71 $ed }T T{ $0079 M $007a M $0098 M $ada3 M $ae87 M $ae88 M $ae89 M -> $a3 $ad $4c $b4 $a1 $98 $b0 }T
$27a5 >PC $b7 >S $71 >A $9d >X $d5 >Y $62 >P $000f $06 >M $00ac $78 >M $00ad $1d >M $1d78 $c8 >M $27a5 $a1 >M $27a6 $0f >M $27a7 $34 >M
T{ op PC S A X Y P -> $27a7 $b7 $c8 $9d $d5 $e0 }T T{ $000f M $00ac M $00ad M $1d78 M $27a5 M $27a6 M $27a7 M -> $06 $78 $1d $c8 $a1 $0f $34 }T
$e957 >PC $f0 >S $b3 >A $c1 >X $03 >Y $aa >P $0057 $c6 >M $0058 $df >M $0096 $7f >M $dfc6 $84 >M $e957 $a1 >M $e958 $96 >M $e959 $18 >M
T{ op PC S A X Y P -> $e959 $f0 $84 $c1 $03 $a8 }T T{ $0057 M $0058 M $0096 M $dfc6 M $e957 M $e958 M $e959 M -> $c6 $df $7f $84 $a1 $96 $18 }T
$d3f5 >PC $1b >S $b1 >A $e0 >X $33 >Y $6a >P $0064 $77 >M $0065 $c7 >M $0084 $9a >M $c777 $35 >M $d3f5 $a1 >M $d3f6 $84 >M $d3f7 $3d >M
T{ op PC S A X Y P -> $d3f7 $1b $35 $e0 $33 $68 }T T{ $0064 M $0065 M $0084 M $c777 M $d3f5 M $d3f6 M $d3f7 M -> $77 $c7 $9a $35 $a1 $84 $3d }T
$2ecf >PC $11 >S $60 >A $ec >X $2b >Y $26 >P $0081 $3d >M $0082 $fe >M $0095 $73 >M $2ecf $a1 >M $2ed0 $95 >M $2ed1 $e7 >M $fe3d $34 >M
T{ op PC S A X Y P -> $2ed1 $11 $34 $ec $2b $24 }T T{ $0081 M $0082 M $0095 M $2ecf M $2ed0 M $2ed1 M $fe3d M -> $3d $fe $73 $a1 $95 $e7 $34 }T
( a2 )
$b0b0 >PC $fd >S $15 >A $99 >X $0f >Y $a6 >P $b0b0 $a2 >M $b0b1 $71 >M $b0b2 $e1 >M
T{ op PC S A X Y P -> $b0b2 $fd $15 $71 $0f $24 }T T{ $b0b0 M $b0b1 M $b0b2 M -> $a2 $71 $e1 }T
$317e >PC $6c >S $ee >A $ff >X $b5 >Y $27 >P $317e $a2 >M $317f $5a >M $3180 $c4 >M
T{ op PC S A X Y P -> $3180 $6c $ee $5a $b5 $25 }T T{ $317e M $317f M $3180 M -> $a2 $5a $c4 }T
$b6f7 >PC $8b >S $d2 >A $88 >X $6c >Y $e2 >P $b6f7 $a2 >M $b6f8 $50 >M $b6f9 $41 >M
T{ op PC S A X Y P -> $b6f9 $8b $d2 $50 $6c $60 }T T{ $b6f7 M $b6f8 M $b6f9 M -> $a2 $50 $41 }T
$b30f >PC $09 >S $b4 >A $67 >X $f8 >Y $62 >P $b30f $a2 >M $b310 $62 >M $b311 $f4 >M
T{ op PC S A X Y P -> $b311 $09 $b4 $62 $f8 $60 }T T{ $b30f M $b310 M $b311 M -> $a2 $62 $f4 }T
$4fc4 >PC $a3 >S $fe >A $dc >X $81 >Y $e9 >P $4fc4 $a2 >M $4fc5 $2e >M $4fc6 $52 >M
T{ op PC S A X Y P -> $4fc6 $a3 $fe $2e $81 $69 }T T{ $4fc4 M $4fc5 M $4fc6 M -> $a2 $2e $52 }T
$5abd >PC $60 >S $27 >A $c3 >X $4c >Y $25 >P $5abd $a2 >M $5abe $49 >M $5abf $e9 >M
T{ op PC S A X Y P -> $5abf $60 $27 $49 $4c $25 }T T{ $5abd M $5abe M $5abf M -> $a2 $49 $e9 }T
$1001 >PC $62 >S $92 >A $a4 >X $bd >Y $60 >P $1001 $a2 >M $1002 $fe >M $1003 $63 >M
T{ op PC S A X Y P -> $1003 $62 $92 $fe $bd $e0 }T T{ $1001 M $1002 M $1003 M -> $a2 $fe $63 }T
$e7ff >PC $59 >S $3d >A $ec >X $8f >Y $eb >P $e7ff $a2 >M $e800 $dc >M $e801 $17 >M
T{ op PC S A X Y P -> $e801 $59 $3d $dc $8f $e9 }T T{ $e7ff M $e800 M $e801 M -> $a2 $dc $17 }T
$fc2c >PC $f2 >S $97 >A $23 >X $bf >Y $e0 >P $fc2c $a2 >M $fc2d $08 >M $fc2e $a3 >M
T{ op PC S A X Y P -> $fc2e $f2 $97 $08 $bf $60 }T T{ $fc2c M $fc2d M $fc2e M -> $a2 $08 $a3 }T
$bcfa >PC $7c >S $8e >A $f9 >X $fe >Y $a4 >P $bcfa $a2 >M $bcfb $ab >M $bcfc $9e >M
T{ op PC S A X Y P -> $bcfc $7c $8e $ab $fe $a4 }T T{ $bcfa M $bcfb M $bcfc M -> $a2 $ab $9e }T
$a464 >PC $3b >S $b5 >A $89 >X $69 >Y $e3 >P $a464 $a2 >M $a465 $4e >M $a466 $20 >M
T{ op PC S A X Y P -> $a466 $3b $b5 $4e $69 $61 }T T{ $a464 M $a465 M $a466 M -> $a2 $4e $20 }T
$b567 >PC $ca >S $ad >A $6d >X $b6 >Y $6d >P $b567 $a2 >M $b568 $eb >M $b569 $c2 >M
T{ op PC S A X Y P -> $b569 $ca $ad $eb $b6 $ed }T T{ $b567 M $b568 M $b569 M -> $a2 $eb $c2 }T
$c9e7 >PC $85 >S $a5 >A $99 >X $6b >Y $26 >P $c9e7 $a2 >M $c9e8 $5f >M $c9e9 $9d >M
T{ op PC S A X Y P -> $c9e9 $85 $a5 $5f $6b $24 }T T{ $c9e7 M $c9e8 M $c9e9 M -> $a2 $5f $9d }T
$4d06 >PC $74 >S $13 >A $a5 >X $e8 >Y $ac >P $4d06 $a2 >M $4d07 $4c >M $4d08 $87 >M
T{ op PC S A X Y P -> $4d08 $74 $13 $4c $e8 $2c }T T{ $4d06 M $4d07 M $4d08 M -> $a2 $4c $87 }T
$4a24 >PC $78 >S $f3 >A $6b >X $1d >Y $2c >P $4a24 $a2 >M $4a25 $ef >M $4a26 $6e >M
T{ op PC S A X Y P -> $4a26 $78 $f3 $ef $1d $ac }T T{ $4a24 M $4a25 M $4a26 M -> $a2 $ef $6e }T
$0f9d >PC $de >S $8e >A $4d >X $4a >Y $a5 >P $0f9d $a2 >M $0f9e $23 >M $0f9f $dc >M
T{ op PC S A X Y P -> $0f9f $de $8e $23 $4a $25 }T T{ $0f9d M $0f9e M $0f9f M -> $a2 $23 $dc }T
( a3 )
$ee52 >PC $2f >S $16 >A $50 >X $63 >Y $2b >P $ee52 $a3 >M $ee53 $7b >M $ee54 $5e >M
T{ op PC S A X Y P -> $ee53 $2f $16 $50 $63 $2b }T T{ $ee52 M $ee53 M $ee54 M -> $a3 $7b $5e }T
$1fdc >PC $00 >S $aa >A $a5 >X $ec >Y $20 >P $1fdc $a3 >M $1fdd $ce >M $1fde $bb >M
T{ op PC S A X Y P -> $1fdd $00 $aa $a5 $ec $20 }T T{ $1fdc M $1fdd M $1fde M -> $a3 $ce $bb }T
$08d0 >PC $14 >S $21 >A $ac >X $10 >Y $6d >P $08d0 $a3 >M $08d1 $1b >M $08d2 $27 >M
T{ op PC S A X Y P -> $08d1 $14 $21 $ac $10 $6d }T T{ $08d0 M $08d1 M $08d2 M -> $a3 $1b $27 }T
$9d7e >PC $b4 >S $71 >A $a9 >X $dd >Y $6b >P $9d7e $a3 >M $9d7f $e5 >M $9d80 $2c >M
T{ op PC S A X Y P -> $9d7f $b4 $71 $a9 $dd $6b }T T{ $9d7e M $9d7f M $9d80 M -> $a3 $e5 $2c }T
$dc29 >PC $5c >S $59 >A $6f >X $f0 >Y $a5 >P $dc29 $a3 >M $dc2a $f0 >M $dc2b $74 >M
T{ op PC S A X Y P -> $dc2a $5c $59 $6f $f0 $a5 }T T{ $dc29 M $dc2a M $dc2b M -> $a3 $f0 $74 }T
$11d1 >PC $1e >S $42 >A $fc >X $1f >Y $6e >P $11d1 $a3 >M $11d2 $37 >M $11d3 $68 >M
T{ op PC S A X Y P -> $11d2 $1e $42 $fc $1f $6e }T T{ $11d1 M $11d2 M $11d3 M -> $a3 $37 $68 }T
$b8a3 >PC $c1 >S $25 >A $1b >X $7c >Y $28 >P $b8a3 $a3 >M $b8a4 $13 >M $b8a5 $fd >M
T{ op PC S A X Y P -> $b8a4 $c1 $25 $1b $7c $28 }T T{ $b8a3 M $b8a4 M $b8a5 M -> $a3 $13 $fd }T
$fdc6 >PC $74 >S $77 >A $7a >X $83 >Y $26 >P $fdc6 $a3 >M $fdc7 $67 >M $fdc8 $50 >M
T{ op PC S A X Y P -> $fdc7 $74 $77 $7a $83 $26 }T T{ $fdc6 M $fdc7 M $fdc8 M -> $a3 $67 $50 }T
$dab5 >PC $64 >S $27 >A $2c >X $65 >Y $e2 >P $dab5 $a3 >M $dab6 $14 >M $dab7 $f8 >M
T{ op PC S A X Y P -> $dab6 $64 $27 $2c $65 $e2 }T T{ $dab5 M $dab6 M $dab7 M -> $a3 $14 $f8 }T
$0ba2 >PC $22 >S $98 >A $53 >X $33 >Y $24 >P $0ba2 $a3 >M $0ba3 $ff >M $0ba4 $d5 >M
T{ op PC S A X Y P -> $0ba3 $22 $98 $53 $33 $24 }T T{ $0ba2 M $0ba3 M $0ba4 M -> $a3 $ff $d5 }T
$a7fa >PC $eb >S $50 >A $83 >X $8b >Y $e4 >P $a7fa $a3 >M $a7fb $16 >M $a7fc $ff >M
T{ op PC S A X Y P -> $a7fb $eb $50 $83 $8b $e4 }T T{ $a7fa M $a7fb M $a7fc M -> $a3 $16 $ff }T
$e26c >PC $bd >S $2e >A $79 >X $f2 >Y $22 >P $e26c $a3 >M $e26d $cd >M $e26e $31 >M
T{ op PC S A X Y P -> $e26d $bd $2e $79 $f2 $22 }T T{ $e26c M $e26d M $e26e M -> $a3 $cd $31 }T
$9fb6 >PC $ef >S $6a >A $a1 >X $60 >Y $62 >P $9fb6 $a3 >M $9fb7 $0d >M $9fb8 $7e >M
T{ op PC S A X Y P -> $9fb7 $ef $6a $a1 $60 $62 }T T{ $9fb6 M $9fb7 M $9fb8 M -> $a3 $0d $7e }T
$1b6f >PC $55 >S $eb >A $4d >X $c0 >Y $28 >P $1b6f $a3 >M $1b70 $b3 >M $1b71 $ce >M
T{ op PC S A X Y P -> $1b70 $55 $eb $4d $c0 $28 }T T{ $1b6f M $1b70 M $1b71 M -> $a3 $b3 $ce }T
$d1f2 >PC $56 >S $46 >A $80 >X $bf >Y $67 >P $d1f2 $a3 >M $d1f3 $c4 >M $d1f4 $f6 >M
T{ op PC S A X Y P -> $d1f3 $56 $46 $80 $bf $67 }T T{ $d1f2 M $d1f3 M $d1f4 M -> $a3 $c4 $f6 }T
$e0b3 >PC $9b >S $e8 >A $d2 >X $64 >Y $a6 >P $e0b3 $a3 >M $e0b4 $60 >M $e0b5 $5d >M
T{ op PC S A X Y P -> $e0b4 $9b $e8 $d2 $64 $a6 }T T{ $e0b3 M $e0b4 M $e0b5 M -> $a3 $60 $5d }T
( a4 )
$3cc7 >PC $8a >S $d2 >A $3d >X $cd >Y $24 >P $0019 $91 >M $3cc7 $a4 >M $3cc8 $19 >M $3cc9 $ea >M
T{ op PC S A X Y P -> $3cc9 $8a $d2 $3d $91 $a4 }T T{ $0019 M $3cc7 M $3cc8 M $3cc9 M -> $91 $a4 $19 $ea }T
$6c94 >PC $9e >S $cc >A $ea >X $1e >Y $ab >P $007f $b7 >M $6c94 $a4 >M $6c95 $7f >M $6c96 $2d >M
T{ op PC S A X Y P -> $6c96 $9e $cc $ea $b7 $a9 }T T{ $007f M $6c94 M $6c95 M $6c96 M -> $b7 $a4 $7f $2d }T
$2916 >PC $00 >S $c3 >A $73 >X $2d >Y $e8 >P $00d5 $13 >M $2916 $a4 >M $2917 $d5 >M $2918 $bc >M
T{ op PC S A X Y P -> $2918 $00 $c3 $73 $13 $68 }T T{ $00d5 M $2916 M $2917 M $2918 M -> $13 $a4 $d5 $bc }T
$e230 >PC $43 >S $7a >A $27 >X $b6 >Y $e7 >P $0040 $2c >M $e230 $a4 >M $e231 $40 >M $e232 $53 >M
T{ op PC S A X Y P -> $e232 $43 $7a $27 $2c $65 }T T{ $0040 M $e230 M $e231 M $e232 M -> $2c $a4 $40 $53 }T
$b1ca >PC $a0 >S $27 >A $00 >X $eb >Y $ab >P $0062 $1d >M $b1ca $a4 >M $b1cb $62 >M $b1cc $68 >M
T{ op PC S A X Y P -> $b1cc $a0 $27 $00 $1d $29 }T T{ $0062 M $b1ca M $b1cb M $b1cc M -> $1d $a4 $62 $68 }T
$3a42 >PC $5b >S $ec >A $fc >X $89 >Y $65 >P $0029 $9c >M $3a42 $a4 >M $3a43 $29 >M $3a44 $7c >M
T{ op PC S A X Y P -> $3a44 $5b $ec $fc $9c $e5 }T T{ $0029 M $3a42 M $3a43 M $3a44 M -> $9c $a4 $29 $7c }T
$d2be >PC $e2 >S $73 >A $9b >X $23 >Y $6d >P $0006 $d0 >M $d2be $a4 >M $d2bf $06 >M $d2c0 $db >M
T{ op PC S A X Y P -> $d2c0 $e2 $73 $9b $d0 $ed }T T{ $0006 M $d2be M $d2bf M $d2c0 M -> $d0 $a4 $06 $db }T
$8ea4 >PC $a2 >S $4b >A $72 >X $b9 >Y $aa >P $0079 $ff >M $8ea4 $a4 >M $8ea5 $79 >M $8ea6 $a6 >M
T{ op PC S A X Y P -> $8ea6 $a2 $4b $72 $ff $a8 }T T{ $0079 M $8ea4 M $8ea5 M $8ea6 M -> $ff $a4 $79 $a6 }T
$6e92 >PC $48 >S $9d >A $b6 >X $11 >Y $e3 >P $0002 $f9 >M $6e92 $a4 >M $6e93 $02 >M $6e94 $9e >M
T{ op PC S A X Y P -> $6e94 $48 $9d $b6 $f9 $e1 }T T{ $0002 M $6e92 M $6e93 M $6e94 M -> $f9 $a4 $02 $9e }T
$be3c >PC $75 >S $b4 >A $8b >X $ad >Y $27 >P $00ca $e9 >M $be3c $a4 >M $be3d $ca >M $be3e $75 >M
T{ op PC S A X Y P -> $be3e $75 $b4 $8b $e9 $a5 }T T{ $00ca M $be3c M $be3d M $be3e M -> $e9 $a4 $ca $75 }T
$b1eb >PC $cf >S $6f >A $99 >X $ef >Y $2f >P $0018 $07 >M $b1eb $a4 >M $b1ec $18 >M $b1ed $6d >M
T{ op PC S A X Y P -> $b1ed $cf $6f $99 $07 $2d }T T{ $0018 M $b1eb M $b1ec M $b1ed M -> $07 $a4 $18 $6d }T
$6176 >PC $01 >S $56 >A $70 >X $d9 >Y $69 >P $0043 $80 >M $6176 $a4 >M $6177 $43 >M $6178 $52 >M
T{ op PC S A X Y P -> $6178 $01 $56 $70 $80 $e9 }T T{ $0043 M $6176 M $6177 M $6178 M -> $80 $a4 $43 $52 }T
$7a1e >PC $d9 >S $a1 >A $24 >X $89 >Y $2e >P $0071 $79 >M $7a1e $a4 >M $7a1f $71 >M $7a20 $28 >M
T{ op PC S A X Y P -> $7a20 $d9 $a1 $24 $79 $2c }T T{ $0071 M $7a1e M $7a1f M $7a20 M -> $79 $a4 $71 $28 }T
$96f2 >PC $55 >S $2b >A $77 >X $b2 >Y $65 >P $0071 $ef >M $96f2 $a4 >M $96f3 $71 >M $96f4 $dd >M
T{ op PC S A X Y P -> $96f4 $55 $2b $77 $ef $e5 }T T{ $0071 M $96f2 M $96f3 M $96f4 M -> $ef $a4 $71 $dd }T
$791b >PC $08 >S $29 >A $8b >X $33 >Y $a6 >P $0070 $c4 >M $791b $a4 >M $791c $70 >M $791d $e7 >M
T{ op PC S A X Y P -> $791d $08 $29 $8b $c4 $a4 }T T{ $0070 M $791b M $791c M $791d M -> $c4 $a4 $70 $e7 }T
$ca79 >PC $5d >S $87 >A $7d >X $0d >Y $a9 >P $0082 $a4 >M $ca79 $a4 >M $ca7a $82 >M $ca7b $e5 >M
T{ op PC S A X Y P -> $ca7b $5d $87 $7d $a4 $a9 }T T{ $0082 M $ca79 M $ca7a M $ca7b M -> $a4 $a4 $82 $e5 }T
( a5 )
$532a >PC $84 >S $87 >A $1d >X $bd >Y $69 >P $00a9 $b9 >M $532a $a5 >M $532b $a9 >M $532c $a4 >M
T{ op PC S A X Y P -> $532c $84 $b9 $1d $bd $e9 }T T{ $00a9 M $532a M $532b M $532c M -> $b9 $a5 $a9 $a4 }T
$66ec >PC $74 >S $be >A $97 >X $3c >Y $e4 >P $0031 $f4 >M $66ec $a5 >M $66ed $31 >M $66ee $49 >M
T{ op PC S A X Y P -> $66ee $74 $f4 $97 $3c $e4 }T T{ $0031 M $66ec M $66ed M $66ee M -> $f4 $a5 $31 $49 }T
$0610 >PC $5b >S $73 >A $73 >X $9d >Y $a6 >P $0066 $32 >M $0610 $a5 >M $0611 $66 >M $0612 $01 >M
T{ op PC S A X Y P -> $0612 $5b $32 $73 $9d $24 }T T{ $0066 M $0610 M $0611 M $0612 M -> $32 $a5 $66 $01 }T
$4598 >PC $56 >S $9b >A $bd >X $4f >Y $e2 >P $001d $88 >M $4598 $a5 >M $4599 $1d >M $459a $78 >M
T{ op PC S A X Y P -> $459a $56 $88 $bd $4f $e0 }T T{ $001d M $4598 M $4599 M $459a M -> $88 $a5 $1d $78 }T
$af7a >PC $30 >S $c6 >A $f2 >X $9b >Y $e3 >P $0060 $09 >M $af7a $a5 >M $af7b $60 >M $af7c $1a >M
T{ op PC S A X Y P -> $af7c $30 $09 $f2 $9b $61 }T T{ $0060 M $af7a M $af7b M $af7c M -> $09 $a5 $60 $1a }T
$4d20 >PC $70 >S $2d >A $e2 >X $79 >Y $28 >P $00dd $e3 >M $4d20 $a5 >M $4d21 $dd >M $4d22 $4b >M
T{ op PC S A X Y P -> $4d22 $70 $e3 $e2 $79 $a8 }T T{ $00dd M $4d20 M $4d21 M $4d22 M -> $e3 $a5 $dd $4b }T
$8fdb >PC $96 >S $42 >A $cf >X $fa >Y $6a >P $003e $ef >M $8fdb $a5 >M $8fdc $3e >M $8fdd $f8 >M
T{ op PC S A X Y P -> $8fdd $96 $ef $cf $fa $e8 }T T{ $003e M $8fdb M $8fdc M $8fdd M -> $ef $a5 $3e $f8 }T
$2565 >PC $30 >S $26 >A $89 >X $2d >Y $e1 >P $00e9 $b1 >M $2565 $a5 >M $2566 $e9 >M $2567 $3c >M
T{ op PC S A X Y P -> $2567 $30 $b1 $89 $2d $e1 }T T{ $00e9 M $2565 M $2566 M $2567 M -> $b1 $a5 $e9 $3c }T
$6569 >PC $83 >S $53 >A $2f >X $16 >Y $a0 >P $00d8 $d3 >M $6569 $a5 >M $656a $d8 >M $656b $d1 >M
T{ op PC S A X Y P -> $656b $83 $d3 $2f $16 $a0 }T T{ $00d8 M $6569 M $656a M $656b M -> $d3 $a5 $d8 $d1 }T
$8bef >PC $7a >S $ef >A $67 >X $b9 >Y $a8 >P $005f $7d >M $8bef $a5 >M $8bf0 $5f >M $8bf1 $b5 >M
T{ op PC S A X Y P -> $8bf1 $7a $7d $67 $b9 $28 }T T{ $005f M $8bef M $8bf0 M $8bf1 M -> $7d $a5 $5f $b5 }T
$5d3b >PC $e8 >S $bc >A $c0 >X $78 >Y $68 >P $003d $91 >M $5d3b $a5 >M $5d3c $3d >M $5d3d $62 >M
T{ op PC S A X Y P -> $5d3d $e8 $91 $c0 $78 $e8 }T T{ $003d M $5d3b M $5d3c M $5d3d M -> $91 $a5 $3d $62 }T
$e7f0 >PC $b9 >S $8e >A $98 >X $39 >Y $64 >P $0001 $a0 >M $e7f0 $a5 >M $e7f1 $01 >M $e7f2 $aa >M
T{ op PC S A X Y P -> $e7f2 $b9 $a0 $98 $39 $e4 }T T{ $0001 M $e7f0 M $e7f1 M $e7f2 M -> $a0 $a5 $01 $aa }T
$dfd0 >PC $4b >S $52 >A $6c >X $53 >Y $ac >P $0038 $1a >M $dfd0 $a5 >M $dfd1 $38 >M $dfd2 $f2 >M
T{ op PC S A X Y P -> $dfd2 $4b $1a $6c $53 $2c }T T{ $0038 M $dfd0 M $dfd1 M $dfd2 M -> $1a $a5 $38 $f2 }T
$e1c1 >PC $99 >S $d4 >A $ae >X $cd >Y $a8 >P $00f7 $ba >M $e1c1 $a5 >M $e1c2 $f7 >M $e1c3 $87 >M
T{ op PC S A X Y P -> $e1c3 $99 $ba $ae $cd $a8 }T T{ $00f7 M $e1c1 M $e1c2 M $e1c3 M -> $ba $a5 $f7 $87 }T
$c15c >PC $74 >S $91 >A $aa >X $a9 >Y $29 >P $00f4 $93 >M $c15c $a5 >M $c15d $f4 >M $c15e $c4 >M
T{ op PC S A X Y P -> $c15e $74 $93 $aa $a9 $a9 }T T{ $00f4 M $c15c M $c15d M $c15e M -> $93 $a5 $f4 $c4 }T
$5d2c >PC $b9 >S $f7 >A $d8 >X $f4 >Y $61 >P $0010 $db >M $5d2c $a5 >M $5d2d $10 >M $5d2e $57 >M
T{ op PC S A X Y P -> $5d2e $b9 $db $d8 $f4 $e1 }T T{ $0010 M $5d2c M $5d2d M $5d2e M -> $db $a5 $10 $57 }T
( a6 )
$cdfd >PC $76 >S $cd >A $83 >X $05 >Y $a9 >P $00b1 $5d >M $cdfd $a6 >M $cdfe $b1 >M $cdff $5d >M
T{ op PC S A X Y P -> $cdff $76 $cd $5d $05 $29 }T T{ $00b1 M $cdfd M $cdfe M $cdff M -> $5d $a6 $b1 $5d }T
$47d4 >PC $37 >S $9f >A $82 >X $a8 >Y $63 >P $00f6 $cc >M $47d4 $a6 >M $47d5 $f6 >M $47d6 $a3 >M
T{ op PC S A X Y P -> $47d6 $37 $9f $cc $a8 $e1 }T T{ $00f6 M $47d4 M $47d5 M $47d6 M -> $cc $a6 $f6 $a3 }T
$66ef >PC $f1 >S $3b >A $b2 >X $5e >Y $61 >P $0023 $f1 >M $66ef $a6 >M $66f0 $23 >M $66f1 $63 >M
T{ op PC S A X Y P -> $66f1 $f1 $3b $f1 $5e $e1 }T T{ $0023 M $66ef M $66f0 M $66f1 M -> $f1 $a6 $23 $63 }T
$6b28 >PC $8f >S $3f >A $30 >X $cc >Y $27 >P $004b $1c >M $6b28 $a6 >M $6b29 $4b >M $6b2a $6e >M
T{ op PC S A X Y P -> $6b2a $8f $3f $1c $cc $25 }T T{ $004b M $6b28 M $6b29 M $6b2a M -> $1c $a6 $4b $6e }T
$c1e1 >PC $92 >S $f6 >A $28 >X $df >Y $2b >P $00a1 $60 >M $c1e1 $a6 >M $c1e2 $a1 >M $c1e3 $32 >M
T{ op PC S A X Y P -> $c1e3 $92 $f6 $60 $df $29 }T T{ $00a1 M $c1e1 M $c1e2 M $c1e3 M -> $60 $a6 $a1 $32 }T
$2af1 >PC $b6 >S $4d >A $cf >X $76 >Y $eb >P $0006 $aa >M $2af1 $a6 >M $2af2 $06 >M $2af3 $ed >M
T{ op PC S A X Y P -> $2af3 $b6 $4d $aa $76 $e9 }T T{ $0006 M $2af1 M $2af2 M $2af3 M -> $aa $a6 $06 $ed }T
$9f64 >PC $b1 >S $45 >A $06 >X $91 >Y $25 >P $00ba $89 >M $9f64 $a6 >M $9f65 $ba >M $9f66 $25 >M
T{ op PC S A X Y P -> $9f66 $b1 $45 $89 $91 $a5 }T T{ $00ba M $9f64 M $9f65 M $9f66 M -> $89 $a6 $ba $25 }T
$5d8f >PC $39 >S $f4 >A $f3 >X $00 >Y $eb >P $0065 $2a >M $5d8f $a6 >M $5d90 $65 >M $5d91 $c5 >M
T{ op PC S A X Y P -> $5d91 $39 $f4 $2a $00 $69 }T T{ $0065 M $5d8f M $5d90 M $5d91 M -> $2a $a6 $65 $c5 }T
$31d3 >PC $55 >S $e6 >A $c0 >X $59 >Y $24 >P $002c $76 >M $31d3 $a6 >M $31d4 $2c >M $31d5 $e9 >M
T{ op PC S A X Y P -> $31d5 $55 $e6 $76 $59 $24 }T T{ $002c M $31d3 M $31d4 M $31d5 M -> $76 $a6 $2c $e9 }T
$c5c7 >PC $38 >S $61 >A $5b >X $e6 >Y $a6 >P $0045 $a0 >M $c5c7 $a6 >M $c5c8 $45 >M $c5c9 $5e >M
T{ op PC S A X Y P -> $c5c9 $38 $61 $a0 $e6 $a4 }T T{ $0045 M $c5c7 M $c5c8 M $c5c9 M -> $a0 $a6 $45 $5e }T
$1958 >PC $ff >S $d0 >A $81 >X $9a >Y $aa >P $004a $85 >M $1958 $a6 >M $1959 $4a >M $195a $8e >M
T{ op PC S A X Y P -> $195a $ff $d0 $85 $9a $a8 }T T{ $004a M $1958 M $1959 M $195a M -> $85 $a6 $4a $8e }T
$2ce4 >PC $76 >S $24 >A $7f >X $dd >Y $eb >P $00f9 $2f >M $2ce4 $a6 >M $2ce5 $f9 >M $2ce6 $c1 >M
T{ op PC S A X Y P -> $2ce6 $76 $24 $2f $dd $69 }T T{ $00f9 M $2ce4 M $2ce5 M $2ce6 M -> $2f $a6 $f9 $c1 }T
$d9ad >PC $5c >S $e4 >A $f2 >X $f0 >Y $2a >P $009c $ce >M $d9ad $a6 >M $d9ae $9c >M $d9af $72 >M
T{ op PC S A X Y P -> $d9af $5c $e4 $ce $f0 $a8 }T T{ $009c M $d9ad M $d9ae M $d9af M -> $ce $a6 $9c $72 }T
$e640 >PC $eb >S $a6 >A $ce >X $05 >Y $a8 >P $00c7 $e6 >M $e640 $a6 >M $e641 $c7 >M $e642 $5e >M
T{ op PC S A X Y P -> $e642 $eb $a6 $e6 $05 $a8 }T T{ $00c7 M $e640 M $e641 M $e642 M -> $e6 $a6 $c7 $5e }T
$4eb9 >PC $7a >S $eb >A $f5 >X $02 >Y $e9 >P $00f1 $37 >M $4eb9 $a6 >M $4eba $f1 >M $4ebb $a4 >M
T{ op PC S A X Y P -> $4ebb $7a $eb $37 $02 $69 }T T{ $00f1 M $4eb9 M $4eba M $4ebb M -> $37 $a6 $f1 $a4 }T
$5a68 >PC $c8 >S $35 >A $b6 >X $3f >Y $66 >P $0050 $03 >M $5a68 $a6 >M $5a69 $50 >M $5a6a $ea >M
T{ op PC S A X Y P -> $5a6a $c8 $35 $03 $3f $64 }T T{ $0050 M $5a68 M $5a69 M $5a6a M -> $03 $a6 $50 $ea }T
( a7 )
$60d2 >PC $53 >S $19 >A $6d >X $fd >Y $e9 >P $0029 $c1 >M $60d2 $a7 >M $60d3 $29 >M $60d4 $e1 >M
T{ op PC S A X Y P -> $60d4 $53 $19 $6d $fd $e9 }T T{ $0029 M $60d2 M $60d3 M $60d4 M -> $c5 $a7 $29 $e1 }T
$9bd2 >PC $3d >S $19 >A $53 >X $5d >Y $e2 >P $0079 $6d >M $9bd2 $a7 >M $9bd3 $79 >M $9bd4 $1f >M
T{ op PC S A X Y P -> $9bd4 $3d $19 $53 $5d $e2 }T T{ $0079 M $9bd2 M $9bd3 M $9bd4 M -> $6d $a7 $79 $1f }T
$a62d >PC $03 >S $67 >A $42 >X $61 >Y $2f >P $00f4 $fe >M $a62d $a7 >M $a62e $f4 >M $a62f $22 >M
T{ op PC S A X Y P -> $a62f $03 $67 $42 $61 $2f }T T{ $00f4 M $a62d M $a62e M $a62f M -> $fe $a7 $f4 $22 }T
$bb7c >PC $f8 >S $45 >A $89 >X $6d >Y $e8 >P $005e $d8 >M $bb7c $a7 >M $bb7d $5e >M $bb7e $45 >M
T{ op PC S A X Y P -> $bb7e $f8 $45 $89 $6d $e8 }T T{ $005e M $bb7c M $bb7d M $bb7e M -> $dc $a7 $5e $45 }T
$0c9a >PC $2a >S $6c >A $4d >X $e4 >Y $2d >P $0079 $47 >M $0c9a $a7 >M $0c9b $79 >M $0c9c $dd >M
T{ op PC S A X Y P -> $0c9c $2a $6c $4d $e4 $2d }T T{ $0079 M $0c9a M $0c9b M $0c9c M -> $47 $a7 $79 $dd }T
$164a >PC $30 >S $83 >A $df >X $19 >Y $e3 >P $00a9 $c9 >M $164a $a7 >M $164b $a9 >M $164c $be >M
T{ op PC S A X Y P -> $164c $30 $83 $df $19 $e3 }T T{ $00a9 M $164a M $164b M $164c M -> $cd $a7 $a9 $be }T
$60ed >PC $d1 >S $53 >A $d5 >X $e5 >Y $2b >P $00b1 $a0 >M $60ed $a7 >M $60ee $b1 >M $60ef $e1 >M
T{ op PC S A X Y P -> $60ef $d1 $53 $d5 $e5 $2b }T T{ $00b1 M $60ed M $60ee M $60ef M -> $a4 $a7 $b1 $e1 }T
$95f7 >PC $26 >S $06 >A $bd >X $9f >Y $e8 >P $002f $27 >M $95f7 $a7 >M $95f8 $2f >M $95f9 $8e >M
T{ op PC S A X Y P -> $95f9 $26 $06 $bd $9f $e8 }T T{ $002f M $95f7 M $95f8 M $95f9 M -> $27 $a7 $2f $8e }T
$7eb9 >PC $09 >S $36 >A $1b >X $b7 >Y $6f >P $0071 $34 >M $7eb9 $a7 >M $7eba $71 >M $7ebb $cb >M
T{ op PC S A X Y P -> $7ebb $09 $36 $1b $b7 $6f }T T{ $0071 M $7eb9 M $7eba M $7ebb M -> $34 $a7 $71 $cb }T
$689d >PC $24 >S $a6 >A $e1 >X $73 >Y $e7 >P $0070 $e6 >M $689d $a7 >M $689e $70 >M $689f $2c >M
T{ op PC S A X Y P -> $689f $24 $a6 $e1 $73 $e7 }T T{ $0070 M $689d M $689e M $689f M -> $e6 $a7 $70 $2c }T
$1b39 >PC $5b >S $2b >A $6a >X $6f >Y $2b >P $00d2 $69 >M $1b39 $a7 >M $1b3a $d2 >M $1b3b $b4 >M
T{ op PC S A X Y P -> $1b3b $5b $2b $6a $6f $2b }T T{ $00d2 M $1b39 M $1b3a M $1b3b M -> $6d $a7 $d2 $b4 }T
$b646 >PC $d6 >S $6f >A $35 >X $3d >Y $ed >P $005e $17 >M $b646 $a7 >M $b647 $5e >M $b648 $31 >M
T{ op PC S A X Y P -> $b648 $d6 $6f $35 $3d $ed }T T{ $005e M $b646 M $b647 M $b648 M -> $17 $a7 $5e $31 }T
$3e34 >PC $df >S $3e >A $d7 >X $2d >Y $ac >P $0040 $5a >M $3e34 $a7 >M $3e35 $40 >M $3e36 $44 >M
T{ op PC S A X Y P -> $3e36 $df $3e $d7 $2d $ac }T T{ $0040 M $3e34 M $3e35 M $3e36 M -> $5e $a7 $40 $44 }T
$12cd >PC $cd >S $5b >A $16 >X $3d >Y $a5 >P $0057 $d9 >M $12cd $a7 >M $12ce $57 >M $12cf $c7 >M
T{ op PC S A X Y P -> $12cf $cd $5b $16 $3d $a5 }T T{ $0057 M $12cd M $12ce M $12cf M -> $dd $a7 $57 $c7 }T
$07d4 >PC $d3 >S $1b >A $ac >X $fb >Y $a8 >P $00e0 $09 >M $07d4 $a7 >M $07d5 $e0 >M $07d6 $fb >M
T{ op PC S A X Y P -> $07d6 $d3 $1b $ac $fb $a8 }T T{ $00e0 M $07d4 M $07d5 M $07d6 M -> $0d $a7 $e0 $fb }T
$b8d5 >PC $45 >S $51 >A $fd >X $d2 >Y $aa >P $00d1 $71 >M $b8d5 $a7 >M $b8d6 $d1 >M $b8d7 $eb >M
T{ op PC S A X Y P -> $b8d7 $45 $51 $fd $d2 $aa }T T{ $00d1 M $b8d5 M $b8d6 M $b8d7 M -> $75 $a7 $d1 $eb }T
( a8 )
$f898 >PC $b7 >S $a2 >A $6a >X $50 >Y $61 >P $f898 $a8 >M $f899 $7b >M $f89a $d7 >M
T{ op PC S A X Y P -> $f899 $b7 $a2 $6a $a2 $e1 }T T{ $f898 M $f899 M $f89a M -> $a8 $7b $d7 }T
$01b2 >PC $33 >S $78 >A $a2 >X $74 >Y $2c >P $01b2 $a8 >M $01b3 $1c >M $01b4 $1a >M
T{ op PC S A X Y P -> $01b3 $33 $78 $a2 $78 $2c }T T{ $01b2 M $01b3 M $01b4 M -> $a8 $1c $1a }T
$b02d >PC $7c >S $ef >A $9e >X $8f >Y $a8 >P $b02d $a8 >M $b02e $50 >M $b02f $78 >M
T{ op PC S A X Y P -> $b02e $7c $ef $9e $ef $a8 }T T{ $b02d M $b02e M $b02f M -> $a8 $50 $78 }T
$db46 >PC $c4 >S $df >A $22 >X $f2 >Y $e4 >P $db46 $a8 >M $db47 $bb >M $db48 $84 >M
T{ op PC S A X Y P -> $db47 $c4 $df $22 $df $e4 }T T{ $db46 M $db47 M $db48 M -> $a8 $bb $84 }T
$457f >PC $0e >S $91 >A $c2 >X $0e >Y $ec >P $457f $a8 >M $4580 $63 >M $4581 $f7 >M
T{ op PC S A X Y P -> $4580 $0e $91 $c2 $91 $ec }T T{ $457f M $4580 M $4581 M -> $a8 $63 $f7 }T
$84cb >PC $06 >S $a7 >A $a8 >X $ef >Y $21 >P $84cb $a8 >M $84cc $81 >M $84cd $d8 >M
T{ op PC S A X Y P -> $84cc $06 $a7 $a8 $a7 $a1 }T T{ $84cb M $84cc M $84cd M -> $a8 $81 $d8 }T
$24d7 >PC $84 >S $aa >A $f3 >X $85 >Y $a0 >P $24d7 $a8 >M $24d8 $36 >M $24d9 $42 >M
T{ op PC S A X Y P -> $24d8 $84 $aa $f3 $aa $a0 }T T{ $24d7 M $24d8 M $24d9 M -> $a8 $36 $42 }T
$6127 >PC $a3 >S $44 >A $05 >X $05 >Y $a6 >P $6127 $a8 >M $6128 $06 >M $6129 $93 >M
T{ op PC S A X Y P -> $6128 $a3 $44 $05 $44 $24 }T T{ $6127 M $6128 M $6129 M -> $a8 $06 $93 }T
$fa3f >PC $e6 >S $3f >A $20 >X $56 >Y $63 >P $fa3f $a8 >M $fa40 $d6 >M $fa41 $73 >M
T{ op PC S A X Y P -> $fa40 $e6 $3f $20 $3f $61 }T T{ $fa3f M $fa40 M $fa41 M -> $a8 $d6 $73 }T
$7e29 >PC $91 >S $a3 >A $bf >X $dd >Y $e4 >P $7e29 $a8 >M $7e2a $a6 >M $7e2b $23 >M
T{ op PC S A X Y P -> $7e2a $91 $a3 $bf $a3 $e4 }T T{ $7e29 M $7e2a M $7e2b M -> $a8 $a6 $23 }T
$fb52 >PC $73 >S $88 >A $f8 >X $3e >Y $20 >P $fb52 $a8 >M $fb53 $17 >M $fb54 $a6 >M
T{ op PC S A X Y P -> $fb53 $73 $88 $f8 $88 $a0 }T T{ $fb52 M $fb53 M $fb54 M -> $a8 $17 $a6 }T
$1162 >PC $cc >S $83 >A $eb >X $5b >Y $24 >P $1162 $a8 >M $1163 $03 >M $1164 $6f >M
T{ op PC S A X Y P -> $1163 $cc $83 $eb $83 $a4 }T T{ $1162 M $1163 M $1164 M -> $a8 $03 $6f }T
$2a95 >PC $6b >S $30 >A $4c >X $57 >Y $62 >P $2a95 $a8 >M $2a96 $70 >M $2a97 $b1 >M
T{ op PC S A X Y P -> $2a96 $6b $30 $4c $30 $60 }T T{ $2a95 M $2a96 M $2a97 M -> $a8 $70 $b1 }T
$3e5d >PC $b7 >S $e2 >A $08 >X $0d >Y $e3 >P $3e5d $a8 >M $3e5e $91 >M $3e5f $84 >M
T{ op PC S A X Y P -> $3e5e $b7 $e2 $08 $e2 $e1 }T T{ $3e5d M $3e5e M $3e5f M -> $a8 $91 $84 }T
$90b0 >PC $f6 >S $8d >A $fa >X $1a >Y $6a >P $90b0 $a8 >M $90b1 $19 >M $90b2 $de >M
T{ op PC S A X Y P -> $90b1 $f6 $8d $fa $8d $e8 }T T{ $90b0 M $90b1 M $90b2 M -> $a8 $19 $de }T
$ccb1 >PC $51 >S $84 >A $dd >X $78 >Y $a1 >P $ccb1 $a8 >M $ccb2 $42 >M $ccb3 $98 >M
T{ op PC S A X Y P -> $ccb2 $51 $84 $dd $84 $a1 }T T{ $ccb1 M $ccb2 M $ccb3 M -> $a8 $42 $98 }T
( a9 )
$0837 >PC $e7 >S $8b >A $b5 >X $6d >Y $ab >P $0837 $a9 >M $0838 $96 >M $0839 $cb >M
T{ op PC S A X Y P -> $0839 $e7 $96 $b5 $6d $a9 }T T{ $0837 M $0838 M $0839 M -> $a9 $96 $cb }T
$705b >PC $4b >S $27 >A $e8 >X $d4 >Y $64 >P $705b $a9 >M $705c $38 >M $705d $21 >M
T{ op PC S A X Y P -> $705d $4b $38 $e8 $d4 $64 }T T{ $705b M $705c M $705d M -> $a9 $38 $21 }T
$d71d >PC $07 >S $c4 >A $e4 >X $fb >Y $e2 >P $d71d $a9 >M $d71e $8b >M $d71f $11 >M
T{ op PC S A X Y P -> $d71f $07 $8b $e4 $fb $e0 }T T{ $d71d M $d71e M $d71f M -> $a9 $8b $11 }T
$fe68 >PC $40 >S $e9 >A $01 >X $31 >Y $64 >P $fe68 $a9 >M $fe69 $51 >M $fe6a $92 >M
T{ op PC S A X Y P -> $fe6a $40 $51 $01 $31 $64 }T T{ $fe68 M $fe69 M $fe6a M -> $a9 $51 $92 }T
$9d62 >PC $99 >S $49 >A $ea >X $c4 >Y $68 >P $9d62 $a9 >M $9d63 $23 >M $9d64 $e6 >M
T{ op PC S A X Y P -> $9d64 $99 $23 $ea $c4 $68 }T T{ $9d62 M $9d63 M $9d64 M -> $a9 $23 $e6 }T
$1665 >PC $0c >S $43 >A $c5 >X $81 >Y $64 >P $1665 $a9 >M $1666 $bf >M $1667 $95 >M
T{ op PC S A X Y P -> $1667 $0c $bf $c5 $81 $e4 }T T{ $1665 M $1666 M $1667 M -> $a9 $bf $95 }T
$8d90 >PC $6d >S $e9 >A $f9 >X $42 >Y $22 >P $8d90 $a9 >M $8d91 $57 >M $8d92 $04 >M
T{ op PC S A X Y P -> $8d92 $6d $57 $f9 $42 $20 }T T{ $8d90 M $8d91 M $8d92 M -> $a9 $57 $04 }T
$725d >PC $2b >S $8c >A $23 >X $2f >Y $a5 >P $725d $a9 >M $725e $86 >M $725f $e9 >M
T{ op PC S A X Y P -> $725f $2b $86 $23 $2f $a5 }T T{ $725d M $725e M $725f M -> $a9 $86 $e9 }T
$8277 >PC $9a >S $fe >A $dd >X $85 >Y $62 >P $8277 $a9 >M $8278 $5c >M $8279 $20 >M
T{ op PC S A X Y P -> $8279 $9a $5c $dd $85 $60 }T T{ $8277 M $8278 M $8279 M -> $a9 $5c $20 }T
$8607 >PC $40 >S $36 >A $34 >X $20 >Y $aa >P $8607 $a9 >M $8608 $cd >M $8609 $5b >M
T{ op PC S A X Y P -> $8609 $40 $cd $34 $20 $a8 }T T{ $8607 M $8608 M $8609 M -> $a9 $cd $5b }T
$9676 >PC $a6 >S $14 >A $2c >X $59 >Y $60 >P $9676 $a9 >M $9677 $cb >M $9678 $3b >M
T{ op PC S A X Y P -> $9678 $a6 $cb $2c $59 $e0 }T T{ $9676 M $9677 M $9678 M -> $a9 $cb $3b }T
$10c2 >PC $22 >S $e5 >A $35 >X $20 >Y $a9 >P $10c2 $a9 >M $10c3 $03 >M $10c4 $27 >M
T{ op PC S A X Y P -> $10c4 $22 $03 $35 $20 $29 }T T{ $10c2 M $10c3 M $10c4 M -> $a9 $03 $27 }T
$0c31 >PC $ec >S $e1 >A $d6 >X $6e >Y $e5 >P $0c31 $a9 >M $0c32 $a7 >M $0c33 $8a >M
T{ op PC S A X Y P -> $0c33 $ec $a7 $d6 $6e $e5 }T T{ $0c31 M $0c32 M $0c33 M -> $a9 $a7 $8a }T
$3478 >PC $af >S $09 >A $ab >X $59 >Y $27 >P $3478 $a9 >M $3479 $b8 >M $347a $a8 >M
T{ op PC S A X Y P -> $347a $af $b8 $ab $59 $a5 }T T{ $3478 M $3479 M $347a M -> $a9 $b8 $a8 }T
$13a7 >PC $9f >S $16 >A $8a >X $7a >Y $66 >P $13a7 $a9 >M $13a8 $00 >M $13a9 $2c >M
T{ op PC S A X Y P -> $13a9 $9f $00 $8a $7a $66 }T T{ $13a7 M $13a8 M $13a9 M -> $a9 $00 $2c }T
$ed9a >PC $47 >S $ba >A $fb >X $78 >Y $a9 >P $ed9a $a9 >M $ed9b $2f >M $ed9c $21 >M
T{ op PC S A X Y P -> $ed9c $47 $2f $fb $78 $29 }T T{ $ed9a M $ed9b M $ed9c M -> $a9 $2f $21 }T
( aa )
$42ba >PC $71 >S $79 >A $98 >X $d5 >Y $20 >P $42ba $aa >M $42bb $b9 >M $42bc $e5 >M
T{ op PC S A X Y P -> $42bb $71 $79 $79 $d5 $20 }T T{ $42ba M $42bb M $42bc M -> $aa $b9 $e5 }T
$7a2f >PC $b7 >S $65 >A $97 >X $bd >Y $a4 >P $7a2f $aa >M $7a30 $b7 >M $7a31 $7f >M
T{ op PC S A X Y P -> $7a30 $b7 $65 $65 $bd $24 }T T{ $7a2f M $7a30 M $7a31 M -> $aa $b7 $7f }T
$0e67 >PC $34 >S $83 >A $3f >X $d6 >Y $2a >P $0e67 $aa >M $0e68 $be >M $0e69 $de >M
T{ op PC S A X Y P -> $0e68 $34 $83 $83 $d6 $a8 }T T{ $0e67 M $0e68 M $0e69 M -> $aa $be $de }T
$77b9 >PC $55 >S $2d >A $dd >X $ee >Y $20 >P $77b9 $aa >M $77ba $59 >M $77bb $e7 >M
T{ op PC S A X Y P -> $77ba $55 $2d $2d $ee $20 }T T{ $77b9 M $77ba M $77bb M -> $aa $59 $e7 }T
$1cbf >PC $e9 >S $0c >A $3a >X $ee >Y $aa >P $1cbf $aa >M $1cc0 $53 >M $1cc1 $3b >M
T{ op PC S A X Y P -> $1cc0 $e9 $0c $0c $ee $28 }T T{ $1cbf M $1cc0 M $1cc1 M -> $aa $53 $3b }T
$095e >PC $95 >S $af >A $fd >X $92 >Y $2a >P $095e $aa >M $095f $ed >M $0960 $b7 >M
T{ op PC S A X Y P -> $095f $95 $af $af $92 $a8 }T T{ $095e M $095f M $0960 M -> $aa $ed $b7 }T
$15aa >PC $be >S $d3 >A $c7 >X $34 >Y $e6 >P $15aa $aa >M $15ab $1c >M $15ac $f0 >M
T{ op PC S A X Y P -> $15ab $be $d3 $d3 $34 $e4 }T T{ $15aa M $15ab M $15ac M -> $aa $1c $f0 }T
$3f8d >PC $23 >S $4e >A $6a >X $03 >Y $e9 >P $3f8d $aa >M $3f8e $84 >M $3f8f $88 >M
T{ op PC S A X Y P -> $3f8e $23 $4e $4e $03 $69 }T T{ $3f8d M $3f8e M $3f8f M -> $aa $84 $88 }T
$6d2a >PC $99 >S $06 >A $49 >X $40 >Y $a2 >P $6d2a $aa >M $6d2b $d9 >M $6d2c $de >M
T{ op PC S A X Y P -> $6d2b $99 $06 $06 $40 $20 }T T{ $6d2a M $6d2b M $6d2c M -> $aa $d9 $de }T
$af77 >PC $f9 >S $ca >A $c1 >X $27 >Y $6f >P $af77 $aa >M $af78 $71 >M $af79 $35 >M
T{ op PC S A X Y P -> $af78 $f9 $ca $ca $27 $ed }T T{ $af77 M $af78 M $af79 M -> $aa $71 $35 }T
$6cc0 >PC $3f >S $2d >A $29 >X $d0 >Y $6a >P $6cc0 $aa >M $6cc1 $a6 >M $6cc2 $16 >M
T{ op PC S A X Y P -> $6cc1 $3f $2d $2d $d0 $68 }T T{ $6cc0 M $6cc1 M $6cc2 M -> $aa $a6 $16 }T
$3fdc >PC $85 >S $18 >A $0e >X $fa >Y $a2 >P $3fdc $aa >M $3fdd $c3 >M $3fde $fa >M
T{ op PC S A X Y P -> $3fdd $85 $18 $18 $fa $20 }T T{ $3fdc M $3fdd M $3fde M -> $aa $c3 $fa }T
$0136 >PC $81 >S $a8 >A $21 >X $e3 >Y $ee >P $0136 $aa >M $0137 $e6 >M $0138 $1b >M
T{ op PC S A X Y P -> $0137 $81 $a8 $a8 $e3 $ec }T T{ $0136 M $0137 M $0138 M -> $aa $e6 $1b }T
$4d4a >PC $ce >S $d9 >A $02 >X $af >Y $23 >P $4d4a $aa >M $4d4b $5c >M $4d4c $c0 >M
T{ op PC S A X Y P -> $4d4b $ce $d9 $d9 $af $a1 }T T{ $4d4a M $4d4b M $4d4c M -> $aa $5c $c0 }T
$d23a >PC $2a >S $9d >A $03 >X $a2 >Y $a9 >P $d23a $aa >M $d23b $08 >M $d23c $77 >M
T{ op PC S A X Y P -> $d23b $2a $9d $9d $a2 $a9 }T T{ $d23a M $d23b M $d23c M -> $aa $08 $77 }T
$3e19 >PC $91 >S $8a >A $1f >X $d0 >Y $aa >P $3e19 $aa >M $3e1a $b0 >M $3e1b $6c >M
T{ op PC S A X Y P -> $3e1a $91 $8a $8a $d0 $a8 }T T{ $3e19 M $3e1a M $3e1b M -> $aa $b0 $6c }T
( ab )
$7881 >PC $53 >S $64 >A $38 >X $ea >Y $a2 >P $7881 $ab >M $7882 $9f >M $7883 $79 >M
T{ op PC S A X Y P -> $7882 $53 $64 $38 $ea $a2 }T T{ $7881 M $7882 M $7883 M -> $ab $9f $79 }T
$c1cf >PC $9c >S $25 >A $0e >X $56 >Y $ad >P $c1cf $ab >M $c1d0 $84 >M $c1d1 $ac >M
T{ op PC S A X Y P -> $c1d0 $9c $25 $0e $56 $ad }T T{ $c1cf M $c1d0 M $c1d1 M -> $ab $84 $ac }T
$07b9 >PC $cd >S $7a >A $82 >X $0e >Y $6f >P $07b9 $ab >M $07ba $f4 >M $07bb $4a >M
T{ op PC S A X Y P -> $07ba $cd $7a $82 $0e $6f }T T{ $07b9 M $07ba M $07bb M -> $ab $f4 $4a }T
$7644 >PC $44 >S $66 >A $ae >X $e5 >Y $e5 >P $7644 $ab >M $7645 $46 >M $7646 $6c >M
T{ op PC S A X Y P -> $7645 $44 $66 $ae $e5 $e5 }T T{ $7644 M $7645 M $7646 M -> $ab $46 $6c }T
$8501 >PC $21 >S $39 >A $37 >X $d6 >Y $6e >P $8501 $ab >M $8502 $50 >M $8503 $a6 >M
T{ op PC S A X Y P -> $8502 $21 $39 $37 $d6 $6e }T T{ $8501 M $8502 M $8503 M -> $ab $50 $a6 }T
$8a3b >PC $1f >S $c4 >A $52 >X $93 >Y $2f >P $8a3b $ab >M $8a3c $5e >M $8a3d $07 >M
T{ op PC S A X Y P -> $8a3c $1f $c4 $52 $93 $2f }T T{ $8a3b M $8a3c M $8a3d M -> $ab $5e $07 }T
$d706 >PC $8e >S $f2 >A $84 >X $1e >Y $6d >P $d706 $ab >M $d707 $1b >M $d708 $d9 >M
T{ op PC S A X Y P -> $d707 $8e $f2 $84 $1e $6d }T T{ $d706 M $d707 M $d708 M -> $ab $1b $d9 }T
$b8d1 >PC $c3 >S $84 >A $c9 >X $2b >Y $e0 >P $b8d1 $ab >M $b8d2 $41 >M $b8d3 $25 >M
T{ op PC S A X Y P -> $b8d2 $c3 $84 $c9 $2b $e0 }T T{ $b8d1 M $b8d2 M $b8d3 M -> $ab $41 $25 }T
$f70c >PC $e6 >S $a3 >A $c3 >X $fe >Y $6c >P $f70c $ab >M $f70d $ec >M $f70e $c9 >M
T{ op PC S A X Y P -> $f70d $e6 $a3 $c3 $fe $6c }T T{ $f70c M $f70d M $f70e M -> $ab $ec $c9 }T
$6ade >PC $fa >S $92 >A $30 >X $44 >Y $62 >P $6ade $ab >M $6adf $22 >M $6ae0 $5d >M
T{ op PC S A X Y P -> $6adf $fa $92 $30 $44 $62 }T T{ $6ade M $6adf M $6ae0 M -> $ab $22 $5d }T
$8b19 >PC $39 >S $1c >A $14 >X $a7 >Y $2d >P $8b19 $ab >M $8b1a $80 >M $8b1b $fc >M
T{ op PC S A X Y P -> $8b1a $39 $1c $14 $a7 $2d }T T{ $8b19 M $8b1a M $8b1b M -> $ab $80 $fc }T
$0afe >PC $a5 >S $41 >A $74 >X $b6 >Y $2b >P $0afe $ab >M $0aff $16 >M $0b00 $5d >M
T{ op PC S A X Y P -> $0aff $a5 $41 $74 $b6 $2b }T T{ $0afe M $0aff M $0b00 M -> $ab $16 $5d }T
$1a17 >PC $eb >S $ad >A $d7 >X $50 >Y $e5 >P $1a17 $ab >M $1a18 $53 >M $1a19 $01 >M
T{ op PC S A X Y P -> $1a18 $eb $ad $d7 $50 $e5 }T T{ $1a17 M $1a18 M $1a19 M -> $ab $53 $01 }T
$598f >PC $94 >S $4d >A $ac >X $25 >Y $2c >P $598f $ab >M $5990 $0b >M $5991 $d8 >M
T{ op PC S A X Y P -> $5990 $94 $4d $ac $25 $2c }T T{ $598f M $5990 M $5991 M -> $ab $0b $d8 }T
$9c07 >PC $5b >S $73 >A $40 >X $f8 >Y $24 >P $9c07 $ab >M $9c08 $04 >M $9c09 $1c >M
T{ op PC S A X Y P -> $9c08 $5b $73 $40 $f8 $24 }T T{ $9c07 M $9c08 M $9c09 M -> $ab $04 $1c }T
$23ce >PC $3f >S $b3 >A $1b >X $f2 >Y $ad >P $23ce $ab >M $23cf $91 >M $23d0 $0f >M
T{ op PC S A X Y P -> $23cf $3f $b3 $1b $f2 $ad }T T{ $23ce M $23cf M $23d0 M -> $ab $91 $0f }T
( ac )
$a0ae >PC $92 >S $59 >A $62 >X $d9 >Y $6c >P $a0ae $ac >M $a0af $54 >M $a0b0 $df >M $a0b1 $78 >M $df54 $a9 >M
T{ op PC S A X Y P -> $a0b1 $92 $59 $62 $a9 $ec }T T{ $a0ae M $a0af M $a0b0 M $a0b1 M $df54 M -> $ac $54 $df $78 $a9 }T
$3809 >PC $5a >S $f6 >A $d2 >X $5c >Y $20 >P $22ce $8b >M $3809 $ac >M $380a $ce >M $380b $22 >M $380c $16 >M
T{ op PC S A X Y P -> $380c $5a $f6 $d2 $8b $a0 }T T{ $22ce M $3809 M $380a M $380b M $380c M -> $8b $ac $ce $22 $16 }T
$4673 >PC $51 >S $05 >A $2b >X $9b >Y $2a >P $4673 $ac >M $4674 $1c >M $4675 $80 >M $4676 $49 >M $801c $32 >M
T{ op PC S A X Y P -> $4676 $51 $05 $2b $32 $28 }T T{ $4673 M $4674 M $4675 M $4676 M $801c M -> $ac $1c $80 $49 $32 }T
$9c06 >PC $bb >S $1c >A $48 >X $2c >Y $67 >P $450a $7a >M $9c06 $ac >M $9c07 $0a >M $9c08 $45 >M $9c09 $a5 >M
T{ op PC S A X Y P -> $9c09 $bb $1c $48 $7a $65 }T T{ $450a M $9c06 M $9c07 M $9c08 M $9c09 M -> $7a $ac $0a $45 $a5 }T
$ff13 >PC $2a >S $9a >A $f7 >X $3d >Y $2b >P $4fa5 $53 >M $ff13 $ac >M $ff14 $a5 >M $ff15 $4f >M $ff16 $d4 >M
T{ op PC S A X Y P -> $ff16 $2a $9a $f7 $53 $29 }T T{ $4fa5 M $ff13 M $ff14 M $ff15 M $ff16 M -> $53 $ac $a5 $4f $d4 }T
$e11d >PC $e9 >S $d0 >A $cf >X $17 >Y $2b >P $c14b $1d >M $e11d $ac >M $e11e $4b >M $e11f $c1 >M $e120 $06 >M
T{ op PC S A X Y P -> $e120 $e9 $d0 $cf $1d $29 }T T{ $c14b M $e11d M $e11e M $e11f M $e120 M -> $1d $ac $4b $c1 $06 }T
$4cd3 >PC $24 >S $96 >A $83 >X $6a >Y $66 >P $4cd3 $ac >M $4cd4 $16 >M $4cd5 $98 >M $4cd6 $15 >M $9816 $d2 >M
T{ op PC S A X Y P -> $4cd6 $24 $96 $83 $d2 $e4 }T T{ $4cd3 M $4cd4 M $4cd5 M $4cd6 M $9816 M -> $ac $16 $98 $15 $d2 }T
$6f9f >PC $67 >S $e7 >A $03 >X $a5 >Y $a7 >P $07f7 $7f >M $6f9f $ac >M $6fa0 $f7 >M $6fa1 $07 >M $6fa2 $fa >M
T{ op PC S A X Y P -> $6fa2 $67 $e7 $03 $7f $25 }T T{ $07f7 M $6f9f M $6fa0 M $6fa1 M $6fa2 M -> $7f $ac $f7 $07 $fa }T
$92a8 >PC $40 >S $42 >A $91 >X $08 >Y $2d >P $6ee6 $dc >M $92a8 $ac >M $92a9 $e6 >M $92aa $6e >M $92ab $ec >M
T{ op PC S A X Y P -> $92ab $40 $42 $91 $dc $ad }T T{ $6ee6 M $92a8 M $92a9 M $92aa M $92ab M -> $dc $ac $e6 $6e $ec }T
$e907 >PC $28 >S $09 >A $78 >X $d0 >Y $6f >P $d001 $98 >M $e907 $ac >M $e908 $01 >M $e909 $d0 >M $e90a $0b >M
T{ op PC S A X Y P -> $e90a $28 $09 $78 $98 $ed }T T{ $d001 M $e907 M $e908 M $e909 M $e90a M -> $98 $ac $01 $d0 $0b }T
$a87c >PC $d4 >S $57 >A $29 >X $16 >Y $20 >P $1cfc $df >M $a87c $ac >M $a87d $fc >M $a87e $1c >M $a87f $ab >M
T{ op PC S A X Y P -> $a87f $d4 $57 $29 $df $a0 }T T{ $1cfc M $a87c M $a87d M $a87e M $a87f M -> $df $ac $fc $1c $ab }T
$082b >PC $d2 >S $e0 >A $6d >X $09 >Y $af >P $082b $ac >M $082c $e1 >M $082d $93 >M $082e $69 >M $93e1 $ef >M
T{ op PC S A X Y P -> $082e $d2 $e0 $6d $ef $ad }T T{ $082b M $082c M $082d M $082e M $93e1 M -> $ac $e1 $93 $69 $ef }T
$3a8a >PC $72 >S $1f >A $aa >X $c8 >Y $e0 >P $3a8a $ac >M $3a8b $8a >M $3a8c $82 >M $3a8d $dd >M $828a $b7 >M
T{ op PC S A X Y P -> $3a8d $72 $1f $aa $b7 $e0 }T T{ $3a8a M $3a8b M $3a8c M $3a8d M $828a M -> $ac $8a $82 $dd $b7 }T
$461b >PC $a3 >S $be >A $b5 >X $aa >Y $aa >P $461b $ac >M $461c $51 >M $461d $d7 >M $461e $a8 >M $d751 $d6 >M
T{ op PC S A X Y P -> $461e $a3 $be $b5 $d6 $a8 }T T{ $461b M $461c M $461d M $461e M $d751 M -> $ac $51 $d7 $a8 $d6 }T
$6493 >PC $68 >S $61 >A $a0 >X $2b >Y $ef >P $6493 $ac >M $6494 $33 >M $6495 $e2 >M $6496 $59 >M $e233 $7d >M
T{ op PC S A X Y P -> $6496 $68 $61 $a0 $7d $6d }T T{ $6493 M $6494 M $6495 M $6496 M $e233 M -> $ac $33 $e2 $59 $7d }T
$8c7b >PC $69 >S $c3 >A $54 >X $7c >Y $27 >P $5ced $2e >M $8c7b $ac >M $8c7c $ed >M $8c7d $5c >M $8c7e $1f >M
T{ op PC S A X Y P -> $8c7e $69 $c3 $54 $2e $25 }T T{ $5ced M $8c7b M $8c7c M $8c7d M $8c7e M -> $2e $ac $ed $5c $1f }T
( ad )
$ea6f >PC $cf >S $8c >A $cc >X $09 >Y $26 >P $6e3d $59 >M $ea6f $ad >M $ea70 $3d >M $ea71 $6e >M $ea72 $a8 >M
T{ op PC S A X Y P -> $ea72 $cf $59 $cc $09 $24 }T T{ $6e3d M $ea6f M $ea70 M $ea71 M $ea72 M -> $59 $ad $3d $6e $a8 }T
$c3c0 >PC $16 >S $02 >A $a8 >X $fb >Y $a6 >P $7ff8 $a2 >M $c3c0 $ad >M $c3c1 $f8 >M $c3c2 $7f >M $c3c3 $7a >M
T{ op PC S A X Y P -> $c3c3 $16 $a2 $a8 $fb $a4 }T T{ $7ff8 M $c3c0 M $c3c1 M $c3c2 M $c3c3 M -> $a2 $ad $f8 $7f $7a }T
$5686 >PC $bb >S $8c >A $09 >X $00 >Y $65 >P $5686 $ad >M $5687 $6c >M $5688 $ab >M $5689 $de >M $ab6c $f3 >M
T{ op PC S A X Y P -> $5689 $bb $f3 $09 $00 $e5 }T T{ $5686 M $5687 M $5688 M $5689 M $ab6c M -> $ad $6c $ab $de $f3 }T
$3125 >PC $ce >S $84 >A $0d >X $36 >Y $61 >P $3125 $ad >M $3126 $6a >M $3127 $4c >M $3128 $3e >M $4c6a $70 >M
T{ op PC S A X Y P -> $3128 $ce $70 $0d $36 $61 }T T{ $3125 M $3126 M $3127 M $3128 M $4c6a M -> $ad $6a $4c $3e $70 }T
$e4d5 >PC $27 >S $d6 >A $40 >X $1e >Y $a6 >P $cda8 $a5 >M $e4d5 $ad >M $e4d6 $a8 >M $e4d7 $cd >M $e4d8 $4b >M
T{ op PC S A X Y P -> $e4d8 $27 $a5 $40 $1e $a4 }T T{ $cda8 M $e4d5 M $e4d6 M $e4d7 M $e4d8 M -> $a5 $ad $a8 $cd $4b }T
$a9d7 >PC $89 >S $70 >A $4a >X $8f >Y $ed >P $1fac $8e >M $a9d7 $ad >M $a9d8 $ac >M $a9d9 $1f >M $a9da $c4 >M
T{ op PC S A X Y P -> $a9da $89 $8e $4a $8f $ed }T T{ $1fac M $a9d7 M $a9d8 M $a9d9 M $a9da M -> $8e $ad $ac $1f $c4 }T
$900d >PC $02 >S $c2 >A $ac >X $b2 >Y $e4 >P $900d $ad >M $900e $dc >M $900f $ca >M $9010 $88 >M $cadc $af >M
T{ op PC S A X Y P -> $9010 $02 $af $ac $b2 $e4 }T T{ $900d M $900e M $900f M $9010 M $cadc M -> $ad $dc $ca $88 $af }T
$136d >PC $06 >S $3d >A $d3 >X $65 >Y $24 >P $136d $ad >M $136e $ff >M $136f $40 >M $1370 $6a >M $40ff $20 >M
T{ op PC S A X Y P -> $1370 $06 $20 $d3 $65 $24 }T T{ $136d M $136e M $136f M $1370 M $40ff M -> $ad $ff $40 $6a $20 }T
$6832 >PC $c9 >S $02 >A $0d >X $79 >Y $60 >P $6832 $ad >M $6833 $20 >M $6834 $b7 >M $6835 $56 >M $b720 $35 >M
T{ op PC S A X Y P -> $6835 $c9 $35 $0d $79 $60 }T T{ $6832 M $6833 M $6834 M $6835 M $b720 M -> $ad $20 $b7 $56 $35 }T
$c326 >PC $ae >S $73 >A $9c >X $04 >Y $aa >P $5e48 $c9 >M $c326 $ad >M $c327 $48 >M $c328 $5e >M $c329 $3d >M
T{ op PC S A X Y P -> $c329 $ae $c9 $9c $04 $a8 }T T{ $5e48 M $c326 M $c327 M $c328 M $c329 M -> $c9 $ad $48 $5e $3d }T
$cc41 >PC $07 >S $95 >A $22 >X $27 >Y $e3 >P $86ef $49 >M $cc41 $ad >M $cc42 $ef >M $cc43 $86 >M $cc44 $30 >M
T{ op PC S A X Y P -> $cc44 $07 $49 $22 $27 $61 }T T{ $86ef M $cc41 M $cc42 M $cc43 M $cc44 M -> $49 $ad $ef $86 $30 }T
$6bfd >PC $1a >S $b0 >A $eb >X $e2 >Y $24 >P $6bfd $ad >M $6bfe $e7 >M $6bff $a5 >M $6c00 $ea >M $a5e7 $fe >M
T{ op PC S A X Y P -> $6c00 $1a $fe $eb $e2 $a4 }T T{ $6bfd M $6bfe M $6bff M $6c00 M $a5e7 M -> $ad $e7 $a5 $ea $fe }T
$784a >PC $ce >S $55 >A $a7 >X $40 >Y $20 >P $0292 $a5 >M $784a $ad >M $784b $92 >M $784c $02 >M $784d $5e >M
T{ op PC S A X Y P -> $784d $ce $a5 $a7 $40 $a0 }T T{ $0292 M $784a M $784b M $784c M $784d M -> $a5 $ad $92 $02 $5e }T
$2891 >PC $40 >S $cb >A $19 >X $7c >Y $ae >P $2891 $ad >M $2892 $8c >M $2893 $e9 >M $2894 $55 >M $e98c $6f >M
T{ op PC S A X Y P -> $2894 $40 $6f $19 $7c $2c }T T{ $2891 M $2892 M $2893 M $2894 M $e98c M -> $ad $8c $e9 $55 $6f }T
$65dc >PC $7a >S $36 >A $3a >X $47 >Y $61 >P $65dc $ad >M $65dd $59 >M $65de $ba >M $65df $4e >M $ba59 $4b >M
T{ op PC S A X Y P -> $65df $7a $4b $3a $47 $61 }T T{ $65dc M $65dd M $65de M $65df M $ba59 M -> $ad $59 $ba $4e $4b }T
$c900 >PC $ec >S $5d >A $a8 >X $78 >Y $e9 >P $8682 $a7 >M $c900 $ad >M $c901 $82 >M $c902 $86 >M $c903 $61 >M
T{ op PC S A X Y P -> $c903 $ec $a7 $a8 $78 $e9 }T T{ $8682 M $c900 M $c901 M $c902 M $c903 M -> $a7 $ad $82 $86 $61 }T
( ae )
$d177 >PC $e9 >S $e6 >A $51 >X $08 >Y $62 >P $6382 $d9 >M $d177 $ae >M $d178 $82 >M $d179 $63 >M $d17a $e4 >M
T{ op PC S A X Y P -> $d17a $e9 $e6 $d9 $08 $e0 }T T{ $6382 M $d177 M $d178 M $d179 M $d17a M -> $d9 $ae $82 $63 $e4 }T
$8ab1 >PC $09 >S $fa >A $5c >X $5d >Y $ae >P $335f $19 >M $8ab1 $ae >M $8ab2 $5f >M $8ab3 $33 >M $8ab4 $21 >M
T{ op PC S A X Y P -> $8ab4 $09 $fa $19 $5d $2c }T T{ $335f M $8ab1 M $8ab2 M $8ab3 M $8ab4 M -> $19 $ae $5f $33 $21 }T
$937c >PC $ff >S $4b >A $4a >X $2b >Y $23 >P $4af2 $8a >M $937c $ae >M $937d $f2 >M $937e $4a >M $937f $40 >M
T{ op PC S A X Y P -> $937f $ff $4b $8a $2b $a1 }T T{ $4af2 M $937c M $937d M $937e M $937f M -> $8a $ae $f2 $4a $40 }T
$ba85 >PC $c0 >S $1f >A $df >X $7a >Y $6a >P $6530 $56 >M $ba85 $ae >M $ba86 $30 >M $ba87 $65 >M $ba88 $fd >M
T{ op PC S A X Y P -> $ba88 $c0 $1f $56 $7a $68 }T T{ $6530 M $ba85 M $ba86 M $ba87 M $ba88 M -> $56 $ae $30 $65 $fd }T
$1cc2 >PC $f9 >S $ab >A $8c >X $09 >Y $ed >P $19c5 $42 >M $1cc2 $ae >M $1cc3 $c5 >M $1cc4 $19 >M $1cc5 $54 >M
T{ op PC S A X Y P -> $1cc5 $f9 $ab $42 $09 $6d }T T{ $19c5 M $1cc2 M $1cc3 M $1cc4 M $1cc5 M -> $42 $ae $c5 $19 $54 }T
$c921 >PC $d1 >S $ae >A $78 >X $9c >Y $6c >P $64b7 $f2 >M $c921 $ae >M $c922 $b7 >M $c923 $64 >M $c924 $30 >M
T{ op PC S A X Y P -> $c924 $d1 $ae $f2 $9c $ec }T T{ $64b7 M $c921 M $c922 M $c923 M $c924 M -> $f2 $ae $b7 $64 $30 }T
$a093 >PC $c9 >S $e6 >A $37 >X $b7 >Y $e1 >P $a093 $ae >M $a094 $53 >M $a095 $a6 >M $a096 $07 >M $a653 $9b >M
T{ op PC S A X Y P -> $a096 $c9 $e6 $9b $b7 $e1 }T T{ $a093 M $a094 M $a095 M $a096 M $a653 M -> $ae $53 $a6 $07 $9b }T
$d70e >PC $26 >S $4d >A $eb >X $7c >Y $a4 >P $9b2a $d0 >M $d70e $ae >M $d70f $2a >M $d710 $9b >M $d711 $2f >M
T{ op PC S A X Y P -> $d711 $26 $4d $d0 $7c $a4 }T T{ $9b2a M $d70e M $d70f M $d710 M $d711 M -> $d0 $ae $2a $9b $2f }T
$40a1 >PC $6b >S $c7 >A $f4 >X $68 >Y $2c >P $40a1 $ae >M $40a2 $7d >M $40a3 $4c >M $40a4 $5c >M $4c7d $b1 >M
T{ op PC S A X Y P -> $40a4 $6b $c7 $b1 $68 $ac }T T{ $40a1 M $40a2 M $40a3 M $40a4 M $4c7d M -> $ae $7d $4c $5c $b1 }T
$5e1d >PC $ca >S $e4 >A $ca >X $bc >Y $63 >P $21dd $84 >M $5e1d $ae >M $5e1e $dd >M $5e1f $21 >M $5e20 $a2 >M
T{ op PC S A X Y P -> $5e20 $ca $e4 $84 $bc $e1 }T T{ $21dd M $5e1d M $5e1e M $5e1f M $5e20 M -> $84 $ae $dd $21 $a2 }T
$6d96 >PC $ed >S $7c >A $be >X $e9 >Y $60 >P $6568 $90 >M $6d96 $ae >M $6d97 $68 >M $6d98 $65 >M $6d99 $cd >M
T{ op PC S A X Y P -> $6d99 $ed $7c $90 $e9 $e0 }T T{ $6568 M $6d96 M $6d97 M $6d98 M $6d99 M -> $90 $ae $68 $65 $cd }T
$d720 >PC $ed >S $d7 >A $ea >X $00 >Y $6b >P $3545 $fa >M $d720 $ae >M $d721 $45 >M $d722 $35 >M $d723 $bb >M
T{ op PC S A X Y P -> $d723 $ed $d7 $fa $00 $e9 }T T{ $3545 M $d720 M $d721 M $d722 M $d723 M -> $fa $ae $45 $35 $bb }T
$c06b >PC $9a >S $9b >A $ab >X $82 >Y $e6 >P $c06b $ae >M $c06c $2a >M $c06d $d3 >M $c06e $4a >M $d32a $70 >M
T{ op PC S A X Y P -> $c06e $9a $9b $70 $82 $64 }T T{ $c06b M $c06c M $c06d M $c06e M $d32a M -> $ae $2a $d3 $4a $70 }T
$5305 >PC $dc >S $25 >A $2a >X $6f >Y $a2 >P $16f9 $db >M $5305 $ae >M $5306 $f9 >M $5307 $16 >M $5308 $02 >M
T{ op PC S A X Y P -> $5308 $dc $25 $db $6f $a0 }T T{ $16f9 M $5305 M $5306 M $5307 M $5308 M -> $db $ae $f9 $16 $02 }T
$181f >PC $73 >S $1a >A $4d >X $64 >Y $2a >P $1223 $ac >M $181f $ae >M $1820 $23 >M $1821 $12 >M $1822 $e9 >M
T{ op PC S A X Y P -> $1822 $73 $1a $ac $64 $a8 }T T{ $1223 M $181f M $1820 M $1821 M $1822 M -> $ac $ae $23 $12 $e9 }T
$3c00 >PC $51 >S $30 >A $dc >X $3e >Y $6b >P $3c00 $ae >M $3c01 $5e >M $3c02 $e5 >M $3c03 $db >M $e55e $ae >M
T{ op PC S A X Y P -> $3c03 $51 $30 $ae $3e $e9 }T T{ $3c00 M $3c01 M $3c02 M $3c03 M $e55e M -> $ae $5e $e5 $db $ae }T
( af )
$c973 >PC $84 >S $b7 >A $24 >X $66 >Y $6e >P $00b2 $65 >M $c973 $af >M $c974 $b2 >M $c975 $28 >M $c99e $15 >M
T{ op PC S A X Y P -> $c99e $84 $b7 $24 $66 $6e }T T{ $00b2 M $c973 M $c974 M $c975 M $c99e M -> $65 $af $b2 $28 $15 }T
$566e >PC $88 >S $d2 >A $62 >X $5f >Y $24 >P $0086 $98 >M $566e $af >M $566f $86 >M $5670 $46 >M $5671 $f8 >M $56b7 $d6 >M
T{ op PC S A X Y P -> $5671 $88 $d2 $62 $5f $24 }T T{ $0086 M $566e M $566f M $5670 M $5671 M $56b7 M -> $98 $af $86 $46 $f8 $d6 }T
$1bf6 >PC $7d >S $a6 >A $b9 >X $34 >Y $67 >P $0001 $fd >M $1b66 $5e >M $1bf6 $af >M $1bf7 $01 >M $1bf8 $6d >M $1c66 $23 >M
T{ op PC S A X Y P -> $1c66 $7d $a6 $b9 $34 $67 }T T{ $0001 M $1b66 M $1bf6 M $1bf7 M $1bf8 M $1c66 M -> $fd $5e $af $01 $6d $23 }T
$2e2c >PC $23 >S $e1 >A $98 >X $2c >Y $e1 >P $006a $af >M $2e2c $af >M $2e2d $6a >M $2e2e $72 >M $2ea1 $d7 >M
T{ op PC S A X Y P -> $2ea1 $23 $e1 $98 $2c $e1 }T T{ $006a M $2e2c M $2e2d M $2e2e M $2ea1 M -> $af $af $6a $72 $d7 }T
$7d8a >PC $34 >S $6f >A $ea >X $91 >Y $a7 >P $0033 $5f >M $7d41 $73 >M $7d8a $af >M $7d8b $33 >M $7d8c $b4 >M
T{ op PC S A X Y P -> $7d41 $34 $6f $ea $91 $a7 }T T{ $0033 M $7d41 M $7d8a M $7d8b M $7d8c M -> $5f $73 $af $33 $b4 }T
$9bef >PC $06 >S $f2 >A $57 >X $be >Y $af >P $0076 $7a >M $9bc1 $b5 >M $9bef $af >M $9bf0 $76 >M $9bf1 $cf >M $9bf2 $4d >M
T{ op PC S A X Y P -> $9bf2 $06 $f2 $57 $be $af }T T{ $0076 M $9bc1 M $9bef M $9bf0 M $9bf1 M $9bf2 M -> $7a $b5 $af $76 $cf $4d }T
$2d2f >PC $ce >S $87 >A $92 >X $38 >Y $24 >P $009f $24 >M $2cf1 $98 >M $2d2f $af >M $2d30 $9f >M $2d31 $bf >M $2df1 $ba >M
T{ op PC S A X Y P -> $2cf1 $ce $87 $92 $38 $24 }T T{ $009f M $2cf1 M $2d2f M $2d30 M $2d31 M $2df1 M -> $24 $98 $af $9f $bf $ba }T
$4dc3 >PC $c3 >S $fc >A $fe >X $8d >Y $ae >P $0015 $d0 >M $4d11 $f9 >M $4dc3 $af >M $4dc4 $15 >M $4dc5 $4b >M $4dc6 $8d >M
T{ op PC S A X Y P -> $4dc6 $c3 $fc $fe $8d $ae }T T{ $0015 M $4d11 M $4dc3 M $4dc4 M $4dc5 M $4dc6 M -> $d0 $f9 $af $15 $4b $8d }T
$a449 >PC $02 >S $db >A $89 >X $4e >Y $e8 >P $007b $a3 >M $a41a $9f >M $a449 $af >M $a44a $7b >M $a44b $ce >M $a44c $28 >M
T{ op PC S A X Y P -> $a44c $02 $db $89 $4e $e8 }T T{ $007b M $a41a M $a449 M $a44a M $a44b M $a44c M -> $a3 $9f $af $7b $ce $28 }T
$0325 >PC $e2 >S $d4 >A $33 >X $c3 >Y $e3 >P $00b4 $98 >M $0325 $af >M $0326 $b4 >M $0327 $87 >M $0328 $d4 >M $03af $b7 >M
T{ op PC S A X Y P -> $0328 $e2 $d4 $33 $c3 $e3 }T T{ $00b4 M $0325 M $0326 M $0327 M $0328 M $03af M -> $98 $af $b4 $87 $d4 $b7 }T
$daa8 >PC $64 >S $06 >A $85 >X $04 >Y $e4 >P $004f $69 >M $da42 $5f >M $daa8 $af >M $daa9 $4f >M $daaa $97 >M $daab $77 >M
T{ op PC S A X Y P -> $daab $64 $06 $85 $04 $e4 }T T{ $004f M $da42 M $daa8 M $daa9 M $daaa M $daab M -> $69 $5f $af $4f $97 $77 }T
$e19a >PC $cb >S $d1 >A $a2 >X $63 >Y $20 >P $005a $53 >M $e19a $af >M $e19b $5a >M $e19c $06 >M $e19d $ba >M $e1a3 $85 >M
T{ op PC S A X Y P -> $e19d $cb $d1 $a2 $63 $20 }T T{ $005a M $e19a M $e19b M $e19c M $e19d M $e1a3 M -> $53 $af $5a $06 $ba $85 }T
$cebf >PC $d1 >S $30 >A $b6 >X $36 >Y $a8 >P $0008 $7f >M $ce4e $9d >M $cebf $af >M $cec0 $08 >M $cec1 $8c >M
T{ op PC S A X Y P -> $ce4e $d1 $30 $b6 $36 $a8 }T T{ $0008 M $ce4e M $cebf M $cec0 M $cec1 M -> $7f $9d $af $08 $8c }T
$3dd2 >PC $ed >S $0e >A $78 >X $a8 >Y $66 >P $00bc $e3 >M $3dcc $9c >M $3dd2 $af >M $3dd3 $bc >M $3dd4 $f7 >M $3dd5 $ba >M
T{ op PC S A X Y P -> $3dd5 $ed $0e $78 $a8 $66 }T T{ $00bc M $3dcc M $3dd2 M $3dd3 M $3dd4 M $3dd5 M -> $e3 $9c $af $bc $f7 $ba }T
$4ce1 >PC $95 >S $b2 >A $b4 >X $e6 >Y $ed >P $0034 $12 >M $4c0a $14 >M $4ce1 $af >M $4ce2 $34 >M $4ce3 $26 >M $4ce4 $b7 >M
T{ op PC S A X Y P -> $4ce4 $95 $b2 $b4 $e6 $ed }T T{ $0034 M $4c0a M $4ce1 M $4ce2 M $4ce3 M $4ce4 M -> $12 $14 $af $34 $26 $b7 }T
$af18 >PC $80 >S $2e >A $fb >X $77 >Y $ab >P $00a8 $8b >M $af18 $af >M $af19 $a8 >M $af1a $30 >M $af1b $18 >M $af4b $3d >M
T{ op PC S A X Y P -> $af1b $80 $2e $fb $77 $ab }T T{ $00a8 M $af18 M $af19 M $af1a M $af1b M $af4b M -> $8b $af $a8 $30 $18 $3d }T
( b0 )
$732a >PC $36 >S $56 >A $9a >X $c9 >Y $e9 >P $7319 $3e >M $732a $b0 >M $732b $ed >M $732c $4c >M
T{ op PC S A X Y P -> $7319 $36 $56 $9a $c9 $e9 }T T{ $7319 M $732a M $732b M $732c M -> $3e $b0 $ed $4c }T
$131e >PC $4b >S $10 >A $b0 >X $fb >Y $ef >P $12f7 $dc >M $131e $b0 >M $131f $d7 >M $1320 $c9 >M $13f7 $53 >M
T{ op PC S A X Y P -> $12f7 $4b $10 $b0 $fb $ef }T T{ $12f7 M $131e M $131f M $1320 M $13f7 M -> $dc $b0 $d7 $c9 $53 }T
$8dce >PC $d9 >S $8b >A $d6 >X $28 >Y $60 >P $8dce $b0 >M $8dcf $a8 >M $8dd0 $d7 >M
T{ op PC S A X Y P -> $8dd0 $d9 $8b $d6 $28 $60 }T T{ $8dce M $8dcf M $8dd0 M -> $b0 $a8 $d7 }T
$70e4 >PC $83 >S $61 >A $d3 >X $05 >Y $66 >P $70e4 $b0 >M $70e5 $9a >M $70e6 $a5 >M
T{ op PC S A X Y P -> $70e6 $83 $61 $d3 $05 $66 }T T{ $70e4 M $70e5 M $70e6 M -> $b0 $9a $a5 }T
$3d14 >PC $be >S $c2 >A $51 >X $9c >Y $6a >P $3d14 $b0 >M $3d15 $18 >M $3d16 $74 >M
T{ op PC S A X Y P -> $3d16 $be $c2 $51 $9c $6a }T T{ $3d14 M $3d15 M $3d16 M -> $b0 $18 $74 }T
$ba62 >PC $30 >S $fa >A $c1 >X $94 >Y $eb >P $ba62 $b0 >M $ba63 $54 >M $ba64 $50 >M $bab8 $6a >M
T{ op PC S A X Y P -> $bab8 $30 $fa $c1 $94 $eb }T T{ $ba62 M $ba63 M $ba64 M $bab8 M -> $b0 $54 $50 $6a }T
$6dff >PC $12 >S $be >A $2d >X $4b >Y $e5 >P $6dff $b0 >M $6e00 $08 >M $6e01 $15 >M $6e09 $fe >M
T{ op PC S A X Y P -> $6e09 $12 $be $2d $4b $e5 }T T{ $6dff M $6e00 M $6e01 M $6e09 M -> $b0 $08 $15 $fe }T
$e2fc >PC $41 >S $88 >A $73 >X $24 >Y $e7 >P $e25d $57 >M $e2fc $b0 >M $e2fd $5f >M $e2fe $eb >M $e35d $65 >M
T{ op PC S A X Y P -> $e35d $41 $88 $73 $24 $e7 }T T{ $e25d M $e2fc M $e2fd M $e2fe M $e35d M -> $57 $b0 $5f $eb $65 }T
$9665 >PC $32 >S $7b >A $1b >X $9d >Y $64 >P $9665 $b0 >M $9666 $ce >M $9667 $a6 >M
T{ op PC S A X Y P -> $9667 $32 $7b $1b $9d $64 }T T{ $9665 M $9666 M $9667 M -> $b0 $ce $a6 }T
$7aec >PC $86 >S $90 >A $d2 >X $31 >Y $ed >P $7a21 $59 >M $7aec $b0 >M $7aed $33 >M $7aee $1d >M $7b21 $de >M
T{ op PC S A X Y P -> $7b21 $86 $90 $d2 $31 $ed }T T{ $7a21 M $7aec M $7aed M $7aee M $7b21 M -> $59 $b0 $33 $1d $de }T
$1e74 >PC $29 >S $bf >A $11 >X $b2 >Y $25 >P $1e74 $b0 >M $1e75 $42 >M $1e76 $f5 >M $1eb8 $7f >M
T{ op PC S A X Y P -> $1eb8 $29 $bf $11 $b2 $25 }T T{ $1e74 M $1e75 M $1e76 M $1eb8 M -> $b0 $42 $f5 $7f }T
$2bb9 >PC $7a >S $9a >A $a4 >X $d8 >Y $e8 >P $2bb9 $b0 >M $2bba $05 >M $2bbb $4e >M
T{ op PC S A X Y P -> $2bbb $7a $9a $a4 $d8 $e8 }T T{ $2bb9 M $2bba M $2bbb M -> $b0 $05 $4e }T
$6caf >PC $b3 >S $85 >A $7e >X $bb >Y $21 >P $6c91 $2e >M $6caf $b0 >M $6cb0 $e0 >M $6cb1 $c5 >M
T{ op PC S A X Y P -> $6c91 $b3 $85 $7e $bb $21 }T T{ $6c91 M $6caf M $6cb0 M $6cb1 M -> $2e $b0 $e0 $c5 }T
$8846 >PC $5a >S $66 >A $af >X $4f >Y $2d >P $8836 $26 >M $8846 $b0 >M $8847 $ee >M $8848 $c0 >M
T{ op PC S A X Y P -> $8836 $5a $66 $af $4f $2d }T T{ $8836 M $8846 M $8847 M $8848 M -> $26 $b0 $ee $c0 }T
$a509 >PC $81 >S $46 >A $33 >X $fd >Y $25 >P $a4e2 $44 >M $a509 $b0 >M $a50a $d7 >M $a50b $85 >M $a5e2 $f8 >M
T{ op PC S A X Y P -> $a4e2 $81 $46 $33 $fd $25 }T T{ $a4e2 M $a509 M $a50a M $a50b M $a5e2 M -> $44 $b0 $d7 $85 $f8 }T
$83bf >PC $02 >S $24 >A $0f >X $32 >Y $29 >P $839a $21 >M $83bf $b0 >M $83c0 $d9 >M $83c1 $36 >M
T{ op PC S A X Y P -> $839a $02 $24 $0f $32 $29 }T T{ $839a M $83bf M $83c0 M $83c1 M -> $21 $b0 $d9 $36 }T
( b1 )
$7653 >PC $67 >S $49 >A $5a >X $94 >Y $2f >P $00bc $05 >M $00bd $a0 >M $7653 $b1 >M $7654 $bc >M $7655 $52 >M $a099 $67 >M
T{ op PC S A X Y P -> $7655 $67 $67 $5a $94 $2d }T T{ $00bc M $00bd M $7653 M $7654 M $7655 M $a099 M -> $05 $a0 $b1 $bc $52 $67 }T
$8b85 >PC $9a >S $2f >A $1c >X $ca >Y $2d >P $003f $6a >M $0040 $8a >M $8b34 $fd >M $8b85 $b1 >M $8b86 $3f >M $8b87 $b1 >M
T{ op PC S A X Y P -> $8b87 $9a $fd $1c $ca $ad }T T{ $003f M $0040 M $8b34 M $8b85 M $8b86 M $8b87 M -> $6a $8a $fd $b1 $3f $b1 }T
$83b5 >PC $fc >S $27 >A $cd >X $62 >Y $21 >P $0004 $40 >M $0005 $d8 >M $83b5 $b1 >M $83b6 $04 >M $83b7 $0b >M $d8a2 $37 >M
T{ op PC S A X Y P -> $83b7 $fc $37 $cd $62 $21 }T T{ $0004 M $0005 M $83b5 M $83b6 M $83b7 M $d8a2 M -> $40 $d8 $b1 $04 $0b $37 }T
$5f90 >PC $9a >S $e9 >A $cf >X $81 >Y $21 >P $008c $dd >M $008d $58 >M $595e $6d >M $5f90 $b1 >M $5f91 $8c >M $5f92 $ea >M
T{ op PC S A X Y P -> $5f92 $9a $6d $cf $81 $21 }T T{ $008c M $008d M $595e M $5f90 M $5f91 M $5f92 M -> $dd $58 $6d $b1 $8c $ea }T
$0b8d >PC $da >S $d4 >A $4c >X $0b >Y $2f >P $0049 $60 >M $004a $bd >M $0b8d $b1 >M $0b8e $49 >M $0b8f $8d >M $bd6b $91 >M
T{ op PC S A X Y P -> $0b8f $da $91 $4c $0b $ad }T T{ $0049 M $004a M $0b8d M $0b8e M $0b8f M $bd6b M -> $60 $bd $b1 $49 $8d $91 }T
$5f61 >PC $67 >S $ec >A $60 >X $14 >Y $29 >P $000a $5c >M $000b $62 >M $5f61 $b1 >M $5f62 $0a >M $5f63 $1e >M $6270 $39 >M
T{ op PC S A X Y P -> $5f63 $67 $39 $60 $14 $29 }T T{ $000a M $000b M $5f61 M $5f62 M $5f63 M $6270 M -> $5c $62 $b1 $0a $1e $39 }T
$d3f8 >PC $d2 >S $da >A $07 >X $75 >Y $65 >P $0041 $51 >M $0042 $f4 >M $d3f8 $b1 >M $d3f9 $41 >M $d3fa $8d >M $f4c6 $28 >M
T{ op PC S A X Y P -> $d3fa $d2 $28 $07 $75 $65 }T T{ $0041 M $0042 M $d3f8 M $d3f9 M $d3fa M $f4c6 M -> $51 $f4 $b1 $41 $8d $28 }T
$6283 >PC $eb >S $b4 >A $28 >X $02 >Y $6d >P $00f3 $fb >M $00f4 $02 >M $02fd $a8 >M $6283 $b1 >M $6284 $f3 >M $6285 $1c >M
T{ op PC S A X Y P -> $6285 $eb $a8 $28 $02 $ed }T T{ $00f3 M $00f4 M $02fd M $6283 M $6284 M $6285 M -> $fb $02 $a8 $b1 $f3 $1c }T
$1b2f >PC $29 >S $05 >A $e5 >X $47 >Y $6d >P $00fc $84 >M $00fd $e0 >M $1b2f $b1 >M $1b30 $fc >M $1b31 $f5 >M $e0cb $af >M
T{ op PC S A X Y P -> $1b31 $29 $af $e5 $47 $ed }T T{ $00fc M $00fd M $1b2f M $1b30 M $1b31 M $e0cb M -> $84 $e0 $b1 $fc $f5 $af }T
$a792 >PC $e9 >S $08 >A $f8 >X $2b >Y $a9 >P $00ac $50 >M $00ad $a2 >M $a27b $12 >M $a792 $b1 >M $a793 $ac >M $a794 $12 >M
T{ op PC S A X Y P -> $a794 $e9 $12 $f8 $2b $29 }T T{ $00ac M $00ad M $a27b M $a792 M $a793 M $a794 M -> $50 $a2 $12 $b1 $ac $12 }T
$c22a >PC $37 >S $14 >A $f5 >X $fb >Y $6c >P $0010 $0f >M $0011 $63 >M $640a $d4 >M $c22a $b1 >M $c22b $10 >M $c22c $13 >M
T{ op PC S A X Y P -> $c22c $37 $d4 $f5 $fb $ec }T T{ $0010 M $0011 M $640a M $c22a M $c22b M $c22c M -> $0f $63 $d4 $b1 $10 $13 }T
$f23e >PC $94 >S $de >A $a0 >X $e9 >Y $28 >P $004c $16 >M $004d $e7 >M $e7ff $96 >M $f23e $b1 >M $f23f $4c >M $f240 $da >M
T{ op PC S A X Y P -> $f240 $94 $96 $a0 $e9 $a8 }T T{ $004c M $004d M $e7ff M $f23e M $f23f M $f240 M -> $16 $e7 $96 $b1 $4c $da }T
$7b77 >PC $e1 >S $43 >A $1a >X $06 >Y $a1 >P $00a0 $e5 >M $00a1 $94 >M $7b77 $b1 >M $7b78 $a0 >M $7b79 $30 >M $94eb $f3 >M
T{ op PC S A X Y P -> $7b79 $e1 $f3 $1a $06 $a1 }T T{ $00a0 M $00a1 M $7b77 M $7b78 M $7b79 M $94eb M -> $e5 $94 $b1 $a0 $30 $f3 }T
$5aa8 >PC $45 >S $a4 >A $c5 >X $a2 >Y $eb >P $0014 $9f >M $0015 $7e >M $5aa8 $b1 >M $5aa9 $14 >M $5aaa $1b >M $7f41 $0f >M
T{ op PC S A X Y P -> $5aaa $45 $0f $c5 $a2 $69 }T T{ $0014 M $0015 M $5aa8 M $5aa9 M $5aaa M $7f41 M -> $9f $7e $b1 $14 $1b $0f }T
$0881 >PC $53 >S $d9 >A $b0 >X $42 >Y $ee >P $0086 $af >M $0087 $d2 >M $0881 $b1 >M $0882 $86 >M $0883 $55 >M $d2f1 $f6 >M
T{ op PC S A X Y P -> $0883 $53 $f6 $b0 $42 $ec }T T{ $0086 M $0087 M $0881 M $0882 M $0883 M $d2f1 M -> $af $d2 $b1 $86 $55 $f6 }T
$3fb7 >PC $f2 >S $da >A $59 >X $c0 >Y $ee >P $00be $46 >M $00bf $ba >M $3fb7 $b1 >M $3fb8 $be >M $3fb9 $af >M $bb06 $a1 >M
T{ op PC S A X Y P -> $3fb9 $f2 $a1 $59 $c0 $ec }T T{ $00be M $00bf M $3fb7 M $3fb8 M $3fb9 M $bb06 M -> $46 $ba $b1 $be $af $a1 }T
( b2 )
$c428 >PC $42 >S $63 >A $92 >X $57 >Y $6a >P $00d2 $17 >M $00d3 $7c >M $7c17 $60 >M $c428 $b2 >M $c429 $d2 >M $c42a $b3 >M
T{ op PC S A X Y P -> $c42a $42 $60 $92 $57 $68 }T T{ $00d2 M $00d3 M $7c17 M $c428 M $c429 M $c42a M -> $17 $7c $60 $b2 $d2 $b3 }T
$9022 >PC $cc >S $48 >A $95 >X $bb >Y $62 >P $00a0 $e5 >M $00a1 $ad >M $9022 $b2 >M $9023 $a0 >M $9024 $4c >M $ade5 $ec >M
T{ op PC S A X Y P -> $9024 $cc $ec $95 $bb $e0 }T T{ $00a0 M $00a1 M $9022 M $9023 M $9024 M $ade5 M -> $e5 $ad $b2 $a0 $4c $ec }T
$ac13 >PC $2f >S $00 >A $2b >X $aa >Y $2b >P $0094 $1f >M $0095 $46 >M $461f $75 >M $ac13 $b2 >M $ac14 $94 >M $ac15 $b3 >M
T{ op PC S A X Y P -> $ac15 $2f $75 $2b $aa $29 }T T{ $0094 M $0095 M $461f M $ac13 M $ac14 M $ac15 M -> $1f $46 $75 $b2 $94 $b3 }T
$c350 >PC $09 >S $2b >A $5b >X $80 >Y $e3 >P $006d $96 >M $006e $ac >M $ac96 $f0 >M $c350 $b2 >M $c351 $6d >M $c352 $ae >M
T{ op PC S A X Y P -> $c352 $09 $f0 $5b $80 $e1 }T T{ $006d M $006e M $ac96 M $c350 M $c351 M $c352 M -> $96 $ac $f0 $b2 $6d $ae }T
$4d16 >PC $64 >S $d3 >A $19 >X $c8 >Y $6a >P $0045 $41 >M $0046 $8c >M $4d16 $b2 >M $4d17 $45 >M $4d18 $df >M $8c41 $03 >M
T{ op PC S A X Y P -> $4d18 $64 $03 $19 $c8 $68 }T T{ $0045 M $0046 M $4d16 M $4d17 M $4d18 M $8c41 M -> $41 $8c $b2 $45 $df $03 }T
$cb9d >PC $2f >S $1a >A $bc >X $b7 >Y $ae >P $00a7 $16 >M $00a8 $ed >M $cb9d $b2 >M $cb9e $a7 >M $cb9f $36 >M $ed16 $e6 >M
T{ op PC S A X Y P -> $cb9f $2f $e6 $bc $b7 $ac }T T{ $00a7 M $00a8 M $cb9d M $cb9e M $cb9f M $ed16 M -> $16 $ed $b2 $a7 $36 $e6 }T
$ada4 >PC $16 >S $34 >A $ba >X $c8 >Y $ec >P $00bd $d2 >M $00be $3f >M $3fd2 $9d >M $ada4 $b2 >M $ada5 $bd >M $ada6 $73 >M
T{ op PC S A X Y P -> $ada6 $16 $9d $ba $c8 $ec }T T{ $00bd M $00be M $3fd2 M $ada4 M $ada5 M $ada6 M -> $d2 $3f $9d $b2 $bd $73 }T
$7f26 >PC $c9 >S $67 >A $de >X $c9 >Y $60 >P $004b $07 >M $004c $25 >M $2507 $31 >M $7f26 $b2 >M $7f27 $4b >M $7f28 $d9 >M
T{ op PC S A X Y P -> $7f28 $c9 $31 $de $c9 $60 }T T{ $004b M $004c M $2507 M $7f26 M $7f27 M $7f28 M -> $07 $25 $31 $b2 $4b $d9 }T
$df41 >PC $3a >S $b2 >A $4a >X $ad >Y $e6 >P $004d $88 >M $004e $76 >M $7688 $2f >M $df41 $b2 >M $df42 $4d >M $df43 $ac >M
T{ op PC S A X Y P -> $df43 $3a $2f $4a $ad $64 }T T{ $004d M $004e M $7688 M $df41 M $df42 M $df43 M -> $88 $76 $2f $b2 $4d $ac }T
$5583 >PC $69 >S $12 >A $b4 >X $c5 >Y $61 >P $00c9 $20 >M $00ca $ab >M $5583 $b2 >M $5584 $c9 >M $5585 $44 >M $ab20 $8e >M
T{ op PC S A X Y P -> $5585 $69 $8e $b4 $c5 $e1 }T T{ $00c9 M $00ca M $5583 M $5584 M $5585 M $ab20 M -> $20 $ab $b2 $c9 $44 $8e }T
$4e1a >PC $b7 >S $34 >A $a8 >X $9e >Y $6a >P $00f5 $f0 >M $00f6 $f0 >M $4e1a $b2 >M $4e1b $f5 >M $4e1c $25 >M $f0f0 $a9 >M
T{ op PC S A X Y P -> $4e1c $b7 $a9 $a8 $9e $e8 }T T{ $00f5 M $00f6 M $4e1a M $4e1b M $4e1c M $f0f0 M -> $f0 $f0 $b2 $f5 $25 $a9 }T
$4d1d >PC $0b >S $63 >A $96 >X $0d >Y $2f >P $0073 $bd >M $0074 $bc >M $4d1d $b2 >M $4d1e $73 >M $4d1f $bf >M $bcbd $3a >M
T{ op PC S A X Y P -> $4d1f $0b $3a $96 $0d $2d }T T{ $0073 M $0074 M $4d1d M $4d1e M $4d1f M $bcbd M -> $bd $bc $b2 $73 $bf $3a }T
$1db8 >PC $56 >S $4c >A $50 >X $80 >Y $e0 >P $00ae $29 >M $00af $c4 >M $1db8 $b2 >M $1db9 $ae >M $1dba $d2 >M $c429 $50 >M
T{ op PC S A X Y P -> $1dba $56 $50 $50 $80 $60 }T T{ $00ae M $00af M $1db8 M $1db9 M $1dba M $c429 M -> $29 $c4 $b2 $ae $d2 $50 }T
$0e5d >PC $95 >S $ef >A $25 >X $d9 >Y $27 >P $00e1 $ce >M $00e2 $c9 >M $0e5d $b2 >M $0e5e $e1 >M $0e5f $54 >M $c9ce $1d >M
T{ op PC S A X Y P -> $0e5f $95 $1d $25 $d9 $25 }T T{ $00e1 M $00e2 M $0e5d M $0e5e M $0e5f M $c9ce M -> $ce $c9 $b2 $e1 $54 $1d }T
$1e13 >PC $7e >S $c8 >A $fd >X $7b >Y $2c >P $00e0 $47 >M $00e1 $ee >M $1e13 $b2 >M $1e14 $e0 >M $1e15 $7f >M $ee47 $c5 >M
T{ op PC S A X Y P -> $1e15 $7e $c5 $fd $7b $ac }T T{ $00e0 M $00e1 M $1e13 M $1e14 M $1e15 M $ee47 M -> $47 $ee $b2 $e0 $7f $c5 }T
$1141 >PC $b1 >S $43 >A $38 >X $82 >Y $e4 >P $0068 $e3 >M $0069 $a1 >M $1141 $b2 >M $1142 $68 >M $1143 $38 >M $a1e3 $8b >M
T{ op PC S A X Y P -> $1143 $b1 $8b $38 $82 $e4 }T T{ $0068 M $0069 M $1141 M $1142 M $1143 M $a1e3 M -> $e3 $a1 $b2 $68 $38 $8b }T
( b3 )
$6d66 >PC $25 >S $ff >A $f5 >X $24 >Y $ad >P $6d66 $b3 >M $6d67 $6b >M $6d68 $7b >M
T{ op PC S A X Y P -> $6d67 $25 $ff $f5 $24 $ad }T T{ $6d66 M $6d67 M $6d68 M -> $b3 $6b $7b }T
$b6e3 >PC $78 >S $3f >A $7f >X $75 >Y $63 >P $b6e3 $b3 >M $b6e4 $73 >M $b6e5 $c0 >M
T{ op PC S A X Y P -> $b6e4 $78 $3f $7f $75 $63 }T T{ $b6e3 M $b6e4 M $b6e5 M -> $b3 $73 $c0 }T
$c23f >PC $f5 >S $87 >A $e6 >X $45 >Y $21 >P $c23f $b3 >M $c240 $c5 >M $c241 $a2 >M
T{ op PC S A X Y P -> $c240 $f5 $87 $e6 $45 $21 }T T{ $c23f M $c240 M $c241 M -> $b3 $c5 $a2 }T
$11e1 >PC $57 >S $01 >A $85 >X $17 >Y $2c >P $11e1 $b3 >M $11e2 $5e >M $11e3 $ce >M
T{ op PC S A X Y P -> $11e2 $57 $01 $85 $17 $2c }T T{ $11e1 M $11e2 M $11e3 M -> $b3 $5e $ce }T
$8044 >PC $b9 >S $b7 >A $df >X $8b >Y $64 >P $8044 $b3 >M $8045 $44 >M $8046 $8f >M
T{ op PC S A X Y P -> $8045 $b9 $b7 $df $8b $64 }T T{ $8044 M $8045 M $8046 M -> $b3 $44 $8f }T
$6fb8 >PC $3f >S $ec >A $91 >X $1f >Y $e6 >P $6fb8 $b3 >M $6fb9 $ed >M $6fba $10 >M
T{ op PC S A X Y P -> $6fb9 $3f $ec $91 $1f $e6 }T T{ $6fb8 M $6fb9 M $6fba M -> $b3 $ed $10 }T
$fcee >PC $72 >S $ac >A $6e >X $43 >Y $ae >P $fcee $b3 >M $fcef $4e >M $fcf0 $14 >M
T{ op PC S A X Y P -> $fcef $72 $ac $6e $43 $ae }T T{ $fcee M $fcef M $fcf0 M -> $b3 $4e $14 }T
$14f4 >PC $2f >S $b5 >A $d2 >X $39 >Y $e7 >P $14f4 $b3 >M $14f5 $ce >M $14f6 $9d >M
T{ op PC S A X Y P -> $14f5 $2f $b5 $d2 $39 $e7 }T T{ $14f4 M $14f5 M $14f6 M -> $b3 $ce $9d }T
$1be3 >PC $11 >S $59 >A $49 >X $a7 >Y $2e >P $1be3 $b3 >M $1be4 $bc >M $1be5 $2c >M
T{ op PC S A X Y P -> $1be4 $11 $59 $49 $a7 $2e }T T{ $1be3 M $1be4 M $1be5 M -> $b3 $bc $2c }T
$c3bd >PC $46 >S $53 >A $e9 >X $fc >Y $61 >P $c3bd $b3 >M $c3be $2b >M $c3bf $72 >M
T{ op PC S A X Y P -> $c3be $46 $53 $e9 $fc $61 }T T{ $c3bd M $c3be M $c3bf M -> $b3 $2b $72 }T
$a819 >PC $d1 >S $c2 >A $0f >X $5e >Y $62 >P $a819 $b3 >M $a81a $81 >M $a81b $98 >M
T{ op PC S A X Y P -> $a81a $d1 $c2 $0f $5e $62 }T T{ $a819 M $a81a M $a81b M -> $b3 $81 $98 }T
$90e6 >PC $d4 >S $d5 >A $29 >X $9a >Y $27 >P $90e6 $b3 >M $90e7 $43 >M $90e8 $1a >M
T{ op PC S A X Y P -> $90e7 $d4 $d5 $29 $9a $27 }T T{ $90e6 M $90e7 M $90e8 M -> $b3 $43 $1a }T
$d996 >PC $63 >S $97 >A $0f >X $55 >Y $a3 >P $d996 $b3 >M $d997 $f3 >M $d998 $c0 >M
T{ op PC S A X Y P -> $d997 $63 $97 $0f $55 $a3 }T T{ $d996 M $d997 M $d998 M -> $b3 $f3 $c0 }T
$4295 >PC $ad >S $4d >A $7b >X $0c >Y $ed >P $4295 $b3 >M $4296 $58 >M $4297 $c6 >M
T{ op PC S A X Y P -> $4296 $ad $4d $7b $0c $ed }T T{ $4295 M $4296 M $4297 M -> $b3 $58 $c6 }T
$2c14 >PC $10 >S $81 >A $5c >X $20 >Y $65 >P $2c14 $b3 >M $2c15 $d5 >M $2c16 $69 >M
T{ op PC S A X Y P -> $2c15 $10 $81 $5c $20 $65 }T T{ $2c14 M $2c15 M $2c16 M -> $b3 $d5 $69 }T
$53b6 >PC $3a >S $83 >A $1e >X $e6 >Y $22 >P $53b6 $b3 >M $53b7 $fd >M $53b8 $e9 >M
T{ op PC S A X Y P -> $53b7 $3a $83 $1e $e6 $22 }T T{ $53b6 M $53b7 M $53b8 M -> $b3 $fd $e9 }T
( b4 )
$40a0 >PC $00 >S $1a >A $6a >X $d5 >Y $af >P $0056 $a4 >M $00c0 $e3 >M $40a0 $b4 >M $40a1 $56 >M $40a2 $fa >M
T{ op PC S A X Y P -> $40a2 $00 $1a $6a $e3 $ad }T T{ $0056 M $00c0 M $40a0 M $40a1 M $40a2 M -> $a4 $e3 $b4 $56 $fa }T
$09bf >PC $f8 >S $59 >A $21 >X $eb >Y $2b >P $0009 $9e >M $002a $45 >M $09bf $b4 >M $09c0 $09 >M $09c1 $f9 >M
T{ op PC S A X Y P -> $09c1 $f8 $59 $21 $45 $29 }T T{ $0009 M $002a M $09bf M $09c0 M $09c1 M -> $9e $45 $b4 $09 $f9 }T
$aa61 >PC $54 >S $8a >A $0b >X $3f >Y $a8 >P $009f $55 >M $00aa $e9 >M $aa61 $b4 >M $aa62 $9f >M $aa63 $d4 >M
T{ op PC S A X Y P -> $aa63 $54 $8a $0b $e9 $a8 }T T{ $009f M $00aa M $aa61 M $aa62 M $aa63 M -> $55 $e9 $b4 $9f $d4 }T
$0c96 >PC $0b >S $65 >A $03 >X $0b >Y $e5 >P $00c6 $ec >M $00c9 $30 >M $0c96 $b4 >M $0c97 $c6 >M $0c98 $fe >M
T{ op PC S A X Y P -> $0c98 $0b $65 $03 $30 $65 }T T{ $00c6 M $00c9 M $0c96 M $0c97 M $0c98 M -> $ec $30 $b4 $c6 $fe }T
$25e0 >PC $92 >S $63 >A $32 >X $00 >Y $61 >P $00a5 $09 >M $00d7 $62 >M $25e0 $b4 >M $25e1 $a5 >M $25e2 $50 >M
T{ op PC S A X Y P -> $25e2 $92 $63 $32 $62 $61 }T T{ $00a5 M $00d7 M $25e0 M $25e1 M $25e2 M -> $09 $62 $b4 $a5 $50 }T
$894b >PC $8a >S $02 >A $7c >X $a4 >Y $23 >P $006e $4b >M $00ea $42 >M $894b $b4 >M $894c $6e >M $894d $1b >M
T{ op PC S A X Y P -> $894d $8a $02 $7c $42 $21 }T T{ $006e M $00ea M $894b M $894c M $894d M -> $4b $42 $b4 $6e $1b }T
$a5e5 >PC $79 >S $b7 >A $62 >X $96 >Y $ae >P $000f $9a >M $0071 $aa >M $a5e5 $b4 >M $a5e6 $0f >M $a5e7 $43 >M
T{ op PC S A X Y P -> $a5e7 $79 $b7 $62 $aa $ac }T T{ $000f M $0071 M $a5e5 M $a5e6 M $a5e7 M -> $9a $aa $b4 $0f $43 }T
$d081 >PC $17 >S $fe >A $e4 >X $fe >Y $28 >P $001c $40 >M $0038 $22 >M $d081 $b4 >M $d082 $38 >M $d083 $a8 >M
T{ op PC S A X Y P -> $d083 $17 $fe $e4 $40 $28 }T T{ $001c M $0038 M $d081 M $d082 M $d083 M -> $40 $22 $b4 $38 $a8 }T
$9267 >PC $2a >S $11 >A $c4 >X $46 >Y $67 >P $0026 $d5 >M $0062 $d8 >M $9267 $b4 >M $9268 $62 >M $9269 $92 >M
T{ op PC S A X Y P -> $9269 $2a $11 $c4 $d5 $e5 }T T{ $0026 M $0062 M $9267 M $9268 M $9269 M -> $d5 $d8 $b4 $62 $92 }T
$a15c >PC $a7 >S $6c >A $83 >X $5e >Y $27 >P $0079 $79 >M $00fc $bd >M $a15c $b4 >M $a15d $79 >M $a15e $2c >M
T{ op PC S A X Y P -> $a15e $a7 $6c $83 $bd $a5 }T T{ $0079 M $00fc M $a15c M $a15d M $a15e M -> $79 $bd $b4 $79 $2c }T
$7057 >PC $b7 >S $53 >A $8c >X $99 >Y $66 >P $0088 $8b >M $00fc $80 >M $7057 $b4 >M $7058 $fc >M $7059 $bf >M
T{ op PC S A X Y P -> $7059 $b7 $53 $8c $8b $e4 }T T{ $0088 M $00fc M $7057 M $7058 M $7059 M -> $8b $80 $b4 $fc $bf }T
$93ac >PC $74 >S $8b >A $2f >X $a5 >Y $e3 >P $001a $01 >M $0049 $24 >M $93ac $b4 >M $93ad $1a >M $93ae $ed >M
T{ op PC S A X Y P -> $93ae $74 $8b $2f $24 $61 }T T{ $001a M $0049 M $93ac M $93ad M $93ae M -> $01 $24 $b4 $1a $ed }T
$2153 >PC $0d >S $3a >A $f3 >X $d2 >Y $ee >P $0044 $c0 >M $0051 $c9 >M $2153 $b4 >M $2154 $51 >M $2155 $de >M
T{ op PC S A X Y P -> $2155 $0d $3a $f3 $c0 $ec }T T{ $0044 M $0051 M $2153 M $2154 M $2155 M -> $c0 $c9 $b4 $51 $de }T
$3515 >PC $f5 >S $91 >A $35 >X $e1 >Y $23 >P $0092 $dc >M $00c7 $14 >M $3515 $b4 >M $3516 $92 >M $3517 $1f >M
T{ op PC S A X Y P -> $3517 $f5 $91 $35 $14 $21 }T T{ $0092 M $00c7 M $3515 M $3516 M $3517 M -> $dc $14 $b4 $92 $1f }T
$24d0 >PC $c2 >S $4f >A $c4 >X $2e >Y $67 >P $007f $00 >M $00bb $68 >M $24d0 $b4 >M $24d1 $bb >M $24d2 $1a >M
T{ op PC S A X Y P -> $24d2 $c2 $4f $c4 $00 $67 }T T{ $007f M $00bb M $24d0 M $24d1 M $24d2 M -> $00 $68 $b4 $bb $1a }T
$38de >PC $62 >S $49 >A $23 >X $79 >Y $2e >P $0076 $13 >M $0099 $99 >M $38de $b4 >M $38df $76 >M $38e0 $a7 >M
T{ op PC S A X Y P -> $38e0 $62 $49 $23 $99 $ac }T T{ $0076 M $0099 M $38de M $38df M $38e0 M -> $13 $99 $b4 $76 $a7 }T
( b5 )
$b5ec >PC $3f >S $50 >A $04 >X $6d >Y $2b >P $0095 $ca >M $0099 $1c >M $b5ec $b5 >M $b5ed $95 >M $b5ee $9c >M
T{ op PC S A X Y P -> $b5ee $3f $1c $04 $6d $29 }T T{ $0095 M $0099 M $b5ec M $b5ed M $b5ee M -> $ca $1c $b5 $95 $9c }T
$3cb5 >PC $f2 >S $f2 >A $55 >X $8b >Y $a6 >P $0069 $9a >M $00be $ce >M $3cb5 $b5 >M $3cb6 $69 >M $3cb7 $6b >M
T{ op PC S A X Y P -> $3cb7 $f2 $ce $55 $8b $a4 }T T{ $0069 M $00be M $3cb5 M $3cb6 M $3cb7 M -> $9a $ce $b5 $69 $6b }T
$888e >PC $86 >S $b8 >A $53 >X $40 >Y $ac >P $0041 $e3 >M $0094 $34 >M $888e $b5 >M $888f $41 >M $8890 $68 >M
T{ op PC S A X Y P -> $8890 $86 $34 $53 $40 $2c }T T{ $0041 M $0094 M $888e M $888f M $8890 M -> $e3 $34 $b5 $41 $68 }T
$fb5c >PC $de >S $11 >A $42 >X $44 >Y $ac >P $00bb $a5 >M $00fd $ad >M $fb5c $b5 >M $fb5d $bb >M $fb5e $51 >M
T{ op PC S A X Y P -> $fb5e $de $ad $42 $44 $ac }T T{ $00bb M $00fd M $fb5c M $fb5d M $fb5e M -> $a5 $ad $b5 $bb $51 }T
$152a >PC $2a >S $ef >A $9b >X $05 >Y $ab >P $001e $3a >M $0083 $2f >M $152a $b5 >M $152b $83 >M $152c $4d >M
T{ op PC S A X Y P -> $152c $2a $3a $9b $05 $29 }T T{ $001e M $0083 M $152a M $152b M $152c M -> $3a $2f $b5 $83 $4d }T
$1ef7 >PC $39 >S $53 >A $31 >X $56 >Y $e2 >P $003e $eb >M $006f $cd >M $1ef7 $b5 >M $1ef8 $3e >M $1ef9 $be >M
T{ op PC S A X Y P -> $1ef9 $39 $cd $31 $56 $e0 }T T{ $003e M $006f M $1ef7 M $1ef8 M $1ef9 M -> $eb $cd $b5 $3e $be }T
$b86f >PC $78 >S $db >A $65 >X $ab >Y $6c >P $004d $dc >M $00e8 $9d >M $b86f $b5 >M $b870 $e8 >M $b871 $86 >M
T{ op PC S A X Y P -> $b871 $78 $dc $65 $ab $ec }T T{ $004d M $00e8 M $b86f M $b870 M $b871 M -> $dc $9d $b5 $e8 $86 }T
$e5ce >PC $b3 >S $8a >A $59 >X $d0 >Y $2c >P $008a $32 >M $00e3 $06 >M $e5ce $b5 >M $e5cf $8a >M $e5d0 $98 >M
T{ op PC S A X Y P -> $e5d0 $b3 $06 $59 $d0 $2c }T T{ $008a M $00e3 M $e5ce M $e5cf M $e5d0 M -> $32 $06 $b5 $8a $98 }T
$c257 >PC $2a >S $90 >A $8e >X $6d >Y $a9 >P $0052 $5e >M $00c4 $88 >M $c257 $b5 >M $c258 $c4 >M $c259 $c9 >M
T{ op PC S A X Y P -> $c259 $2a $5e $8e $6d $29 }T T{ $0052 M $00c4 M $c257 M $c258 M $c259 M -> $5e $88 $b5 $c4 $c9 }T
$49d5 >PC $e5 >S $d6 >A $b4 >X $e2 >Y $27 >P $003f $db >M $00f3 $4f >M $49d5 $b5 >M $49d6 $3f >M $49d7 $11 >M
T{ op PC S A X Y P -> $49d7 $e5 $4f $b4 $e2 $25 }T T{ $003f M $00f3 M $49d5 M $49d6 M $49d7 M -> $db $4f $b5 $3f $11 }T
$2582 >PC $3e >S $15 >A $dc >X $66 >Y $aa >P $004c $30 >M $0070 $22 >M $2582 $b5 >M $2583 $70 >M $2584 $00 >M
T{ op PC S A X Y P -> $2584 $3e $30 $dc $66 $28 }T T{ $004c M $0070 M $2582 M $2583 M $2584 M -> $30 $22 $b5 $70 $00 }T
$f8e6 >PC $dc >S $50 >A $6a >X $a1 >Y $a7 >P $0085 $ca >M $00ef $55 >M $f8e6 $b5 >M $f8e7 $85 >M $f8e8 $48 >M
T{ op PC S A X Y P -> $f8e8 $dc $55 $6a $a1 $25 }T T{ $0085 M $00ef M $f8e6 M $f8e7 M $f8e8 M -> $ca $55 $b5 $85 $48 }T
$8a10 >PC $66 >S $34 >A $46 >X $29 >Y $e7 >P $009c $60 >M $00e2 $db >M $8a10 $b5 >M $8a11 $9c >M $8a12 $38 >M
T{ op PC S A X Y P -> $8a12 $66 $db $46 $29 $e5 }T T{ $009c M $00e2 M $8a10 M $8a11 M $8a12 M -> $60 $db $b5 $9c $38 }T
$16f5 >PC $04 >S $1e >A $0c >X $e9 >Y $28 >P $0094 $bd >M $00a0 $8e >M $16f5 $b5 >M $16f6 $94 >M $16f7 $1c >M
T{ op PC S A X Y P -> $16f7 $04 $8e $0c $e9 $a8 }T T{ $0094 M $00a0 M $16f5 M $16f6 M $16f7 M -> $bd $8e $b5 $94 $1c }T
$0b28 >PC $9c >S $aa >A $68 >X $79 >Y $2f >P $0062 $4f >M $00ca $35 >M $0b28 $b5 >M $0b29 $62 >M $0b2a $aa >M
T{ op PC S A X Y P -> $0b2a $9c $35 $68 $79 $2d }T T{ $0062 M $00ca M $0b28 M $0b29 M $0b2a M -> $4f $35 $b5 $62 $aa }T
$5947 >PC $59 >S $04 >A $5a >X $f6 >Y $60 >P $0038 $a2 >M $0092 $45 >M $5947 $b5 >M $5948 $38 >M $5949 $c0 >M
T{ op PC S A X Y P -> $5949 $59 $45 $5a $f6 $60 }T T{ $0038 M $0092 M $5947 M $5948 M $5949 M -> $a2 $45 $b5 $38 $c0 }T
( b6 )
$353e >PC $70 >S $7a >A $a4 >X $e6 >Y $2e >P $003d $75 >M $0057 $37 >M $353e $b6 >M $353f $57 >M $3540 $6b >M
T{ op PC S A X Y P -> $3540 $70 $7a $75 $e6 $2c }T T{ $003d M $0057 M $353e M $353f M $3540 M -> $75 $37 $b6 $57 $6b }T
$9557 >PC $59 >S $4a >A $95 >X $bd >Y $a0 >P $00ae $07 >M $00f1 $55 >M $9557 $b6 >M $9558 $f1 >M $9559 $76 >M
T{ op PC S A X Y P -> $9559 $59 $4a $07 $bd $20 }T T{ $00ae M $00f1 M $9557 M $9558 M $9559 M -> $07 $55 $b6 $f1 $76 }T
$6a48 >PC $6e >S $82 >A $dd >X $e3 >Y $62 >P $008e $07 >M $00ab $d1 >M $6a48 $b6 >M $6a49 $ab >M $6a4a $b2 >M
T{ op PC S A X Y P -> $6a4a $6e $82 $07 $e3 $60 }T T{ $008e M $00ab M $6a48 M $6a49 M $6a4a M -> $07 $d1 $b6 $ab $b2 }T
$2b7c >PC $5f >S $8b >A $23 >X $e9 >Y $28 >P $009a $2c >M $00b1 $e7 >M $2b7c $b6 >M $2b7d $b1 >M $2b7e $9d >M
T{ op PC S A X Y P -> $2b7e $5f $8b $2c $e9 $28 }T T{ $009a M $00b1 M $2b7c M $2b7d M $2b7e M -> $2c $e7 $b6 $b1 $9d }T
$f7c6 >PC $2b >S $18 >A $4f >X $a1 >Y $6f >P $001a $42 >M $00bb $70 >M $f7c6 $b6 >M $f7c7 $1a >M $f7c8 $5e >M
T{ op PC S A X Y P -> $f7c8 $2b $18 $70 $a1 $6d }T T{ $001a M $00bb M $f7c6 M $f7c7 M $f7c8 M -> $42 $70 $b6 $1a $5e }T
$3ef2 >PC $9f >S $60 >A $b6 >X $eb >Y $e6 >P $00b5 $a3 >M $00ca $d4 >M $3ef2 $b6 >M $3ef3 $ca >M $3ef4 $7b >M
T{ op PC S A X Y P -> $3ef4 $9f $60 $a3 $eb $e4 }T T{ $00b5 M $00ca M $3ef2 M $3ef3 M $3ef4 M -> $a3 $d4 $b6 $ca $7b }T
$2743 >PC $67 >S $17 >A $8c >X $ba >Y $26 >P $001a $8b >M $00d4 $e0 >M $2743 $b6 >M $2744 $1a >M $2745 $aa >M
T{ op PC S A X Y P -> $2745 $67 $17 $e0 $ba $a4 }T T{ $001a M $00d4 M $2743 M $2744 M $2745 M -> $8b $e0 $b6 $1a $aa }T
$1800 >PC $0d >S $60 >A $0b >X $92 >Y $29 >P $005a $87 >M $00c8 $71 >M $1800 $b6 >M $1801 $c8 >M $1802 $5d >M
T{ op PC S A X Y P -> $1802 $0d $60 $87 $92 $a9 }T T{ $005a M $00c8 M $1800 M $1801 M $1802 M -> $87 $71 $b6 $c8 $5d }T
$7598 >PC $68 >S $db >A $45 >X $d8 >Y $62 >P $003c $42 >M $0064 $2a >M $7598 $b6 >M $7599 $64 >M $759a $39 >M
T{ op PC S A X Y P -> $759a $68 $db $42 $d8 $60 }T T{ $003c M $0064 M $7598 M $7599 M $759a M -> $42 $2a $b6 $64 $39 }T
$6cfe >PC $df >S $90 >A $71 >X $d6 >Y $2e >P $0042 $4f >M $006c $c1 >M $6cfe $b6 >M $6cff $6c >M $6d00 $4e >M
T{ op PC S A X Y P -> $6d00 $df $90 $4f $d6 $2c }T T{ $0042 M $006c M $6cfe M $6cff M $6d00 M -> $4f $c1 $b6 $6c $4e }T
$45cd >PC $a5 >S $4a >A $e8 >X $9f >Y $66 >P $000d $71 >M $00ac $ee >M $45cd $b6 >M $45ce $0d >M $45cf $04 >M
T{ op PC S A X Y P -> $45cf $a5 $4a $ee $9f $e4 }T T{ $000d M $00ac M $45cd M $45ce M $45cf M -> $71 $ee $b6 $0d $04 }T
$0ab8 >PC $bb >S $0e >A $9a >X $95 >Y $e2 >P $0044 $e5 >M $00d9 $cb >M $0ab8 $b6 >M $0ab9 $44 >M $0aba $20 >M
T{ op PC S A X Y P -> $0aba $bb $0e $cb $95 $e0 }T T{ $0044 M $00d9 M $0ab8 M $0ab9 M $0aba M -> $e5 $cb $b6 $44 $20 }T
$1097 >PC $c3 >S $5f >A $5a >X $27 >Y $a9 >P $0064 $c7 >M $008b $7f >M $1097 $b6 >M $1098 $64 >M $1099 $e0 >M
T{ op PC S A X Y P -> $1099 $c3 $5f $7f $27 $29 }T T{ $0064 M $008b M $1097 M $1098 M $1099 M -> $c7 $7f $b6 $64 $e0 }T
$ac1f >PC $d2 >S $ab >A $b7 >X $d9 >Y $ee >P $0007 $ef >M $002e $c9 >M $ac1f $b6 >M $ac20 $2e >M $ac21 $d4 >M
T{ op PC S A X Y P -> $ac21 $d2 $ab $ef $d9 $ec }T T{ $0007 M $002e M $ac1f M $ac20 M $ac21 M -> $ef $c9 $b6 $2e $d4 }T
$cc6f >PC $b5 >S $19 >A $d0 >X $37 >Y $64 >P $0049 $60 >M $0080 $76 >M $cc6f $b6 >M $cc70 $49 >M $cc71 $bc >M
T{ op PC S A X Y P -> $cc71 $b5 $19 $76 $37 $64 }T T{ $0049 M $0080 M $cc6f M $cc70 M $cc71 M -> $60 $76 $b6 $49 $bc }T
$53be >PC $e0 >S $23 >A $4e >X $c2 >Y $ae >P $0022 $b7 >M $0060 $97 >M $53be $b6 >M $53bf $60 >M $53c0 $a7 >M
T{ op PC S A X Y P -> $53c0 $e0 $23 $b7 $c2 $ac }T T{ $0022 M $0060 M $53be M $53bf M $53c0 M -> $b7 $97 $b6 $60 $a7 }T
( b7 )
$4ac9 >PC $49 >S $1f >A $d4 >X $e3 >Y $e3 >P $00b7 $98 >M $4ac9 $b7 >M $4aca $b7 >M $4acb $47 >M
T{ op PC S A X Y P -> $4acb $49 $1f $d4 $e3 $e3 }T T{ $00b7 M $4ac9 M $4aca M $4acb M -> $98 $b7 $b7 $47 }T
$53a9 >PC $3c >S $5c >A $60 >X $42 >Y $a3 >P $00fc $09 >M $53a9 $b7 >M $53aa $fc >M $53ab $40 >M
T{ op PC S A X Y P -> $53ab $3c $5c $60 $42 $a3 }T T{ $00fc M $53a9 M $53aa M $53ab M -> $09 $b7 $fc $40 }T
$c617 >PC $54 >S $e9 >A $c9 >X $98 >Y $aa >P $00f2 $17 >M $c617 $b7 >M $c618 $f2 >M $c619 $31 >M
T{ op PC S A X Y P -> $c619 $54 $e9 $c9 $98 $aa }T T{ $00f2 M $c617 M $c618 M $c619 M -> $1f $b7 $f2 $31 }T
$be7a >PC $97 >S $f0 >A $23 >X $82 >Y $e5 >P $0066 $68 >M $be7a $b7 >M $be7b $66 >M $be7c $92 >M
T{ op PC S A X Y P -> $be7c $97 $f0 $23 $82 $e5 }T T{ $0066 M $be7a M $be7b M $be7c M -> $68 $b7 $66 $92 }T
$199b >PC $d1 >S $85 >A $34 >X $61 >Y $ea >P $009b $ad >M $199b $b7 >M $199c $9b >M $199d $78 >M
T{ op PC S A X Y P -> $199d $d1 $85 $34 $61 $ea }T T{ $009b M $199b M $199c M $199d M -> $ad $b7 $9b $78 }T
$820d >PC $e2 >S $85 >A $e4 >X $f1 >Y $6f >P $0025 $08 >M $820d $b7 >M $820e $25 >M $820f $e5 >M
T{ op PC S A X Y P -> $820f $e2 $85 $e4 $f1 $6f }T T{ $0025 M $820d M $820e M $820f M -> $08 $b7 $25 $e5 }T
$bbe2 >PC $45 >S $7d >A $23 >X $18 >Y $e1 >P $00fb $4c >M $bbe2 $b7 >M $bbe3 $fb >M $bbe4 $4e >M
T{ op PC S A X Y P -> $bbe4 $45 $7d $23 $18 $e1 }T T{ $00fb M $bbe2 M $bbe3 M $bbe4 M -> $4c $b7 $fb $4e }T
$b76a >PC $82 >S $14 >A $f1 >X $44 >Y $6a >P $0081 $fa >M $b76a $b7 >M $b76b $81 >M $b76c $25 >M
T{ op PC S A X Y P -> $b76c $82 $14 $f1 $44 $6a }T T{ $0081 M $b76a M $b76b M $b76c M -> $fa $b7 $81 $25 }T
$a28e >PC $10 >S $ed >A $55 >X $e2 >Y $a0 >P $0037 $ce >M $a28e $b7 >M $a28f $37 >M $a290 $8e >M
T{ op PC S A X Y P -> $a290 $10 $ed $55 $e2 $a0 }T T{ $0037 M $a28e M $a28f M $a290 M -> $ce $b7 $37 $8e }T
$3e67 >PC $73 >S $08 >A $d8 >X $a8 >Y $66 >P $00e6 $b8 >M $3e67 $b7 >M $3e68 $e6 >M $3e69 $8b >M
T{ op PC S A X Y P -> $3e69 $73 $08 $d8 $a8 $66 }T T{ $00e6 M $3e67 M $3e68 M $3e69 M -> $b8 $b7 $e6 $8b }T
$02b6 >PC $6f >S $ef >A $4f >X $be >Y $ae >P $005d $c4 >M $02b6 $b7 >M $02b7 $5d >M $02b8 $c1 >M
T{ op PC S A X Y P -> $02b8 $6f $ef $4f $be $ae }T T{ $005d M $02b6 M $02b7 M $02b8 M -> $cc $b7 $5d $c1 }T
$b963 >PC $78 >S $ce >A $35 >X $d5 >Y $2c >P $005c $0d >M $b963 $b7 >M $b964 $5c >M $b965 $14 >M
T{ op PC S A X Y P -> $b965 $78 $ce $35 $d5 $2c }T T{ $005c M $b963 M $b964 M $b965 M -> $0d $b7 $5c $14 }T
$56cd >PC $f8 >S $50 >A $3e >X $4f >Y $aa >P $0063 $c6 >M $56cd $b7 >M $56ce $63 >M $56cf $cf >M
T{ op PC S A X Y P -> $56cf $f8 $50 $3e $4f $aa }T T{ $0063 M $56cd M $56ce M $56cf M -> $ce $b7 $63 $cf }T
$11a9 >PC $12 >S $a5 >A $b8 >X $3f >Y $a8 >P $00a9 $02 >M $11a9 $b7 >M $11aa $a9 >M $11ab $3c >M
T{ op PC S A X Y P -> $11ab $12 $a5 $b8 $3f $a8 }T T{ $00a9 M $11a9 M $11aa M $11ab M -> $0a $b7 $a9 $3c }T
$6c0d >PC $be >S $71 >A $32 >X $cf >Y $28 >P $005e $c9 >M $6c0d $b7 >M $6c0e $5e >M $6c0f $4e >M
T{ op PC S A X Y P -> $6c0f $be $71 $32 $cf $28 }T T{ $005e M $6c0d M $6c0e M $6c0f M -> $c9 $b7 $5e $4e }T
$895b >PC $e4 >S $d2 >A $38 >X $31 >Y $e7 >P $00ad $4c >M $895b $b7 >M $895c $ad >M $895d $ab >M
T{ op PC S A X Y P -> $895d $e4 $d2 $38 $31 $e7 }T T{ $00ad M $895b M $895c M $895d M -> $4c $b7 $ad $ab }T
( b8 )
$9e97 >PC $e3 >S $38 >A $7e >X $4e >Y $e8 >P $9e97 $b8 >M $9e98 $e6 >M $9e99 $99 >M
T{ op PC S A X Y P -> $9e98 $e3 $38 $7e $4e $a8 }T T{ $9e97 M $9e98 M $9e99 M -> $b8 $e6 $99 }T
$2e81 >PC $2f >S $5a >A $bd >X $ad >Y $23 >P $2e81 $b8 >M $2e82 $6f >M $2e83 $f3 >M
T{ op PC S A X Y P -> $2e82 $2f $5a $bd $ad $23 }T T{ $2e81 M $2e82 M $2e83 M -> $b8 $6f $f3 }T
$33c4 >PC $57 >S $eb >A $0d >X $64 >Y $ee >P $33c4 $b8 >M $33c5 $60 >M $33c6 $dc >M
T{ op PC S A X Y P -> $33c5 $57 $eb $0d $64 $ae }T T{ $33c4 M $33c5 M $33c6 M -> $b8 $60 $dc }T
$a50b >PC $f2 >S $5e >A $66 >X $69 >Y $6d >P $a50b $b8 >M $a50c $d1 >M $a50d $74 >M
T{ op PC S A X Y P -> $a50c $f2 $5e $66 $69 $2d }T T{ $a50b M $a50c M $a50d M -> $b8 $d1 $74 }T
$27b8 >PC $15 >S $ac >A $04 >X $e0 >Y $a6 >P $27b8 $b8 >M $27b9 $4e >M $27ba $9d >M
T{ op PC S A X Y P -> $27b9 $15 $ac $04 $e0 $a6 }T T{ $27b8 M $27b9 M $27ba M -> $b8 $4e $9d }T
$c8b7 >PC $72 >S $2e >A $c7 >X $86 >Y $65 >P $c8b7 $b8 >M $c8b8 $f6 >M $c8b9 $45 >M
T{ op PC S A X Y P -> $c8b8 $72 $2e $c7 $86 $25 }T T{ $c8b7 M $c8b8 M $c8b9 M -> $b8 $f6 $45 }T
$ff57 >PC $42 >S $b6 >A $a0 >X $84 >Y $6c >P $ff57 $b8 >M $ff58 $7c >M $ff59 $22 >M
T{ op PC S A X Y P -> $ff58 $42 $b6 $a0 $84 $2c }T T{ $ff57 M $ff58 M $ff59 M -> $b8 $7c $22 }T
$9353 >PC $ae >S $e9 >A $5d >X $b8 >Y $23 >P $9353 $b8 >M $9354 $86 >M $9355 $e9 >M
T{ op PC S A X Y P -> $9354 $ae $e9 $5d $b8 $23 }T T{ $9353 M $9354 M $9355 M -> $b8 $86 $e9 }T
$9064 >PC $b5 >S $8e >A $8c >X $5e >Y $24 >P $9064 $b8 >M $9065 $80 >M $9066 $a8 >M
T{ op PC S A X Y P -> $9065 $b5 $8e $8c $5e $24 }T T{ $9064 M $9065 M $9066 M -> $b8 $80 $a8 }T
$abd3 >PC $0a >S $aa >A $4a >X $96 >Y $e3 >P $abd3 $b8 >M $abd4 $84 >M $abd5 $d1 >M
T{ op PC S A X Y P -> $abd4 $0a $aa $4a $96 $a3 }T T{ $abd3 M $abd4 M $abd5 M -> $b8 $84 $d1 }T
$2ee1 >PC $61 >S $57 >A $73 >X $4a >Y $ea >P $2ee1 $b8 >M $2ee2 $ba >M $2ee3 $5e >M
T{ op PC S A X Y P -> $2ee2 $61 $57 $73 $4a $aa }T T{ $2ee1 M $2ee2 M $2ee3 M -> $b8 $ba $5e }T
$5ecf >PC $a2 >S $f8 >A $91 >X $2f >Y $28 >P $5ecf $b8 >M $5ed0 $4f >M $5ed1 $4e >M
T{ op PC S A X Y P -> $5ed0 $a2 $f8 $91 $2f $28 }T T{ $5ecf M $5ed0 M $5ed1 M -> $b8 $4f $4e }T
$e7a5 >PC $be >S $64 >A $17 >X $cf >Y $ab >P $e7a5 $b8 >M $e7a6 $1f >M $e7a7 $f1 >M
T{ op PC S A X Y P -> $e7a6 $be $64 $17 $cf $ab }T T{ $e7a5 M $e7a6 M $e7a7 M -> $b8 $1f $f1 }T
$7802 >PC $6d >S $7b >A $33 >X $0f >Y $ae >P $7802 $b8 >M $7803 $83 >M $7804 $2a >M
T{ op PC S A X Y P -> $7803 $6d $7b $33 $0f $ae }T T{ $7802 M $7803 M $7804 M -> $b8 $83 $2a }T
$79b3 >PC $d3 >S $fa >A $e2 >X $d9 >Y $ac >P $79b3 $b8 >M $79b4 $e4 >M $79b5 $dd >M
T{ op PC S A X Y P -> $79b4 $d3 $fa $e2 $d9 $ac }T T{ $79b3 M $79b4 M $79b5 M -> $b8 $e4 $dd }T
$017c >PC $9d >S $81 >A $67 >X $40 >Y $a7 >P $017c $b8 >M $017d $f3 >M $017e $cb >M
T{ op PC S A X Y P -> $017d $9d $81 $67 $40 $a7 }T T{ $017c M $017d M $017e M -> $b8 $f3 $cb }T
( b9 )
$cc7b >PC $8f >S $71 >A $88 >X $7b >Y $e2 >P $752f $43 >M $cc7b $b9 >M $cc7c $b4 >M $cc7d $74 >M $cc7e $8e >M
T{ op PC S A X Y P -> $cc7e $8f $43 $88 $7b $60 }T T{ $752f M $cc7b M $cc7c M $cc7d M $cc7e M -> $43 $b9 $b4 $74 $8e }T
$0edd >PC $e7 >S $fd >A $50 >X $a5 >Y $2f >P $0edd $b9 >M $0ede $46 >M $0edf $6e >M $0ee0 $a7 >M $6eeb $9a >M
T{ op PC S A X Y P -> $0ee0 $e7 $9a $50 $a5 $ad }T T{ $0edd M $0ede M $0edf M $0ee0 M $6eeb M -> $b9 $46 $6e $a7 $9a }T
$4954 >PC $af >S $e0 >A $af >X $06 >Y $e9 >P $4954 $b9 >M $4955 $a3 >M $4956 $e4 >M $4957 $70 >M $e4a9 $93 >M
T{ op PC S A X Y P -> $4957 $af $93 $af $06 $e9 }T T{ $4954 M $4955 M $4956 M $4957 M $e4a9 M -> $b9 $a3 $e4 $70 $93 }T
$7fd2 >PC $4c >S $2f >A $76 >X $53 >Y $6b >P $7fd2 $b9 >M $7fd3 $1a >M $7fd4 $ed >M $7fd5 $1c >M $ed6d $b6 >M
T{ op PC S A X Y P -> $7fd5 $4c $b6 $76 $53 $e9 }T T{ $7fd2 M $7fd3 M $7fd4 M $7fd5 M $ed6d M -> $b9 $1a $ed $1c $b6 }T
$0557 >PC $48 >S $2e >A $8c >X $15 >Y $6f >P $0557 $b9 >M $0558 $e9 >M $0559 $4c >M $055a $02 >M $4cfe $ca >M
T{ op PC S A X Y P -> $055a $48 $ca $8c $15 $ed }T T{ $0557 M $0558 M $0559 M $055a M $4cfe M -> $b9 $e9 $4c $02 $ca }T
$947f >PC $1c >S $05 >A $af >X $52 >Y $22 >P $947f $b9 >M $9480 $e5 >M $9481 $d3 >M $9482 $82 >M $d437 $1f >M
T{ op PC S A X Y P -> $9482 $1c $1f $af $52 $20 }T T{ $947f M $9480 M $9481 M $9482 M $d437 M -> $b9 $e5 $d3 $82 $1f }T
$6cdc >PC $d3 >S $01 >A $25 >X $ce >Y $61 >P $6cdc $b9 >M $6cdd $89 >M $6cde $cb >M $6cdf $4e >M $cc57 $87 >M
T{ op PC S A X Y P -> $6cdf $d3 $87 $25 $ce $e1 }T T{ $6cdc M $6cdd M $6cde M $6cdf M $cc57 M -> $b9 $89 $cb $4e $87 }T
$c99e >PC $05 >S $2b >A $8b >X $c0 >Y $ed >P $bc4d $77 >M $c99e $b9 >M $c99f $8d >M $c9a0 $bb >M $c9a1 $05 >M
T{ op PC S A X Y P -> $c9a1 $05 $77 $8b $c0 $6d }T T{ $bc4d M $c99e M $c99f M $c9a0 M $c9a1 M -> $77 $b9 $8d $bb $05 }T
$305b >PC $1a >S $15 >A $96 >X $bd >Y $28 >P $24d6 $ad >M $305b $b9 >M $305c $19 >M $305d $24 >M $305e $58 >M
T{ op PC S A X Y P -> $305e $1a $ad $96 $bd $a8 }T T{ $24d6 M $305b M $305c M $305d M $305e M -> $ad $b9 $19 $24 $58 }T
$baf0 >PC $11 >S $8f >A $77 >X $2c >Y $e6 >P $97bf $85 >M $baf0 $b9 >M $baf1 $93 >M $baf2 $97 >M $baf3 $5a >M
T{ op PC S A X Y P -> $baf3 $11 $85 $77 $2c $e4 }T T{ $97bf M $baf0 M $baf1 M $baf2 M $baf3 M -> $85 $b9 $93 $97 $5a }T
$c6d7 >PC $e0 >S $6d >A $fc >X $65 >Y $67 >P $c6d7 $b9 >M $c6d8 $c8 >M $c6d9 $d2 >M $c6da $da >M $d32d $97 >M
T{ op PC S A X Y P -> $c6da $e0 $97 $fc $65 $e5 }T T{ $c6d7 M $c6d8 M $c6d9 M $c6da M $d32d M -> $b9 $c8 $d2 $da $97 }T
$e3d8 >PC $18 >S $ed >A $f4 >X $5b >Y $e6 >P $d108 $62 >M $e3d8 $b9 >M $e3d9 $ad >M $e3da $d0 >M $e3db $af >M
T{ op PC S A X Y P -> $e3db $18 $62 $f4 $5b $64 }T T{ $d108 M $e3d8 M $e3d9 M $e3da M $e3db M -> $62 $b9 $ad $d0 $af }T
$37d8 >PC $d3 >S $e2 >A $07 >X $3e >Y $2a >P $37d8 $b9 >M $37d9 $39 >M $37da $d7 >M $37db $5b >M $d777 $1c >M
T{ op PC S A X Y P -> $37db $d3 $1c $07 $3e $28 }T T{ $37d8 M $37d9 M $37da M $37db M $d777 M -> $b9 $39 $d7 $5b $1c }T
$3c27 >PC $6a >S $da >A $77 >X $53 >Y $a7 >P $3c27 $b9 >M $3c28 $08 >M $3c29 $97 >M $3c2a $a8 >M $975b $b2 >M
T{ op PC S A X Y P -> $3c2a $6a $b2 $77 $53 $a5 }T T{ $3c27 M $3c28 M $3c29 M $3c2a M $975b M -> $b9 $08 $97 $a8 $b2 }T
$f0ef >PC $f0 >S $40 >A $2a >X $63 >Y $22 >P $ab86 $50 >M $f0ef $b9 >M $f0f0 $23 >M $f0f1 $ab >M $f0f2 $b1 >M
T{ op PC S A X Y P -> $f0f2 $f0 $50 $2a $63 $20 }T T{ $ab86 M $f0ef M $f0f0 M $f0f1 M $f0f2 M -> $50 $b9 $23 $ab $b1 }T
$3bd7 >PC $92 >S $01 >A $d1 >X $75 >Y $a3 >P $250b $6d >M $3bd7 $b9 >M $3bd8 $96 >M $3bd9 $24 >M $3bda $2e >M
T{ op PC S A X Y P -> $3bda $92 $6d $d1 $75 $21 }T T{ $250b M $3bd7 M $3bd8 M $3bd9 M $3bda M -> $6d $b9 $96 $24 $2e }T
( ba )
$138f >PC $49 >S $12 >A $83 >X $9b >Y $ab >P $138f $ba >M $1390 $0a >M $1391 $e5 >M
T{ op PC S A X Y P -> $1390 $49 $12 $49 $9b $29 }T T{ $138f M $1390 M $1391 M -> $ba $0a $e5 }T
$8113 >PC $1b >S $d6 >A $c3 >X $2a >Y $ef >P $8113 $ba >M $8114 $6c >M $8115 $c9 >M
T{ op PC S A X Y P -> $8114 $1b $d6 $1b $2a $6d }T T{ $8113 M $8114 M $8115 M -> $ba $6c $c9 }T
$bb8d >PC $b2 >S $15 >A $0c >X $73 >Y $2e >P $bb8d $ba >M $bb8e $23 >M $bb8f $e9 >M
T{ op PC S A X Y P -> $bb8e $b2 $15 $b2 $73 $ac }T T{ $bb8d M $bb8e M $bb8f M -> $ba $23 $e9 }T
$69cb >PC $53 >S $a2 >A $c5 >X $47 >Y $a2 >P $69cb $ba >M $69cc $e9 >M $69cd $ac >M
T{ op PC S A X Y P -> $69cc $53 $a2 $53 $47 $20 }T T{ $69cb M $69cc M $69cd M -> $ba $e9 $ac }T
$c094 >PC $3d >S $83 >A $3c >X $34 >Y $64 >P $c094 $ba >M $c095 $8c >M $c096 $c0 >M
T{ op PC S A X Y P -> $c095 $3d $83 $3d $34 $64 }T T{ $c094 M $c095 M $c096 M -> $ba $8c $c0 }T
$eac6 >PC $9a >S $a9 >A $d3 >X $96 >Y $a9 >P $eac6 $ba >M $eac7 $50 >M $eac8 $10 >M
T{ op PC S A X Y P -> $eac7 $9a $a9 $9a $96 $a9 }T T{ $eac6 M $eac7 M $eac8 M -> $ba $50 $10 }T
$35c2 >PC $0e >S $f5 >A $59 >X $da >Y $6a >P $35c2 $ba >M $35c3 $37 >M $35c4 $77 >M
T{ op PC S A X Y P -> $35c3 $0e $f5 $0e $da $68 }T T{ $35c2 M $35c3 M $35c4 M -> $ba $37 $77 }T
$20f7 >PC $9f >S $fd >A $b8 >X $69 >Y $ac >P $20f7 $ba >M $20f8 $99 >M $20f9 $30 >M
T{ op PC S A X Y P -> $20f8 $9f $fd $9f $69 $ac }T T{ $20f7 M $20f8 M $20f9 M -> $ba $99 $30 }T
$6116 >PC $d8 >S $b5 >A $f1 >X $3a >Y $2f >P $6116 $ba >M $6117 $9b >M $6118 $88 >M
T{ op PC S A X Y P -> $6117 $d8 $b5 $d8 $3a $ad }T T{ $6116 M $6117 M $6118 M -> $ba $9b $88 }T
$7c5e >PC $79 >S $e2 >A $3b >X $95 >Y $60 >P $7c5e $ba >M $7c5f $6b >M $7c60 $d7 >M
T{ op PC S A X Y P -> $7c5f $79 $e2 $79 $95 $60 }T T{ $7c5e M $7c5f M $7c60 M -> $ba $6b $d7 }T
$5010 >PC $61 >S $d0 >A $23 >X $77 >Y $2e >P $5010 $ba >M $5011 $ad >M $5012 $ec >M
T{ op PC S A X Y P -> $5011 $61 $d0 $61 $77 $2c }T T{ $5010 M $5011 M $5012 M -> $ba $ad $ec }T
$7c1d >PC $22 >S $40 >A $db >X $69 >Y $af >P $7c1d $ba >M $7c1e $65 >M $7c1f $55 >M
T{ op PC S A X Y P -> $7c1e $22 $40 $22 $69 $2d }T T{ $7c1d M $7c1e M $7c1f M -> $ba $65 $55 }T
$9666 >PC $25 >S $32 >A $f4 >X $cc >Y $61 >P $9666 $ba >M $9667 $a8 >M $9668 $d2 >M
T{ op PC S A X Y P -> $9667 $25 $32 $25 $cc $61 }T T{ $9666 M $9667 M $9668 M -> $ba $a8 $d2 }T
$291d >PC $01 >S $07 >A $cf >X $6e >Y $26 >P $291d $ba >M $291e $60 >M $291f $da >M
T{ op PC S A X Y P -> $291e $01 $07 $01 $6e $24 }T T{ $291d M $291e M $291f M -> $ba $60 $da }T
$4744 >PC $1a >S $37 >A $33 >X $d2 >Y $e3 >P $4744 $ba >M $4745 $0a >M $4746 $13 >M
T{ op PC S A X Y P -> $4745 $1a $37 $1a $d2 $61 }T T{ $4744 M $4745 M $4746 M -> $ba $0a $13 }T
$8b39 >PC $61 >S $59 >A $db >X $e4 >Y $ec >P $8b39 $ba >M $8b3a $1f >M $8b3b $60 >M
T{ op PC S A X Y P -> $8b3a $61 $59 $61 $e4 $6c }T T{ $8b39 M $8b3a M $8b3b M -> $ba $1f $60 }T
( bb )
$d313 >PC $9e >S $9d >A $e3 >X $67 >Y $e3 >P $d313 $bb >M $d314 $10 >M $d315 $2d >M
T{ op PC S A X Y P -> $d314 $9e $9d $e3 $67 $e3 }T T{ $d313 M $d314 M $d315 M -> $bb $10 $2d }T
$3e29 >PC $55 >S $6e >A $32 >X $9d >Y $a5 >P $3e29 $bb >M $3e2a $06 >M $3e2b $92 >M
T{ op PC S A X Y P -> $3e2a $55 $6e $32 $9d $a5 }T T{ $3e29 M $3e2a M $3e2b M -> $bb $06 $92 }T
$2fc4 >PC $b5 >S $ea >A $80 >X $99 >Y $ac >P $2fc4 $bb >M $2fc5 $f7 >M $2fc6 $e3 >M
T{ op PC S A X Y P -> $2fc5 $b5 $ea $80 $99 $ac }T T{ $2fc4 M $2fc5 M $2fc6 M -> $bb $f7 $e3 }T
$0ad3 >PC $06 >S $6b >A $d9 >X $c2 >Y $28 >P $0ad3 $bb >M $0ad4 $41 >M $0ad5 $d6 >M
T{ op PC S A X Y P -> $0ad4 $06 $6b $d9 $c2 $28 }T T{ $0ad3 M $0ad4 M $0ad5 M -> $bb $41 $d6 }T
$e185 >PC $da >S $86 >A $28 >X $2e >Y $29 >P $e185 $bb >M $e186 $8c >M $e187 $e2 >M
T{ op PC S A X Y P -> $e186 $da $86 $28 $2e $29 }T T{ $e185 M $e186 M $e187 M -> $bb $8c $e2 }T
$813c >PC $0b >S $52 >A $f7 >X $48 >Y $61 >P $813c $bb >M $813d $2a >M $813e $f1 >M
T{ op PC S A X Y P -> $813d $0b $52 $f7 $48 $61 }T T{ $813c M $813d M $813e M -> $bb $2a $f1 }T
$bf14 >PC $13 >S $94 >A $5e >X $e3 >Y $e1 >P $bf14 $bb >M $bf15 $45 >M $bf16 $a8 >M
T{ op PC S A X Y P -> $bf15 $13 $94 $5e $e3 $e1 }T T{ $bf14 M $bf15 M $bf16 M -> $bb $45 $a8 }T
$489d >PC $66 >S $8a >A $65 >X $9a >Y $27 >P $489d $bb >M $489e $ec >M $489f $91 >M
T{ op PC S A X Y P -> $489e $66 $8a $65 $9a $27 }T T{ $489d M $489e M $489f M -> $bb $ec $91 }T
$3601 >PC $75 >S $94 >A $fd >X $d3 >Y $ef >P $3601 $bb >M $3602 $b0 >M $3603 $ba >M
T{ op PC S A X Y P -> $3602 $75 $94 $fd $d3 $ef }T T{ $3601 M $3602 M $3603 M -> $bb $b0 $ba }T
$e660 >PC $23 >S $f7 >A $59 >X $59 >Y $ae >P $e660 $bb >M $e661 $55 >M $e662 $90 >M
T{ op PC S A X Y P -> $e661 $23 $f7 $59 $59 $ae }T T{ $e660 M $e661 M $e662 M -> $bb $55 $90 }T
$d1d4 >PC $72 >S $36 >A $27 >X $2d >Y $61 >P $d1d4 $bb >M $d1d5 $aa >M $d1d6 $c8 >M
T{ op PC S A X Y P -> $d1d5 $72 $36 $27 $2d $61 }T T{ $d1d4 M $d1d5 M $d1d6 M -> $bb $aa $c8 }T
$6bac >PC $94 >S $e3 >A $a2 >X $58 >Y $65 >P $6bac $bb >M $6bad $13 >M $6bae $71 >M
T{ op PC S A X Y P -> $6bad $94 $e3 $a2 $58 $65 }T T{ $6bac M $6bad M $6bae M -> $bb $13 $71 }T
$7474 >PC $81 >S $43 >A $5d >X $c2 >Y $27 >P $7474 $bb >M $7475 $5e >M $7476 $40 >M
T{ op PC S A X Y P -> $7475 $81 $43 $5d $c2 $27 }T T{ $7474 M $7475 M $7476 M -> $bb $5e $40 }T
$f0cb >PC $d4 >S $71 >A $80 >X $dd >Y $ac >P $f0cb $bb >M $f0cc $12 >M $f0cd $9c >M
T{ op PC S A X Y P -> $f0cc $d4 $71 $80 $dd $ac }T T{ $f0cb M $f0cc M $f0cd M -> $bb $12 $9c }T
$e8ac >PC $1f >S $11 >A $fb >X $f7 >Y $ad >P $e8ac $bb >M $e8ad $26 >M $e8ae $5e >M
T{ op PC S A X Y P -> $e8ad $1f $11 $fb $f7 $ad }T T{ $e8ac M $e8ad M $e8ae M -> $bb $26 $5e }T
$e0e2 >PC $1e >S $b1 >A $6d >X $c2 >Y $25 >P $e0e2 $bb >M $e0e3 $3c >M $e0e4 $00 >M
T{ op PC S A X Y P -> $e0e3 $1e $b1 $6d $c2 $25 }T T{ $e0e2 M $e0e3 M $e0e4 M -> $bb $3c $00 }T
( bc )
$b879 >PC $e2 >S $f5 >A $2e >X $e6 >Y $a2 >P $0100 $09 >M $b879 $bc >M $b87a $d2 >M $b87b $00 >M $b87c $66 >M
T{ op PC S A X Y P -> $b87c $e2 $f5 $2e $09 $20 }T T{ $0100 M $b879 M $b87a M $b87b M $b87c M -> $09 $bc $d2 $00 $66 }T
$7566 >PC $04 >S $8e >A $2d >X $4a >Y $ee >P $4a90 $27 >M $7566 $bc >M $7567 $63 >M $7568 $4a >M $7569 $ff >M
T{ op PC S A X Y P -> $7569 $04 $8e $2d $27 $6c }T T{ $4a90 M $7566 M $7567 M $7568 M $7569 M -> $27 $bc $63 $4a $ff }T
$a26d >PC $ff >S $a1 >A $08 >X $5b >Y $a2 >P $a26d $bc >M $a26e $bf >M $a26f $bc >M $a270 $8e >M $bcc7 $d8 >M
T{ op PC S A X Y P -> $a270 $ff $a1 $08 $d8 $a0 }T T{ $a26d M $a26e M $a26f M $a270 M $bcc7 M -> $bc $bf $bc $8e $d8 }T
$e7da >PC $ca >S $03 >A $93 >X $37 >Y $ef >P $08e1 $9f >M $e7da $bc >M $e7db $4e >M $e7dc $08 >M $e7dd $66 >M
T{ op PC S A X Y P -> $e7dd $ca $03 $93 $9f $ed }T T{ $08e1 M $e7da M $e7db M $e7dc M $e7dd M -> $9f $bc $4e $08 $66 }T
$79e1 >PC $6d >S $de >A $5a >X $a0 >Y $28 >P $79e1 $bc >M $79e2 $ee >M $79e3 $c4 >M $79e4 $73 >M $c548 $1d >M
T{ op PC S A X Y P -> $79e4 $6d $de $5a $1d $28 }T T{ $79e1 M $79e2 M $79e3 M $79e4 M $c548 M -> $bc $ee $c4 $73 $1d }T
$944d >PC $60 >S $61 >A $96 >X $a2 >Y $ed >P $4df5 $58 >M $944d $bc >M $944e $5f >M $944f $4d >M $9450 $01 >M
T{ op PC S A X Y P -> $9450 $60 $61 $96 $58 $6d }T T{ $4df5 M $944d M $944e M $944f M $9450 M -> $58 $bc $5f $4d $01 }T
$9c04 >PC $f1 >S $6a >A $08 >X $24 >Y $24 >P $1c64 $d2 >M $9c04 $bc >M $9c05 $5c >M $9c06 $1c >M $9c07 $af >M
T{ op PC S A X Y P -> $9c07 $f1 $6a $08 $d2 $a4 }T T{ $1c64 M $9c04 M $9c05 M $9c06 M $9c07 M -> $d2 $bc $5c $1c $af }T
$db22 >PC $33 >S $1a >A $d4 >X $a8 >Y $2a >P $7906 $1a >M $db22 $bc >M $db23 $32 >M $db24 $78 >M $db25 $a4 >M
T{ op PC S A X Y P -> $db25 $33 $1a $d4 $1a $28 }T T{ $7906 M $db22 M $db23 M $db24 M $db25 M -> $1a $bc $32 $78 $a4 }T
$9fb8 >PC $6c >S $1c >A $a3 >X $b5 >Y $6c >P $9fb8 $bc >M $9fb9 $c0 >M $9fba $b5 >M $9fbb $28 >M $b663 $1e >M
T{ op PC S A X Y P -> $9fbb $6c $1c $a3 $1e $6c }T T{ $9fb8 M $9fb9 M $9fba M $9fbb M $b663 M -> $bc $c0 $b5 $28 $1e }T
$72b0 >PC $c2 >S $fc >A $d7 >X $b6 >Y $a4 >P $670e $0e >M $72b0 $bc >M $72b1 $37 >M $72b2 $66 >M $72b3 $ca >M
T{ op PC S A X Y P -> $72b3 $c2 $fc $d7 $0e $24 }T T{ $670e M $72b0 M $72b1 M $72b2 M $72b3 M -> $0e $bc $37 $66 $ca }T
$46a6 >PC $a6 >S $fa >A $d8 >X $2c >Y $aa >P $46a6 $bc >M $46a7 $f6 >M $46a8 $e1 >M $46a9 $88 >M $e2ce $81 >M
T{ op PC S A X Y P -> $46a9 $a6 $fa $d8 $81 $a8 }T T{ $46a6 M $46a7 M $46a8 M $46a9 M $e2ce M -> $bc $f6 $e1 $88 $81 }T
$6ea4 >PC $43 >S $d9 >A $0f >X $6a >Y $ab >P $6ea4 $bc >M $6ea5 $68 >M $6ea6 $9a >M $6ea7 $ff >M $9a77 $c4 >M
T{ op PC S A X Y P -> $6ea7 $43 $d9 $0f $c4 $a9 }T T{ $6ea4 M $6ea5 M $6ea6 M $6ea7 M $9a77 M -> $bc $68 $9a $ff $c4 }T
$ee71 >PC $d7 >S $c0 >A $0b >X $95 >Y $2b >P $20bc $71 >M $ee71 $bc >M $ee72 $b1 >M $ee73 $20 >M $ee74 $03 >M
T{ op PC S A X Y P -> $ee74 $d7 $c0 $0b $71 $29 }T T{ $20bc M $ee71 M $ee72 M $ee73 M $ee74 M -> $71 $bc $b1 $20 $03 }T
$ed21 >PC $35 >S $a2 >A $0e >X $10 >Y $65 >P $32c7 $9d >M $ed21 $bc >M $ed22 $b9 >M $ed23 $32 >M $ed24 $42 >M
T{ op PC S A X Y P -> $ed24 $35 $a2 $0e $9d $e5 }T T{ $32c7 M $ed21 M $ed22 M $ed23 M $ed24 M -> $9d $bc $b9 $32 $42 }T
$c616 >PC $17 >S $01 >A $d4 >X $31 >Y $6c >P $a4c8 $51 >M $c616 $bc >M $c617 $f4 >M $c618 $a3 >M $c619 $bb >M
T{ op PC S A X Y P -> $c619 $17 $01 $d4 $51 $6c }T T{ $a4c8 M $c616 M $c617 M $c618 M $c619 M -> $51 $bc $f4 $a3 $bb }T
$6732 >PC $a5 >S $d8 >A $c1 >X $13 >Y $65 >P $6732 $bc >M $6733 $30 >M $6734 $92 >M $6735 $2f >M $92f1 $63 >M
T{ op PC S A X Y P -> $6735 $a5 $d8 $c1 $63 $65 }T T{ $6732 M $6733 M $6734 M $6735 M $92f1 M -> $bc $30 $92 $2f $63 }T
( bd )
$a267 >PC $c2 >S $35 >A $2d >X $48 >Y $6a >P $9357 $fb >M $a267 $bd >M $a268 $2a >M $a269 $93 >M $a26a $c5 >M
T{ op PC S A X Y P -> $a26a $c2 $fb $2d $48 $e8 }T T{ $9357 M $a267 M $a268 M $a269 M $a26a M -> $fb $bd $2a $93 $c5 }T
$7112 >PC $4c >S $d3 >A $7c >X $67 >Y $23 >P $090c $78 >M $7112 $bd >M $7113 $90 >M $7114 $08 >M $7115 $6a >M
T{ op PC S A X Y P -> $7115 $4c $78 $7c $67 $21 }T T{ $090c M $7112 M $7113 M $7114 M $7115 M -> $78 $bd $90 $08 $6a }T
$a381 >PC $f9 >S $95 >A $63 >X $df >Y $6d >P $721b $6d >M $a381 $bd >M $a382 $b8 >M $a383 $71 >M $a384 $27 >M
T{ op PC S A X Y P -> $a384 $f9 $6d $63 $df $6d }T T{ $721b M $a381 M $a382 M $a383 M $a384 M -> $6d $bd $b8 $71 $27 }T
$3579 >PC $c9 >S $0c >A $6c >X $59 >Y $a1 >P $071e $b0 >M $3579 $bd >M $357a $b2 >M $357b $06 >M $357c $d1 >M
T{ op PC S A X Y P -> $357c $c9 $b0 $6c $59 $a1 }T T{ $071e M $3579 M $357a M $357b M $357c M -> $b0 $bd $b2 $06 $d1 }T
$a880 >PC $a8 >S $3b >A $de >X $ae >Y $e8 >P $a06f $0e >M $a880 $bd >M $a881 $91 >M $a882 $9f >M $a883 $f4 >M
T{ op PC S A X Y P -> $a883 $a8 $0e $de $ae $68 }T T{ $a06f M $a880 M $a881 M $a882 M $a883 M -> $0e $bd $91 $9f $f4 }T
$9507 >PC $4a >S $a0 >A $fc >X $5f >Y $2c >P $9507 $bd >M $9508 $3f >M $9509 $cb >M $950a $ea >M $cc3b $42 >M
T{ op PC S A X Y P -> $950a $4a $42 $fc $5f $2c }T T{ $9507 M $9508 M $9509 M $950a M $cc3b M -> $bd $3f $cb $ea $42 }T
$8eac >PC $9e >S $11 >A $ee >X $8d >Y $2b >P $8eac $bd >M $8ead $46 >M $8eae $8e >M $8eaf $f6 >M $8f34 $6e >M
T{ op PC S A X Y P -> $8eaf $9e $6e $ee $8d $29 }T T{ $8eac M $8ead M $8eae M $8eaf M $8f34 M -> $bd $46 $8e $f6 $6e }T
$ad98 >PC $93 >S $30 >A $de >X $19 >Y $25 >P $9a6c $40 >M $ad98 $bd >M $ad99 $8e >M $ad9a $99 >M $ad9b $01 >M
T{ op PC S A X Y P -> $ad9b $93 $40 $de $19 $25 }T T{ $9a6c M $ad98 M $ad99 M $ad9a M $ad9b M -> $40 $bd $8e $99 $01 }T
$713c >PC $7f >S $a1 >A $d2 >X $c4 >Y $20 >P $4673 $9a >M $713c $bd >M $713d $a1 >M $713e $45 >M $713f $08 >M
T{ op PC S A X Y P -> $713f $7f $9a $d2 $c4 $a0 }T T{ $4673 M $713c M $713d M $713e M $713f M -> $9a $bd $a1 $45 $08 }T
$04bf >PC $4d >S $62 >A $93 >X $c7 >Y $ea >P $04bf $bd >M $04c0 $1d >M $04c1 $af >M $04c2 $5c >M $afb0 $6f >M
T{ op PC S A X Y P -> $04c2 $4d $6f $93 $c7 $68 }T T{ $04bf M $04c0 M $04c1 M $04c2 M $afb0 M -> $bd $1d $af $5c $6f }T
$4eed >PC $ba >S $3a >A $31 >X $8d >Y $a1 >P $26e9 $5a >M $4eed $bd >M $4eee $b8 >M $4eef $26 >M $4ef0 $6a >M
T{ op PC S A X Y P -> $4ef0 $ba $5a $31 $8d $21 }T T{ $26e9 M $4eed M $4eee M $4eef M $4ef0 M -> $5a $bd $b8 $26 $6a }T
$14e7 >PC $78 >S $52 >A $ee >X $a5 >Y $2e >P $14e7 $bd >M $14e8 $97 >M $14e9 $68 >M $14ea $7a >M $6985 $46 >M
T{ op PC S A X Y P -> $14ea $78 $46 $ee $a5 $2c }T T{ $14e7 M $14e8 M $14e9 M $14ea M $6985 M -> $bd $97 $68 $7a $46 }T
$e3f3 >PC $4e >S $82 >A $b8 >X $7d >Y $67 >P $2ce9 $38 >M $e3f3 $bd >M $e3f4 $31 >M $e3f5 $2c >M $e3f6 $3a >M
T{ op PC S A X Y P -> $e3f6 $4e $38 $b8 $7d $65 }T T{ $2ce9 M $e3f3 M $e3f4 M $e3f5 M $e3f6 M -> $38 $bd $31 $2c $3a }T
$8e5f >PC $58 >S $4e >A $a9 >X $f1 >Y $ab >P $095d $d5 >M $8e5f $bd >M $8e60 $b4 >M $8e61 $08 >M $8e62 $08 >M
T{ op PC S A X Y P -> $8e62 $58 $d5 $a9 $f1 $a9 }T T{ $095d M $8e5f M $8e60 M $8e61 M $8e62 M -> $d5 $bd $b4 $08 $08 }T
$d793 >PC $64 >S $d3 >A $15 >X $21 >Y $e3 >P $606d $a6 >M $d793 $bd >M $d794 $58 >M $d795 $60 >M $d796 $5e >M
T{ op PC S A X Y P -> $d796 $64 $a6 $15 $21 $e1 }T T{ $606d M $d793 M $d794 M $d795 M $d796 M -> $a6 $bd $58 $60 $5e }T
$9ff7 >PC $5b >S $70 >A $eb >X $d5 >Y $2b >P $6441 $36 >M $9ff7 $bd >M $9ff8 $56 >M $9ff9 $63 >M $9ffa $7d >M
T{ op PC S A X Y P -> $9ffa $5b $36 $eb $d5 $29 }T T{ $6441 M $9ff7 M $9ff8 M $9ff9 M $9ffa M -> $36 $bd $56 $63 $7d }T
( be )
$8eea >PC $52 >S $ef >A $13 >X $cd >Y $ac >P $3de6 $8a >M $8eea $be >M $8eeb $19 >M $8eec $3d >M $8eed $e5 >M
T{ op PC S A X Y P -> $8eed $52 $ef $8a $cd $ac }T T{ $3de6 M $8eea M $8eeb M $8eec M $8eed M -> $8a $be $19 $3d $e5 }T
$79cd >PC $8c >S $f4 >A $d9 >X $aa >Y $6b >P $79cd $be >M $79ce $80 >M $79cf $7e >M $79d0 $8d >M $7f2a $57 >M
T{ op PC S A X Y P -> $79d0 $8c $f4 $57 $aa $69 }T T{ $79cd M $79ce M $79cf M $79d0 M $7f2a M -> $be $80 $7e $8d $57 }T
$3ddb >PC $2f >S $79 >A $3b >X $3b >Y $65 >P $3ddb $be >M $3ddc $5d >M $3ddd $8b >M $3dde $cb >M $8b98 $d8 >M
T{ op PC S A X Y P -> $3dde $2f $79 $d8 $3b $e5 }T T{ $3ddb M $3ddc M $3ddd M $3dde M $8b98 M -> $be $5d $8b $cb $d8 }T
$5306 >PC $eb >S $d9 >A $b9 >X $5a >Y $2c >P $5306 $be >M $5307 $60 >M $5308 $69 >M $5309 $01 >M $69ba $d4 >M
T{ op PC S A X Y P -> $5309 $eb $d9 $d4 $5a $ac }T T{ $5306 M $5307 M $5308 M $5309 M $69ba M -> $be $60 $69 $01 $d4 }T
$4e9e >PC $66 >S $ef >A $75 >X $3e >Y $23 >P $4e9e $be >M $4e9f $2f >M $4ea0 $5e >M $4ea1 $d0 >M $5e6d $a8 >M
T{ op PC S A X Y P -> $4ea1 $66 $ef $a8 $3e $a1 }T T{ $4e9e M $4e9f M $4ea0 M $4ea1 M $5e6d M -> $be $2f $5e $d0 $a8 }T
$55c9 >PC $f0 >S $d0 >A $17 >X $fb >Y $21 >P $1319 $9f >M $55c9 $be >M $55ca $1e >M $55cb $12 >M $55cc $a8 >M
T{ op PC S A X Y P -> $55cc $f0 $d0 $9f $fb $a1 }T T{ $1319 M $55c9 M $55ca M $55cb M $55cc M -> $9f $be $1e $12 $a8 }T
$9c04 >PC $1a >S $69 >A $08 >X $e7 >Y $ec >P $9b6a $0c >M $9c04 $be >M $9c05 $83 >M $9c06 $9a >M $9c07 $83 >M
T{ op PC S A X Y P -> $9c07 $1a $69 $0c $e7 $6c }T T{ $9b6a M $9c04 M $9c05 M $9c06 M $9c07 M -> $0c $be $83 $9a $83 }T
$10a0 >PC $c8 >S $02 >A $e8 >X $68 >Y $ed >P $10a0 $be >M $10a1 $4b >M $10a2 $f4 >M $10a3 $82 >M $f4b3 $f7 >M
T{ op PC S A X Y P -> $10a3 $c8 $02 $f7 $68 $ed }T T{ $10a0 M $10a1 M $10a2 M $10a3 M $f4b3 M -> $be $4b $f4 $82 $f7 }T
$e701 >PC $22 >S $d0 >A $2c >X $bc >Y $61 >P $dc61 $fd >M $e701 $be >M $e702 $a5 >M $e703 $db >M $e704 $46 >M
T{ op PC S A X Y P -> $e704 $22 $d0 $fd $bc $e1 }T T{ $dc61 M $e701 M $e702 M $e703 M $e704 M -> $fd $be $a5 $db $46 }T
$af64 >PC $b0 >S $ee >A $10 >X $4f >Y $a3 >P $1048 $47 >M $af64 $be >M $af65 $f9 >M $af66 $0f >M $af67 $21 >M
T{ op PC S A X Y P -> $af67 $b0 $ee $47 $4f $21 }T T{ $1048 M $af64 M $af65 M $af66 M $af67 M -> $47 $be $f9 $0f $21 }T
$f491 >PC $5b >S $d7 >A $3a >X $11 >Y $e0 >P $0ef1 $5f >M $f491 $be >M $f492 $e0 >M $f493 $0e >M $f494 $86 >M
T{ op PC S A X Y P -> $f494 $5b $d7 $5f $11 $60 }T T{ $0ef1 M $f491 M $f492 M $f493 M $f494 M -> $5f $be $e0 $0e $86 }T
$69ed >PC $51 >S $0b >A $06 >X $50 >Y $66 >P $69ed $be >M $69ee $cc >M $69ef $a8 >M $69f0 $e5 >M $a91c $1e >M
T{ op PC S A X Y P -> $69f0 $51 $0b $1e $50 $64 }T T{ $69ed M $69ee M $69ef M $69f0 M $a91c M -> $be $cc $a8 $e5 $1e }T
$944a >PC $44 >S $60 >A $2c >X $0a >Y $63 >P $944a $be >M $944b $28 >M $944c $c1 >M $944d $5e >M $c132 $b4 >M
T{ op PC S A X Y P -> $944d $44 $60 $b4 $0a $e1 }T T{ $944a M $944b M $944c M $944d M $c132 M -> $be $28 $c1 $5e $b4 }T
$9a47 >PC $f2 >S $1f >A $0d >X $51 >Y $a1 >P $9a47 $be >M $9a48 $8a >M $9a49 $ad >M $9a4a $8f >M $addb $3a >M
T{ op PC S A X Y P -> $9a4a $f2 $1f $3a $51 $21 }T T{ $9a47 M $9a48 M $9a49 M $9a4a M $addb M -> $be $8a $ad $8f $3a }T
$845d >PC $65 >S $08 >A $8b >X $c3 >Y $eb >P $550b $5e >M $845d $be >M $845e $48 >M $845f $54 >M $8460 $cf >M
T{ op PC S A X Y P -> $8460 $65 $08 $5e $c3 $69 }T T{ $550b M $845d M $845e M $845f M $8460 M -> $5e $be $48 $54 $cf }T
$c10c >PC $52 >S $3e >A $ab >X $f8 >Y $e0 >P $b9bc $c1 >M $c10c $be >M $c10d $c4 >M $c10e $b8 >M $c10f $1c >M
T{ op PC S A X Y P -> $c10f $52 $3e $c1 $f8 $e0 }T T{ $b9bc M $c10c M $c10d M $c10e M $c10f M -> $c1 $be $c4 $b8 $1c }T
( bf )
$72a4 >PC $49 >S $6c >A $9a >X $f8 >Y $a1 >P $0080 $45 >M $72a4 $bf >M $72a5 $80 >M $72a6 $07 >M $72a7 $da >M $72ae $a5 >M
T{ op PC S A X Y P -> $72a7 $49 $6c $9a $f8 $a1 }T T{ $0080 M $72a4 M $72a5 M $72a6 M $72a7 M $72ae M -> $45 $bf $80 $07 $da $a5 }T
$ed4c >PC $7c >S $9c >A $d2 >X $8d >Y $a3 >P $00d5 $ab >M $ecfb $0a >M $ed4c $bf >M $ed4d $d5 >M $ed4e $ac >M $edfb $36 >M
T{ op PC S A X Y P -> $ecfb $7c $9c $d2 $8d $a3 }T T{ $00d5 M $ecfb M $ed4c M $ed4d M $ed4e M $edfb M -> $ab $0a $bf $d5 $ac $36 }T
$3ea4 >PC $11 >S $15 >A $c0 >X $44 >Y $6a >P $00c0 $5c >M $3ea4 $bf >M $3ea5 $c0 >M $3ea6 $3e >M $3ee5 $7e >M
T{ op PC S A X Y P -> $3ee5 $11 $15 $c0 $44 $6a }T T{ $00c0 M $3ea4 M $3ea5 M $3ea6 M $3ee5 M -> $5c $bf $c0 $3e $7e }T
$fbe3 >PC $cd >S $2c >A $e3 >X $33 >Y $66 >P $0026 $d5 >M $fbde $8c >M $fbe3 $bf >M $fbe4 $26 >M $fbe5 $f8 >M $fbe6 $b1 >M
T{ op PC S A X Y P -> $fbe6 $cd $2c $e3 $33 $66 }T T{ $0026 M $fbde M $fbe3 M $fbe4 M $fbe5 M $fbe6 M -> $d5 $8c $bf $26 $f8 $b1 }T
$9cb9 >PC $5f >S $02 >A $17 >X $71 >Y $20 >P $0085 $fd >M $9c80 $23 >M $9cb9 $bf >M $9cba $85 >M $9cbb $c4 >M
T{ op PC S A X Y P -> $9c80 $5f $02 $17 $71 $20 }T T{ $0085 M $9c80 M $9cb9 M $9cba M $9cbb M -> $fd $23 $bf $85 $c4 }T
$afc4 >PC $fb >S $ef >A $5e >X $8a >Y $e2 >P $006a $5c >M $af18 $f2 >M $afc4 $bf >M $afc5 $6a >M $afc6 $51 >M $b018 $d0 >M
T{ op PC S A X Y P -> $b018 $fb $ef $5e $8a $e2 }T T{ $006a M $af18 M $afc4 M $afc5 M $afc6 M $b018 M -> $5c $f2 $bf $6a $51 $d0 }T
$51e0 >PC $a7 >S $e1 >A $11 >X $47 >Y $6c >P $00c0 $48 >M $5125 $7c >M $51e0 $bf >M $51e1 $c0 >M $51e2 $42 >M $5225 $13 >M
T{ op PC S A X Y P -> $5225 $a7 $e1 $11 $47 $6c }T T{ $00c0 M $5125 M $51e0 M $51e1 M $51e2 M $5225 M -> $48 $7c $bf $c0 $42 $13 }T
$069a >PC $52 >S $2c >A $c4 >X $dd >Y $a2 >P $00d3 $7f >M $0683 $86 >M $069a $bf >M $069b $d3 >M $069c $e6 >M
T{ op PC S A X Y P -> $0683 $52 $2c $c4 $dd $a2 }T T{ $00d3 M $0683 M $069a M $069b M $069c M -> $7f $86 $bf $d3 $e6 }T
$c686 >PC $f0 >S $4b >A $36 >X $40 >Y $6e >P $005f $4d >M $c686 $bf >M $c687 $5f >M $c688 $74 >M $c6fd $49 >M
T{ op PC S A X Y P -> $c6fd $f0 $4b $36 $40 $6e }T T{ $005f M $c686 M $c687 M $c688 M $c6fd M -> $4d $bf $5f $74 $49 }T
$6118 >PC $8d >S $f9 >A $da >X $b7 >Y $e0 >P $009e $a9 >M $60ec $4c >M $6118 $bf >M $6119 $9e >M $611a $d1 >M $61ec $71 >M
T{ op PC S A X Y P -> $60ec $8d $f9 $da $b7 $e0 }T T{ $009e M $60ec M $6118 M $6119 M $611a M $61ec M -> $a9 $4c $bf $9e $d1 $71 }T
$d646 >PC $45 >S $20 >A $84 >X $3e >Y $61 >P $0005 $08 >M $d616 $06 >M $d646 $bf >M $d647 $05 >M $d648 $cd >M
T{ op PC S A X Y P -> $d616 $45 $20 $84 $3e $61 }T T{ $0005 M $d616 M $d646 M $d647 M $d648 M -> $08 $06 $bf $05 $cd }T
$7226 >PC $2d >S $10 >A $86 >X $fd >Y $a5 >P $00ad $6a >M $7226 $bf >M $7227 $ad >M $7228 $19 >M $7242 $bd >M
T{ op PC S A X Y P -> $7242 $2d $10 $86 $fd $a5 }T T{ $00ad M $7226 M $7227 M $7228 M $7242 M -> $6a $bf $ad $19 $bd }T
$524e >PC $53 >S $ab >A $ab >X $f4 >Y $ac >P $00e5 $fd >M $5247 $8f >M $524e $bf >M $524f $e5 >M $5250 $f6 >M
T{ op PC S A X Y P -> $5247 $53 $ab $ab $f4 $ac }T T{ $00e5 M $5247 M $524e M $524f M $5250 M -> $fd $8f $bf $e5 $f6 }T
$f1c5 >PC $55 >S $91 >A $eb >X $6c >Y $60 >P $00ce $f1 >M $f136 $e5 >M $f1c5 $bf >M $f1c6 $ce >M $f1c7 $6e >M $f1c8 $0a >M
T{ op PC S A X Y P -> $f1c8 $55 $91 $eb $6c $60 }T T{ $00ce M $f136 M $f1c5 M $f1c6 M $f1c7 M $f1c8 M -> $f1 $e5 $bf $ce $6e $0a }T
$0975 >PC $62 >S $7d >A $da >X $05 >Y $26 >P $006d $af >M $0965 $16 >M $0975 $bf >M $0976 $6d >M $0977 $ed >M
T{ op PC S A X Y P -> $0965 $62 $7d $da $05 $26 }T T{ $006d M $0965 M $0975 M $0976 M $0977 M -> $af $16 $bf $6d $ed }T
$de38 >PC $af >S $44 >A $9b >X $68 >Y $aa >P $009f $ed >M $ddfc $00 >M $de38 $bf >M $de39 $9f >M $de3a $c1 >M $defc $9c >M
T{ op PC S A X Y P -> $ddfc $af $44 $9b $68 $aa }T T{ $009f M $ddfc M $de38 M $de39 M $de3a M $defc M -> $ed $00 $bf $9f $c1 $9c }T
( c0 )
$9021 >PC $84 >S $b9 >A $7f >X $14 >Y $e0 >P $9021 $c0 >M $9022 $2d >M $9023 $ec >M
T{ op PC S A X Y P -> $9023 $84 $b9 $7f $14 $e0 }T T{ $9021 M $9022 M $9023 M -> $c0 $2d $ec }T
$1848 >PC $cd >S $f4 >A $5b >X $34 >Y $64 >P $1848 $c0 >M $1849 $14 >M $184a $2f >M
T{ op PC S A X Y P -> $184a $cd $f4 $5b $34 $65 }T T{ $1848 M $1849 M $184a M -> $c0 $14 $2f }T
$1a88 >PC $5f >S $9d >A $ce >X $5e >Y $ab >P $1a88 $c0 >M $1a89 $50 >M $1a8a $0e >M
T{ op PC S A X Y P -> $1a8a $5f $9d $ce $5e $29 }T T{ $1a88 M $1a89 M $1a8a M -> $c0 $50 $0e }T
$fc98 >PC $17 >S $9b >A $b2 >X $1c >Y $a2 >P $fc98 $c0 >M $fc99 $2f >M $fc9a $5d >M
T{ op PC S A X Y P -> $fc9a $17 $9b $b2 $1c $a0 }T T{ $fc98 M $fc99 M $fc9a M -> $c0 $2f $5d }T
$dda8 >PC $e0 >S $a6 >A $77 >X $74 >Y $a9 >P $dda8 $c0 >M $dda9 $7e >M $ddaa $b6 >M
T{ op PC S A X Y P -> $ddaa $e0 $a6 $77 $74 $a8 }T T{ $dda8 M $dda9 M $ddaa M -> $c0 $7e $b6 }T
$fdd8 >PC $90 >S $61 >A $5a >X $81 >Y $2d >P $fdd8 $c0 >M $fdd9 $f8 >M $fdda $a2 >M
T{ op PC S A X Y P -> $fdda $90 $61 $5a $81 $ac }T T{ $fdd8 M $fdd9 M $fdda M -> $c0 $f8 $a2 }T
$aefb >PC $fa >S $b3 >A $da >X $15 >Y $aa >P $aefb $c0 >M $aefc $8a >M $aefd $aa >M
T{ op PC S A X Y P -> $aefd $fa $b3 $da $15 $a8 }T T{ $aefb M $aefc M $aefd M -> $c0 $8a $aa }T
$5014 >PC $7f >S $53 >A $10 >X $37 >Y $6d >P $5014 $c0 >M $5015 $e5 >M $5016 $5f >M
T{ op PC S A X Y P -> $5016 $7f $53 $10 $37 $6c }T T{ $5014 M $5015 M $5016 M -> $c0 $e5 $5f }T
$a58b >PC $ac >S $b1 >A $3d >X $2f >Y $6f >P $a58b $c0 >M $a58c $f7 >M $a58d $c4 >M
T{ op PC S A X Y P -> $a58d $ac $b1 $3d $2f $6c }T T{ $a58b M $a58c M $a58d M -> $c0 $f7 $c4 }T
$11e9 >PC $c3 >S $18 >A $ed >X $c2 >Y $a8 >P $11e9 $c0 >M $11ea $e8 >M $11eb $05 >M
T{ op PC S A X Y P -> $11eb $c3 $18 $ed $c2 $a8 }T T{ $11e9 M $11ea M $11eb M -> $c0 $e8 $05 }T
$9336 >PC $93 >S $71 >A $99 >X $23 >Y $e8 >P $9336 $c0 >M $9337 $bb >M $9338 $3c >M
T{ op PC S A X Y P -> $9338 $93 $71 $99 $23 $68 }T T{ $9336 M $9337 M $9338 M -> $c0 $bb $3c }T
$f00a >PC $58 >S $fc >A $5d >X $06 >Y $2d >P $f00a $c0 >M $f00b $44 >M $f00c $85 >M
T{ op PC S A X Y P -> $f00c $58 $fc $5d $06 $ac }T T{ $f00a M $f00b M $f00c M -> $c0 $44 $85 }T
$0344 >PC $cc >S $71 >A $b3 >X $04 >Y $2c >P $0344 $c0 >M $0345 $48 >M $0346 $08 >M
T{ op PC S A X Y P -> $0346 $cc $71 $b3 $04 $ac }T T{ $0344 M $0345 M $0346 M -> $c0 $48 $08 }T
$558b >PC $68 >S $0e >A $3d >X $43 >Y $29 >P $558b $c0 >M $558c $c1 >M $558d $7a >M
T{ op PC S A X Y P -> $558d $68 $0e $3d $43 $a8 }T T{ $558b M $558c M $558d M -> $c0 $c1 $7a }T
$11dc >PC $7d >S $ff >A $2d >X $e7 >Y $26 >P $11dc $c0 >M $11dd $77 >M $11de $7a >M
T{ op PC S A X Y P -> $11de $7d $ff $2d $e7 $25 }T T{ $11dc M $11dd M $11de M -> $c0 $77 $7a }T
$227f >PC $8e >S $7b >A $54 >X $38 >Y $6e >P $227f $c0 >M $2280 $c9 >M $2281 $e5 >M
T{ op PC S A X Y P -> $2281 $8e $7b $54 $38 $6c }T T{ $227f M $2280 M $2281 M -> $c0 $c9 $e5 }T
( c1 )
$8942 >PC $1b >S $03 >A $83 >X $94 >Y $25 >P $0055 $ff >M $00d8 $c1 >M $00d9 $74 >M $74c1 $8e >M $8942 $c1 >M $8943 $55 >M $8944 $af >M
T{ op PC S A X Y P -> $8944 $1b $03 $83 $94 $24 }T T{ $0055 M $00d8 M $00d9 M $74c1 M $8942 M $8943 M $8944 M -> $ff $c1 $74 $8e $c1 $55 $af }T
$944d >PC $e7 >S $b3 >A $35 >X $f6 >Y $2e >P $006b $dd >M $00a0 $65 >M $00a1 $1c >M $1c65 $ec >M $944d $c1 >M $944e $6b >M $944f $13 >M
T{ op PC S A X Y P -> $944f $e7 $b3 $35 $f6 $ac }T T{ $006b M $00a0 M $00a1 M $1c65 M $944d M $944e M $944f M -> $dd $65 $1c $ec $c1 $6b $13 }T
$54b5 >PC $50 >S $36 >A $8c >X $a8 >Y $a1 >P $0020 $70 >M $00ac $16 >M $00ad $6f >M $54b5 $c1 >M $54b6 $20 >M $54b7 $43 >M $6f16 $c3 >M
T{ op PC S A X Y P -> $54b7 $50 $36 $8c $a8 $20 }T T{ $0020 M $00ac M $00ad M $54b5 M $54b6 M $54b7 M $6f16 M -> $70 $16 $6f $c1 $20 $43 $c3 }T
$34d2 >PC $cc >S $e7 >A $ef >X $0a >Y $6d >P $00a4 $33 >M $00a5 $83 >M $00b5 $1a >M $34d2 $c1 >M $34d3 $b5 >M $34d4 $4b >M $8333 $b1 >M
T{ op PC S A X Y P -> $34d4 $cc $e7 $ef $0a $6d }T T{ $00a4 M $00a5 M $00b5 M $34d2 M $34d3 M $34d4 M $8333 M -> $33 $83 $1a $c1 $b5 $4b $b1 }T
$9d1f >PC $8f >S $98 >A $7a >X $d6 >Y $eb >P $006c $59 >M $00e6 $5a >M $00e7 $04 >M $045a $c8 >M $9d1f $c1 >M $9d20 $6c >M $9d21 $dc >M
T{ op PC S A X Y P -> $9d21 $8f $98 $7a $d6 $e8 }T T{ $006c M $00e6 M $00e7 M $045a M $9d1f M $9d20 M $9d21 M -> $59 $5a $04 $c8 $c1 $6c $dc }T
$baf3 >PC $54 >S $48 >A $33 >X $f2 >Y $a3 >P $0034 $4e >M $0067 $12 >M $0068 $28 >M $2812 $ef >M $baf3 $c1 >M $baf4 $34 >M $baf5 $a1 >M
T{ op PC S A X Y P -> $baf5 $54 $48 $33 $f2 $20 }T T{ $0034 M $0067 M $0068 M $2812 M $baf3 M $baf4 M $baf5 M -> $4e $12 $28 $ef $c1 $34 $a1 }T
$7704 >PC $38 >S $d6 >A $6f >X $9f >Y $a3 >P $0067 $76 >M $0068 $a3 >M $00f8 $e8 >M $7704 $c1 >M $7705 $f8 >M $7706 $df >M $a376 $05 >M
T{ op PC S A X Y P -> $7706 $38 $d6 $6f $9f $a1 }T T{ $0067 M $0068 M $00f8 M $7704 M $7705 M $7706 M $a376 M -> $76 $a3 $e8 $c1 $f8 $df $05 }T
$c511 >PC $4a >S $54 >A $7d >X $ac >Y $e6 >P $004d $e7 >M $004e $54 >M $00d0 $a5 >M $54e7 $fa >M $c511 $c1 >M $c512 $d0 >M $c513 $3e >M
T{ op PC S A X Y P -> $c513 $4a $54 $7d $ac $64 }T T{ $004d M $004e M $00d0 M $54e7 M $c511 M $c512 M $c513 M -> $e7 $54 $a5 $fa $c1 $d0 $3e }T
$9843 >PC $51 >S $bd >A $99 >X $bb >Y $a1 >P $001b $b7 >M $001c $d0 >M $0082 $84 >M $9843 $c1 >M $9844 $82 >M $9845 $5c >M $d0b7 $20 >M
T{ op PC S A X Y P -> $9845 $51 $bd $99 $bb $a1 }T T{ $001b M $001c M $0082 M $9843 M $9844 M $9845 M $d0b7 M -> $b7 $d0 $84 $c1 $82 $5c $20 }T
$8d0b >PC $a5 >S $5e >A $a5 >X $8d >Y $2e >P $0001 $9d >M $00a6 $f2 >M $00a7 $04 >M $04f2 $7d >M $8d0b $c1 >M $8d0c $01 >M $8d0d $3d >M
T{ op PC S A X Y P -> $8d0d $a5 $5e $a5 $8d $ac }T T{ $0001 M $00a6 M $00a7 M $04f2 M $8d0b M $8d0c M $8d0d M -> $9d $f2 $04 $7d $c1 $01 $3d }T
$99cf >PC $98 >S $3a >A $85 >X $25 >Y $a0 >P $0003 $d0 >M $0004 $6a >M $007e $91 >M $6ad0 $07 >M $99cf $c1 >M $99d0 $7e >M $99d1 $35 >M
T{ op PC S A X Y P -> $99d1 $98 $3a $85 $25 $21 }T T{ $0003 M $0004 M $007e M $6ad0 M $99cf M $99d0 M $99d1 M -> $d0 $6a $91 $07 $c1 $7e $35 }T
$c256 >PC $f6 >S $f9 >A $8d >X $f8 >Y $a8 >P $002c $1f >M $00b9 $3e >M $00ba $1f >M $1f3e $1a >M $c256 $c1 >M $c257 $2c >M $c258 $78 >M
T{ op PC S A X Y P -> $c258 $f6 $f9 $8d $f8 $a9 }T T{ $002c M $00b9 M $00ba M $1f3e M $c256 M $c257 M $c258 M -> $1f $3e $1f $1a $c1 $2c $78 }T
$fd6d >PC $13 >S $58 >A $e9 >X $ac >Y $27 >P $0096 $74 >M $0097 $3a >M $00ad $b1 >M $3a74 $a3 >M $fd6d $c1 >M $fd6e $ad >M $fd6f $96 >M
T{ op PC S A X Y P -> $fd6f $13 $58 $e9 $ac $a4 }T T{ $0096 M $0097 M $00ad M $3a74 M $fd6d M $fd6e M $fd6f M -> $74 $3a $b1 $a3 $c1 $ad $96 }T
$7b6f >PC $8f >S $6d >A $1e >X $9b >Y $e7 >P $0028 $9b >M $0046 $ef >M $0047 $33 >M $33ef $88 >M $7b6f $c1 >M $7b70 $28 >M $7b71 $95 >M
T{ op PC S A X Y P -> $7b71 $8f $6d $1e $9b $e4 }T T{ $0028 M $0046 M $0047 M $33ef M $7b6f M $7b70 M $7b71 M -> $9b $ef $33 $88 $c1 $28 $95 }T
$6ac4 >PC $f6 >S $55 >A $cd >X $42 >Y $ea >P $00c2 $ce >M $00c3 $45 >M $00f5 $0b >M $45ce $08 >M $6ac4 $c1 >M $6ac5 $f5 >M $6ac6 $af >M
T{ op PC S A X Y P -> $6ac6 $f6 $55 $cd $42 $69 }T T{ $00c2 M $00c3 M $00f5 M $45ce M $6ac4 M $6ac5 M $6ac6 M -> $ce $45 $0b $08 $c1 $f5 $af }T
$11fc >PC $50 >S $ef >A $62 >X $bf >Y $27 >P $007a $b6 >M $00dc $22 >M $00dd $c2 >M $11fc $c1 >M $11fd $7a >M $11fe $96 >M $c222 $14 >M
T{ op PC S A X Y P -> $11fe $50 $ef $62 $bf $a5 }T T{ $007a M $00dc M $00dd M $11fc M $11fd M $11fe M $c222 M -> $b6 $22 $c2 $c1 $7a $96 $14 }T
( c2 )
$f860 >PC $e8 >S $ec >A $c1 >X $fa >Y $ad >P $f860 $c2 >M $f861 $f5 >M $f862 $ba >M
T{ op PC S A X Y P -> $f862 $e8 $ec $c1 $fa $ad }T T{ $f860 M $f861 M $f862 M -> $c2 $f5 $ba }T
$1e94 >PC $8c >S $97 >A $38 >X $fb >Y $22 >P $1e94 $c2 >M $1e95 $f1 >M $1e96 $ad >M
T{ op PC S A X Y P -> $1e96 $8c $97 $38 $fb $22 }T T{ $1e94 M $1e95 M $1e96 M -> $c2 $f1 $ad }T
$16f7 >PC $7f >S $80 >A $97 >X $d1 >Y $a5 >P $16f7 $c2 >M $16f8 $ff >M $16f9 $50 >M
T{ op PC S A X Y P -> $16f9 $7f $80 $97 $d1 $a5 }T T{ $16f7 M $16f8 M $16f9 M -> $c2 $ff $50 }T
$2473 >PC $6f >S $50 >A $cb >X $5f >Y $ee >P $2473 $c2 >M $2474 $81 >M $2475 $a9 >M
T{ op PC S A X Y P -> $2475 $6f $50 $cb $5f $ee }T T{ $2473 M $2474 M $2475 M -> $c2 $81 $a9 }T
$d6b9 >PC $43 >S $17 >A $72 >X $f5 >Y $ac >P $d6b9 $c2 >M $d6ba $53 >M $d6bb $e8 >M
T{ op PC S A X Y P -> $d6bb $43 $17 $72 $f5 $ac }T T{ $d6b9 M $d6ba M $d6bb M -> $c2 $53 $e8 }T
$d0f0 >PC $f2 >S $3e >A $55 >X $8d >Y $6a >P $d0f0 $c2 >M $d0f1 $60 >M $d0f2 $c5 >M
T{ op PC S A X Y P -> $d0f2 $f2 $3e $55 $8d $6a }T T{ $d0f0 M $d0f1 M $d0f2 M -> $c2 $60 $c5 }T
$97dd >PC $f6 >S $1d >A $74 >X $29 >Y $a2 >P $97dd $c2 >M $97de $cb >M $97df $dc >M
T{ op PC S A X Y P -> $97df $f6 $1d $74 $29 $a2 }T T{ $97dd M $97de M $97df M -> $c2 $cb $dc }T
$c102 >PC $03 >S $e4 >A $09 >X $bb >Y $68 >P $c102 $c2 >M $c103 $85 >M $c104 $04 >M
T{ op PC S A X Y P -> $c104 $03 $e4 $09 $bb $68 }T T{ $c102 M $c103 M $c104 M -> $c2 $85 $04 }T
$044a >PC $e1 >S $13 >A $f7 >X $21 >Y $26 >P $044a $c2 >M $044b $80 >M $044c $da >M
T{ op PC S A X Y P -> $044c $e1 $13 $f7 $21 $26 }T T{ $044a M $044b M $044c M -> $c2 $80 $da }T
$0dc2 >PC $dc >S $35 >A $d7 >X $04 >Y $eb >P $0dc2 $c2 >M $0dc3 $48 >M $0dc4 $45 >M
T{ op PC S A X Y P -> $0dc4 $dc $35 $d7 $04 $eb }T T{ $0dc2 M $0dc3 M $0dc4 M -> $c2 $48 $45 }T
$aef1 >PC $a1 >S $32 >A $00 >X $af >Y $28 >P $aef1 $c2 >M $aef2 $a2 >M $aef3 $53 >M
T{ op PC S A X Y P -> $aef3 $a1 $32 $00 $af $28 }T T{ $aef1 M $aef2 M $aef3 M -> $c2 $a2 $53 }T
$8a8a >PC $a8 >S $c4 >A $9d >X $51 >Y $a6 >P $8a8a $c2 >M $8a8b $23 >M $8a8c $a6 >M
T{ op PC S A X Y P -> $8a8c $a8 $c4 $9d $51 $a6 }T T{ $8a8a M $8a8b M $8a8c M -> $c2 $23 $a6 }T
$9697 >PC $ca >S $87 >A $f0 >X $ac >Y $2d >P $9697 $c2 >M $9698 $84 >M $9699 $a6 >M
T{ op PC S A X Y P -> $9699 $ca $87 $f0 $ac $2d }T T{ $9697 M $9698 M $9699 M -> $c2 $84 $a6 }T
$6e50 >PC $24 >S $72 >A $1c >X $93 >Y $21 >P $6e50 $c2 >M $6e51 $d5 >M $6e52 $28 >M
T{ op PC S A X Y P -> $6e52 $24 $72 $1c $93 $21 }T T{ $6e50 M $6e51 M $6e52 M -> $c2 $d5 $28 }T
$05ee >PC $cb >S $ec >A $26 >X $4d >Y $eb >P $05ee $c2 >M $05ef $de >M $05f0 $7f >M
T{ op PC S A X Y P -> $05f0 $cb $ec $26 $4d $eb }T T{ $05ee M $05ef M $05f0 M -> $c2 $de $7f }T
$0f75 >PC $20 >S $2a >A $e0 >X $14 >Y $e5 >P $0f75 $c2 >M $0f76 $49 >M $0f77 $a2 >M
T{ op PC S A X Y P -> $0f77 $20 $2a $e0 $14 $e5 }T T{ $0f75 M $0f76 M $0f77 M -> $c2 $49 $a2 }T
( c3 )
$13b2 >PC $b0 >S $65 >A $7a >X $dd >Y $e3 >P $13b2 $c3 >M $13b3 $b3 >M $13b4 $d5 >M
T{ op PC S A X Y P -> $13b3 $b0 $65 $7a $dd $e3 }T T{ $13b2 M $13b3 M $13b4 M -> $c3 $b3 $d5 }T
$2fb6 >PC $0c >S $b2 >A $f7 >X $b8 >Y $62 >P $2fb6 $c3 >M $2fb7 $ce >M $2fb8 $d9 >M
T{ op PC S A X Y P -> $2fb7 $0c $b2 $f7 $b8 $62 }T T{ $2fb6 M $2fb7 M $2fb8 M -> $c3 $ce $d9 }T
$6d86 >PC $2e >S $56 >A $bd >X $d3 >Y $2d >P $6d86 $c3 >M $6d87 $af >M $6d88 $64 >M
T{ op PC S A X Y P -> $6d87 $2e $56 $bd $d3 $2d }T T{ $6d86 M $6d87 M $6d88 M -> $c3 $af $64 }T
$cf15 >PC $38 >S $34 >A $39 >X $ac >Y $e9 >P $cf15 $c3 >M $cf16 $2a >M $cf17 $05 >M
T{ op PC S A X Y P -> $cf16 $38 $34 $39 $ac $e9 }T T{ $cf15 M $cf16 M $cf17 M -> $c3 $2a $05 }T
$5249 >PC $91 >S $66 >A $e3 >X $37 >Y $65 >P $5249 $c3 >M $524a $b1 >M $524b $c0 >M
T{ op PC S A X Y P -> $524a $91 $66 $e3 $37 $65 }T T{ $5249 M $524a M $524b M -> $c3 $b1 $c0 }T
$eea3 >PC $41 >S $ca >A $6b >X $e4 >Y $aa >P $eea3 $c3 >M $eea4 $f6 >M $eea5 $ec >M
T{ op PC S A X Y P -> $eea4 $41 $ca $6b $e4 $aa }T T{ $eea3 M $eea4 M $eea5 M -> $c3 $f6 $ec }T
$c2db >PC $34 >S $ac >A $0b >X $19 >Y $ae >P $c2db $c3 >M $c2dc $23 >M $c2dd $bb >M
T{ op PC S A X Y P -> $c2dc $34 $ac $0b $19 $ae }T T{ $c2db M $c2dc M $c2dd M -> $c3 $23 $bb }T
$5468 >PC $54 >S $c4 >A $ea >X $90 >Y $a9 >P $5468 $c3 >M $5469 $a7 >M $546a $03 >M
T{ op PC S A X Y P -> $5469 $54 $c4 $ea $90 $a9 }T T{ $5468 M $5469 M $546a M -> $c3 $a7 $03 }T
$0d7e >PC $94 >S $9d >A $6c >X $50 >Y $6f >P $0d7e $c3 >M $0d7f $a3 >M $0d80 $f2 >M
T{ op PC S A X Y P -> $0d7f $94 $9d $6c $50 $6f }T T{ $0d7e M $0d7f M $0d80 M -> $c3 $a3 $f2 }T
$70ee >PC $97 >S $f9 >A $01 >X $f4 >Y $6f >P $70ee $c3 >M $70ef $c9 >M $70f0 $c7 >M
T{ op PC S A X Y P -> $70ef $97 $f9 $01 $f4 $6f }T T{ $70ee M $70ef M $70f0 M -> $c3 $c9 $c7 }T
$9a84 >PC $d7 >S $4d >A $48 >X $f5 >Y $2e >P $9a84 $c3 >M $9a85 $85 >M $9a86 $69 >M
T{ op PC S A X Y P -> $9a85 $d7 $4d $48 $f5 $2e }T T{ $9a84 M $9a85 M $9a86 M -> $c3 $85 $69 }T
$3c5e >PC $d2 >S $2a >A $e2 >X $15 >Y $25 >P $3c5e $c3 >M $3c5f $e8 >M $3c60 $98 >M
T{ op PC S A X Y P -> $3c5f $d2 $2a $e2 $15 $25 }T T{ $3c5e M $3c5f M $3c60 M -> $c3 $e8 $98 }T
$bf79 >PC $eb >S $f5 >A $c2 >X $99 >Y $aa >P $bf79 $c3 >M $bf7a $03 >M $bf7b $ee >M
T{ op PC S A X Y P -> $bf7a $eb $f5 $c2 $99 $aa }T T{ $bf79 M $bf7a M $bf7b M -> $c3 $03 $ee }T
$7efa >PC $f0 >S $2a >A $5a >X $37 >Y $a6 >P $7efa $c3 >M $7efb $c3 >M $7efc $16 >M
T{ op PC S A X Y P -> $7efb $f0 $2a $5a $37 $a6 }T T{ $7efa M $7efb M $7efc M -> $c3 $c3 $16 }T
$e53b >PC $a3 >S $b0 >A $37 >X $19 >Y $a3 >P $e53b $c3 >M $e53c $bd >M $e53d $b1 >M
T{ op PC S A X Y P -> $e53c $a3 $b0 $37 $19 $a3 }T T{ $e53b M $e53c M $e53d M -> $c3 $bd $b1 }T
$fea0 >PC $04 >S $83 >A $3e >X $51 >Y $a7 >P $fea0 $c3 >M $fea1 $dd >M $fea2 $0c >M
T{ op PC S A X Y P -> $fea1 $04 $83 $3e $51 $a7 }T T{ $fea0 M $fea1 M $fea2 M -> $c3 $dd $0c }T
( c4 )
$4949 >PC $ac >S $bb >A $b3 >X $4d >Y $2e >P $00d8 $75 >M $4949 $c4 >M $494a $d8 >M $494b $70 >M
T{ op PC S A X Y P -> $494b $ac $bb $b3 $4d $ac }T T{ $00d8 M $4949 M $494a M $494b M -> $75 $c4 $d8 $70 }T
$b91a >PC $94 >S $6d >A $1c >X $b6 >Y $e1 >P $00b8 $0b >M $b91a $c4 >M $b91b $b8 >M $b91c $4a >M
T{ op PC S A X Y P -> $b91c $94 $6d $1c $b6 $e1 }T T{ $00b8 M $b91a M $b91b M $b91c M -> $0b $c4 $b8 $4a }T
$ae67 >PC $7b >S $97 >A $c6 >X $f1 >Y $22 >P $00ae $20 >M $ae67 $c4 >M $ae68 $ae >M $ae69 $fd >M
T{ op PC S A X Y P -> $ae69 $7b $97 $c6 $f1 $a1 }T T{ $00ae M $ae67 M $ae68 M $ae69 M -> $20 $c4 $ae $fd }T
$8000 >PC $f6 >S $4d >A $e5 >X $de >Y $ea >P $009f $6f >M $8000 $c4 >M $8001 $9f >M $8002 $ac >M
T{ op PC S A X Y P -> $8002 $f6 $4d $e5 $de $69 }T T{ $009f M $8000 M $8001 M $8002 M -> $6f $c4 $9f $ac }T
$b5ed >PC $30 >S $bd >A $b5 >X $b1 >Y $6d >P $00ae $3c >M $b5ed $c4 >M $b5ee $ae >M $b5ef $fc >M
T{ op PC S A X Y P -> $b5ef $30 $bd $b5 $b1 $6d }T T{ $00ae M $b5ed M $b5ee M $b5ef M -> $3c $c4 $ae $fc }T
$b2fa >PC $40 >S $fd >A $6d >X $99 >Y $a2 >P $0037 $6c >M $b2fa $c4 >M $b2fb $37 >M $b2fc $dd >M
T{ op PC S A X Y P -> $b2fc $40 $fd $6d $99 $21 }T T{ $0037 M $b2fa M $b2fb M $b2fc M -> $6c $c4 $37 $dd }T
$f1f1 >PC $8a >S $32 >A $b3 >X $d3 >Y $e4 >P $0049 $4f >M $f1f1 $c4 >M $f1f2 $49 >M $f1f3 $5e >M
T{ op PC S A X Y P -> $f1f3 $8a $32 $b3 $d3 $e5 }T T{ $0049 M $f1f1 M $f1f2 M $f1f3 M -> $4f $c4 $49 $5e }T
$1aa8 >PC $5e >S $61 >A $9d >X $9f >Y $2e >P $00ed $4f >M $1aa8 $c4 >M $1aa9 $ed >M $1aaa $74 >M
T{ op PC S A X Y P -> $1aaa $5e $61 $9d $9f $2d }T T{ $00ed M $1aa8 M $1aa9 M $1aaa M -> $4f $c4 $ed $74 }T
$da75 >PC $d2 >S $7e >A $1c >X $f7 >Y $28 >P $00b1 $d7 >M $da75 $c4 >M $da76 $b1 >M $da77 $17 >M
T{ op PC S A X Y P -> $da77 $d2 $7e $1c $f7 $29 }T T{ $00b1 M $da75 M $da76 M $da77 M -> $d7 $c4 $b1 $17 }T
$6c72 >PC $34 >S $f5 >A $2b >X $8d >Y $eb >P $00f5 $db >M $6c72 $c4 >M $6c73 $f5 >M $6c74 $69 >M
T{ op PC S A X Y P -> $6c74 $34 $f5 $2b $8d $e8 }T T{ $00f5 M $6c72 M $6c73 M $6c74 M -> $db $c4 $f5 $69 }T
$da36 >PC $4c >S $2e >A $cd >X $2e >Y $af >P $00df $05 >M $da36 $c4 >M $da37 $df >M $da38 $06 >M
T{ op PC S A X Y P -> $da38 $4c $2e $cd $2e $2d }T T{ $00df M $da36 M $da37 M $da38 M -> $05 $c4 $df $06 }T
$0fb7 >PC $db >S $a2 >A $97 >X $cd >Y $29 >P $004e $bf >M $0fb7 $c4 >M $0fb8 $4e >M $0fb9 $e2 >M
T{ op PC S A X Y P -> $0fb9 $db $a2 $97 $cd $29 }T T{ $004e M $0fb7 M $0fb8 M $0fb9 M -> $bf $c4 $4e $e2 }T
$f52c >PC $6f >S $3b >A $e8 >X $31 >Y $a1 >P $00c1 $f3 >M $f52c $c4 >M $f52d $c1 >M $f52e $ee >M
T{ op PC S A X Y P -> $f52e $6f $3b $e8 $31 $20 }T T{ $00c1 M $f52c M $f52d M $f52e M -> $f3 $c4 $c1 $ee }T
$82e3 >PC $2d >S $9a >A $05 >X $9e >Y $6d >P $0087 $3a >M $82e3 $c4 >M $82e4 $87 >M $82e5 $de >M
T{ op PC S A X Y P -> $82e5 $2d $9a $05 $9e $6d }T T{ $0087 M $82e3 M $82e4 M $82e5 M -> $3a $c4 $87 $de }T
$3425 >PC $bc >S $10 >A $84 >X $c7 >Y $25 >P $0001 $cd >M $3425 $c4 >M $3426 $01 >M $3427 $20 >M
T{ op PC S A X Y P -> $3427 $bc $10 $84 $c7 $a4 }T T{ $0001 M $3425 M $3426 M $3427 M -> $cd $c4 $01 $20 }T
$5e13 >PC $e8 >S $45 >A $1b >X $63 >Y $a9 >P $004c $be >M $5e13 $c4 >M $5e14 $4c >M $5e15 $be >M
T{ op PC S A X Y P -> $5e15 $e8 $45 $1b $63 $a8 }T T{ $004c M $5e13 M $5e14 M $5e15 M -> $be $c4 $4c $be }T
( c5 )
$5729 >PC $16 >S $e8 >A $2c >X $d6 >Y $62 >P $00c5 $01 >M $5729 $c5 >M $572a $c5 >M $572b $d1 >M
T{ op PC S A X Y P -> $572b $16 $e8 $2c $d6 $e1 }T T{ $00c5 M $5729 M $572a M $572b M -> $01 $c5 $c5 $d1 }T
$336b >PC $f2 >S $40 >A $f0 >X $83 >Y $65 >P $007b $1c >M $336b $c5 >M $336c $7b >M $336d $be >M
T{ op PC S A X Y P -> $336d $f2 $40 $f0 $83 $65 }T T{ $007b M $336b M $336c M $336d M -> $1c $c5 $7b $be }T
$1873 >PC $7a >S $0b >A $98 >X $12 >Y $62 >P $00e3 $ad >M $1873 $c5 >M $1874 $e3 >M $1875 $a8 >M
T{ op PC S A X Y P -> $1875 $7a $0b $98 $12 $60 }T T{ $00e3 M $1873 M $1874 M $1875 M -> $ad $c5 $e3 $a8 }T
$1276 >PC $01 >S $e7 >A $bb >X $73 >Y $e3 >P $003a $31 >M $1276 $c5 >M $1277 $3a >M $1278 $c2 >M
T{ op PC S A X Y P -> $1278 $01 $e7 $bb $73 $e1 }T T{ $003a M $1276 M $1277 M $1278 M -> $31 $c5 $3a $c2 }T
$b9f9 >PC $c9 >S $2d >A $21 >X $46 >Y $28 >P $0014 $e8 >M $b9f9 $c5 >M $b9fa $14 >M $b9fb $c5 >M
T{ op PC S A X Y P -> $b9fb $c9 $2d $21 $46 $28 }T T{ $0014 M $b9f9 M $b9fa M $b9fb M -> $e8 $c5 $14 $c5 }T
$c77d >PC $be >S $13 >A $32 >X $52 >Y $24 >P $0020 $b3 >M $c77d $c5 >M $c77e $20 >M $c77f $9e >M
T{ op PC S A X Y P -> $c77f $be $13 $32 $52 $24 }T T{ $0020 M $c77d M $c77e M $c77f M -> $b3 $c5 $20 $9e }T
$f683 >PC $8d >S $8e >A $b5 >X $2b >Y $6d >P $004a $7a >M $f683 $c5 >M $f684 $4a >M $f685 $71 >M
T{ op PC S A X Y P -> $f685 $8d $8e $b5 $2b $6d }T T{ $004a M $f683 M $f684 M $f685 M -> $7a $c5 $4a $71 }T
$df0a >PC $9a >S $85 >A $b2 >X $5b >Y $af >P $00a3 $69 >M $df0a $c5 >M $df0b $a3 >M $df0c $b8 >M
T{ op PC S A X Y P -> $df0c $9a $85 $b2 $5b $2d }T T{ $00a3 M $df0a M $df0b M $df0c M -> $69 $c5 $a3 $b8 }T
$f63a >PC $f8 >S $82 >A $38 >X $58 >Y $26 >P $0036 $67 >M $f63a $c5 >M $f63b $36 >M $f63c $1e >M
T{ op PC S A X Y P -> $f63c $f8 $82 $38 $58 $25 }T T{ $0036 M $f63a M $f63b M $f63c M -> $67 $c5 $36 $1e }T
$b01d >PC $1c >S $da >A $78 >X $83 >Y $a6 >P $008e $cf >M $b01d $c5 >M $b01e $8e >M $b01f $dc >M
T{ op PC S A X Y P -> $b01f $1c $da $78 $83 $25 }T T{ $008e M $b01d M $b01e M $b01f M -> $cf $c5 $8e $dc }T
$9538 >PC $83 >S $f6 >A $4c >X $86 >Y $20 >P $00ab $d3 >M $9538 $c5 >M $9539 $ab >M $953a $fe >M
T{ op PC S A X Y P -> $953a $83 $f6 $4c $86 $21 }T T{ $00ab M $9538 M $9539 M $953a M -> $d3 $c5 $ab $fe }T
$42c0 >PC $91 >S $54 >A $96 >X $88 >Y $6c >P $0081 $35 >M $42c0 $c5 >M $42c1 $81 >M $42c2 $a0 >M
T{ op PC S A X Y P -> $42c2 $91 $54 $96 $88 $6d }T T{ $0081 M $42c0 M $42c1 M $42c2 M -> $35 $c5 $81 $a0 }T
$7964 >PC $89 >S $39 >A $7b >X $ad >Y $6b >P $000c $b7 >M $7964 $c5 >M $7965 $0c >M $7966 $05 >M
T{ op PC S A X Y P -> $7966 $89 $39 $7b $ad $e8 }T T{ $000c M $7964 M $7965 M $7966 M -> $b7 $c5 $0c $05 }T
$67f5 >PC $87 >S $58 >A $ae >X $53 >Y $63 >P $00d4 $09 >M $67f5 $c5 >M $67f6 $d4 >M $67f7 $b5 >M
T{ op PC S A X Y P -> $67f7 $87 $58 $ae $53 $61 }T T{ $00d4 M $67f5 M $67f6 M $67f7 M -> $09 $c5 $d4 $b5 }T
$b7fe >PC $d5 >S $b5 >A $02 >X $82 >Y $a2 >P $00e7 $78 >M $b7fe $c5 >M $b7ff $e7 >M $b800 $76 >M
T{ op PC S A X Y P -> $b800 $d5 $b5 $02 $82 $21 }T T{ $00e7 M $b7fe M $b7ff M $b800 M -> $78 $c5 $e7 $76 }T
$8498 >PC $ae >S $fd >A $29 >X $bc >Y $2a >P $004c $7b >M $8498 $c5 >M $8499 $4c >M $849a $1e >M
T{ op PC S A X Y P -> $849a $ae $fd $29 $bc $a9 }T T{ $004c M $8498 M $8499 M $849a M -> $7b $c5 $4c $1e }T
( c6 )
$858f >PC $91 >S $9e >A $82 >X $c7 >Y $e3 >P $0038 $23 >M $858f $c6 >M $8590 $38 >M $8591 $15 >M
T{ op PC S A X Y P -> $8591 $91 $9e $82 $c7 $61 }T T{ $0038 M $858f M $8590 M $8591 M -> $22 $c6 $38 $15 }T
$574c >PC $8c >S $b4 >A $6f >X $5e >Y $6f >P $007b $ef >M $574c $c6 >M $574d $7b >M $574e $de >M
T{ op PC S A X Y P -> $574e $8c $b4 $6f $5e $ed }T T{ $007b M $574c M $574d M $574e M -> $ee $c6 $7b $de }T
$8b32 >PC $64 >S $8c >A $6c >X $e8 >Y $60 >P $0094 $60 >M $8b32 $c6 >M $8b33 $94 >M $8b34 $e5 >M
T{ op PC S A X Y P -> $8b34 $64 $8c $6c $e8 $60 }T T{ $0094 M $8b32 M $8b33 M $8b34 M -> $5f $c6 $94 $e5 }T
$65eb >PC $24 >S $ba >A $84 >X $40 >Y $ea >P $00e9 $1c >M $65eb $c6 >M $65ec $e9 >M $65ed $f5 >M
T{ op PC S A X Y P -> $65ed $24 $ba $84 $40 $68 }T T{ $00e9 M $65eb M $65ec M $65ed M -> $1b $c6 $e9 $f5 }T
$6166 >PC $12 >S $36 >A $71 >X $9e >Y $a2 >P $0036 $63 >M $6166 $c6 >M $6167 $36 >M $6168 $56 >M
T{ op PC S A X Y P -> $6168 $12 $36 $71 $9e $20 }T T{ $0036 M $6166 M $6167 M $6168 M -> $62 $c6 $36 $56 }T
$5064 >PC $11 >S $41 >A $bb >X $30 >Y $a9 >P $00fc $27 >M $5064 $c6 >M $5065 $fc >M $5066 $d1 >M
T{ op PC S A X Y P -> $5066 $11 $41 $bb $30 $29 }T T{ $00fc M $5064 M $5065 M $5066 M -> $26 $c6 $fc $d1 }T
$9d89 >PC $92 >S $32 >A $ee >X $8b >Y $6b >P $003b $ef >M $9d89 $c6 >M $9d8a $3b >M $9d8b $49 >M
T{ op PC S A X Y P -> $9d8b $92 $32 $ee $8b $e9 }T T{ $003b M $9d89 M $9d8a M $9d8b M -> $ee $c6 $3b $49 }T
$6b85 >PC $cb >S $f5 >A $a6 >X $77 >Y $25 >P $003e $54 >M $6b85 $c6 >M $6b86 $3e >M $6b87 $b2 >M
T{ op PC S A X Y P -> $6b87 $cb $f5 $a6 $77 $25 }T T{ $003e M $6b85 M $6b86 M $6b87 M -> $53 $c6 $3e $b2 }T
$5c40 >PC $7a >S $be >A $d6 >X $b7 >Y $a7 >P $00ff $ef >M $5c40 $c6 >M $5c41 $ff >M $5c42 $da >M
T{ op PC S A X Y P -> $5c42 $7a $be $d6 $b7 $a5 }T T{ $00ff M $5c40 M $5c41 M $5c42 M -> $ee $c6 $ff $da }T
$6a3d >PC $9e >S $52 >A $c9 >X $c8 >Y $2c >P $00f0 $49 >M $6a3d $c6 >M $6a3e $f0 >M $6a3f $c6 >M
T{ op PC S A X Y P -> $6a3f $9e $52 $c9 $c8 $2c }T T{ $00f0 M $6a3d M $6a3e M $6a3f M -> $48 $c6 $f0 $c6 }T
$fc8c >PC $cd >S $75 >A $74 >X $d3 >Y $ab >P $003f $ce >M $fc8c $c6 >M $fc8d $3f >M $fc8e $2c >M
T{ op PC S A X Y P -> $fc8e $cd $75 $74 $d3 $a9 }T T{ $003f M $fc8c M $fc8d M $fc8e M -> $cd $c6 $3f $2c }T
$22c1 >PC $3b >S $3b >A $34 >X $90 >Y $ab >P $0096 $44 >M $22c1 $c6 >M $22c2 $96 >M $22c3 $d5 >M
T{ op PC S A X Y P -> $22c3 $3b $3b $34 $90 $29 }T T{ $0096 M $22c1 M $22c2 M $22c3 M -> $43 $c6 $96 $d5 }T
$1583 >PC $ac >S $e7 >A $dc >X $f1 >Y $69 >P $000f $e5 >M $1583 $c6 >M $1584 $0f >M $1585 $89 >M
T{ op PC S A X Y P -> $1585 $ac $e7 $dc $f1 $e9 }T T{ $000f M $1583 M $1584 M $1585 M -> $e4 $c6 $0f $89 }T
$051f >PC $32 >S $1d >A $93 >X $06 >Y $a6 >P $0011 $eb >M $051f $c6 >M $0520 $11 >M $0521 $7f >M
T{ op PC S A X Y P -> $0521 $32 $1d $93 $06 $a4 }T T{ $0011 M $051f M $0520 M $0521 M -> $ea $c6 $11 $7f }T
$d73a >PC $f1 >S $51 >A $53 >X $e8 >Y $ea >P $0025 $17 >M $d73a $c6 >M $d73b $25 >M $d73c $83 >M
T{ op PC S A X Y P -> $d73c $f1 $51 $53 $e8 $68 }T T{ $0025 M $d73a M $d73b M $d73c M -> $16 $c6 $25 $83 }T
$244b >PC $1c >S $4c >A $01 >X $8b >Y $ad >P $0060 $89 >M $244b $c6 >M $244c $60 >M $244d $b3 >M
T{ op PC S A X Y P -> $244d $1c $4c $01 $8b $ad }T T{ $0060 M $244b M $244c M $244d M -> $88 $c6 $60 $b3 }T
( c7 )
$7d2c >PC $61 >S $c6 >A $f3 >X $04 >Y $e3 >P $0053 $21 >M $7d2c $c7 >M $7d2d $53 >M $7d2e $ac >M
T{ op PC S A X Y P -> $7d2e $61 $c6 $f3 $04 $e3 }T T{ $0053 M $7d2c M $7d2d M $7d2e M -> $31 $c7 $53 $ac }T
$0a68 >PC $74 >S $ff >A $ff >X $3e >Y $aa >P $00c5 $3d >M $0a68 $c7 >M $0a69 $c5 >M $0a6a $ae >M
T{ op PC S A X Y P -> $0a6a $74 $ff $ff $3e $aa }T T{ $00c5 M $0a68 M $0a69 M $0a6a M -> $3d $c7 $c5 $ae }T
$e6b1 >PC $a3 >S $9f >A $fa >X $2f >Y $e0 >P $0010 $7a >M $e6b1 $c7 >M $e6b2 $10 >M $e6b3 $09 >M
T{ op PC S A X Y P -> $e6b3 $a3 $9f $fa $2f $e0 }T T{ $0010 M $e6b1 M $e6b2 M $e6b3 M -> $7a $c7 $10 $09 }T
$2fe8 >PC $b2 >S $2e >A $bc >X $9f >Y $6e >P $00cd $64 >M $2fe8 $c7 >M $2fe9 $cd >M $2fea $29 >M
T{ op PC S A X Y P -> $2fea $b2 $2e $bc $9f $6e }T T{ $00cd M $2fe8 M $2fe9 M $2fea M -> $74 $c7 $cd $29 }T
$09b1 >PC $72 >S $58 >A $66 >X $d3 >Y $2a >P $005e $85 >M $09b1 $c7 >M $09b2 $5e >M $09b3 $46 >M
T{ op PC S A X Y P -> $09b3 $72 $58 $66 $d3 $2a }T T{ $005e M $09b1 M $09b2 M $09b3 M -> $95 $c7 $5e $46 }T
$f26d >PC $4c >S $49 >A $7a >X $36 >Y $e0 >P $003d $03 >M $f26d $c7 >M $f26e $3d >M $f26f $61 >M
T{ op PC S A X Y P -> $f26f $4c $49 $7a $36 $e0 }T T{ $003d M $f26d M $f26e M $f26f M -> $13 $c7 $3d $61 }T
$cb0d >PC $56 >S $b0 >A $1a >X $9a >Y $ad >P $00f2 $2e >M $cb0d $c7 >M $cb0e $f2 >M $cb0f $7b >M
T{ op PC S A X Y P -> $cb0f $56 $b0 $1a $9a $ad }T T{ $00f2 M $cb0d M $cb0e M $cb0f M -> $3e $c7 $f2 $7b }T
$83f8 >PC $65 >S $8f >A $cc >X $80 >Y $29 >P $002b $25 >M $83f8 $c7 >M $83f9 $2b >M $83fa $73 >M
T{ op PC S A X Y P -> $83fa $65 $8f $cc $80 $29 }T T{ $002b M $83f8 M $83f9 M $83fa M -> $35 $c7 $2b $73 }T
$9acf >PC $f9 >S $72 >A $d9 >X $aa >Y $6b >P $0037 $17 >M $9acf $c7 >M $9ad0 $37 >M $9ad1 $ad >M
T{ op PC S A X Y P -> $9ad1 $f9 $72 $d9 $aa $6b }T T{ $0037 M $9acf M $9ad0 M $9ad1 M -> $17 $c7 $37 $ad }T
$7bea >PC $12 >S $b7 >A $b8 >X $78 >Y $ae >P $00e5 $9e >M $7bea $c7 >M $7beb $e5 >M $7bec $28 >M
T{ op PC S A X Y P -> $7bec $12 $b7 $b8 $78 $ae }T T{ $00e5 M $7bea M $7beb M $7bec M -> $9e $c7 $e5 $28 }T
$0275 >PC $6a >S $07 >A $f4 >X $2b >Y $20 >P $000a $ed >M $0275 $c7 >M $0276 $0a >M $0277 $36 >M
T{ op PC S A X Y P -> $0277 $6a $07 $f4 $2b $20 }T T{ $000a M $0275 M $0276 M $0277 M -> $fd $c7 $0a $36 }T
$ea1b >PC $e3 >S $56 >A $dc >X $16 >Y $e0 >P $00d9 $de >M $ea1b $c7 >M $ea1c $d9 >M $ea1d $3c >M
T{ op PC S A X Y P -> $ea1d $e3 $56 $dc $16 $e0 }T T{ $00d9 M $ea1b M $ea1c M $ea1d M -> $de $c7 $d9 $3c }T
$b3f1 >PC $62 >S $aa >A $bc >X $6d >Y $25 >P $009c $54 >M $b3f1 $c7 >M $b3f2 $9c >M $b3f3 $c2 >M
T{ op PC S A X Y P -> $b3f3 $62 $aa $bc $6d $25 }T T{ $009c M $b3f1 M $b3f2 M $b3f3 M -> $54 $c7 $9c $c2 }T
$3a9b >PC $cd >S $29 >A $92 >X $46 >Y $6c >P $004b $c0 >M $3a9b $c7 >M $3a9c $4b >M $3a9d $48 >M
T{ op PC S A X Y P -> $3a9d $cd $29 $92 $46 $6c }T T{ $004b M $3a9b M $3a9c M $3a9d M -> $d0 $c7 $4b $48 }T
$9009 >PC $e3 >S $9d >A $4d >X $bd >Y $6e >P $0015 $6b >M $9009 $c7 >M $900a $15 >M $900b $d1 >M
T{ op PC S A X Y P -> $900b $e3 $9d $4d $bd $6e }T T{ $0015 M $9009 M $900a M $900b M -> $7b $c7 $15 $d1 }T
$898c >PC $3e >S $a1 >A $04 >X $84 >Y $a6 >P $003e $12 >M $898c $c7 >M $898d $3e >M $898e $3e >M
T{ op PC S A X Y P -> $898e $3e $a1 $04 $84 $a6 }T T{ $003e M $898c M $898d M $898e M -> $12 $c7 $3e $3e }T
( c8 )
$a3d6 >PC $80 >S $17 >A $98 >X $15 >Y $22 >P $a3d6 $c8 >M $a3d7 $9f >M $a3d8 $72 >M
T{ op PC S A X Y P -> $a3d7 $80 $17 $98 $16 $20 }T T{ $a3d6 M $a3d7 M $a3d8 M -> $c8 $9f $72 }T
$037b >PC $c1 >S $4b >A $f0 >X $16 >Y $2a >P $037b $c8 >M $037c $bb >M $037d $b8 >M
T{ op PC S A X Y P -> $037c $c1 $4b $f0 $17 $28 }T T{ $037b M $037c M $037d M -> $c8 $bb $b8 }T
$0593 >PC $81 >S $13 >A $c5 >X $85 >Y $69 >P $0593 $c8 >M $0594 $83 >M $0595 $b6 >M
T{ op PC S A X Y P -> $0594 $81 $13 $c5 $86 $e9 }T T{ $0593 M $0594 M $0595 M -> $c8 $83 $b6 }T
$26a6 >PC $28 >S $01 >A $62 >X $e6 >Y $22 >P $26a6 $c8 >M $26a7 $a3 >M $26a8 $6c >M
T{ op PC S A X Y P -> $26a7 $28 $01 $62 $e7 $a0 }T T{ $26a6 M $26a7 M $26a8 M -> $c8 $a3 $6c }T
$cdbb >PC $b6 >S $77 >A $59 >X $7d >Y $e6 >P $cdbb $c8 >M $cdbc $0c >M $cdbd $02 >M
T{ op PC S A X Y P -> $cdbc $b6 $77 $59 $7e $64 }T T{ $cdbb M $cdbc M $cdbd M -> $c8 $0c $02 }T
$de8b >PC $4e >S $29 >A $68 >X $8d >Y $6b >P $de8b $c8 >M $de8c $d4 >M $de8d $42 >M
T{ op PC S A X Y P -> $de8c $4e $29 $68 $8e $e9 }T T{ $de8b M $de8c M $de8d M -> $c8 $d4 $42 }T
$470a >PC $ad >S $e0 >A $12 >X $10 >Y $ed >P $470a $c8 >M $470b $0e >M $470c $e6 >M
T{ op PC S A X Y P -> $470b $ad $e0 $12 $11 $6d }T T{ $470a M $470b M $470c M -> $c8 $0e $e6 }T
$dd34 >PC $6f >S $2a >A $bf >X $69 >Y $a8 >P $dd34 $c8 >M $dd35 $c5 >M $dd36 $d5 >M
T{ op PC S A X Y P -> $dd35 $6f $2a $bf $6a $28 }T T{ $dd34 M $dd35 M $dd36 M -> $c8 $c5 $d5 }T
$bce3 >PC $14 >S $6a >A $e1 >X $6c >Y $e4 >P $bce3 $c8 >M $bce4 $09 >M $bce5 $7a >M
T{ op PC S A X Y P -> $bce4 $14 $6a $e1 $6d $64 }T T{ $bce3 M $bce4 M $bce5 M -> $c8 $09 $7a }T
$182e >PC $d9 >S $92 >A $ad >X $af >Y $a9 >P $182e $c8 >M $182f $65 >M $1830 $cd >M
T{ op PC S A X Y P -> $182f $d9 $92 $ad $b0 $a9 }T T{ $182e M $182f M $1830 M -> $c8 $65 $cd }T
$5960 >PC $87 >S $88 >A $ce >X $0b >Y $63 >P $5960 $c8 >M $5961 $e0 >M $5962 $7e >M
T{ op PC S A X Y P -> $5961 $87 $88 $ce $0c $61 }T T{ $5960 M $5961 M $5962 M -> $c8 $e0 $7e }T
$d83a >PC $42 >S $d6 >A $07 >X $cf >Y $a8 >P $d83a $c8 >M $d83b $21 >M $d83c $76 >M
T{ op PC S A X Y P -> $d83b $42 $d6 $07 $d0 $a8 }T T{ $d83a M $d83b M $d83c M -> $c8 $21 $76 }T
$5076 >PC $c7 >S $c3 >A $97 >X $b3 >Y $27 >P $5076 $c8 >M $5077 $9e >M $5078 $2f >M
T{ op PC S A X Y P -> $5077 $c7 $c3 $97 $b4 $a5 }T T{ $5076 M $5077 M $5078 M -> $c8 $9e $2f }T
$23da >PC $f0 >S $55 >A $0c >X $ed >Y $e8 >P $23da $c8 >M $23db $06 >M $23dc $8e >M
T{ op PC S A X Y P -> $23db $f0 $55 $0c $ee $e8 }T T{ $23da M $23db M $23dc M -> $c8 $06 $8e }T
$abfd >PC $45 >S $aa >A $79 >X $bf >Y $2d >P $abfd $c8 >M $abfe $03 >M $abff $dd >M
T{ op PC S A X Y P -> $abfe $45 $aa $79 $c0 $ad }T T{ $abfd M $abfe M $abff M -> $c8 $03 $dd }T
$515c >PC $ae >S $fe >A $7d >X $ee >Y $a9 >P $515c $c8 >M $515d $c1 >M $515e $1c >M
T{ op PC S A X Y P -> $515d $ae $fe $7d $ef $a9 }T T{ $515c M $515d M $515e M -> $c8 $c1 $1c }T
( c9 )
$76f0 >PC $d6 >S $58 >A $78 >X $4f >Y $ab >P $76f0 $c9 >M $76f1 $41 >M $76f2 $21 >M
T{ op PC S A X Y P -> $76f2 $d6 $58 $78 $4f $29 }T T{ $76f0 M $76f1 M $76f2 M -> $c9 $41 $21 }T
$2c96 >PC $14 >S $78 >A $76 >X $0b >Y $a9 >P $2c96 $c9 >M $2c97 $ed >M $2c98 $3e >M
T{ op PC S A X Y P -> $2c98 $14 $78 $76 $0b $a8 }T T{ $2c96 M $2c97 M $2c98 M -> $c9 $ed $3e }T
$842c >PC $21 >S $3d >A $82 >X $90 >Y $a9 >P $842c $c9 >M $842d $43 >M $842e $57 >M
T{ op PC S A X Y P -> $842e $21 $3d $82 $90 $a8 }T T{ $842c M $842d M $842e M -> $c9 $43 $57 }T
$c73e >PC $5f >S $71 >A $3d >X $38 >Y $2b >P $c73e $c9 >M $c73f $dd >M $c740 $82 >M
T{ op PC S A X Y P -> $c740 $5f $71 $3d $38 $a8 }T T{ $c73e M $c73f M $c740 M -> $c9 $dd $82 }T
$2b13 >PC $d1 >S $d5 >A $05 >X $6b >Y $69 >P $2b13 $c9 >M $2b14 $b2 >M $2b15 $c8 >M
T{ op PC S A X Y P -> $2b15 $d1 $d5 $05 $6b $69 }T T{ $2b13 M $2b14 M $2b15 M -> $c9 $b2 $c8 }T
$0ee3 >PC $a6 >S $dc >A $3a >X $e3 >Y $20 >P $0ee3 $c9 >M $0ee4 $3b >M $0ee5 $dc >M
T{ op PC S A X Y P -> $0ee5 $a6 $dc $3a $e3 $a1 }T T{ $0ee3 M $0ee4 M $0ee5 M -> $c9 $3b $dc }T
$85b7 >PC $1e >S $40 >A $df >X $4a >Y $6f >P $85b7 $c9 >M $85b8 $ee >M $85b9 $99 >M
T{ op PC S A X Y P -> $85b9 $1e $40 $df $4a $6c }T T{ $85b7 M $85b8 M $85b9 M -> $c9 $ee $99 }T
$6f96 >PC $a0 >S $85 >A $c0 >X $2f >Y $27 >P $6f96 $c9 >M $6f97 $0b >M $6f98 $f5 >M
T{ op PC S A X Y P -> $6f98 $a0 $85 $c0 $2f $25 }T T{ $6f96 M $6f97 M $6f98 M -> $c9 $0b $f5 }T
$7add >PC $39 >S $9f >A $f9 >X $78 >Y $ee >P $7add $c9 >M $7ade $6b >M $7adf $36 >M
T{ op PC S A X Y P -> $7adf $39 $9f $f9 $78 $6d }T T{ $7add M $7ade M $7adf M -> $c9 $6b $36 }T
$d114 >PC $45 >S $5f >A $2a >X $97 >Y $6f >P $d114 $c9 >M $d115 $64 >M $d116 $95 >M
T{ op PC S A X Y P -> $d116 $45 $5f $2a $97 $ec }T T{ $d114 M $d115 M $d116 M -> $c9 $64 $95 }T
$b930 >PC $fc >S $06 >A $22 >X $4b >Y $e4 >P $b930 $c9 >M $b931 $b3 >M $b932 $73 >M
T{ op PC S A X Y P -> $b932 $fc $06 $22 $4b $64 }T T{ $b930 M $b931 M $b932 M -> $c9 $b3 $73 }T
$2a8f >PC $c5 >S $0d >A $9c >X $6d >Y $27 >P $2a8f $c9 >M $2a90 $c7 >M $2a91 $6c >M
T{ op PC S A X Y P -> $2a91 $c5 $0d $9c $6d $24 }T T{ $2a8f M $2a90 M $2a91 M -> $c9 $c7 $6c }T
$b42a >PC $96 >S $e4 >A $d5 >X $ea >Y $68 >P $b42a $c9 >M $b42b $b0 >M $b42c $39 >M
T{ op PC S A X Y P -> $b42c $96 $e4 $d5 $ea $69 }T T{ $b42a M $b42b M $b42c M -> $c9 $b0 $39 }T
$caa5 >PC $5e >S $8c >A $1c >X $c7 >Y $ef >P $caa5 $c9 >M $caa6 $e4 >M $caa7 $4f >M
T{ op PC S A X Y P -> $caa7 $5e $8c $1c $c7 $ec }T T{ $caa5 M $caa6 M $caa7 M -> $c9 $e4 $4f }T
$950f >PC $83 >S $94 >A $ee >X $59 >Y $e7 >P $950f $c9 >M $9510 $29 >M $9511 $83 >M
T{ op PC S A X Y P -> $9511 $83 $94 $ee $59 $65 }T T{ $950f M $9510 M $9511 M -> $c9 $29 $83 }T
$05fc >PC $26 >S $27 >A $74 >X $df >Y $26 >P $05fc $c9 >M $05fd $55 >M $05fe $88 >M
T{ op PC S A X Y P -> $05fe $26 $27 $74 $df $a4 }T T{ $05fc M $05fd M $05fe M -> $c9 $55 $88 }T
( ca )
$d48f >PC $08 >S $7a >A $68 >X $f3 >Y $a7 >P $d48f $ca >M $d490 $3b >M $d491 $13 >M
T{ op PC S A X Y P -> $d490 $08 $7a $67 $f3 $25 }T T{ $d48f M $d490 M $d491 M -> $ca $3b $13 }T
$9fea >PC $60 >S $2b >A $29 >X $ff >Y $eb >P $9fea $ca >M $9feb $78 >M $9fec $58 >M
T{ op PC S A X Y P -> $9feb $60 $2b $28 $ff $69 }T T{ $9fea M $9feb M $9fec M -> $ca $78 $58 }T
$3162 >PC $ec >S $5a >A $1b >X $7e >Y $65 >P $3162 $ca >M $3163 $cf >M $3164 $b8 >M
T{ op PC S A X Y P -> $3163 $ec $5a $1a $7e $65 }T T{ $3162 M $3163 M $3164 M -> $ca $cf $b8 }T
$683d >PC $9a >S $00 >A $4a >X $73 >Y $20 >P $683d $ca >M $683e $36 >M $683f $00 >M
T{ op PC S A X Y P -> $683e $9a $00 $49 $73 $20 }T T{ $683d M $683e M $683f M -> $ca $36 $00 }T
$566e >PC $4b >S $3e >A $df >X $2e >Y $6c >P $566e $ca >M $566f $42 >M $5670 $69 >M
T{ op PC S A X Y P -> $566f $4b $3e $de $2e $ec }T T{ $566e M $566f M $5670 M -> $ca $42 $69 }T
$97a4 >PC $5d >S $18 >A $df >X $28 >Y $a5 >P $97a4 $ca >M $97a5 $6a >M $97a6 $08 >M
T{ op PC S A X Y P -> $97a5 $5d $18 $de $28 $a5 }T T{ $97a4 M $97a5 M $97a6 M -> $ca $6a $08 }T
$1030 >PC $44 >S $47 >A $1b >X $1a >Y $68 >P $1030 $ca >M $1031 $f8 >M $1032 $8b >M
T{ op PC S A X Y P -> $1031 $44 $47 $1a $1a $68 }T T{ $1030 M $1031 M $1032 M -> $ca $f8 $8b }T
$4b6f >PC $0a >S $fd >A $78 >X $d6 >Y $61 >P $4b6f $ca >M $4b70 $a2 >M $4b71 $7d >M
T{ op PC S A X Y P -> $4b70 $0a $fd $77 $d6 $61 }T T{ $4b6f M $4b70 M $4b71 M -> $ca $a2 $7d }T
$5b2d >PC $0e >S $35 >A $06 >X $bd >Y $26 >P $5b2d $ca >M $5b2e $55 >M $5b2f $3d >M
T{ op PC S A X Y P -> $5b2e $0e $35 $05 $bd $24 }T T{ $5b2d M $5b2e M $5b2f M -> $ca $55 $3d }T
$a372 >PC $53 >S $13 >A $2f >X $3a >Y $26 >P $a372 $ca >M $a373 $26 >M $a374 $b2 >M
T{ op PC S A X Y P -> $a373 $53 $13 $2e $3a $24 }T T{ $a372 M $a373 M $a374 M -> $ca $26 $b2 }T
$b166 >PC $0d >S $ab >A $bc >X $64 >Y $68 >P $b166 $ca >M $b167 $0b >M $b168 $3c >M
T{ op PC S A X Y P -> $b167 $0d $ab $bb $64 $e8 }T T{ $b166 M $b167 M $b168 M -> $ca $0b $3c }T
$2425 >PC $6e >S $f4 >A $ea >X $97 >Y $68 >P $2425 $ca >M $2426 $e7 >M $2427 $b6 >M
T{ op PC S A X Y P -> $2426 $6e $f4 $e9 $97 $e8 }T T{ $2425 M $2426 M $2427 M -> $ca $e7 $b6 }T
$522e >PC $4e >S $62 >A $b3 >X $c8 >Y $e8 >P $522e $ca >M $522f $ab >M $5230 $b1 >M
T{ op PC S A X Y P -> $522f $4e $62 $b2 $c8 $e8 }T T{ $522e M $522f M $5230 M -> $ca $ab $b1 }T
$fe9b >PC $9b >S $c6 >A $9e >X $ce >Y $64 >P $fe9b $ca >M $fe9c $ba >M $fe9d $e9 >M
T{ op PC S A X Y P -> $fe9c $9b $c6 $9d $ce $e4 }T T{ $fe9b M $fe9c M $fe9d M -> $ca $ba $e9 }T
$b4ff >PC $f2 >S $4b >A $25 >X $85 >Y $a4 >P $b4ff $ca >M $b500 $ba >M $b501 $37 >M
T{ op PC S A X Y P -> $b500 $f2 $4b $24 $85 $24 }T T{ $b4ff M $b500 M $b501 M -> $ca $ba $37 }T
$e568 >PC $29 >S $77 >A $4b >X $1b >Y $27 >P $e568 $ca >M $e569 $d6 >M $e56a $0a >M
T{ op PC S A X Y P -> $e569 $29 $77 $4a $1b $25 }T T{ $e568 M $e569 M $e56a M -> $ca $d6 $0a }T
( cb )
\ Skipping invalid json wdc65c02/v1/cb.json
( cc )
$3762 >PC $f4 >S $15 >A $0f >X $72 >Y $ef >P $3762 $cc >M $3763 $c7 >M $3764 $d1 >M $3765 $fa >M $d1c7 $60 >M
T{ op PC S A X Y P -> $3765 $f4 $15 $0f $72 $6d }T T{ $3762 M $3763 M $3764 M $3765 M $d1c7 M -> $cc $c7 $d1 $fa $60 }T
$587b >PC $81 >S $06 >A $a0 >X $00 >Y $2e >P $282b $c5 >M $587b $cc >M $587c $2b >M $587d $28 >M $587e $ba >M
T{ op PC S A X Y P -> $587e $81 $06 $a0 $00 $2c }T T{ $282b M $587b M $587c M $587d M $587e M -> $c5 $cc $2b $28 $ba }T
$7ed4 >PC $86 >S $79 >A $7b >X $54 >Y $ad >P $7ed4 $cc >M $7ed5 $95 >M $7ed6 $e9 >M $7ed7 $c9 >M $e995 $77 >M
T{ op PC S A X Y P -> $7ed7 $86 $79 $7b $54 $ac }T T{ $7ed4 M $7ed5 M $7ed6 M $7ed7 M $e995 M -> $cc $95 $e9 $c9 $77 }T
$737c >PC $d5 >S $9a >A $70 >X $c4 >Y $e2 >P $737c $cc >M $737d $21 >M $737e $df >M $737f $22 >M $df21 $b1 >M
T{ op PC S A X Y P -> $737f $d5 $9a $70 $c4 $61 }T T{ $737c M $737d M $737e M $737f M $df21 M -> $cc $21 $df $22 $b1 }T
$35bd >PC $fe >S $21 >A $aa >X $f3 >Y $65 >P $35bd $cc >M $35be $8a >M $35bf $57 >M $35c0 $da >M $578a $d1 >M
T{ op PC S A X Y P -> $35c0 $fe $21 $aa $f3 $65 }T T{ $35bd M $35be M $35bf M $35c0 M $578a M -> $cc $8a $57 $da $d1 }T
$6897 >PC $44 >S $c5 >A $19 >X $81 >Y $2c >P $6897 $cc >M $6898 $e8 >M $6899 $da >M $689a $33 >M $dae8 $3d >M
T{ op PC S A X Y P -> $689a $44 $c5 $19 $81 $2d }T T{ $6897 M $6898 M $6899 M $689a M $dae8 M -> $cc $e8 $da $33 $3d }T
$6dc7 >PC $0c >S $f4 >A $2d >X $a3 >Y $e7 >P $5490 $69 >M $6dc7 $cc >M $6dc8 $90 >M $6dc9 $54 >M $6dca $d0 >M
T{ op PC S A X Y P -> $6dca $0c $f4 $2d $a3 $65 }T T{ $5490 M $6dc7 M $6dc8 M $6dc9 M $6dca M -> $69 $cc $90 $54 $d0 }T
$16b3 >PC $10 >S $98 >A $58 >X $a3 >Y $6c >P $16b3 $cc >M $16b4 $d2 >M $16b5 $e6 >M $16b6 $91 >M $e6d2 $d7 >M
T{ op PC S A X Y P -> $16b6 $10 $98 $58 $a3 $ec }T T{ $16b3 M $16b4 M $16b5 M $16b6 M $e6d2 M -> $cc $d2 $e6 $91 $d7 }T
$38b0 >PC $31 >S $51 >A $88 >X $ce >Y $ad >P $38b0 $cc >M $38b1 $44 >M $38b2 $c3 >M $38b3 $a4 >M $c344 $c4 >M
T{ op PC S A X Y P -> $38b3 $31 $51 $88 $ce $2d }T T{ $38b0 M $38b1 M $38b2 M $38b3 M $c344 M -> $cc $44 $c3 $a4 $c4 }T
$d460 >PC $13 >S $bc >A $5c >X $a0 >Y $a5 >P $cf55 $d7 >M $d460 $cc >M $d461 $55 >M $d462 $cf >M $d463 $90 >M
T{ op PC S A X Y P -> $d463 $13 $bc $5c $a0 $a4 }T T{ $cf55 M $d460 M $d461 M $d462 M $d463 M -> $d7 $cc $55 $cf $90 }T
$90b2 >PC $01 >S $af >A $6c >X $42 >Y $e2 >P $90b2 $cc >M $90b3 $79 >M $90b4 $b3 >M $90b5 $17 >M $b379 $33 >M
T{ op PC S A X Y P -> $90b5 $01 $af $6c $42 $61 }T T{ $90b2 M $90b3 M $90b4 M $90b5 M $b379 M -> $cc $79 $b3 $17 $33 }T
$1696 >PC $0c >S $19 >A $80 >X $dc >Y $af >P $1696 $cc >M $1697 $84 >M $1698 $e4 >M $1699 $a8 >M $e484 $e3 >M
T{ op PC S A X Y P -> $1699 $0c $19 $80 $dc $ac }T T{ $1696 M $1697 M $1698 M $1699 M $e484 M -> $cc $84 $e4 $a8 $e3 }T
$8017 >PC $cc >S $fe >A $9a >X $34 >Y $a6 >P $50df $a4 >M $8017 $cc >M $8018 $df >M $8019 $50 >M $801a $07 >M
T{ op PC S A X Y P -> $801a $cc $fe $9a $34 $a4 }T T{ $50df M $8017 M $8018 M $8019 M $801a M -> $a4 $cc $df $50 $07 }T
$7eea >PC $1d >S $54 >A $2e >X $8c >Y $eb >P $7eea $cc >M $7eeb $d3 >M $7eec $fd >M $7eed $cf >M $fdd3 $12 >M
T{ op PC S A X Y P -> $7eed $1d $54 $2e $8c $69 }T T{ $7eea M $7eeb M $7eec M $7eed M $fdd3 M -> $cc $d3 $fd $cf $12 }T
$c38b >PC $81 >S $48 >A $38 >X $dd >Y $66 >P $3271 $8a >M $c38b $cc >M $c38c $71 >M $c38d $32 >M $c38e $a1 >M
T{ op PC S A X Y P -> $c38e $81 $48 $38 $dd $65 }T T{ $3271 M $c38b M $c38c M $c38d M $c38e M -> $8a $cc $71 $32 $a1 }T
$3c47 >PC $4c >S $c6 >A $ac >X $38 >Y $68 >P $3c47 $cc >M $3c48 $8f >M $3c49 $48 >M $3c4a $62 >M $488f $61 >M
T{ op PC S A X Y P -> $3c4a $4c $c6 $ac $38 $e8 }T T{ $3c47 M $3c48 M $3c49 M $3c4a M $488f M -> $cc $8f $48 $62 $61 }T
( cd )
$2658 >PC $d5 >S $c5 >A $5e >X $88 >Y $65 >P $2658 $cd >M $2659 $55 >M $265a $93 >M $265b $db >M $9355 $62 >M
T{ op PC S A X Y P -> $265b $d5 $c5 $5e $88 $65 }T T{ $2658 M $2659 M $265a M $265b M $9355 M -> $cd $55 $93 $db $62 }T
$6017 >PC $92 >S $6b >A $e5 >X $e1 >Y $ad >P $2707 $4c >M $6017 $cd >M $6018 $07 >M $6019 $27 >M $601a $d1 >M
T{ op PC S A X Y P -> $601a $92 $6b $e5 $e1 $2d }T T{ $2707 M $6017 M $6018 M $6019 M $601a M -> $4c $cd $07 $27 $d1 }T
$91c8 >PC $e3 >S $25 >A $10 >X $22 >Y $ae >P $5a71 $c6 >M $91c8 $cd >M $91c9 $71 >M $91ca $5a >M $91cb $70 >M
T{ op PC S A X Y P -> $91cb $e3 $25 $10 $22 $2c }T T{ $5a71 M $91c8 M $91c9 M $91ca M $91cb M -> $c6 $cd $71 $5a $70 }T
$5200 >PC $35 >S $bf >A $95 >X $fb >Y $a7 >P $1a70 $07 >M $5200 $cd >M $5201 $70 >M $5202 $1a >M $5203 $fd >M
T{ op PC S A X Y P -> $5203 $35 $bf $95 $fb $a5 }T T{ $1a70 M $5200 M $5201 M $5202 M $5203 M -> $07 $cd $70 $1a $fd }T
$dd73 >PC $45 >S $16 >A $0f >X $a6 >Y $23 >P $4e00 $29 >M $dd73 $cd >M $dd74 $00 >M $dd75 $4e >M $dd76 $68 >M
T{ op PC S A X Y P -> $dd76 $45 $16 $0f $a6 $a0 }T T{ $4e00 M $dd73 M $dd74 M $dd75 M $dd76 M -> $29 $cd $00 $4e $68 }T
$d8f5 >PC $aa >S $da >A $2e >X $4e >Y $69 >P $8cdd $f0 >M $d8f5 $cd >M $d8f6 $dd >M $d8f7 $8c >M $d8f8 $11 >M
T{ op PC S A X Y P -> $d8f8 $aa $da $2e $4e $e8 }T T{ $8cdd M $d8f5 M $d8f6 M $d8f7 M $d8f8 M -> $f0 $cd $dd $8c $11 }T
$8544 >PC $1c >S $77 >A $2b >X $c1 >Y $a0 >P $7426 $4f >M $8544 $cd >M $8545 $26 >M $8546 $74 >M $8547 $b0 >M
T{ op PC S A X Y P -> $8547 $1c $77 $2b $c1 $21 }T T{ $7426 M $8544 M $8545 M $8546 M $8547 M -> $4f $cd $26 $74 $b0 }T
$1d06 >PC $94 >S $31 >A $5b >X $94 >Y $e8 >P $1d06 $cd >M $1d07 $7a >M $1d08 $d1 >M $1d09 $b3 >M $d17a $cf >M
T{ op PC S A X Y P -> $1d09 $94 $31 $5b $94 $68 }T T{ $1d06 M $1d07 M $1d08 M $1d09 M $d17a M -> $cd $7a $d1 $b3 $cf }T
$2551 >PC $92 >S $86 >A $3d >X $27 >Y $ab >P $2551 $cd >M $2552 $2d >M $2553 $4a >M $2554 $d2 >M $4a2d $a1 >M
T{ op PC S A X Y P -> $2554 $92 $86 $3d $27 $a8 }T T{ $2551 M $2552 M $2553 M $2554 M $4a2d M -> $cd $2d $4a $d2 $a1 }T
$5411 >PC $c9 >S $0f >A $3f >X $21 >Y $ac >P $5411 $cd >M $5412 $5b >M $5413 $ea >M $5414 $ec >M $ea5b $3f >M
T{ op PC S A X Y P -> $5414 $c9 $0f $3f $21 $ac }T T{ $5411 M $5412 M $5413 M $5414 M $ea5b M -> $cd $5b $ea $ec $3f }T
$8f16 >PC $af >S $9c >A $de >X $32 >Y $2c >P $43ad $2e >M $8f16 $cd >M $8f17 $ad >M $8f18 $43 >M $8f19 $e0 >M
T{ op PC S A X Y P -> $8f19 $af $9c $de $32 $2d }T T{ $43ad M $8f16 M $8f17 M $8f18 M $8f19 M -> $2e $cd $ad $43 $e0 }T
$737a >PC $58 >S $d2 >A $33 >X $2e >Y $ab >P $5374 $b9 >M $737a $cd >M $737b $74 >M $737c $53 >M $737d $22 >M
T{ op PC S A X Y P -> $737d $58 $d2 $33 $2e $29 }T T{ $5374 M $737a M $737b M $737c M $737d M -> $b9 $cd $74 $53 $22 }T
$782a >PC $ba >S $71 >A $aa >X $91 >Y $23 >P $6c17 $a3 >M $782a $cd >M $782b $17 >M $782c $6c >M $782d $c4 >M
T{ op PC S A X Y P -> $782d $ba $71 $aa $91 $a0 }T T{ $6c17 M $782a M $782b M $782c M $782d M -> $a3 $cd $17 $6c $c4 }T
$2624 >PC $7b >S $a1 >A $77 >X $57 >Y $ea >P $2624 $cd >M $2625 $52 >M $2626 $6a >M $2627 $48 >M $6a52 $9c >M
T{ op PC S A X Y P -> $2627 $7b $a1 $77 $57 $69 }T T{ $2624 M $2625 M $2626 M $2627 M $6a52 M -> $cd $52 $6a $48 $9c }T
$3ef3 >PC $f4 >S $4d >A $e4 >X $9a >Y $62 >P $3ef3 $cd >M $3ef4 $de >M $3ef5 $49 >M $3ef6 $2e >M $49de $99 >M
T{ op PC S A X Y P -> $3ef6 $f4 $4d $e4 $9a $e0 }T T{ $3ef3 M $3ef4 M $3ef5 M $3ef6 M $49de M -> $cd $de $49 $2e $99 }T
$8700 >PC $97 >S $73 >A $ae >X $2c >Y $2f >P $4202 $77 >M $8700 $cd >M $8701 $02 >M $8702 $42 >M $8703 $4d >M
T{ op PC S A X Y P -> $8703 $97 $73 $ae $2c $ac }T T{ $4202 M $8700 M $8701 M $8702 M $8703 M -> $77 $cd $02 $42 $4d }T
( ce )
$4bc0 >PC $83 >S $50 >A $3e >X $62 >Y $66 >P $4bc0 $ce >M $4bc1 $4d >M $4bc2 $ac >M $4bc3 $c8 >M $ac4d $b3 >M
T{ op PC S A X Y P -> $4bc3 $83 $50 $3e $62 $e4 }T T{ $4bc0 M $4bc1 M $4bc2 M $4bc3 M $ac4d M -> $ce $4d $ac $c8 $b2 }T
$6063 >PC $c0 >S $61 >A $7e >X $5f >Y $a5 >P $6063 $ce >M $6064 $4d >M $6065 $e9 >M $6066 $97 >M $e94d $f5 >M
T{ op PC S A X Y P -> $6066 $c0 $61 $7e $5f $a5 }T T{ $6063 M $6064 M $6065 M $6066 M $e94d M -> $ce $4d $e9 $97 $f4 }T
$3a88 >PC $37 >S $42 >A $f0 >X $b5 >Y $6a >P $27c6 $f6 >M $3a88 $ce >M $3a89 $c6 >M $3a8a $27 >M $3a8b $0a >M
T{ op PC S A X Y P -> $3a8b $37 $42 $f0 $b5 $e8 }T T{ $27c6 M $3a88 M $3a89 M $3a8a M $3a8b M -> $f5 $ce $c6 $27 $0a }T
$764a >PC $df >S $aa >A $d3 >X $0e >Y $69 >P $764a $ce >M $764b $fc >M $764c $d6 >M $764d $5e >M $d6fc $06 >M
T{ op PC S A X Y P -> $764d $df $aa $d3 $0e $69 }T T{ $764a M $764b M $764c M $764d M $d6fc M -> $ce $fc $d6 $5e $05 }T
$54a4 >PC $78 >S $e5 >A $f0 >X $56 >Y $2b >P $54a4 $ce >M $54a5 $3c >M $54a6 $fd >M $54a7 $a0 >M $fd3c $cc >M
T{ op PC S A X Y P -> $54a7 $78 $e5 $f0 $56 $a9 }T T{ $54a4 M $54a5 M $54a6 M $54a7 M $fd3c M -> $ce $3c $fd $a0 $cb }T
$9a10 >PC $05 >S $a7 >A $ff >X $4e >Y $e5 >P $1de4 $8e >M $9a10 $ce >M $9a11 $e4 >M $9a12 $1d >M $9a13 $16 >M
T{ op PC S A X Y P -> $9a13 $05 $a7 $ff $4e $e5 }T T{ $1de4 M $9a10 M $9a11 M $9a12 M $9a13 M -> $8d $ce $e4 $1d $16 }T
$a74b >PC $a4 >S $ac >A $27 >X $5a >Y $eb >P $7923 $4a >M $a74b $ce >M $a74c $23 >M $a74d $79 >M $a74e $45 >M
T{ op PC S A X Y P -> $a74e $a4 $ac $27 $5a $69 }T T{ $7923 M $a74b M $a74c M $a74d M $a74e M -> $49 $ce $23 $79 $45 }T
$09ba >PC $ea >S $a0 >A $80 >X $a6 >Y $28 >P $09ba $ce >M $09bb $f7 >M $09bc $6a >M $09bd $f9 >M $6af7 $a8 >M
T{ op PC S A X Y P -> $09bd $ea $a0 $80 $a6 $a8 }T T{ $09ba M $09bb M $09bc M $09bd M $6af7 M -> $ce $f7 $6a $f9 $a7 }T
$f393 >PC $0e >S $b6 >A $4c >X $45 >Y $67 >P $d304 $5e >M $f393 $ce >M $f394 $04 >M $f395 $d3 >M $f396 $9d >M
T{ op PC S A X Y P -> $f396 $0e $b6 $4c $45 $65 }T T{ $d304 M $f393 M $f394 M $f395 M $f396 M -> $5d $ce $04 $d3 $9d }T
$28e7 >PC $59 >S $b0 >A $2c >X $cf >Y $a8 >P $075a $fb >M $28e7 $ce >M $28e8 $5a >M $28e9 $07 >M $28ea $af >M
T{ op PC S A X Y P -> $28ea $59 $b0 $2c $cf $a8 }T T{ $075a M $28e7 M $28e8 M $28e9 M $28ea M -> $fa $ce $5a $07 $af }T
$f117 >PC $df >S $18 >A $c3 >X $27 >Y $21 >P $8795 $ee >M $f117 $ce >M $f118 $95 >M $f119 $87 >M $f11a $b4 >M
T{ op PC S A X Y P -> $f11a $df $18 $c3 $27 $a1 }T T{ $8795 M $f117 M $f118 M $f119 M $f11a M -> $ed $ce $95 $87 $b4 }T
$db44 >PC $c5 >S $3c >A $e7 >X $97 >Y $2b >P $5afe $96 >M $db44 $ce >M $db45 $fe >M $db46 $5a >M $db47 $39 >M
T{ op PC S A X Y P -> $db47 $c5 $3c $e7 $97 $a9 }T T{ $5afe M $db44 M $db45 M $db46 M $db47 M -> $95 $ce $fe $5a $39 }T
$d7cf >PC $73 >S $50 >A $fe >X $14 >Y $66 >P $77de $1c >M $d7cf $ce >M $d7d0 $de >M $d7d1 $77 >M $d7d2 $26 >M
T{ op PC S A X Y P -> $d7d2 $73 $50 $fe $14 $64 }T T{ $77de M $d7cf M $d7d0 M $d7d1 M $d7d2 M -> $1b $ce $de $77 $26 }T
$e546 >PC $0a >S $16 >A $06 >X $cb >Y $a4 >P $b565 $02 >M $e546 $ce >M $e547 $65 >M $e548 $b5 >M $e549 $13 >M
T{ op PC S A X Y P -> $e549 $0a $16 $06 $cb $24 }T T{ $b565 M $e546 M $e547 M $e548 M $e549 M -> $01 $ce $65 $b5 $13 }T
$61ce >PC $04 >S $fb >A $28 >X $40 >Y $e7 >P $61ce $ce >M $61cf $b5 >M $61d0 $c6 >M $61d1 $70 >M $c6b5 $bc >M
T{ op PC S A X Y P -> $61d1 $04 $fb $28 $40 $e5 }T T{ $61ce M $61cf M $61d0 M $61d1 M $c6b5 M -> $ce $b5 $c6 $70 $bb }T
$77c4 >PC $d9 >S $d8 >A $93 >X $fb >Y $25 >P $77c4 $ce >M $77c5 $fa >M $77c6 $96 >M $77c7 $1e >M $96fa $7b >M
T{ op PC S A X Y P -> $77c7 $d9 $d8 $93 $fb $25 }T T{ $77c4 M $77c5 M $77c6 M $77c7 M $96fa M -> $ce $fa $96 $1e $7a }T
( cf )
$c246 >PC $67 >S $da >A $bd >X $4c >Y $e0 >P $00a7 $22 >M $c246 $cf >M $c247 $a7 >M $c248 $06 >M $c249 $d8 >M $c24f $e9 >M
T{ op PC S A X Y P -> $c249 $67 $da $bd $4c $e0 }T T{ $00a7 M $c246 M $c247 M $c248 M $c249 M $c24f M -> $22 $cf $a7 $06 $d8 $e9 }T
$9083 >PC $23 >S $21 >A $27 >X $2f >Y $ae >P $009e $7e >M $9083 $cf >M $9084 $9e >M $9085 $3b >M $90c1 $f7 >M
T{ op PC S A X Y P -> $90c1 $23 $21 $27 $2f $ae }T T{ $009e M $9083 M $9084 M $9085 M $90c1 M -> $7e $cf $9e $3b $f7 }T
$2b1b >PC $31 >S $a9 >A $b4 >X $99 >Y $6e >P $0014 $46 >M $2b1b $cf >M $2b1c $14 >M $2b1d $bb >M $2b1e $b0 >M $2bd9 $b8 >M
T{ op PC S A X Y P -> $2b1e $31 $a9 $b4 $99 $6e }T T{ $0014 M $2b1b M $2b1c M $2b1d M $2b1e M $2bd9 M -> $46 $cf $14 $bb $b0 $b8 }T
$00a4 >PC $cb >S $68 >A $f6 >X $c2 >Y $61 >P $00a4 $cf >M $00a5 $a5 >M $00a6 $2c >M $00a7 $45 >M $00d3 $ab >M
T{ op PC S A X Y P -> $00a7 $cb $68 $f6 $c2 $61 }T T{ $00a4 M $00a5 M $00a6 M $00a7 M $00d3 M -> $cf $a5 $2c $45 $ab }T
$be61 >PC $60 >S $2e >A $97 >X $3d >Y $6a >P $00ef $43 >M $be3e $31 >M $be61 $cf >M $be62 $ef >M $be63 $da >M $be64 $8a >M
T{ op PC S A X Y P -> $be64 $60 $2e $97 $3d $6a }T T{ $00ef M $be3e M $be61 M $be62 M $be63 M $be64 M -> $43 $31 $cf $ef $da $8a }T
$8bd7 >PC $77 >S $73 >A $e7 >X $de >Y $2b >P $008f $75 >M $8b1f $68 >M $8bd7 $cf >M $8bd8 $8f >M $8bd9 $45 >M $8c1f $8d >M
T{ op PC S A X Y P -> $8c1f $77 $73 $e7 $de $2b }T T{ $008f M $8b1f M $8bd7 M $8bd8 M $8bd9 M $8c1f M -> $75 $68 $cf $8f $45 $8d }T
$0934 >PC $27 >S $84 >A $26 >X $90 >Y $a0 >P $0063 $4a >M $0934 $cf >M $0935 $63 >M $0936 $31 >M $0937 $f4 >M $0968 $64 >M
T{ op PC S A X Y P -> $0937 $27 $84 $26 $90 $a0 }T T{ $0063 M $0934 M $0935 M $0936 M $0937 M $0968 M -> $4a $cf $63 $31 $f4 $64 }T
$0f9b >PC $9f >S $d3 >A $42 >X $a2 >Y $e8 >P $0097 $a8 >M $0f9b $cf >M $0f9c $97 >M $0f9d $06 >M $0f9e $3d >M $0fa4 $01 >M
T{ op PC S A X Y P -> $0f9e $9f $d3 $42 $a2 $e8 }T T{ $0097 M $0f9b M $0f9c M $0f9d M $0f9e M $0fa4 M -> $a8 $cf $97 $06 $3d $01 }T
$e46c >PC $b1 >S $6f >A $9b >X $6f >Y $a4 >P $004c $05 >M $e431 $07 >M $e46c $cf >M $e46d $4c >M $e46e $c2 >M $e46f $66 >M
T{ op PC S A X Y P -> $e46f $b1 $6f $9b $6f $a4 }T T{ $004c M $e431 M $e46c M $e46d M $e46e M $e46f M -> $05 $07 $cf $4c $c2 $66 }T
$d652 >PC $b2 >S $b1 >A $82 >X $09 >Y $62 >P $00d6 $e5 >M $d652 $cf >M $d653 $d6 >M $d654 $6c >M $d655 $c9 >M $d6c1 $22 >M
T{ op PC S A X Y P -> $d655 $b2 $b1 $82 $09 $62 }T T{ $00d6 M $d652 M $d653 M $d654 M $d655 M $d6c1 M -> $e5 $cf $d6 $6c $c9 $22 }T
$b87c >PC $f7 >S $fb >A $75 >X $e1 >Y $64 >P $0097 $00 >M $b80e $45 >M $b87c $cf >M $b87d $97 >M $b87e $8f >M $b87f $d2 >M
T{ op PC S A X Y P -> $b87f $f7 $fb $75 $e1 $64 }T T{ $0097 M $b80e M $b87c M $b87d M $b87e M $b87f M -> $00 $45 $cf $97 $8f $d2 }T
$3a7d >PC $ea >S $2b >A $4f >X $b9 >Y $6d >P $00c0 $84 >M $3a56 $29 >M $3a7d $cf >M $3a7e $c0 >M $3a7f $d6 >M $3a80 $b0 >M
T{ op PC S A X Y P -> $3a80 $ea $2b $4f $b9 $6d }T T{ $00c0 M $3a56 M $3a7d M $3a7e M $3a7f M $3a80 M -> $84 $29 $cf $c0 $d6 $b0 }T
$0a59 >PC $2e >S $14 >A $e4 >X $76 >Y $a6 >P $004c $10 >M $0a16 $b3 >M $0a59 $cf >M $0a5a $4c >M $0a5b $ba >M
T{ op PC S A X Y P -> $0a16 $2e $14 $e4 $76 $a6 }T T{ $004c M $0a16 M $0a59 M $0a5a M $0a5b M -> $10 $b3 $cf $4c $ba }T
$a133 >PC $5d >S $de >A $e7 >X $de >Y $27 >P $00ec $f5 >M $a0dd $f4 >M $a133 $cf >M $a134 $ec >M $a135 $a7 >M $a1dd $21 >M
T{ op PC S A X Y P -> $a0dd $5d $de $e7 $de $27 }T T{ $00ec M $a0dd M $a133 M $a134 M $a135 M $a1dd M -> $f5 $f4 $cf $ec $a7 $21 }T
$d3ed >PC $90 >S $ab >A $4d >X $f9 >Y $e6 >P $0069 $16 >M $d32b $cb >M $d3ed $cf >M $d3ee $69 >M $d3ef $3b >M $d42b $eb >M
T{ op PC S A X Y P -> $d42b $90 $ab $4d $f9 $e6 }T T{ $0069 M $d32b M $d3ed M $d3ee M $d3ef M $d42b M -> $16 $cb $cf $69 $3b $eb }T
$7d88 >PC $90 >S $2d >A $4f >X $77 >Y $a3 >P $00d9 $f6 >M $7d88 $cf >M $7d89 $d9 >M $7d8a $33 >M $7dbe $f5 >M
T{ op PC S A X Y P -> $7dbe $90 $2d $4f $77 $a3 }T T{ $00d9 M $7d88 M $7d89 M $7d8a M $7dbe M -> $f6 $cf $d9 $33 $f5 }T
( d0 )
$830b >PC $10 >S $4b >A $a1 >X $3b >Y $22 >P $830b $d0 >M $830c $6f >M $830d $49 >M
T{ op PC S A X Y P -> $830d $10 $4b $a1 $3b $22 }T T{ $830b M $830c M $830d M -> $d0 $6f $49 }T
$2870 >PC $13 >S $0a >A $31 >X $fa >Y $6e >P $2870 $d0 >M $2871 $3b >M $2872 $9c >M
T{ op PC S A X Y P -> $2872 $13 $0a $31 $fa $6e }T T{ $2870 M $2871 M $2872 M -> $d0 $3b $9c }T
$13e9 >PC $5f >S $9a >A $24 >X $ff >Y $2b >P $13e9 $d0 >M $13ea $81 >M $13eb $16 >M
T{ op PC S A X Y P -> $13eb $5f $9a $24 $ff $2b }T T{ $13e9 M $13ea M $13eb M -> $d0 $81 $16 }T
$f3a3 >PC $ea >S $73 >A $fe >X $3e >Y $2c >P $f352 $f4 >M $f3a3 $d0 >M $f3a4 $ad >M $f3a5 $aa >M
T{ op PC S A X Y P -> $f352 $ea $73 $fe $3e $2c }T T{ $f352 M $f3a3 M $f3a4 M $f3a5 M -> $f4 $d0 $ad $aa }T
$9ebd >PC $89 >S $f5 >A $39 >X $21 >Y $aa >P $9ebd $d0 >M $9ebe $2e >M $9ebf $68 >M
T{ op PC S A X Y P -> $9ebf $89 $f5 $39 $21 $aa }T T{ $9ebd M $9ebe M $9ebf M -> $d0 $2e $68 }T
$0230 >PC $b8 >S $b3 >A $2c >X $29 >Y $e6 >P $0230 $d0 >M $0231 $98 >M $0232 $c8 >M
T{ op PC S A X Y P -> $0232 $b8 $b3 $2c $29 $e6 }T T{ $0230 M $0231 M $0232 M -> $d0 $98 $c8 }T
$e2a8 >PC $2d >S $87 >A $37 >X $71 >Y $ea >P $e2a8 $d0 >M $e2a9 $20 >M $e2aa $17 >M
T{ op PC S A X Y P -> $e2aa $2d $87 $37 $71 $ea }T T{ $e2a8 M $e2a9 M $e2aa M -> $d0 $20 $17 }T
$d306 >PC $e0 >S $2c >A $c2 >X $21 >Y $23 >P $d306 $d0 >M $d307 $5d >M $d308 $e1 >M
T{ op PC S A X Y P -> $d308 $e0 $2c $c2 $21 $23 }T T{ $d306 M $d307 M $d308 M -> $d0 $5d $e1 }T
$f889 >PC $33 >S $c0 >A $d9 >X $b0 >Y $aa >P $f889 $d0 >M $f88a $5f >M $f88b $e3 >M
T{ op PC S A X Y P -> $f88b $33 $c0 $d9 $b0 $aa }T T{ $f889 M $f88a M $f88b M -> $d0 $5f $e3 }T
$4777 >PC $83 >S $03 >A $db >X $e5 >Y $68 >P $4724 $63 >M $4777 $d0 >M $4778 $ab >M $4779 $66 >M
T{ op PC S A X Y P -> $4724 $83 $03 $db $e5 $68 }T T{ $4724 M $4777 M $4778 M $4779 M -> $63 $d0 $ab $66 }T
$a4e5 >PC $bb >S $32 >A $64 >X $9f >Y $27 >P $a4e5 $d0 >M $a4e6 $6a >M $a4e7 $d1 >M
T{ op PC S A X Y P -> $a4e7 $bb $32 $64 $9f $27 }T T{ $a4e5 M $a4e6 M $a4e7 M -> $d0 $6a $d1 }T
$f561 >PC $66 >S $9c >A $6a >X $6b >Y $61 >P $f54f $83 >M $f561 $d0 >M $f562 $ec >M $f563 $3b >M
T{ op PC S A X Y P -> $f54f $66 $9c $6a $6b $61 }T T{ $f54f M $f561 M $f562 M $f563 M -> $83 $d0 $ec $3b }T
$c30e >PC $5e >S $c4 >A $9f >X $97 >Y $a3 >P $c30e $d0 >M $c30f $b5 >M $c310 $6c >M
T{ op PC S A X Y P -> $c310 $5e $c4 $9f $97 $a3 }T T{ $c30e M $c30f M $c310 M -> $d0 $b5 $6c }T
$cae6 >PC $57 >S $1d >A $62 >X $43 >Y $6f >P $cae6 $d0 >M $cae7 $a9 >M $cae8 $fb >M
T{ op PC S A X Y P -> $cae8 $57 $1d $62 $43 $6f }T T{ $cae6 M $cae7 M $cae8 M -> $d0 $a9 $fb }T
$8666 >PC $a0 >S $25 >A $e8 >X $f2 >Y $69 >P $8666 $d0 >M $8667 $4d >M $8668 $ce >M $86b5 $5b >M
T{ op PC S A X Y P -> $86b5 $a0 $25 $e8 $f2 $69 }T T{ $8666 M $8667 M $8668 M $86b5 M -> $d0 $4d $ce $5b }T
$4fbd >PC $d4 >S $13 >A $a4 >X $d7 >Y $a8 >P $4f03 $d0 >M $4fbd $d0 >M $4fbe $44 >M $4fbf $58 >M $5003 $c5 >M
T{ op PC S A X Y P -> $5003 $d4 $13 $a4 $d7 $a8 }T T{ $4f03 M $4fbd M $4fbe M $4fbf M $5003 M -> $d0 $d0 $44 $58 $c5 }T
( d1 )
$afc5 >PC $1c >S $95 >A $72 >X $59 >Y $25 >P $00bc $c6 >M $00bd $8a >M $8b1f $4d >M $afc5 $d1 >M $afc6 $bc >M $afc7 $84 >M
T{ op PC S A X Y P -> $afc7 $1c $95 $72 $59 $25 }T T{ $00bc M $00bd M $8b1f M $afc5 M $afc6 M $afc7 M -> $c6 $8a $4d $d1 $bc $84 }T
$cd1c >PC $3d >S $93 >A $a7 >X $51 >Y $6b >P $000d $64 >M $000e $45 >M $45b5 $19 >M $cd1c $d1 >M $cd1d $0d >M $cd1e $94 >M
T{ op PC S A X Y P -> $cd1e $3d $93 $a7 $51 $69 }T T{ $000d M $000e M $45b5 M $cd1c M $cd1d M $cd1e M -> $64 $45 $19 $d1 $0d $94 }T
$f71c >PC $4a >S $6d >A $24 >X $53 >Y $ac >P $006d $67 >M $006e $95 >M $95ba $ce >M $f71c $d1 >M $f71d $6d >M $f71e $b2 >M
T{ op PC S A X Y P -> $f71e $4a $6d $24 $53 $ac }T T{ $006d M $006e M $95ba M $f71c M $f71d M $f71e M -> $67 $95 $ce $d1 $6d $b2 }T
$92e4 >PC $c6 >S $a9 >A $b1 >X $c0 >Y $ee >P $0039 $ad >M $003a $0d >M $0e6d $88 >M $92e4 $d1 >M $92e5 $39 >M $92e6 $7b >M
T{ op PC S A X Y P -> $92e6 $c6 $a9 $b1 $c0 $6d }T T{ $0039 M $003a M $0e6d M $92e4 M $92e5 M $92e6 M -> $ad $0d $88 $d1 $39 $7b }T
$02d8 >PC $ba >S $32 >A $0f >X $8f >Y $20 >P $009e $6d >M $009f $2e >M $02d8 $d1 >M $02d9 $9e >M $02da $a5 >M $2efc $af >M
T{ op PC S A X Y P -> $02da $ba $32 $0f $8f $a0 }T T{ $009e M $009f M $02d8 M $02d9 M $02da M $2efc M -> $6d $2e $d1 $9e $a5 $af }T
$c743 >PC $4f >S $7e >A $a9 >X $66 >Y $e3 >P $008b $c1 >M $008c $d1 >M $c743 $d1 >M $c744 $8b >M $c745 $97 >M $d227 $93 >M
T{ op PC S A X Y P -> $c745 $4f $7e $a9 $66 $e0 }T T{ $008b M $008c M $c743 M $c744 M $c745 M $d227 M -> $c1 $d1 $d1 $8b $97 $93 }T
$b060 >PC $56 >S $b3 >A $ff >X $b8 >Y $27 >P $00eb $e7 >M $00ec $c5 >M $b060 $d1 >M $b061 $eb >M $b062 $ff >M $c69f $7b >M
T{ op PC S A X Y P -> $b062 $56 $b3 $ff $b8 $25 }T T{ $00eb M $00ec M $b060 M $b061 M $b062 M $c69f M -> $e7 $c5 $d1 $eb $ff $7b }T
$2577 >PC $36 >S $7f >A $01 >X $62 >Y $6c >P $0017 $de >M $0018 $3e >M $2577 $d1 >M $2578 $17 >M $2579 $b0 >M $3f40 $88 >M
T{ op PC S A X Y P -> $2579 $36 $7f $01 $62 $ec }T T{ $0017 M $0018 M $2577 M $2578 M $2579 M $3f40 M -> $de $3e $d1 $17 $b0 $88 }T
$6da6 >PC $51 >S $b3 >A $56 >X $8c >Y $e9 >P $007f $0c >M $0080 $12 >M $1298 $2b >M $6da6 $d1 >M $6da7 $7f >M $6da8 $05 >M
T{ op PC S A X Y P -> $6da8 $51 $b3 $56 $8c $e9 }T T{ $007f M $0080 M $1298 M $6da6 M $6da7 M $6da8 M -> $0c $12 $2b $d1 $7f $05 }T
$634a >PC $30 >S $bb >A $bc >X $8c >Y $eb >P $0068 $da >M $0069 $f6 >M $634a $d1 >M $634b $68 >M $634c $92 >M $f766 $eb >M
T{ op PC S A X Y P -> $634c $30 $bb $bc $8c $e8 }T T{ $0068 M $0069 M $634a M $634b M $634c M $f766 M -> $da $f6 $d1 $68 $92 $eb }T
$0085 >PC $4e >S $8d >A $f7 >X $87 >Y $a3 >P $005b $25 >M $005c $d9 >M $0085 $d1 >M $0086 $5b >M $0087 $26 >M $d9ac $20 >M
T{ op PC S A X Y P -> $0087 $4e $8d $f7 $87 $21 }T T{ $005b M $005c M $0085 M $0086 M $0087 M $d9ac M -> $25 $d9 $d1 $5b $26 $20 }T
$90bd >PC $5a >S $a8 >A $2b >X $cd >Y $25 >P $0010 $3f >M $0011 $0c >M $0d0c $ec >M $90bd $d1 >M $90be $10 >M $90bf $8c >M
T{ op PC S A X Y P -> $90bf $5a $a8 $2b $cd $a4 }T T{ $0010 M $0011 M $0d0c M $90bd M $90be M $90bf M -> $3f $0c $ec $d1 $10 $8c }T
$1e79 >PC $d9 >S $d1 >A $5b >X $9a >Y $6b >P $007c $e5 >M $007d $03 >M $047f $ae >M $1e79 $d1 >M $1e7a $7c >M $1e7b $ac >M
T{ op PC S A X Y P -> $1e7b $d9 $d1 $5b $9a $69 }T T{ $007c M $007d M $047f M $1e79 M $1e7a M $1e7b M -> $e5 $03 $ae $d1 $7c $ac }T
$4e75 >PC $d9 >S $6e >A $92 >X $0e >Y $ed >P $009e $18 >M $009f $81 >M $4e75 $d1 >M $4e76 $9e >M $4e77 $56 >M $8126 $ae >M
T{ op PC S A X Y P -> $4e77 $d9 $6e $92 $0e $ec }T T{ $009e M $009f M $4e75 M $4e76 M $4e77 M $8126 M -> $18 $81 $d1 $9e $56 $ae }T
$6ea7 >PC $14 >S $ef >A $63 >X $83 >Y $63 >P $00f2 $6d >M $00f3 $19 >M $19f0 $3d >M $6ea7 $d1 >M $6ea8 $f2 >M $6ea9 $27 >M
T{ op PC S A X Y P -> $6ea9 $14 $ef $63 $83 $e1 }T T{ $00f2 M $00f3 M $19f0 M $6ea7 M $6ea8 M $6ea9 M -> $6d $19 $3d $d1 $f2 $27 }T
$0cd1 >PC $31 >S $ee >A $90 >X $06 >Y $64 >P $00d3 $93 >M $00d4 $40 >M $0cd1 $d1 >M $0cd2 $d3 >M $0cd3 $f2 >M $4099 $fc >M
T{ op PC S A X Y P -> $0cd3 $31 $ee $90 $06 $e4 }T T{ $00d3 M $00d4 M $0cd1 M $0cd2 M $0cd3 M $4099 M -> $93 $40 $d1 $d3 $f2 $fc }T
( d2 )
$a279 >PC $00 >S $17 >A $ca >X $33 >Y $63 >P $0000 $7f >M $0001 $27 >M $277f $23 >M $a279 $d2 >M $a27a $00 >M $a27b $1c >M
T{ op PC S A X Y P -> $a27b $00 $17 $ca $33 $e0 }T T{ $0000 M $0001 M $277f M $a279 M $a27a M $a27b M -> $7f $27 $23 $d2 $00 $1c }T
$a671 >PC $eb >S $88 >A $e0 >X $e2 >Y $66 >P $0016 $7c >M $0017 $9a >M $9a7c $ce >M $a671 $d2 >M $a672 $16 >M $a673 $c5 >M
T{ op PC S A X Y P -> $a673 $eb $88 $e0 $e2 $e4 }T T{ $0016 M $0017 M $9a7c M $a671 M $a672 M $a673 M -> $7c $9a $ce $d2 $16 $c5 }T
$1f4e >PC $b6 >S $42 >A $0c >X $ea >Y $28 >P $0018 $79 >M $0019 $7d >M $1f4e $d2 >M $1f4f $18 >M $1f50 $14 >M $7d79 $7b >M
T{ op PC S A X Y P -> $1f50 $b6 $42 $0c $ea $a8 }T T{ $0018 M $0019 M $1f4e M $1f4f M $1f50 M $7d79 M -> $79 $7d $d2 $18 $14 $7b }T
$cf11 >PC $d2 >S $78 >A $04 >X $5a >Y $68 >P $00d3 $a4 >M $00d4 $d3 >M $cf11 $d2 >M $cf12 $d3 >M $cf13 $bc >M $d3a4 $d1 >M
T{ op PC S A X Y P -> $cf13 $d2 $78 $04 $5a $e8 }T T{ $00d3 M $00d4 M $cf11 M $cf12 M $cf13 M $d3a4 M -> $a4 $d3 $d2 $d3 $bc $d1 }T
$0455 >PC $c4 >S $fb >A $c2 >X $52 >Y $a2 >P $0042 $ee >M $0043 $cc >M $0455 $d2 >M $0456 $42 >M $0457 $0a >M $ccee $16 >M
T{ op PC S A X Y P -> $0457 $c4 $fb $c2 $52 $a1 }T T{ $0042 M $0043 M $0455 M $0456 M $0457 M $ccee M -> $ee $cc $d2 $42 $0a $16 }T
$a2ce >PC $3e >S $d8 >A $27 >X $fc >Y $e8 >P $003b $8e >M $003c $5c >M $5c8e $1b >M $a2ce $d2 >M $a2cf $3b >M $a2d0 $5c >M
T{ op PC S A X Y P -> $a2d0 $3e $d8 $27 $fc $e9 }T T{ $003b M $003c M $5c8e M $a2ce M $a2cf M $a2d0 M -> $8e $5c $1b $d2 $3b $5c }T
$2b32 >PC $6d >S $e8 >A $79 >X $c0 >Y $67 >P $00f0 $5c >M $00f1 $aa >M $2b32 $d2 >M $2b33 $f0 >M $2b34 $89 >M $aa5c $6a >M
T{ op PC S A X Y P -> $2b34 $6d $e8 $79 $c0 $65 }T T{ $00f0 M $00f1 M $2b32 M $2b33 M $2b34 M $aa5c M -> $5c $aa $d2 $f0 $89 $6a }T
$28be >PC $53 >S $d8 >A $00 >X $c9 >Y $6b >P $00ec $10 >M $00ed $f0 >M $28be $d2 >M $28bf $ec >M $28c0 $fd >M $f010 $0d >M
T{ op PC S A X Y P -> $28c0 $53 $d8 $00 $c9 $e9 }T T{ $00ec M $00ed M $28be M $28bf M $28c0 M $f010 M -> $10 $f0 $d2 $ec $fd $0d }T
$cfb3 >PC $28 >S $eb >A $ec >X $c2 >Y $63 >P $0089 $8b >M $008a $f9 >M $cfb3 $d2 >M $cfb4 $89 >M $cfb5 $b2 >M $f98b $ef >M
T{ op PC S A X Y P -> $cfb5 $28 $eb $ec $c2 $e0 }T T{ $0089 M $008a M $cfb3 M $cfb4 M $cfb5 M $f98b M -> $8b $f9 $d2 $89 $b2 $ef }T
$8d89 >PC $c3 >S $25 >A $e4 >X $7e >Y $24 >P $00e0 $6c >M $00e1 $47 >M $476c $5a >M $8d89 $d2 >M $8d8a $e0 >M $8d8b $81 >M
T{ op PC S A X Y P -> $8d8b $c3 $25 $e4 $7e $a4 }T T{ $00e0 M $00e1 M $476c M $8d89 M $8d8a M $8d8b M -> $6c $47 $5a $d2 $e0 $81 }T
$a4c2 >PC $fb >S $e7 >A $03 >X $bf >Y $e3 >P $0078 $c7 >M $0079 $bb >M $a4c2 $d2 >M $a4c3 $78 >M $a4c4 $90 >M $bbc7 $4a >M
T{ op PC S A X Y P -> $a4c4 $fb $e7 $03 $bf $e1 }T T{ $0078 M $0079 M $a4c2 M $a4c3 M $a4c4 M $bbc7 M -> $c7 $bb $d2 $78 $90 $4a }T
$984b >PC $46 >S $2c >A $30 >X $4b >Y $60 >P $0049 $99 >M $004a $0c >M $0c99 $9b >M $984b $d2 >M $984c $49 >M $984d $09 >M
T{ op PC S A X Y P -> $984d $46 $2c $30 $4b $e0 }T T{ $0049 M $004a M $0c99 M $984b M $984c M $984d M -> $99 $0c $9b $d2 $49 $09 }T
$0cc6 >PC $ef >S $45 >A $7c >X $4c >Y $e7 >P $00ef $90 >M $00f0 $ad >M $0cc6 $d2 >M $0cc7 $ef >M $0cc8 $16 >M $ad90 $f8 >M
T{ op PC S A X Y P -> $0cc8 $ef $45 $7c $4c $64 }T T{ $00ef M $00f0 M $0cc6 M $0cc7 M $0cc8 M $ad90 M -> $90 $ad $d2 $ef $16 $f8 }T
$b48a >PC $78 >S $c7 >A $d1 >X $1a >Y $20 >P $00c8 $72 >M $00c9 $3f >M $3f72 $c9 >M $b48a $d2 >M $b48b $c8 >M $b48c $ca >M
T{ op PC S A X Y P -> $b48c $78 $c7 $d1 $1a $a0 }T T{ $00c8 M $00c9 M $3f72 M $b48a M $b48b M $b48c M -> $72 $3f $c9 $d2 $c8 $ca }T
$612f >PC $6d >S $38 >A $72 >X $c6 >Y $65 >P $0020 $7b >M $0021 $42 >M $427b $c8 >M $612f $d2 >M $6130 $20 >M $6131 $45 >M
T{ op PC S A X Y P -> $6131 $6d $38 $72 $c6 $64 }T T{ $0020 M $0021 M $427b M $612f M $6130 M $6131 M -> $7b $42 $c8 $d2 $20 $45 }T
$0410 >PC $c0 >S $3a >A $2f >X $17 >Y $a1 >P $00be $58 >M $00bf $60 >M $0410 $d2 >M $0411 $be >M $0412 $ad >M $6058 $d1 >M
T{ op PC S A X Y P -> $0412 $c0 $3a $2f $17 $20 }T T{ $00be M $00bf M $0410 M $0411 M $0412 M $6058 M -> $58 $60 $d2 $be $ad $d1 }T
( d3 )
$a889 >PC $f2 >S $db >A $c8 >X $64 >Y $20 >P $a889 $d3 >M $a88a $30 >M $a88b $cb >M
T{ op PC S A X Y P -> $a88a $f2 $db $c8 $64 $20 }T T{ $a889 M $a88a M $a88b M -> $d3 $30 $cb }T
$dbb5 >PC $84 >S $6f >A $25 >X $06 >Y $a2 >P $dbb5 $d3 >M $dbb6 $dc >M $dbb7 $a9 >M
T{ op PC S A X Y P -> $dbb6 $84 $6f $25 $06 $a2 }T T{ $dbb5 M $dbb6 M $dbb7 M -> $d3 $dc $a9 }T
$5222 >PC $7e >S $92 >A $8e >X $78 >Y $e6 >P $5222 $d3 >M $5223 $b7 >M $5224 $ec >M
T{ op PC S A X Y P -> $5223 $7e $92 $8e $78 $e6 }T T{ $5222 M $5223 M $5224 M -> $d3 $b7 $ec }T
$1ab0 >PC $ec >S $ab >A $65 >X $db >Y $28 >P $1ab0 $d3 >M $1ab1 $44 >M $1ab2 $90 >M
T{ op PC S A X Y P -> $1ab1 $ec $ab $65 $db $28 }T T{ $1ab0 M $1ab1 M $1ab2 M -> $d3 $44 $90 }T
$100a >PC $ca >S $5f >A $18 >X $f1 >Y $ec >P $100a $d3 >M $100b $3c >M $100c $82 >M
T{ op PC S A X Y P -> $100b $ca $5f $18 $f1 $ec }T T{ $100a M $100b M $100c M -> $d3 $3c $82 }T
$aab4 >PC $9d >S $96 >A $74 >X $a9 >Y $6e >P $aab4 $d3 >M $aab5 $a4 >M $aab6 $6e >M
T{ op PC S A X Y P -> $aab5 $9d $96 $74 $a9 $6e }T T{ $aab4 M $aab5 M $aab6 M -> $d3 $a4 $6e }T
$b91f >PC $f7 >S $cf >A $47 >X $69 >Y $61 >P $b91f $d3 >M $b920 $22 >M $b921 $43 >M
T{ op PC S A X Y P -> $b920 $f7 $cf $47 $69 $61 }T T{ $b91f M $b920 M $b921 M -> $d3 $22 $43 }T
$4a75 >PC $47 >S $ab >A $90 >X $8f >Y $a6 >P $4a75 $d3 >M $4a76 $99 >M $4a77 $d8 >M
T{ op PC S A X Y P -> $4a76 $47 $ab $90 $8f $a6 }T T{ $4a75 M $4a76 M $4a77 M -> $d3 $99 $d8 }T
$aaae >PC $94 >S $14 >A $b7 >X $ab >Y $29 >P $aaae $d3 >M $aaaf $bc >M $aab0 $c0 >M
T{ op PC S A X Y P -> $aaaf $94 $14 $b7 $ab $29 }T T{ $aaae M $aaaf M $aab0 M -> $d3 $bc $c0 }T
$d2e1 >PC $33 >S $aa >A $b2 >X $b3 >Y $a7 >P $d2e1 $d3 >M $d2e2 $fc >M $d2e3 $aa >M
T{ op PC S A X Y P -> $d2e2 $33 $aa $b2 $b3 $a7 }T T{ $d2e1 M $d2e2 M $d2e3 M -> $d3 $fc $aa }T
$dee1 >PC $d8 >S $c9 >A $83 >X $b8 >Y $25 >P $dee1 $d3 >M $dee2 $a4 >M $dee3 $a8 >M
T{ op PC S A X Y P -> $dee2 $d8 $c9 $83 $b8 $25 }T T{ $dee1 M $dee2 M $dee3 M -> $d3 $a4 $a8 }T
$9de6 >PC $48 >S $e0 >A $7e >X $61 >Y $63 >P $9de6 $d3 >M $9de7 $c3 >M $9de8 $ac >M
T{ op PC S A X Y P -> $9de7 $48 $e0 $7e $61 $63 }T T{ $9de6 M $9de7 M $9de8 M -> $d3 $c3 $ac }T
$4012 >PC $55 >S $fa >A $db >X $3d >Y $69 >P $4012 $d3 >M $4013 $e6 >M $4014 $a7 >M
T{ op PC S A X Y P -> $4013 $55 $fa $db $3d $69 }T T{ $4012 M $4013 M $4014 M -> $d3 $e6 $a7 }T
$a287 >PC $f7 >S $0c >A $fd >X $30 >Y $a2 >P $a287 $d3 >M $a288 $b9 >M $a289 $de >M
T{ op PC S A X Y P -> $a288 $f7 $0c $fd $30 $a2 }T T{ $a287 M $a288 M $a289 M -> $d3 $b9 $de }T
$bbf1 >PC $8f >S $ec >A $f2 >X $29 >Y $e3 >P $bbf1 $d3 >M $bbf2 $7f >M $bbf3 $4d >M
T{ op PC S A X Y P -> $bbf2 $8f $ec $f2 $29 $e3 }T T{ $bbf1 M $bbf2 M $bbf3 M -> $d3 $7f $4d }T
$b4e5 >PC $de >S $25 >A $cf >X $e6 >Y $ab >P $b4e5 $d3 >M $b4e6 $ed >M $b4e7 $40 >M
T{ op PC S A X Y P -> $b4e6 $de $25 $cf $e6 $ab }T T{ $b4e5 M $b4e6 M $b4e7 M -> $d3 $ed $40 }T
( d4 )
$783a >PC $63 >S $fd >A $19 >X $63 >Y $60 >P $00d5 $93 >M $00ee $dc >M $783a $d4 >M $783b $d5 >M $783c $af >M
T{ op PC S A X Y P -> $783c $63 $fd $19 $63 $60 }T T{ $00d5 M $00ee M $783a M $783b M $783c M -> $93 $dc $d4 $d5 $af }T
$4cd7 >PC $28 >S $fe >A $32 >X $78 >Y $66 >P $0015 $40 >M $00e3 $83 >M $4cd7 $d4 >M $4cd8 $e3 >M $4cd9 $20 >M
T{ op PC S A X Y P -> $4cd9 $28 $fe $32 $78 $66 }T T{ $0015 M $00e3 M $4cd7 M $4cd8 M $4cd9 M -> $40 $83 $d4 $e3 $20 }T
$3db7 >PC $37 >S $4d >A $27 >X $6f >Y $a1 >P $0003 $12 >M $00dc $bd >M $3db7 $d4 >M $3db8 $dc >M $3db9 $06 >M
T{ op PC S A X Y P -> $3db9 $37 $4d $27 $6f $a1 }T T{ $0003 M $00dc M $3db7 M $3db8 M $3db9 M -> $12 $bd $d4 $dc $06 }T
$9996 >PC $d9 >S $2a >A $88 >X $d0 >Y $ac >P $004e $bf >M $00d6 $ca >M $9996 $d4 >M $9997 $4e >M $9998 $17 >M
T{ op PC S A X Y P -> $9998 $d9 $2a $88 $d0 $ac }T T{ $004e M $00d6 M $9996 M $9997 M $9998 M -> $bf $ca $d4 $4e $17 }T
$58f2 >PC $67 >S $07 >A $93 >X $af >Y $25 >P $003b $5c >M $00a8 $ec >M $58f2 $d4 >M $58f3 $a8 >M $58f4 $a5 >M
T{ op PC S A X Y P -> $58f4 $67 $07 $93 $af $25 }T T{ $003b M $00a8 M $58f2 M $58f3 M $58f4 M -> $5c $ec $d4 $a8 $a5 }T
$8aec >PC $e6 >S $5b >A $0a >X $0a >Y $68 >P $00f2 $8c >M $00fc $af >M $8aec $d4 >M $8aed $f2 >M $8aee $0e >M
T{ op PC S A X Y P -> $8aee $e6 $5b $0a $0a $68 }T T{ $00f2 M $00fc M $8aec M $8aed M $8aee M -> $8c $af $d4 $f2 $0e }T
$2ca9 >PC $ab >S $9c >A $3f >X $28 >Y $68 >P $0026 $b9 >M $0065 $81 >M $2ca9 $d4 >M $2caa $26 >M $2cab $38 >M
T{ op PC S A X Y P -> $2cab $ab $9c $3f $28 $68 }T T{ $0026 M $0065 M $2ca9 M $2caa M $2cab M -> $b9 $81 $d4 $26 $38 }T
$1291 >PC $46 >S $b5 >A $df >X $c9 >Y $67 >P $0048 $32 >M $0069 $14 >M $1291 $d4 >M $1292 $69 >M $1293 $fe >M
T{ op PC S A X Y P -> $1293 $46 $b5 $df $c9 $67 }T T{ $0048 M $0069 M $1291 M $1292 M $1293 M -> $32 $14 $d4 $69 $fe }T
$e5ed >PC $05 >S $cc >A $d2 >X $2c >Y $e4 >P $00a1 $e5 >M $00cf $6b >M $e5ed $d4 >M $e5ee $cf >M $e5ef $36 >M
T{ op PC S A X Y P -> $e5ef $05 $cc $d2 $2c $e4 }T T{ $00a1 M $00cf M $e5ed M $e5ee M $e5ef M -> $e5 $6b $d4 $cf $36 }T
$0552 >PC $23 >S $bc >A $63 >X $25 >Y $a0 >P $0067 $e7 >M $00ca $cb >M $0552 $d4 >M $0553 $67 >M $0554 $be >M
T{ op PC S A X Y P -> $0554 $23 $bc $63 $25 $a0 }T T{ $0067 M $00ca M $0552 M $0553 M $0554 M -> $e7 $cb $d4 $67 $be }T
$d17b >PC $79 >S $64 >A $85 >X $23 >Y $2c >P $000a $5a >M $0085 $93 >M $d17b $d4 >M $d17c $85 >M $d17d $d3 >M
T{ op PC S A X Y P -> $d17d $79 $64 $85 $23 $2c }T T{ $000a M $0085 M $d17b M $d17c M $d17d M -> $5a $93 $d4 $85 $d3 }T
$bf1a >PC $5a >S $76 >A $86 >X $8c >Y $a8 >P $004d $3e >M $00d3 $e2 >M $bf1a $d4 >M $bf1b $4d >M $bf1c $0e >M
T{ op PC S A X Y P -> $bf1c $5a $76 $86 $8c $a8 }T T{ $004d M $00d3 M $bf1a M $bf1b M $bf1c M -> $3e $e2 $d4 $4d $0e }T
$45e0 >PC $82 >S $20 >A $6a >X $08 >Y $28 >P $002c $03 >M $00c2 $c8 >M $45e0 $d4 >M $45e1 $c2 >M $45e2 $3f >M
T{ op PC S A X Y P -> $45e2 $82 $20 $6a $08 $28 }T T{ $002c M $00c2 M $45e0 M $45e1 M $45e2 M -> $03 $c8 $d4 $c2 $3f }T
$d999 >PC $af >S $d4 >A $db >X $f3 >Y $aa >P $003a $c7 >M $005f $f8 >M $d999 $d4 >M $d99a $5f >M $d99b $a8 >M
T{ op PC S A X Y P -> $d99b $af $d4 $db $f3 $aa }T T{ $003a M $005f M $d999 M $d99a M $d99b M -> $c7 $f8 $d4 $5f $a8 }T
$3395 >PC $71 >S $ad >A $bc >X $22 >Y $2d >P $0089 $32 >M $00cd $d5 >M $3395 $d4 >M $3396 $cd >M $3397 $5a >M
T{ op PC S A X Y P -> $3397 $71 $ad $bc $22 $2d }T T{ $0089 M $00cd M $3395 M $3396 M $3397 M -> $32 $d5 $d4 $cd $5a }T
$7043 >PC $78 >S $e4 >A $5b >X $11 >Y $e7 >P $006e $33 >M $00c9 $fc >M $7043 $d4 >M $7044 $6e >M $7045 $94 >M
T{ op PC S A X Y P -> $7045 $78 $e4 $5b $11 $e7 }T T{ $006e M $00c9 M $7043 M $7044 M $7045 M -> $33 $fc $d4 $6e $94 }T
( d5 )
$058f >PC $53 >S $9f >A $23 >X $95 >Y $6d >P $0093 $90 >M $00b6 $8f >M $058f $d5 >M $0590 $93 >M $0591 $ba >M
T{ op PC S A X Y P -> $0591 $53 $9f $23 $95 $6d }T T{ $0093 M $00b6 M $058f M $0590 M $0591 M -> $90 $8f $d5 $93 $ba }T
$51a2 >PC $1f >S $b0 >A $d0 >X $fc >Y $af >P $0052 $a4 >M $0082 $b3 >M $51a2 $d5 >M $51a3 $82 >M $51a4 $45 >M
T{ op PC S A X Y P -> $51a4 $1f $b0 $d0 $fc $2d }T T{ $0052 M $0082 M $51a2 M $51a3 M $51a4 M -> $a4 $b3 $d5 $82 $45 }T
$c276 >PC $b1 >S $9e >A $56 >X $58 >Y $24 >P $0015 $99 >M $00bf $6f >M $c276 $d5 >M $c277 $bf >M $c278 $41 >M
T{ op PC S A X Y P -> $c278 $b1 $9e $56 $58 $25 }T T{ $0015 M $00bf M $c276 M $c277 M $c278 M -> $99 $6f $d5 $bf $41 }T
$bd99 >PC $47 >S $dc >A $98 >X $bf >Y $e2 >P $0036 $91 >M $009e $6b >M $bd99 $d5 >M $bd9a $9e >M $bd9b $38 >M
T{ op PC S A X Y P -> $bd9b $47 $dc $98 $bf $61 }T T{ $0036 M $009e M $bd99 M $bd9a M $bd9b M -> $91 $6b $d5 $9e $38 }T
$b963 >PC $48 >S $3a >A $7d >X $05 >Y $6c >P $0044 $f3 >M $00c7 $74 >M $b963 $d5 >M $b964 $c7 >M $b965 $be >M
T{ op PC S A X Y P -> $b965 $48 $3a $7d $05 $6c }T T{ $0044 M $00c7 M $b963 M $b964 M $b965 M -> $f3 $74 $d5 $c7 $be }T
$2c86 >PC $8c >S $c1 >A $99 >X $a6 >Y $e7 >P $006d $1c >M $00d4 $8a >M $2c86 $d5 >M $2c87 $d4 >M $2c88 $62 >M
T{ op PC S A X Y P -> $2c88 $8c $c1 $99 $a6 $e5 }T T{ $006d M $00d4 M $2c86 M $2c87 M $2c88 M -> $1c $8a $d5 $d4 $62 }T
$214b >PC $1a >S $a7 >A $f5 >X $98 >Y $a5 >P $001d $0e >M $0028 $7d >M $214b $d5 >M $214c $28 >M $214d $c2 >M
T{ op PC S A X Y P -> $214d $1a $a7 $f5 $98 $a5 }T T{ $001d M $0028 M $214b M $214c M $214d M -> $0e $7d $d5 $28 $c2 }T
$b509 >PC $a5 >S $80 >A $ac >X $f7 >Y $6c >P $00a1 $d6 >M $00f5 $8f >M $b509 $d5 >M $b50a $f5 >M $b50b $53 >M
T{ op PC S A X Y P -> $b50b $a5 $80 $ac $f7 $ec }T T{ $00a1 M $00f5 M $b509 M $b50a M $b50b M -> $d6 $8f $d5 $f5 $53 }T
$296d >PC $a3 >S $96 >A $5d >X $a6 >Y $e4 >P $0065 $e2 >M $00c2 $34 >M $296d $d5 >M $296e $65 >M $296f $10 >M
T{ op PC S A X Y P -> $296f $a3 $96 $5d $a6 $65 }T T{ $0065 M $00c2 M $296d M $296e M $296f M -> $e2 $34 $d5 $65 $10 }T
$1ee7 >PC $f0 >S $05 >A $1b >X $e8 >Y $63 >P $0023 $96 >M $003e $8c >M $1ee7 $d5 >M $1ee8 $23 >M $1ee9 $ac >M
T{ op PC S A X Y P -> $1ee9 $f0 $05 $1b $e8 $60 }T T{ $0023 M $003e M $1ee7 M $1ee8 M $1ee9 M -> $96 $8c $d5 $23 $ac }T
$b860 >PC $df >S $b4 >A $9e >X $26 >Y $2e >P $000a $ac >M $00a8 $87 >M $b860 $d5 >M $b861 $0a >M $b862 $4d >M
T{ op PC S A X Y P -> $b862 $df $b4 $9e $26 $2d }T T{ $000a M $00a8 M $b860 M $b861 M $b862 M -> $ac $87 $d5 $0a $4d }T
$89d1 >PC $54 >S $fa >A $c3 >X $03 >Y $ee >P $0039 $c2 >M $00fc $18 >M $89d1 $d5 >M $89d2 $39 >M $89d3 $c9 >M
T{ op PC S A X Y P -> $89d3 $54 $fa $c3 $03 $ed }T T{ $0039 M $00fc M $89d1 M $89d2 M $89d3 M -> $c2 $18 $d5 $39 $c9 }T
$e9a3 >PC $4c >S $06 >A $62 >X $a5 >Y $61 >P $0095 $e4 >M $00f7 $5e >M $e9a3 $d5 >M $e9a4 $95 >M $e9a5 $77 >M
T{ op PC S A X Y P -> $e9a5 $4c $06 $62 $a5 $e0 }T T{ $0095 M $00f7 M $e9a3 M $e9a4 M $e9a5 M -> $e4 $5e $d5 $95 $77 }T
$8252 >PC $4a >S $4c >A $08 >X $67 >Y $eb >P $00dc $65 >M $00e4 $16 >M $8252 $d5 >M $8253 $dc >M $8254 $fa >M
T{ op PC S A X Y P -> $8254 $4a $4c $08 $67 $69 }T T{ $00dc M $00e4 M $8252 M $8253 M $8254 M -> $65 $16 $d5 $dc $fa }T
$a8c2 >PC $57 >S $79 >A $c6 >X $42 >Y $a7 >P $00b9 $02 >M $00f3 $e6 >M $a8c2 $d5 >M $a8c3 $f3 >M $a8c4 $8b >M
T{ op PC S A X Y P -> $a8c4 $57 $79 $c6 $42 $25 }T T{ $00b9 M $00f3 M $a8c2 M $a8c3 M $a8c4 M -> $02 $e6 $d5 $f3 $8b }T
$9ad0 >PC $cf >S $67 >A $41 >X $58 >Y $64 >P $001c $b9 >M $00db $a1 >M $9ad0 $d5 >M $9ad1 $db >M $9ad2 $cf >M
T{ op PC S A X Y P -> $9ad2 $cf $67 $41 $58 $e4 }T T{ $001c M $00db M $9ad0 M $9ad1 M $9ad2 M -> $b9 $a1 $d5 $db $cf }T
( d6 )
$95c0 >PC $14 >S $78 >A $41 >X $95 >Y $2c >P $001d $64 >M $00dc $2f >M $95c0 $d6 >M $95c1 $dc >M $95c2 $9f >M
T{ op PC S A X Y P -> $95c2 $14 $78 $41 $95 $2c }T T{ $001d M $00dc M $95c0 M $95c1 M $95c2 M -> $63 $2f $d6 $dc $9f }T
$0291 >PC $c6 >S $58 >A $93 >X $d2 >Y $e7 >P $0081 $bc >M $00ee $21 >M $0291 $d6 >M $0292 $ee >M $0293 $8d >M
T{ op PC S A X Y P -> $0293 $c6 $58 $93 $d2 $e5 }T T{ $0081 M $00ee M $0291 M $0292 M $0293 M -> $bb $21 $d6 $ee $8d }T
$c0f1 >PC $56 >S $d8 >A $2e >X $6b >Y $a8 >P $00ad $84 >M $00db $31 >M $c0f1 $d6 >M $c0f2 $ad >M $c0f3 $a4 >M
T{ op PC S A X Y P -> $c0f3 $56 $d8 $2e $6b $28 }T T{ $00ad M $00db M $c0f1 M $c0f2 M $c0f3 M -> $84 $30 $d6 $ad $a4 }T
$1dfa >PC $16 >S $62 >A $41 >X $c0 >Y $28 >P $0012 $a6 >M $00d1 $06 >M $1dfa $d6 >M $1dfb $d1 >M $1dfc $6e >M
T{ op PC S A X Y P -> $1dfc $16 $62 $41 $c0 $a8 }T T{ $0012 M $00d1 M $1dfa M $1dfb M $1dfc M -> $a5 $06 $d6 $d1 $6e }T
$9932 >PC $d3 >S $9d >A $42 >X $e9 >Y $e9 >P $00a4 $91 >M $00e6 $8e >M $9932 $d6 >M $9933 $a4 >M $9934 $13 >M
T{ op PC S A X Y P -> $9934 $d3 $9d $42 $e9 $e9 }T T{ $00a4 M $00e6 M $9932 M $9933 M $9934 M -> $91 $8d $d6 $a4 $13 }T
$91d8 >PC $20 >S $90 >A $6b >X $68 >Y $65 >P $001a $30 >M $0085 $64 >M $91d8 $d6 >M $91d9 $1a >M $91da $00 >M
T{ op PC S A X Y P -> $91da $20 $90 $6b $68 $65 }T T{ $001a M $0085 M $91d8 M $91d9 M $91da M -> $30 $63 $d6 $1a $00 }T
$32d3 >PC $4f >S $bd >A $72 >X $68 >Y $ea >P $005a $52 >M $00cc $98 >M $32d3 $d6 >M $32d4 $5a >M $32d5 $c6 >M
T{ op PC S A X Y P -> $32d5 $4f $bd $72 $68 $e8 }T T{ $005a M $00cc M $32d3 M $32d4 M $32d5 M -> $52 $97 $d6 $5a $c6 }T
$7b2e >PC $8d >S $9f >A $d8 >X $71 >Y $e2 >P $00bc $a5 >M $00e4 $a8 >M $7b2e $d6 >M $7b2f $e4 >M $7b30 $0c >M
T{ op PC S A X Y P -> $7b30 $8d $9f $d8 $71 $e0 }T T{ $00bc M $00e4 M $7b2e M $7b2f M $7b30 M -> $a4 $a8 $d6 $e4 $0c }T
$e53f >PC $90 >S $75 >A $d1 >X $12 >Y $ab >P $00c8 $75 >M $00f7 $15 >M $e53f $d6 >M $e540 $f7 >M $e541 $3c >M
T{ op PC S A X Y P -> $e541 $90 $75 $d1 $12 $29 }T T{ $00c8 M $00f7 M $e53f M $e540 M $e541 M -> $74 $15 $d6 $f7 $3c }T
$005f >PC $2f >S $1c >A $d1 >X $c2 >Y $e8 >P $000b $87 >M $003a $71 >M $005f $d6 >M $0060 $3a >M $0061 $73 >M
T{ op PC S A X Y P -> $0061 $2f $1c $d1 $c2 $e8 }T T{ $000b M $003a M $005f M $0060 M $0061 M -> $86 $71 $d6 $3a $73 }T
$6a67 >PC $d0 >S $68 >A $f6 >X $eb >Y $a4 >P $00ef $10 >M $00f9 $18 >M $6a67 $d6 >M $6a68 $f9 >M $6a69 $6f >M
T{ op PC S A X Y P -> $6a69 $d0 $68 $f6 $eb $24 }T T{ $00ef M $00f9 M $6a67 M $6a68 M $6a69 M -> $0f $18 $d6 $f9 $6f }T
$609a >PC $da >S $a2 >A $a3 >X $5f >Y $e4 >P $004d $18 >M $00aa $8b >M $609a $d6 >M $609b $aa >M $609c $a8 >M
T{ op PC S A X Y P -> $609c $da $a2 $a3 $5f $64 }T T{ $004d M $00aa M $609a M $609b M $609c M -> $17 $8b $d6 $aa $a8 }T
$fd06 >PC $8e >S $b4 >A $02 >X $6b >Y $24 >P $00ee $b0 >M $00f0 $1a >M $fd06 $d6 >M $fd07 $ee >M $fd08 $0a >M
T{ op PC S A X Y P -> $fd08 $8e $b4 $02 $6b $24 }T T{ $00ee M $00f0 M $fd06 M $fd07 M $fd08 M -> $b0 $19 $d6 $ee $0a }T
$0a57 >PC $6a >S $99 >A $69 >X $c0 >Y $20 >P $006b $1b >M $00d4 $df >M $0a57 $d6 >M $0a58 $6b >M $0a59 $2d >M
T{ op PC S A X Y P -> $0a59 $6a $99 $69 $c0 $a0 }T T{ $006b M $00d4 M $0a57 M $0a58 M $0a59 M -> $1b $de $d6 $6b $2d }T
$266f >PC $d2 >S $e5 >A $21 >X $23 >Y $e9 >P $001b $10 >M $003c $dc >M $266f $d6 >M $2670 $1b >M $2671 $22 >M
T{ op PC S A X Y P -> $2671 $d2 $e5 $21 $23 $e9 }T T{ $001b M $003c M $266f M $2670 M $2671 M -> $10 $db $d6 $1b $22 }T
$7315 >PC $0b >S $97 >A $a0 >X $87 >Y $24 >P $0001 $08 >M $00a1 $23 >M $7315 $d6 >M $7316 $01 >M $7317 $1c >M
T{ op PC S A X Y P -> $7317 $0b $97 $a0 $87 $24 }T T{ $0001 M $00a1 M $7315 M $7316 M $7317 M -> $08 $22 $d6 $01 $1c }T
( d7 )
$ffdd >PC $fa >S $7b >A $be >X $53 >Y $67 >P $00c4 $df >M $ffdd $d7 >M $ffde $c4 >M $ffdf $ca >M
T{ op PC S A X Y P -> $ffdf $fa $7b $be $53 $67 }T T{ $00c4 M $ffdd M $ffde M $ffdf M -> $ff $d7 $c4 $ca }T
$58a0 >PC $16 >S $e0 >A $20 >X $dd >Y $e6 >P $00e4 $f9 >M $58a0 $d7 >M $58a1 $e4 >M $58a2 $1a >M
T{ op PC S A X Y P -> $58a2 $16 $e0 $20 $dd $e6 }T T{ $00e4 M $58a0 M $58a1 M $58a2 M -> $f9 $d7 $e4 $1a }T
$0b4c >PC $1d >S $d0 >A $7e >X $89 >Y $24 >P $0082 $74 >M $0b4c $d7 >M $0b4d $82 >M $0b4e $72 >M
T{ op PC S A X Y P -> $0b4e $1d $d0 $7e $89 $24 }T T{ $0082 M $0b4c M $0b4d M $0b4e M -> $74 $d7 $82 $72 }T
$802d >PC $e1 >S $69 >A $bf >X $13 >Y $64 >P $0067 $b1 >M $802d $d7 >M $802e $67 >M $802f $11 >M
T{ op PC S A X Y P -> $802f $e1 $69 $bf $13 $64 }T T{ $0067 M $802d M $802e M $802f M -> $b1 $d7 $67 $11 }T
$f552 >PC $99 >S $e3 >A $a0 >X $3b >Y $6d >P $00ae $23 >M $f552 $d7 >M $f553 $ae >M $f554 $46 >M
T{ op PC S A X Y P -> $f554 $99 $e3 $a0 $3b $6d }T T{ $00ae M $f552 M $f553 M $f554 M -> $23 $d7 $ae $46 }T
$7703 >PC $8d >S $4b >A $85 >X $8b >Y $a7 >P $00f3 $05 >M $7703 $d7 >M $7704 $f3 >M $7705 $9c >M
T{ op PC S A X Y P -> $7705 $8d $4b $85 $8b $a7 }T T{ $00f3 M $7703 M $7704 M $7705 M -> $25 $d7 $f3 $9c }T
$161e >PC $f4 >S $b9 >A $17 >X $02 >Y $e5 >P $0085 $bb >M $161e $d7 >M $161f $85 >M $1620 $a1 >M
T{ op PC S A X Y P -> $1620 $f4 $b9 $17 $02 $e5 }T T{ $0085 M $161e M $161f M $1620 M -> $bb $d7 $85 $a1 }T
$5ee4 >PC $3d >S $fd >A $d4 >X $cc >Y $2a >P $001a $69 >M $5ee4 $d7 >M $5ee5 $1a >M $5ee6 $3a >M
T{ op PC S A X Y P -> $5ee6 $3d $fd $d4 $cc $2a }T T{ $001a M $5ee4 M $5ee5 M $5ee6 M -> $69 $d7 $1a $3a }T
$9b1f >PC $c4 >S $78 >A $45 >X $95 >Y $2d >P $00ef $d3 >M $9b1f $d7 >M $9b20 $ef >M $9b21 $0c >M
T{ op PC S A X Y P -> $9b21 $c4 $78 $45 $95 $2d }T T{ $00ef M $9b1f M $9b20 M $9b21 M -> $f3 $d7 $ef $0c }T
$f095 >PC $59 >S $71 >A $45 >X $4c >Y $af >P $006f $e2 >M $f095 $d7 >M $f096 $6f >M $f097 $2e >M
T{ op PC S A X Y P -> $f097 $59 $71 $45 $4c $af }T T{ $006f M $f095 M $f096 M $f097 M -> $e2 $d7 $6f $2e }T
$4b8b >PC $b5 >S $25 >A $e6 >X $fd >Y $6b >P $0066 $ec >M $4b8b $d7 >M $4b8c $66 >M $4b8d $a8 >M
T{ op PC S A X Y P -> $4b8d $b5 $25 $e6 $fd $6b }T T{ $0066 M $4b8b M $4b8c M $4b8d M -> $ec $d7 $66 $a8 }T
$c6e5 >PC $09 >S $0f >A $db >X $fd >Y $e1 >P $00a4 $30 >M $c6e5 $d7 >M $c6e6 $a4 >M $c6e7 $fc >M
T{ op PC S A X Y P -> $c6e7 $09 $0f $db $fd $e1 }T T{ $00a4 M $c6e5 M $c6e6 M $c6e7 M -> $30 $d7 $a4 $fc }T
$13d5 >PC $2a >S $00 >A $1a >X $59 >Y $a1 >P $00b6 $7f >M $13d5 $d7 >M $13d6 $b6 >M $13d7 $df >M
T{ op PC S A X Y P -> $13d7 $2a $00 $1a $59 $a1 }T T{ $00b6 M $13d5 M $13d6 M $13d7 M -> $7f $d7 $b6 $df }T
$b469 >PC $92 >S $f5 >A $49 >X $8e >Y $64 >P $00a6 $6e >M $b469 $d7 >M $b46a $a6 >M $b46b $a1 >M
T{ op PC S A X Y P -> $b46b $92 $f5 $49 $8e $64 }T T{ $00a6 M $b469 M $b46a M $b46b M -> $6e $d7 $a6 $a1 }T
$a1c3 >PC $ce >S $c6 >A $1a >X $1d >Y $67 >P $00c5 $a6 >M $a1c3 $d7 >M $a1c4 $c5 >M $a1c5 $df >M
T{ op PC S A X Y P -> $a1c5 $ce $c6 $1a $1d $67 }T T{ $00c5 M $a1c3 M $a1c4 M $a1c5 M -> $a6 $d7 $c5 $df }T
$4eee >PC $fd >S $56 >A $0d >X $1c >Y $2d >P $001e $95 >M $4eee $d7 >M $4eef $1e >M $4ef0 $94 >M
T{ op PC S A X Y P -> $4ef0 $fd $56 $0d $1c $2d }T T{ $001e M $4eee M $4eef M $4ef0 M -> $b5 $d7 $1e $94 }T
( d8 )
$f590 >PC $af >S $47 >A $4f >X $9b >Y $6b >P $f590 $d8 >M $f591 $ad >M $f592 $18 >M
T{ op PC S A X Y P -> $f591 $af $47 $4f $9b $63 }T T{ $f590 M $f591 M $f592 M -> $d8 $ad $18 }T
$12ec >PC $ac >S $93 >A $f8 >X $d1 >Y $66 >P $12ec $d8 >M $12ed $f2 >M $12ee $d6 >M
T{ op PC S A X Y P -> $12ed $ac $93 $f8 $d1 $66 }T T{ $12ec M $12ed M $12ee M -> $d8 $f2 $d6 }T
$a1ec >PC $69 >S $af >A $13 >X $bf >Y $65 >P $a1ec $d8 >M $a1ed $f9 >M $a1ee $96 >M
T{ op PC S A X Y P -> $a1ed $69 $af $13 $bf $65 }T T{ $a1ec M $a1ed M $a1ee M -> $d8 $f9 $96 }T
$0165 >PC $a6 >S $d2 >A $f3 >X $be >Y $e1 >P $0165 $d8 >M $0166 $1d >M $0167 $25 >M
T{ op PC S A X Y P -> $0166 $a6 $d2 $f3 $be $e1 }T T{ $0165 M $0166 M $0167 M -> $d8 $1d $25 }T
$4606 >PC $dc >S $da >A $dd >X $56 >Y $ab >P $4606 $d8 >M $4607 $8c >M $4608 $64 >M
T{ op PC S A X Y P -> $4607 $dc $da $dd $56 $a3 }T T{ $4606 M $4607 M $4608 M -> $d8 $8c $64 }T
$275f >PC $18 >S $21 >A $fc >X $eb >Y $ab >P $275f $d8 >M $2760 $28 >M $2761 $19 >M
T{ op PC S A X Y P -> $2760 $18 $21 $fc $eb $a3 }T T{ $275f M $2760 M $2761 M -> $d8 $28 $19 }T
$d98d >PC $1d >S $aa >A $cd >X $cd >Y $ac >P $d98d $d8 >M $d98e $19 >M $d98f $88 >M
T{ op PC S A X Y P -> $d98e $1d $aa $cd $cd $a4 }T T{ $d98d M $d98e M $d98f M -> $d8 $19 $88 }T
$0c23 >PC $14 >S $05 >A $66 >X $9a >Y $e5 >P $0c23 $d8 >M $0c24 $fc >M $0c25 $f4 >M
T{ op PC S A X Y P -> $0c24 $14 $05 $66 $9a $e5 }T T{ $0c23 M $0c24 M $0c25 M -> $d8 $fc $f4 }T
$7828 >PC $fb >S $c3 >A $03 >X $dc >Y $a7 >P $7828 $d8 >M $7829 $68 >M $782a $d2 >M
T{ op PC S A X Y P -> $7829 $fb $c3 $03 $dc $a7 }T T{ $7828 M $7829 M $782a M -> $d8 $68 $d2 }T
$27b4 >PC $ef >S $d6 >A $3c >X $10 >Y $a0 >P $27b4 $d8 >M $27b5 $e9 >M $27b6 $90 >M
T{ op PC S A X Y P -> $27b5 $ef $d6 $3c $10 $a0 }T T{ $27b4 M $27b5 M $27b6 M -> $d8 $e9 $90 }T
$fcf9 >PC $56 >S $66 >A $24 >X $3e >Y $66 >P $fcf9 $d8 >M $fcfa $b3 >M $fcfb $a7 >M
T{ op PC S A X Y P -> $fcfa $56 $66 $24 $3e $66 }T T{ $fcf9 M $fcfa M $fcfb M -> $d8 $b3 $a7 }T
$c264 >PC $d3 >S $2f >A $9f >X $b6 >Y $eb >P $c264 $d8 >M $c265 $4b >M $c266 $74 >M
T{ op PC S A X Y P -> $c265 $d3 $2f $9f $b6 $e3 }T T{ $c264 M $c265 M $c266 M -> $d8 $4b $74 }T
$0c51 >PC $fe >S $9f >A $73 >X $6f >Y $a5 >P $0c51 $d8 >M $0c52 $58 >M $0c53 $0f >M
T{ op PC S A X Y P -> $0c52 $fe $9f $73 $6f $a5 }T T{ $0c51 M $0c52 M $0c53 M -> $d8 $58 $0f }T
$095f >PC $82 >S $d3 >A $7e >X $c0 >Y $67 >P $095f $d8 >M $0960 $9e >M $0961 $5c >M
T{ op PC S A X Y P -> $0960 $82 $d3 $7e $c0 $67 }T T{ $095f M $0960 M $0961 M -> $d8 $9e $5c }T
$51da >PC $29 >S $6c >A $6d >X $13 >Y $af >P $51da $d8 >M $51db $27 >M $51dc $03 >M
T{ op PC S A X Y P -> $51db $29 $6c $6d $13 $a7 }T T{ $51da M $51db M $51dc M -> $d8 $27 $03 }T
$6634 >PC $ba >S $c3 >A $68 >X $02 >Y $64 >P $6634 $d8 >M $6635 $58 >M $6636 $17 >M
T{ op PC S A X Y P -> $6635 $ba $c3 $68 $02 $64 }T T{ $6634 M $6635 M $6636 M -> $d8 $58 $17 }T
( d9 )
$056b >PC $09 >S $c0 >A $0e >X $e5 >Y $62 >P $056b $d9 >M $056c $c4 >M $056d $22 >M $056e $b7 >M $23a9 $a3 >M
T{ op PC S A X Y P -> $056e $09 $c0 $0e $e5 $61 }T T{ $056b M $056c M $056d M $056e M $23a9 M -> $d9 $c4 $22 $b7 $a3 }T
$a950 >PC $3e >S $0f >A $a7 >X $23 >Y $ec >P $4542 $23 >M $a950 $d9 >M $a951 $1f >M $a952 $45 >M $a953 $fa >M
T{ op PC S A X Y P -> $a953 $3e $0f $a7 $23 $ec }T T{ $4542 M $a950 M $a951 M $a952 M $a953 M -> $23 $d9 $1f $45 $fa }T
$68d1 >PC $53 >S $c0 >A $5a >X $a5 >Y $24 >P $2930 $22 >M $68d1 $d9 >M $68d2 $8b >M $68d3 $28 >M $68d4 $12 >M
T{ op PC S A X Y P -> $68d4 $53 $c0 $5a $a5 $a5 }T T{ $2930 M $68d1 M $68d2 M $68d3 M $68d4 M -> $22 $d9 $8b $28 $12 }T
$2590 >PC $3f >S $83 >A $77 >X $5e >Y $a4 >P $2590 $d9 >M $2591 $0c >M $2592 $ce >M $2593 $c7 >M $ce6a $c9 >M
T{ op PC S A X Y P -> $2593 $3f $83 $77 $5e $a4 }T T{ $2590 M $2591 M $2592 M $2593 M $ce6a M -> $d9 $0c $ce $c7 $c9 }T
$fc31 >PC $50 >S $88 >A $a6 >X $8d >Y $20 >P $6635 $36 >M $fc31 $d9 >M $fc32 $a8 >M $fc33 $65 >M $fc34 $3d >M
T{ op PC S A X Y P -> $fc34 $50 $88 $a6 $8d $21 }T T{ $6635 M $fc31 M $fc32 M $fc33 M $fc34 M -> $36 $d9 $a8 $65 $3d }T
$df07 >PC $4a >S $3c >A $81 >X $05 >Y $2a >P $789f $f9 >M $df07 $d9 >M $df08 $9a >M $df09 $78 >M $df0a $f2 >M
T{ op PC S A X Y P -> $df0a $4a $3c $81 $05 $28 }T T{ $789f M $df07 M $df08 M $df09 M $df0a M -> $f9 $d9 $9a $78 $f2 }T
$dcb4 >PC $f4 >S $a5 >A $59 >X $e2 >Y $aa >P $8c26 $cc >M $dcb4 $d9 >M $dcb5 $44 >M $dcb6 $8b >M $dcb7 $83 >M
T{ op PC S A X Y P -> $dcb7 $f4 $a5 $59 $e2 $a8 }T T{ $8c26 M $dcb4 M $dcb5 M $dcb6 M $dcb7 M -> $cc $d9 $44 $8b $83 }T
$a289 >PC $cf >S $cb >A $c1 >X $64 >Y $e3 >P $95cc $a2 >M $a289 $d9 >M $a28a $68 >M $a28b $95 >M $a28c $db >M
T{ op PC S A X Y P -> $a28c $cf $cb $c1 $64 $61 }T T{ $95cc M $a289 M $a28a M $a28b M $a28c M -> $a2 $d9 $68 $95 $db }T
$06d9 >PC $7b >S $99 >A $de >X $39 >Y $6a >P $06d9 $d9 >M $06da $04 >M $06db $16 >M $06dc $b7 >M $163d $2a >M
T{ op PC S A X Y P -> $06dc $7b $99 $de $39 $69 }T T{ $06d9 M $06da M $06db M $06dc M $163d M -> $d9 $04 $16 $b7 $2a }T
$ffaf >PC $ad >S $af >A $9e >X $b9 >Y $21 >P $6189 $23 >M $ffaf $d9 >M $ffb0 $d0 >M $ffb1 $60 >M $ffb2 $26 >M
T{ op PC S A X Y P -> $ffb2 $ad $af $9e $b9 $a1 }T T{ $6189 M $ffaf M $ffb0 M $ffb1 M $ffb2 M -> $23 $d9 $d0 $60 $26 }T
$482e >PC $55 >S $f1 >A $1a >X $6f >Y $a8 >P $3fc3 $21 >M $482e $d9 >M $482f $54 >M $4830 $3f >M $4831 $f3 >M
T{ op PC S A X Y P -> $4831 $55 $f1 $1a $6f $a9 }T T{ $3fc3 M $482e M $482f M $4830 M $4831 M -> $21 $d9 $54 $3f $f3 }T
$6ded >PC $91 >S $85 >A $d5 >X $20 >Y $26 >P $6ded $d9 >M $6dee $c1 >M $6def $6f >M $6df0 $34 >M $6fe1 $91 >M
T{ op PC S A X Y P -> $6df0 $91 $85 $d5 $20 $a4 }T T{ $6ded M $6dee M $6def M $6df0 M $6fe1 M -> $d9 $c1 $6f $34 $91 }T
$9942 >PC $43 >S $ed >A $c4 >X $55 >Y $e7 >P $9942 $d9 >M $9943 $0a >M $9944 $a3 >M $9945 $9b >M $a35f $6e >M
T{ op PC S A X Y P -> $9945 $43 $ed $c4 $55 $65 }T T{ $9942 M $9943 M $9944 M $9945 M $a35f M -> $d9 $0a $a3 $9b $6e }T
$888a >PC $a2 >S $46 >A $6d >X $21 >Y $e1 >P $888a $d9 >M $888b $b4 >M $888c $de >M $888d $94 >M $ded5 $9a >M
T{ op PC S A X Y P -> $888d $a2 $46 $6d $21 $e0 }T T{ $888a M $888b M $888c M $888d M $ded5 M -> $d9 $b4 $de $94 $9a }T
$e09c >PC $f9 >S $c1 >A $b9 >X $1f >Y $27 >P $3be3 $7e >M $e09c $d9 >M $e09d $c4 >M $e09e $3b >M $e09f $3c >M
T{ op PC S A X Y P -> $e09f $f9 $c1 $b9 $1f $25 }T T{ $3be3 M $e09c M $e09d M $e09e M $e09f M -> $7e $d9 $c4 $3b $3c }T
$33cd >PC $0c >S $db >A $e5 >X $51 >Y $20 >P $33cd $d9 >M $33ce $a5 >M $33cf $60 >M $33d0 $e2 >M $60f6 $ef >M
T{ op PC S A X Y P -> $33d0 $0c $db $e5 $51 $a0 }T T{ $33cd M $33ce M $33cf M $33d0 M $60f6 M -> $d9 $a5 $60 $e2 $ef }T
( da )
$b4c2 >PC $3a >S $9f >A $a8 >X $c3 >Y $2b >P $b4c2 $da >M $b4c3 $0f >M $b4c4 $ae >M
T{ op PC S A X Y P -> $b4c3 $39 $9f $a8 $c3 $2b }T T{ $013a M $b4c2 M $b4c3 M $b4c4 M -> $a8 $da $0f $ae }T
$159d >PC $ad >S $10 >A $c0 >X $2b >Y $a8 >P $159d $da >M $159e $c2 >M $159f $62 >M
T{ op PC S A X Y P -> $159e $ac $10 $c0 $2b $a8 }T T{ $01ad M $159d M $159e M $159f M -> $c0 $da $c2 $62 }T
$df9c >PC $a5 >S $7a >A $04 >X $2a >Y $64 >P $df9c $da >M $df9d $c0 >M $df9e $1c >M
T{ op PC S A X Y P -> $df9d $a4 $7a $04 $2a $64 }T T{ $01a5 M $df9c M $df9d M $df9e M -> $04 $da $c0 $1c }T
$5e69 >PC $5f >S $49 >A $ab >X $d3 >Y $61 >P $5e69 $da >M $5e6a $bf >M $5e6b $96 >M
T{ op PC S A X Y P -> $5e6a $5e $49 $ab $d3 $61 }T T{ $015f M $5e69 M $5e6a M $5e6b M -> $ab $da $bf $96 }T
$f497 >PC $99 >S $be >A $26 >X $c9 >Y $6e >P $f497 $da >M $f498 $72 >M $f499 $5b >M
T{ op PC S A X Y P -> $f498 $98 $be $26 $c9 $6e }T T{ $0199 M $f497 M $f498 M $f499 M -> $26 $da $72 $5b }T
$924d >PC $7e >S $a8 >A $b7 >X $65 >Y $2d >P $924d $da >M $924e $50 >M $924f $48 >M
T{ op PC S A X Y P -> $924e $7d $a8 $b7 $65 $2d }T T{ $017e M $924d M $924e M $924f M -> $b7 $da $50 $48 }T
$439b >PC $2d >S $6d >A $bb >X $26 >Y $a5 >P $439b $da >M $439c $3a >M $439d $40 >M
T{ op PC S A X Y P -> $439c $2c $6d $bb $26 $a5 }T T{ $012d M $439b M $439c M $439d M -> $bb $da $3a $40 }T
$94d1 >PC $60 >S $a3 >A $f7 >X $06 >Y $ac >P $94d1 $da >M $94d2 $4f >M $94d3 $fc >M
T{ op PC S A X Y P -> $94d2 $5f $a3 $f7 $06 $ac }T T{ $0160 M $94d1 M $94d2 M $94d3 M -> $f7 $da $4f $fc }T
$3bc1 >PC $49 >S $2a >A $43 >X $68 >Y $21 >P $3bc1 $da >M $3bc2 $b1 >M $3bc3 $48 >M
T{ op PC S A X Y P -> $3bc2 $48 $2a $43 $68 $21 }T T{ $0149 M $3bc1 M $3bc2 M $3bc3 M -> $43 $da $b1 $48 }T
$d637 >PC $45 >S $54 >A $25 >X $60 >Y $20 >P $d637 $da >M $d638 $1b >M $d639 $27 >M
T{ op PC S A X Y P -> $d638 $44 $54 $25 $60 $20 }T T{ $0145 M $d637 M $d638 M $d639 M -> $25 $da $1b $27 }T
$d0fe >PC $9d >S $60 >A $4a >X $9c >Y $ef >P $d0fe $da >M $d0ff $00 >M $d100 $84 >M
T{ op PC S A X Y P -> $d0ff $9c $60 $4a $9c $ef }T T{ $019d M $d0fe M $d0ff M $d100 M -> $4a $da $00 $84 }T
$8a56 >PC $64 >S $11 >A $f6 >X $34 >Y $6d >P $8a56 $da >M $8a57 $2e >M $8a58 $99 >M
T{ op PC S A X Y P -> $8a57 $63 $11 $f6 $34 $6d }T T{ $0164 M $8a56 M $8a57 M $8a58 M -> $f6 $da $2e $99 }T
$9862 >PC $a7 >S $1e >A $2b >X $e4 >Y $6a >P $9862 $da >M $9863 $d8 >M $9864 $ec >M
T{ op PC S A X Y P -> $9863 $a6 $1e $2b $e4 $6a }T T{ $01a7 M $9862 M $9863 M $9864 M -> $2b $da $d8 $ec }T
$3bc2 >PC $81 >S $3c >A $6b >X $76 >Y $24 >P $3bc2 $da >M $3bc3 $06 >M $3bc4 $6d >M
T{ op PC S A X Y P -> $3bc3 $80 $3c $6b $76 $24 }T T{ $0181 M $3bc2 M $3bc3 M $3bc4 M -> $6b $da $06 $6d }T
$64e8 >PC $cc >S $08 >A $50 >X $0b >Y $e2 >P $64e8 $da >M $64e9 $86 >M $64ea $fc >M
T{ op PC S A X Y P -> $64e9 $cb $08 $50 $0b $e2 }T T{ $01cc M $64e8 M $64e9 M $64ea M -> $50 $da $86 $fc }T
$08aa >PC $51 >S $8f >A $39 >X $38 >Y $6a >P $08aa $da >M $08ab $b6 >M $08ac $f9 >M
T{ op PC S A X Y P -> $08ab $50 $8f $39 $38 $6a }T T{ $0151 M $08aa M $08ab M $08ac M -> $39 $da $b6 $f9 }T
( db )
\ Skipping invalid json wdc65c02/v1/db.json
( dc )
$54e7 >PC $5a >S $fa >A $1b >X $12 >Y $69 >P $54e7 $dc >M $54e8 $45 >M $54e9 $e3 >M $54ea $37 >M
T{ op PC S A X Y P -> $54ea $5a $fa $1b $12 $69 }T T{ $54e7 M $54e8 M $54e9 M $54ea M -> $dc $45 $e3 $37 }T
$161a >PC $2e >S $f3 >A $38 >X $23 >Y $aa >P $161a $dc >M $161b $13 >M $161c $0e >M $161d $21 >M
T{ op PC S A X Y P -> $161d $2e $f3 $38 $23 $aa }T T{ $161a M $161b M $161c M $161d M -> $dc $13 $0e $21 }T
$3383 >PC $c2 >S $1c >A $93 >X $0d >Y $e8 >P $3383 $dc >M $3384 $09 >M $3385 $10 >M $3386 $7d >M
T{ op PC S A X Y P -> $3386 $c2 $1c $93 $0d $e8 }T T{ $3383 M $3384 M $3385 M $3386 M -> $dc $09 $10 $7d }T
$7d43 >PC $be >S $24 >A $f1 >X $4c >Y $e1 >P $7d43 $dc >M $7d44 $1c >M $7d45 $eb >M $7d46 $cf >M
T{ op PC S A X Y P -> $7d46 $be $24 $f1 $4c $e1 }T T{ $7d43 M $7d44 M $7d45 M $7d46 M -> $dc $1c $eb $cf }T
$8f6d >PC $ef >S $1e >A $0b >X $4b >Y $ef >P $8f6d $dc >M $8f6e $43 >M $8f6f $e5 >M $8f70 $df >M
T{ op PC S A X Y P -> $8f70 $ef $1e $0b $4b $ef }T T{ $8f6d M $8f6e M $8f6f M $8f70 M -> $dc $43 $e5 $df }T
$3be6 >PC $c7 >S $2c >A $e5 >X $08 >Y $ec >P $3be6 $dc >M $3be7 $87 >M $3be8 $d2 >M $3be9 $38 >M
T{ op PC S A X Y P -> $3be9 $c7 $2c $e5 $08 $ec }T T{ $3be6 M $3be7 M $3be8 M $3be9 M -> $dc $87 $d2 $38 }T
$52b3 >PC $e1 >S $ba >A $fb >X $7a >Y $20 >P $52b3 $dc >M $52b4 $12 >M $52b5 $7f >M $52b6 $23 >M
T{ op PC S A X Y P -> $52b6 $e1 $ba $fb $7a $20 }T T{ $52b3 M $52b4 M $52b5 M $52b6 M -> $dc $12 $7f $23 }T
$ccb0 >PC $a3 >S $d6 >A $da >X $a1 >Y $25 >P $ccb0 $dc >M $ccb1 $f1 >M $ccb2 $3e >M $ccb3 $86 >M
T{ op PC S A X Y P -> $ccb3 $a3 $d6 $da $a1 $25 }T T{ $ccb0 M $ccb1 M $ccb2 M $ccb3 M -> $dc $f1 $3e $86 }T
$d6b6 >PC $73 >S $b9 >A $59 >X $45 >Y $66 >P $d6b6 $dc >M $d6b7 $50 >M $d6b8 $83 >M $d6b9 $06 >M
T{ op PC S A X Y P -> $d6b9 $73 $b9 $59 $45 $66 }T T{ $d6b6 M $d6b7 M $d6b8 M $d6b9 M -> $dc $50 $83 $06 }T
$aa31 >PC $68 >S $34 >A $fe >X $41 >Y $6b >P $aa31 $dc >M $aa32 $ff >M $aa33 $5c >M $aa34 $ea >M
T{ op PC S A X Y P -> $aa34 $68 $34 $fe $41 $6b }T T{ $aa31 M $aa32 M $aa33 M $aa34 M -> $dc $ff $5c $ea }T
$ba8b >PC $6e >S $00 >A $57 >X $2a >Y $aa >P $ba8b $dc >M $ba8c $17 >M $ba8d $b2 >M $ba8e $90 >M
T{ op PC S A X Y P -> $ba8e $6e $00 $57 $2a $aa }T T{ $ba8b M $ba8c M $ba8d M $ba8e M -> $dc $17 $b2 $90 }T
$7371 >PC $13 >S $10 >A $e0 >X $46 >Y $66 >P $7371 $dc >M $7372 $f2 >M $7373 $eb >M $7374 $ea >M
T{ op PC S A X Y P -> $7374 $13 $10 $e0 $46 $66 }T T{ $7371 M $7372 M $7373 M $7374 M -> $dc $f2 $eb $ea }T
$4c9b >PC $02 >S $b8 >A $3c >X $1c >Y $a2 >P $4c9b $dc >M $4c9c $2e >M $4c9d $31 >M $4c9e $3d >M
T{ op PC S A X Y P -> $4c9e $02 $b8 $3c $1c $a2 }T T{ $4c9b M $4c9c M $4c9d M $4c9e M -> $dc $2e $31 $3d }T
$0523 >PC $87 >S $0d >A $68 >X $18 >Y $2b >P $0523 $dc >M $0524 $92 >M $0525 $a0 >M $0526 $9f >M
T{ op PC S A X Y P -> $0526 $87 $0d $68 $18 $2b }T T{ $0523 M $0524 M $0525 M $0526 M -> $dc $92 $a0 $9f }T
$5928 >PC $bf >S $4d >A $7d >X $2a >Y $e2 >P $5928 $dc >M $5929 $f2 >M $592a $60 >M $592b $82 >M
T{ op PC S A X Y P -> $592b $bf $4d $7d $2a $e2 }T T{ $5928 M $5929 M $592a M $592b M -> $dc $f2 $60 $82 }T
$2d28 >PC $31 >S $f1 >A $9c >X $b6 >Y $a2 >P $2d28 $dc >M $2d29 $d7 >M $2d2a $81 >M $2d2b $bd >M
T{ op PC S A X Y P -> $2d2b $31 $f1 $9c $b6 $a2 }T T{ $2d28 M $2d29 M $2d2a M $2d2b M -> $dc $d7 $81 $bd }T
( dd )
$8a14 >PC $05 >S $ba >A $f3 >X $a7 >Y $a4 >P $360f $9f >M $8a14 $dd >M $8a15 $1c >M $8a16 $35 >M $8a17 $31 >M
T{ op PC S A X Y P -> $8a17 $05 $ba $f3 $a7 $25 }T T{ $360f M $8a14 M $8a15 M $8a16 M $8a17 M -> $9f $dd $1c $35 $31 }T
$1de1 >PC $48 >S $e3 >A $48 >X $68 >Y $eb >P $1de1 $dd >M $1de2 $4b >M $1de3 $fa >M $1de4 $1d >M $fa93 $08 >M
T{ op PC S A X Y P -> $1de4 $48 $e3 $48 $68 $e9 }T T{ $1de1 M $1de2 M $1de3 M $1de4 M $fa93 M -> $dd $4b $fa $1d $08 }T
$36f7 >PC $30 >S $96 >A $65 >X $b1 >Y $ef >P $181a $df >M $36f7 $dd >M $36f8 $b5 >M $36f9 $17 >M $36fa $30 >M
T{ op PC S A X Y P -> $36fa $30 $96 $65 $b1 $ec }T T{ $181a M $36f7 M $36f8 M $36f9 M $36fa M -> $df $dd $b5 $17 $30 }T
$c5f5 >PC $41 >S $ec >A $0f >X $cf >Y $25 >P $c5f5 $dd >M $c5f6 $c5 >M $c5f7 $fc >M $c5f8 $c7 >M $fcd4 $68 >M
T{ op PC S A X Y P -> $c5f8 $41 $ec $0f $cf $a5 }T T{ $c5f5 M $c5f6 M $c5f7 M $c5f8 M $fcd4 M -> $dd $c5 $fc $c7 $68 }T
$6727 >PC $1d >S $b4 >A $b7 >X $52 >Y $a9 >P $6727 $dd >M $6728 $4d >M $6729 $83 >M $672a $b8 >M $8404 $4e >M
T{ op PC S A X Y P -> $672a $1d $b4 $b7 $52 $29 }T T{ $6727 M $6728 M $6729 M $672a M $8404 M -> $dd $4d $83 $b8 $4e }T
$0f97 >PC $26 >S $92 >A $66 >X $6c >Y $e2 >P $0f97 $dd >M $0f98 $8f >M $0f99 $4e >M $0f9a $01 >M $4ef5 $3c >M
T{ op PC S A X Y P -> $0f9a $26 $92 $66 $6c $61 }T T{ $0f97 M $0f98 M $0f99 M $0f9a M $4ef5 M -> $dd $8f $4e $01 $3c }T
$ff47 >PC $91 >S $0c >A $52 >X $8b >Y $e3 >P $a465 $29 >M $ff47 $dd >M $ff48 $13 >M $ff49 $a4 >M $ff4a $75 >M
T{ op PC S A X Y P -> $ff4a $91 $0c $52 $8b $e0 }T T{ $a465 M $ff47 M $ff48 M $ff49 M $ff4a M -> $29 $dd $13 $a4 $75 }T
$7ac8 >PC $f0 >S $c4 >A $77 >X $b7 >Y $2c >P $7ac8 $dd >M $7ac9 $8b >M $7aca $96 >M $7acb $2e >M $9702 $92 >M
T{ op PC S A X Y P -> $7acb $f0 $c4 $77 $b7 $2d }T T{ $7ac8 M $7ac9 M $7aca M $7acb M $9702 M -> $dd $8b $96 $2e $92 }T
$dfb0 >PC $ed >S $9f >A $e0 >X $e5 >Y $a0 >P $3032 $50 >M $dfb0 $dd >M $dfb1 $52 >M $dfb2 $2f >M $dfb3 $bd >M
T{ op PC S A X Y P -> $dfb3 $ed $9f $e0 $e5 $21 }T T{ $3032 M $dfb0 M $dfb1 M $dfb2 M $dfb3 M -> $50 $dd $52 $2f $bd }T
$0341 >PC $2c >S $e4 >A $7d >X $0c >Y $6b >P $0341 $dd >M $0342 $61 >M $0343 $e3 >M $0344 $47 >M $e3de $88 >M
T{ op PC S A X Y P -> $0344 $2c $e4 $7d $0c $69 }T T{ $0341 M $0342 M $0343 M $0344 M $e3de M -> $dd $61 $e3 $47 $88 }T
$55df >PC $54 >S $a5 >A $17 >X $38 >Y $ae >P $491e $a6 >M $55df $dd >M $55e0 $07 >M $55e1 $49 >M $55e2 $b0 >M
T{ op PC S A X Y P -> $55e2 $54 $a5 $17 $38 $ac }T T{ $491e M $55df M $55e0 M $55e1 M $55e2 M -> $a6 $dd $07 $49 $b0 }T
$a0ed >PC $bb >S $22 >A $f1 >X $a8 >Y $ad >P $a0ed $dd >M $a0ee $b2 >M $a0ef $c9 >M $a0f0 $f1 >M $caa3 $33 >M
T{ op PC S A X Y P -> $a0f0 $bb $22 $f1 $a8 $ac }T T{ $a0ed M $a0ee M $a0ef M $a0f0 M $caa3 M -> $dd $b2 $c9 $f1 $33 }T
$64cb >PC $45 >S $36 >A $eb >X $c0 >Y $ab >P $64cb $dd >M $64cc $00 >M $64cd $e4 >M $64ce $6c >M $e4eb $cc >M
T{ op PC S A X Y P -> $64ce $45 $36 $eb $c0 $28 }T T{ $64cb M $64cc M $64cd M $64ce M $e4eb M -> $dd $00 $e4 $6c $cc }T
$577d >PC $55 >S $50 >A $8a >X $0e >Y $e5 >P $577d $dd >M $577e $4a >M $577f $a3 >M $5780 $0e >M $a3d4 $99 >M
T{ op PC S A X Y P -> $5780 $55 $50 $8a $0e $e4 }T T{ $577d M $577e M $577f M $5780 M $a3d4 M -> $dd $4a $a3 $0e $99 }T
$e1df >PC $41 >S $93 >A $4e >X $70 >Y $2e >P $456f $e1 >M $e1df $dd >M $e1e0 $21 >M $e1e1 $45 >M $e1e2 $d9 >M
T{ op PC S A X Y P -> $e1e2 $41 $93 $4e $70 $ac }T T{ $456f M $e1df M $e1e0 M $e1e1 M $e1e2 M -> $e1 $dd $21 $45 $d9 }T
$a732 >PC $35 >S $9e >A $c0 >X $f9 >Y $60 >P $0fec $66 >M $a732 $dd >M $a733 $2c >M $a734 $0f >M $a735 $ae >M
T{ op PC S A X Y P -> $a735 $35 $9e $c0 $f9 $61 }T T{ $0fec M $a732 M $a733 M $a734 M $a735 M -> $66 $dd $2c $0f $ae }T
( de )
$4bc5 >PC $a5 >S $48 >A $4c >X $a6 >Y $ef >P $4bc5 $de >M $4bc6 $6c >M $4bc7 $a7 >M $4bc8 $d1 >M $a7b8 $8b >M
T{ op PC S A X Y P -> $4bc8 $a5 $48 $4c $a6 $ed }T T{ $4bc5 M $4bc6 M $4bc7 M $4bc8 M $a7b8 M -> $de $6c $a7 $d1 $8a }T
$77d2 >PC $91 >S $82 >A $2e >X $25 >Y $e7 >P $352f $de >M $77d2 $de >M $77d3 $01 >M $77d4 $35 >M $77d5 $11 >M
T{ op PC S A X Y P -> $77d5 $91 $82 $2e $25 $e5 }T T{ $352f M $77d2 M $77d3 M $77d4 M $77d5 M -> $dd $de $01 $35 $11 }T
$d744 >PC $57 >S $c2 >A $59 >X $e2 >Y $69 >P $bfff $ed >M $d744 $de >M $d745 $a6 >M $d746 $bf >M $d747 $cb >M
T{ op PC S A X Y P -> $d747 $57 $c2 $59 $e2 $e9 }T T{ $bfff M $d744 M $d745 M $d746 M $d747 M -> $ec $de $a6 $bf $cb }T
$a3af >PC $ed >S $93 >A $bf >X $c0 >Y $ac >P $6b86 $21 >M $a3af $de >M $a3b0 $c7 >M $a3b1 $6a >M $a3b2 $93 >M
T{ op PC S A X Y P -> $a3b2 $ed $93 $bf $c0 $2c }T T{ $6b86 M $a3af M $a3b0 M $a3b1 M $a3b2 M -> $20 $de $c7 $6a $93 }T
$232d >PC $67 >S $bd >A $ed >X $59 >Y $20 >P $232d $de >M $232e $e8 >M $232f $6f >M $2330 $b7 >M $70d5 $d0 >M
T{ op PC S A X Y P -> $2330 $67 $bd $ed $59 $a0 }T T{ $232d M $232e M $232f M $2330 M $70d5 M -> $de $e8 $6f $b7 $cf }T
$6396 >PC $db >S $64 >A $fe >X $eb >Y $e9 >P $46e4 $75 >M $6396 $de >M $6397 $e6 >M $6398 $45 >M $6399 $1a >M
T{ op PC S A X Y P -> $6399 $db $64 $fe $eb $69 }T T{ $46e4 M $6396 M $6397 M $6398 M $6399 M -> $74 $de $e6 $45 $1a }T
$7221 >PC $0e >S $3e >A $04 >X $9a >Y $2e >P $7221 $de >M $7222 $bc >M $7223 $ef >M $7224 $6a >M $efc0 $cd >M
T{ op PC S A X Y P -> $7224 $0e $3e $04 $9a $ac }T T{ $7221 M $7222 M $7223 M $7224 M $efc0 M -> $de $bc $ef $6a $cc }T
$26d2 >PC $18 >S $6b >A $cc >X $59 >Y $62 >P $26d2 $de >M $26d3 $4b >M $26d4 $fb >M $26d5 $39 >M $fc17 $5a >M
T{ op PC S A X Y P -> $26d5 $18 $6b $cc $59 $60 }T T{ $26d2 M $26d3 M $26d4 M $26d5 M $fc17 M -> $de $4b $fb $39 $59 }T
$054c >PC $b2 >S $26 >A $ef >X $41 >Y $22 >P $054c $de >M $054d $71 >M $054e $f9 >M $054f $21 >M $fa60 $fc >M
T{ op PC S A X Y P -> $054f $b2 $26 $ef $41 $a0 }T T{ $054c M $054d M $054e M $054f M $fa60 M -> $de $71 $f9 $21 $fb }T
$f432 >PC $e1 >S $44 >A $0a >X $4e >Y $ee >P $71c0 $73 >M $f432 $de >M $f433 $b6 >M $f434 $71 >M $f435 $16 >M
T{ op PC S A X Y P -> $f435 $e1 $44 $0a $4e $6c }T T{ $71c0 M $f432 M $f433 M $f434 M $f435 M -> $72 $de $b6 $71 $16 }T
$a462 >PC $3b >S $a2 >A $bc >X $18 >Y $e4 >P $831b $8f >M $a462 $de >M $a463 $5f >M $a464 $82 >M $a465 $c0 >M
T{ op PC S A X Y P -> $a465 $3b $a2 $bc $18 $e4 }T T{ $831b M $a462 M $a463 M $a464 M $a465 M -> $8e $de $5f $82 $c0 }T
$d1aa >PC $2b >S $e8 >A $c9 >X $a8 >Y $e1 >P $b4dc $28 >M $d1aa $de >M $d1ab $13 >M $d1ac $b4 >M $d1ad $e1 >M
T{ op PC S A X Y P -> $d1ad $2b $e8 $c9 $a8 $61 }T T{ $b4dc M $d1aa M $d1ab M $d1ac M $d1ad M -> $27 $de $13 $b4 $e1 }T
$6f6b >PC $14 >S $eb >A $fe >X $00 >Y $eb >P $6f6b $de >M $6f6c $21 >M $6f6d $c4 >M $6f6e $ca >M $c51f $f9 >M
T{ op PC S A X Y P -> $6f6e $14 $eb $fe $00 $e9 }T T{ $6f6b M $6f6c M $6f6d M $6f6e M $c51f M -> $de $21 $c4 $ca $f8 }T
$da1a >PC $0b >S $58 >A $4b >X $fc >Y $e2 >P $da1a $de >M $da1b $2b >M $da1c $ed >M $da1d $b0 >M $ed76 $5c >M
T{ op PC S A X Y P -> $da1d $0b $58 $4b $fc $60 }T T{ $da1a M $da1b M $da1c M $da1d M $ed76 M -> $de $2b $ed $b0 $5b }T
$90ae >PC $42 >S $e8 >A $88 >X $1f >Y $65 >P $4b9d $62 >M $90ae $de >M $90af $15 >M $90b0 $4b >M $90b1 $b8 >M
T{ op PC S A X Y P -> $90b1 $42 $e8 $88 $1f $65 }T T{ $4b9d M $90ae M $90af M $90b0 M $90b1 M -> $61 $de $15 $4b $b8 }T
$f637 >PC $5f >S $54 >A $69 >X $08 >Y $2e >P $357b $ed >M $f637 $de >M $f638 $12 >M $f639 $35 >M $f63a $86 >M
T{ op PC S A X Y P -> $f63a $5f $54 $69 $08 $ac }T T{ $357b M $f637 M $f638 M $f639 M $f63a M -> $ec $de $12 $35 $86 }T
( df )
$44d0 >PC $73 >S $bb >A $bd >X $8e >Y $69 >P $00fc $25 >M $44a4 $84 >M $44d0 $df >M $44d1 $fc >M $44d2 $d1 >M
T{ op PC S A X Y P -> $44a4 $73 $bb $bd $8e $69 }T T{ $00fc M $44a4 M $44d0 M $44d1 M $44d2 M -> $25 $84 $df $fc $d1 }T
$a875 >PC $20 >S $78 >A $de >X $b7 >Y $62 >P $006c $26 >M $a875 $df >M $a876 $6c >M $a877 $29 >M $a8a1 $f9 >M
T{ op PC S A X Y P -> $a8a1 $20 $78 $de $b7 $62 }T T{ $006c M $a875 M $a876 M $a877 M $a8a1 M -> $26 $df $6c $29 $f9 }T
$1769 >PC $ca >S $b4 >A $c5 >X $75 >Y $67 >P $0026 $d1 >M $1755 $b2 >M $1769 $df >M $176a $26 >M $176b $e9 >M $176c $60 >M
T{ op PC S A X Y P -> $176c $ca $b4 $c5 $75 $67 }T T{ $0026 M $1755 M $1769 M $176a M $176b M $176c M -> $d1 $b2 $df $26 $e9 $60 }T
$ceb7 >PC $7c >S $cc >A $dd >X $4f >Y $a0 >P $00d0 $6d >M $ce73 $aa >M $ceb7 $df >M $ceb8 $d0 >M $ceb9 $b9 >M
T{ op PC S A X Y P -> $ce73 $7c $cc $dd $4f $a0 }T T{ $00d0 M $ce73 M $ceb7 M $ceb8 M $ceb9 M -> $6d $aa $df $d0 $b9 }T
$be2e >PC $8b >S $ec >A $c4 >X $ce >Y $6e >P $00b7 $bc >M $be2e $df >M $be2f $b7 >M $be30 $43 >M $be74 $bf >M
T{ op PC S A X Y P -> $be74 $8b $ec $c4 $ce $6e }T T{ $00b7 M $be2e M $be2f M $be30 M $be74 M -> $bc $df $b7 $43 $bf }T
$3ef0 >PC $05 >S $44 >A $2e >X $56 >Y $ab >P $009f $04 >M $3eb1 $3a >M $3ef0 $df >M $3ef1 $9f >M $3ef2 $be >M $3ef3 $4b >M
T{ op PC S A X Y P -> $3ef3 $05 $44 $2e $56 $ab }T T{ $009f M $3eb1 M $3ef0 M $3ef1 M $3ef2 M $3ef3 M -> $04 $3a $df $9f $be $4b }T
$3e5c >PC $ca >S $67 >A $c8 >X $67 >Y $6c >P $00f7 $cd >M $3e5c $df >M $3e5d $f7 >M $3e5e $22 >M $3e5f $fc >M $3e81 $7f >M
T{ op PC S A X Y P -> $3e5f $ca $67 $c8 $67 $6c }T T{ $00f7 M $3e5c M $3e5d M $3e5e M $3e5f M $3e81 M -> $cd $df $f7 $22 $fc $7f }T
$051a >PC $bb >S $a2 >A $03 >X $0f >Y $6e >P $008d $69 >M $051a $df >M $051b $8d >M $051c $49 >M $0566 $ea >M
T{ op PC S A X Y P -> $0566 $bb $a2 $03 $0f $6e }T T{ $008d M $051a M $051b M $051c M $0566 M -> $69 $df $8d $49 $ea }T
$bafd >PC $15 >S $20 >A $87 >X $21 >Y $ab >P $0097 $01 >M $bafd $df >M $bafe $97 >M $baff $a3 >M $bb00 $bd >M $bba3 $c2 >M
T{ op PC S A X Y P -> $bb00 $15 $20 $87 $21 $ab }T T{ $0097 M $bafd M $bafe M $baff M $bb00 M $bba3 M -> $01 $df $97 $a3 $bd $c2 }T
$74e4 >PC $51 >S $4c >A $f9 >X $2c >Y $a9 >P $006a $06 >M $7412 $fc >M $74e4 $df >M $74e5 $6a >M $74e6 $2b >M $74e7 $e7 >M
T{ op PC S A X Y P -> $74e7 $51 $4c $f9 $2c $a9 }T T{ $006a M $7412 M $74e4 M $74e5 M $74e6 M $74e7 M -> $06 $fc $df $6a $2b $e7 }T
$203e >PC $1a >S $d1 >A $de >X $cc >Y $67 >P $00c3 $19 >M $203e $df >M $203f $c3 >M $2040 $49 >M $2041 $e1 >M $208a $5a >M
T{ op PC S A X Y P -> $2041 $1a $d1 $de $cc $67 }T T{ $00c3 M $203e M $203f M $2040 M $2041 M $208a M -> $19 $df $c3 $49 $e1 $5a }T
$2d4a >PC $a9 >S $a3 >A $1e >X $81 >Y $ea >P $00c0 $67 >M $2d4a $df >M $2d4b $c0 >M $2d4c $68 >M $2db5 $62 >M
T{ op PC S A X Y P -> $2db5 $a9 $a3 $1e $81 $ea }T T{ $00c0 M $2d4a M $2d4b M $2d4c M $2db5 M -> $67 $df $c0 $68 $62 }T
$177c >PC $2e >S $1d >A $b5 >X $f3 >Y $68 >P $0056 $e3 >M $1762 $0d >M $177c $df >M $177d $56 >M $177e $e3 >M
T{ op PC S A X Y P -> $1762 $2e $1d $b5 $f3 $68 }T T{ $0056 M $1762 M $177c M $177d M $177e M -> $e3 $0d $df $56 $e3 }T
$70f3 >PC $7c >S $fa >A $bc >X $ba >Y $a3 >P $00a9 $ae >M $70de $c5 >M $70f3 $df >M $70f4 $a9 >M $70f5 $e8 >M
T{ op PC S A X Y P -> $70de $7c $fa $bc $ba $a3 }T T{ $00a9 M $70de M $70f3 M $70f4 M $70f5 M -> $ae $c5 $df $a9 $e8 }T
$eb31 >PC $5e >S $c7 >A $b6 >X $4f >Y $e2 >P $005b $b2 >M $ead3 $bb >M $eb31 $df >M $eb32 $5b >M $eb33 $9f >M $ebd3 $58 >M
T{ op PC S A X Y P -> $ead3 $5e $c7 $b6 $4f $e2 }T T{ $005b M $ead3 M $eb31 M $eb32 M $eb33 M $ebd3 M -> $b2 $bb $df $5b $9f $58 }T
$2048 >PC $a2 >S $04 >A $bd >X $2d >Y $6a >P $0028 $c7 >M $2020 $92 >M $2048 $df >M $2049 $28 >M $204a $d5 >M $204b $06 >M
T{ op PC S A X Y P -> $204b $a2 $04 $bd $2d $6a }T T{ $0028 M $2020 M $2048 M $2049 M $204a M $204b M -> $c7 $92 $df $28 $d5 $06 }T
( e0 )
$cecf >PC $b3 >S $87 >A $0f >X $24 >Y $ef >P $cecf $e0 >M $ced0 $f9 >M $ced1 $02 >M
T{ op PC S A X Y P -> $ced1 $b3 $87 $0f $24 $6c }T T{ $cecf M $ced0 M $ced1 M -> $e0 $f9 $02 }T
$b9a5 >PC $b0 >S $ca >A $0d >X $a6 >Y $a2 >P $b9a5 $e0 >M $b9a6 $4f >M $b9a7 $3f >M
T{ op PC S A X Y P -> $b9a7 $b0 $ca $0d $a6 $a0 }T T{ $b9a5 M $b9a6 M $b9a7 M -> $e0 $4f $3f }T
$b599 >PC $5e >S $98 >A $4c >X $4c >Y $e4 >P $b599 $e0 >M $b59a $f3 >M $b59b $bd >M
T{ op PC S A X Y P -> $b59b $5e $98 $4c $4c $64 }T T{ $b599 M $b59a M $b59b M -> $e0 $f3 $bd }T
$6e74 >PC $f5 >S $d6 >A $11 >X $aa >Y $28 >P $6e74 $e0 >M $6e75 $f9 >M $6e76 $52 >M
T{ op PC S A X Y P -> $6e76 $f5 $d6 $11 $aa $28 }T T{ $6e74 M $6e75 M $6e76 M -> $e0 $f9 $52 }T
$7339 >PC $1d >S $8f >A $b5 >X $75 >Y $64 >P $7339 $e0 >M $733a $16 >M $733b $f2 >M
T{ op PC S A X Y P -> $733b $1d $8f $b5 $75 $e5 }T T{ $7339 M $733a M $733b M -> $e0 $16 $f2 }T
$53b4 >PC $5e >S $71 >A $74 >X $0d >Y $62 >P $53b4 $e0 >M $53b5 $f2 >M $53b6 $f0 >M
T{ op PC S A X Y P -> $53b6 $5e $71 $74 $0d $e0 }T T{ $53b4 M $53b5 M $53b6 M -> $e0 $f2 $f0 }T
$1615 >PC $75 >S $a0 >A $7a >X $52 >Y $6e >P $1615 $e0 >M $1616 $18 >M $1617 $20 >M
T{ op PC S A X Y P -> $1617 $75 $a0 $7a $52 $6d }T T{ $1615 M $1616 M $1617 M -> $e0 $18 $20 }T
$3887 >PC $25 >S $56 >A $fc >X $48 >Y $a2 >P $3887 $e0 >M $3888 $12 >M $3889 $d6 >M
T{ op PC S A X Y P -> $3889 $25 $56 $fc $48 $a1 }T T{ $3887 M $3888 M $3889 M -> $e0 $12 $d6 }T
$01fa >PC $25 >S $71 >A $ec >X $6d >Y $ee >P $01fa $e0 >M $01fb $7b >M $01fc $28 >M
T{ op PC S A X Y P -> $01fc $25 $71 $ec $6d $6d }T T{ $01fa M $01fb M $01fc M -> $e0 $7b $28 }T
$b4c6 >PC $c4 >S $e0 >A $b4 >X $8a >Y $29 >P $b4c6 $e0 >M $b4c7 $8a >M $b4c8 $19 >M
T{ op PC S A X Y P -> $b4c8 $c4 $e0 $b4 $8a $29 }T T{ $b4c6 M $b4c7 M $b4c8 M -> $e0 $8a $19 }T
$80ae >PC $b7 >S $b6 >A $d1 >X $68 >Y $e6 >P $80ae $e0 >M $80af $9c >M $80b0 $80 >M
T{ op PC S A X Y P -> $80b0 $b7 $b6 $d1 $68 $65 }T T{ $80ae M $80af M $80b0 M -> $e0 $9c $80 }T
$ca0f >PC $5d >S $85 >A $d3 >X $c1 >Y $24 >P $ca0f $e0 >M $ca10 $e6 >M $ca11 $7c >M
T{ op PC S A X Y P -> $ca11 $5d $85 $d3 $c1 $a4 }T T{ $ca0f M $ca10 M $ca11 M -> $e0 $e6 $7c }T
$627e >PC $25 >S $f2 >A $dc >X $ff >Y $a5 >P $627e $e0 >M $627f $36 >M $6280 $5c >M
T{ op PC S A X Y P -> $6280 $25 $f2 $dc $ff $a5 }T T{ $627e M $627f M $6280 M -> $e0 $36 $5c }T
$d81e >PC $68 >S $88 >A $d9 >X $6f >Y $a3 >P $d81e $e0 >M $d81f $fc >M $d820 $20 >M
T{ op PC S A X Y P -> $d820 $68 $88 $d9 $6f $a0 }T T{ $d81e M $d81f M $d820 M -> $e0 $fc $20 }T
$94fa >PC $ac >S $c2 >A $5d >X $5d >Y $24 >P $94fa $e0 >M $94fb $95 >M $94fc $83 >M
T{ op PC S A X Y P -> $94fc $ac $c2 $5d $5d $a4 }T T{ $94fa M $94fb M $94fc M -> $e0 $95 $83 }T
$91f7 >PC $bc >S $d5 >A $ee >X $c0 >Y $22 >P $91f7 $e0 >M $91f8 $2d >M $91f9 $12 >M
T{ op PC S A X Y P -> $91f9 $bc $d5 $ee $c0 $a1 }T T{ $91f7 M $91f8 M $91f9 M -> $e0 $2d $12 }T
( e1 )
$591d >PC $d7 >S $d2 >A $5d >X $26 >Y $fa >P $005b $6d >M $005c $71 >M $00fe $cb >M $591d $e1 >M $591e $fe >M $716d $59 >M
T{ op PC S A X Y P -> $591f $d7 $72 $5d $26 $79 }T T{ $005b M $005c M $00fe M $591d M $591e M $716d M -> $6d $71 $cb $e1 $fe $59 }T
$f81d >PC $83 >S $7e >A $8a >X $46 >Y $b0 >P $0038 $a3 >M $00c2 $3b >M $00c3 $7a >M $7a3b $d3 >M $f81d $e1 >M $f81e $38 >M
T{ op PC S A X Y P -> $f81f $83 $aa $8a $46 $f0 }T T{ $0038 M $00c2 M $00c3 M $7a3b M $f81d M $f81e M -> $a3 $3b $7a $d3 $e1 $38 }T
$399b >PC $37 >S $2e >A $f8 >X $32 >Y $fe >P $00d1 $76 >M $00d2 $7c >M $00d9 $95 >M $399b $e1 >M $399c $d9 >M $7c76 $d0 >M
T{ op PC S A X Y P -> $399d $37 $fd $f8 $32 $bc }T T{ $00d1 M $00d2 M $00d9 M $399b M $399c M $7c76 M -> $76 $7c $95 $e1 $d9 $d0 }T
$6e5c >PC $a5 >S $c9 >A $ff >X $46 >Y $fc >P $0039 $63 >M $003a $5b >M $5b63 $7a >M $6e5c $e1 >M $6e5d $3a >M
T{ op PC S A X Y P -> $6e5e $a5 $48 $ff $46 $7d }T T{ $0039 M $003a M $5b63 M $6e5c M $6e5d M -> $63 $5b $7a $e1 $3a }T
$a87e >PC $ac >S $63 >A $b1 >X $32 >Y $f6 >P $005b $f2 >M $005c $1a >M $00aa $52 >M $1af2 $a0 >M $a87e $e1 >M $a87f $aa >M
T{ op PC S A X Y P -> $a880 $ac $c2 $b1 $32 $f4 }T T{ $005b M $005c M $00aa M $1af2 M $a87e M $a87f M -> $f2 $1a $52 $a0 $e1 $aa }T
$b514 >PC $9f >S $2e >A $96 >X $aa >Y $b5 >P $0064 $49 >M $0065 $31 >M $00ce $d7 >M $3149 $41 >M $b514 $e1 >M $b515 $ce >M
T{ op PC S A X Y P -> $b516 $9f $ed $96 $aa $b4 }T T{ $0064 M $0065 M $00ce M $3149 M $b514 M $b515 M -> $49 $31 $d7 $41 $e1 $ce }T
$7fff >PC $9b >S $75 >A $21 >X $38 >Y $fe >P $00a2 $6b >M $00c3 $ac >M $00c4 $3f >M $3fac $ce >M $7fff $e1 >M $8000 $a2 >M
T{ op PC S A X Y P -> $8001 $9b $40 $21 $38 $7c }T T{ $00a2 M $00c3 M $00c4 M $3fac M $7fff M $8000 M -> $6b $ac $3f $ce $e1 $a2 }T
$0e05 >PC $28 >S $c6 >A $fe >X $7f >Y $72 >P $00d6 $52 >M $00d7 $78 >M $00d8 $d5 >M $0e05 $e1 >M $0e06 $d8 >M $7852 $22 >M
T{ op PC S A X Y P -> $0e07 $28 $a3 $fe $7f $b1 }T T{ $00d6 M $00d7 M $00d8 M $0e05 M $0e06 M $7852 M -> $52 $78 $d5 $e1 $d8 $22 }T
$e13e >PC $21 >S $90 >A $ce >X $a6 >Y $79 >P $005b $74 >M $005c $5e >M $008d $fc >M $5e74 $2c >M $e13e $e1 >M $e13f $8d >M
T{ op PC S A X Y P -> $e140 $21 $5e $ce $a6 $79 }T T{ $005b M $005c M $008d M $5e74 M $e13e M $e13f M -> $74 $5e $fc $2c $e1 $8d }T
$2bda >PC $63 >S $6d >A $87 >X $61 >Y $f3 >P $006a $39 >M $006b $61 >M $00e3 $fe >M $2bda $e1 >M $2bdb $e3 >M $6139 $2e >M
T{ op PC S A X Y P -> $2bdc $63 $3f $87 $61 $31 }T T{ $006a M $006b M $00e3 M $2bda M $2bdb M $6139 M -> $39 $61 $fe $e1 $e3 $2e }T
$bec4 >PC $5d >S $b6 >A $f1 >X $27 >Y $be >P $0094 $2d >M $0095 $74 >M $00a3 $e0 >M $742d $d5 >M $bec4 $e1 >M $bec5 $a3 >M
T{ op PC S A X Y P -> $bec6 $5d $80 $f1 $27 $bc }T T{ $0094 M $0095 M $00a3 M $742d M $bec4 M $bec5 M -> $2d $74 $e0 $d5 $e1 $a3 }T
$1f1a >PC $fa >S $cf >A $fd >X $4b >Y $b2 >P $00e2 $59 >M $00e3 $ad >M $00e5 $6f >M $1f1a $e1 >M $1f1b $e5 >M $ad59 $0b >M
T{ op PC S A X Y P -> $1f1c $fa $c3 $fd $4b $b1 }T T{ $00e2 M $00e3 M $00e5 M $1f1a M $1f1b M $ad59 M -> $59 $ad $6f $e1 $e5 $0b }T
$973b >PC $a6 >S $96 >A $36 >X $cd >Y $3d >P $00b7 $c5 >M $00ed $cb >M $00ee $be >M $973b $e1 >M $973c $b7 >M $becb $50 >M
T{ op PC S A X Y P -> $973d $a6 $46 $36 $cd $7d }T T{ $00b7 M $00ed M $00ee M $973b M $973c M $becb M -> $c5 $cb $be $e1 $b7 $50 }T
$df23 >PC $25 >S $4f >A $c1 >X $7d >Y $7c >P $0000 $92 >M $0001 $c5 >M $003f $c3 >M $c592 $61 >M $df23 $e1 >M $df24 $3f >M
T{ op PC S A X Y P -> $df25 $25 $8d $c1 $7d $bc }T T{ $0000 M $0001 M $003f M $c592 M $df23 M $df24 M -> $92 $c5 $c3 $61 $e1 $3f }T
$8e51 >PC $9a >S $a7 >A $d9 >X $1d >Y $fd >P $006d $f6 >M $006e $bc >M $0094 $54 >M $8e51 $e1 >M $8e52 $94 >M $bcf6 $57 >M
T{ op PC S A X Y P -> $8e53 $9a $50 $d9 $1d $7d }T T{ $006d M $006e M $0094 M $8e51 M $8e52 M $bcf6 M -> $f6 $bc $54 $e1 $94 $57 }T
$14bb >PC $e5 >S $37 >A $18 >X $10 >Y $72 >P $000b $34 >M $0023 $40 >M $0024 $98 >M $14bb $e1 >M $14bc $0b >M $9840 $9b >M
T{ op PC S A X Y P -> $14bd $e5 $9b $18 $10 $f0 }T T{ $000b M $0023 M $0024 M $14bb M $14bc M $9840 M -> $34 $40 $98 $e1 $0b $9b }T
( e2 )
$b622 >PC $44 >S $e0 >A $a8 >X $e1 >Y $67 >P $b622 $e2 >M $b623 $e1 >M $b624 $f8 >M
T{ op PC S A X Y P -> $b624 $44 $e0 $a8 $e1 $67 }T T{ $b622 M $b623 M $b624 M -> $e2 $e1 $f8 }T
$f2a6 >PC $28 >S $5d >A $ff >X $d4 >Y $e3 >P $f2a6 $e2 >M $f2a7 $21 >M $f2a8 $84 >M
T{ op PC S A X Y P -> $f2a8 $28 $5d $ff $d4 $e3 }T T{ $f2a6 M $f2a7 M $f2a8 M -> $e2 $21 $84 }T
$16c2 >PC $cc >S $2b >A $07 >X $27 >Y $26 >P $16c2 $e2 >M $16c3 $65 >M $16c4 $a1 >M
T{ op PC S A X Y P -> $16c4 $cc $2b $07 $27 $26 }T T{ $16c2 M $16c3 M $16c4 M -> $e2 $65 $a1 }T
$3d75 >PC $c5 >S $1b >A $10 >X $94 >Y $a9 >P $3d75 $e2 >M $3d76 $20 >M $3d77 $57 >M
T{ op PC S A X Y P -> $3d77 $c5 $1b $10 $94 $a9 }T T{ $3d75 M $3d76 M $3d77 M -> $e2 $20 $57 }T
$3a58 >PC $43 >S $27 >A $06 >X $e0 >Y $af >P $3a58 $e2 >M $3a59 $68 >M $3a5a $75 >M
T{ op PC S A X Y P -> $3a5a $43 $27 $06 $e0 $af }T T{ $3a58 M $3a59 M $3a5a M -> $e2 $68 $75 }T
$611d >PC $8e >S $cb >A $6b >X $32 >Y $e2 >P $611d $e2 >M $611e $f9 >M $611f $c2 >M
T{ op PC S A X Y P -> $611f $8e $cb $6b $32 $e2 }T T{ $611d M $611e M $611f M -> $e2 $f9 $c2 }T
$a2ce >PC $19 >S $d7 >A $18 >X $5c >Y $a2 >P $a2ce $e2 >M $a2cf $0f >M $a2d0 $d4 >M
T{ op PC S A X Y P -> $a2d0 $19 $d7 $18 $5c $a2 }T T{ $a2ce M $a2cf M $a2d0 M -> $e2 $0f $d4 }T
$665c >PC $03 >S $cc >A $94 >X $9c >Y $60 >P $665c $e2 >M $665d $c9 >M $665e $89 >M
T{ op PC S A X Y P -> $665e $03 $cc $94 $9c $60 }T T{ $665c M $665d M $665e M -> $e2 $c9 $89 }T
$2c8d >PC $0d >S $78 >A $a4 >X $80 >Y $69 >P $2c8d $e2 >M $2c8e $47 >M $2c8f $54 >M
T{ op PC S A X Y P -> $2c8f $0d $78 $a4 $80 $69 }T T{ $2c8d M $2c8e M $2c8f M -> $e2 $47 $54 }T
$8c94 >PC $2e >S $2b >A $be >X $b7 >Y $a7 >P $8c94 $e2 >M $8c95 $82 >M $8c96 $1b >M
T{ op PC S A X Y P -> $8c96 $2e $2b $be $b7 $a7 }T T{ $8c94 M $8c95 M $8c96 M -> $e2 $82 $1b }T
$a746 >PC $f3 >S $0a >A $3b >X $9b >Y $22 >P $a746 $e2 >M $a747 $20 >M $a748 $d2 >M
T{ op PC S A X Y P -> $a748 $f3 $0a $3b $9b $22 }T T{ $a746 M $a747 M $a748 M -> $e2 $20 $d2 }T
$667b >PC $4e >S $ef >A $ec >X $0e >Y $2c >P $667b $e2 >M $667c $a3 >M $667d $b5 >M
T{ op PC S A X Y P -> $667d $4e $ef $ec $0e $2c }T T{ $667b M $667c M $667d M -> $e2 $a3 $b5 }T
$eca5 >PC $51 >S $06 >A $04 >X $fa >Y $69 >P $eca5 $e2 >M $eca6 $aa >M $eca7 $7a >M
T{ op PC S A X Y P -> $eca7 $51 $06 $04 $fa $69 }T T{ $eca5 M $eca6 M $eca7 M -> $e2 $aa $7a }T
$dc26 >PC $7b >S $9a >A $7a >X $04 >Y $e4 >P $dc26 $e2 >M $dc27 $90 >M $dc28 $df >M
T{ op PC S A X Y P -> $dc28 $7b $9a $7a $04 $e4 }T T{ $dc26 M $dc27 M $dc28 M -> $e2 $90 $df }T
$ff66 >PC $48 >S $01 >A $49 >X $77 >Y $a9 >P $ff66 $e2 >M $ff67 $68 >M $ff68 $54 >M
T{ op PC S A X Y P -> $ff68 $48 $01 $49 $77 $a9 }T T{ $ff66 M $ff67 M $ff68 M -> $e2 $68 $54 }T
$e732 >PC $07 >S $98 >A $8d >X $a4 >Y $25 >P $e732 $e2 >M $e733 $0b >M $e734 $5c >M
T{ op PC S A X Y P -> $e734 $07 $98 $8d $a4 $25 }T T{ $e732 M $e733 M $e734 M -> $e2 $0b $5c }T
( e3 )
$91f0 >PC $56 >S $4a >A $0b >X $c6 >Y $ab >P $91f0 $e3 >M $91f1 $03 >M $91f2 $a2 >M
T{ op PC S A X Y P -> $91f1 $56 $4a $0b $c6 $ab }T T{ $91f0 M $91f1 M $91f2 M -> $e3 $03 $a2 }T
$e0c5 >PC $d7 >S $47 >A $33 >X $e7 >Y $ed >P $e0c5 $e3 >M $e0c6 $4c >M $e0c7 $57 >M
T{ op PC S A X Y P -> $e0c6 $d7 $47 $33 $e7 $ed }T T{ $e0c5 M $e0c6 M $e0c7 M -> $e3 $4c $57 }T
$edd8 >PC $85 >S $b2 >A $0e >X $5a >Y $65 >P $edd8 $e3 >M $edd9 $43 >M $edda $94 >M
T{ op PC S A X Y P -> $edd9 $85 $b2 $0e $5a $65 }T T{ $edd8 M $edd9 M $edda M -> $e3 $43 $94 }T
$32b3 >PC $28 >S $7e >A $de >X $56 >Y $e0 >P $32b3 $e3 >M $32b4 $81 >M $32b5 $6b >M
T{ op PC S A X Y P -> $32b4 $28 $7e $de $56 $e0 }T T{ $32b3 M $32b4 M $32b5 M -> $e3 $81 $6b }T
$7528 >PC $31 >S $34 >A $00 >X $05 >Y $a9 >P $7528 $e3 >M $7529 $2e >M $752a $cd >M
T{ op PC S A X Y P -> $7529 $31 $34 $00 $05 $a9 }T T{ $7528 M $7529 M $752a M -> $e3 $2e $cd }T
$7aa8 >PC $87 >S $eb >A $c1 >X $1a >Y $e3 >P $7aa8 $e3 >M $7aa9 $02 >M $7aaa $e1 >M
T{ op PC S A X Y P -> $7aa9 $87 $eb $c1 $1a $e3 }T T{ $7aa8 M $7aa9 M $7aaa M -> $e3 $02 $e1 }T
$8f64 >PC $27 >S $02 >A $9c >X $00 >Y $26 >P $8f64 $e3 >M $8f65 $0d >M $8f66 $c6 >M
T{ op PC S A X Y P -> $8f65 $27 $02 $9c $00 $26 }T T{ $8f64 M $8f65 M $8f66 M -> $e3 $0d $c6 }T
$45cc >PC $7b >S $4b >A $5c >X $84 >Y $aa >P $45cc $e3 >M $45cd $db >M $45ce $3f >M
T{ op PC S A X Y P -> $45cd $7b $4b $5c $84 $aa }T T{ $45cc M $45cd M $45ce M -> $e3 $db $3f }T
$50e6 >PC $21 >S $48 >A $b6 >X $f4 >Y $2f >P $50e6 $e3 >M $50e7 $f0 >M $50e8 $97 >M
T{ op PC S A X Y P -> $50e7 $21 $48 $b6 $f4 $2f }T T{ $50e6 M $50e7 M $50e8 M -> $e3 $f0 $97 }T
$a111 >PC $cb >S $6a >A $6f >X $cf >Y $e8 >P $a111 $e3 >M $a112 $f7 >M $a113 $0e >M
T{ op PC S A X Y P -> $a112 $cb $6a $6f $cf $e8 }T T{ $a111 M $a112 M $a113 M -> $e3 $f7 $0e }T
$cd8e >PC $ff >S $1d >A $63 >X $68 >Y $28 >P $cd8e $e3 >M $cd8f $51 >M $cd90 $d7 >M
T{ op PC S A X Y P -> $cd8f $ff $1d $63 $68 $28 }T T{ $cd8e M $cd8f M $cd90 M -> $e3 $51 $d7 }T
$5f57 >PC $15 >S $9c >A $26 >X $0c >Y $a8 >P $5f57 $e3 >M $5f58 $57 >M $5f59 $fd >M
T{ op PC S A X Y P -> $5f58 $15 $9c $26 $0c $a8 }T T{ $5f57 M $5f58 M $5f59 M -> $e3 $57 $fd }T
$9f14 >PC $33 >S $5f >A $bf >X $b2 >Y $6a >P $9f14 $e3 >M $9f15 $f9 >M $9f16 $35 >M
T{ op PC S A X Y P -> $9f15 $33 $5f $bf $b2 $6a }T T{ $9f14 M $9f15 M $9f16 M -> $e3 $f9 $35 }T
$f109 >PC $09 >S $41 >A $51 >X $c9 >Y $69 >P $f109 $e3 >M $f10a $19 >M $f10b $37 >M
T{ op PC S A X Y P -> $f10a $09 $41 $51 $c9 $69 }T T{ $f109 M $f10a M $f10b M -> $e3 $19 $37 }T
$610c >PC $62 >S $6e >A $e5 >X $48 >Y $22 >P $610c $e3 >M $610d $39 >M $610e $b4 >M
T{ op PC S A X Y P -> $610d $62 $6e $e5 $48 $22 }T T{ $610c M $610d M $610e M -> $e3 $39 $b4 }T
$0c70 >PC $d7 >S $04 >A $88 >X $c6 >Y $a7 >P $0c70 $e3 >M $0c71 $4d >M $0c72 $b4 >M
T{ op PC S A X Y P -> $0c71 $d7 $04 $88 $c6 $a7 }T T{ $0c70 M $0c71 M $0c72 M -> $e3 $4d $b4 }T
( e4 )
$203a >PC $c4 >S $fa >A $5c >X $a1 >Y $a3 >P $00bb $46 >M $203a $e4 >M $203b $bb >M $203c $50 >M
T{ op PC S A X Y P -> $203c $c4 $fa $5c $a1 $21 }T T{ $00bb M $203a M $203b M $203c M -> $46 $e4 $bb $50 }T
$141b >PC $23 >S $95 >A $44 >X $07 >Y $62 >P $00cf $e4 >M $141b $e4 >M $141c $cf >M $141d $62 >M
T{ op PC S A X Y P -> $141d $23 $95 $44 $07 $60 }T T{ $00cf M $141b M $141c M $141d M -> $e4 $e4 $cf $62 }T
$1158 >PC $9d >S $87 >A $27 >X $0c >Y $e2 >P $00fa $8c >M $1158 $e4 >M $1159 $fa >M $115a $ba >M
T{ op PC S A X Y P -> $115a $9d $87 $27 $0c $e0 }T T{ $00fa M $1158 M $1159 M $115a M -> $8c $e4 $fa $ba }T
$b832 >PC $e8 >S $29 >A $b8 >X $bf >Y $e5 >P $0066 $e4 >M $b832 $e4 >M $b833 $66 >M $b834 $be >M
T{ op PC S A X Y P -> $b834 $e8 $29 $b8 $bf $e4 }T T{ $0066 M $b832 M $b833 M $b834 M -> $e4 $e4 $66 $be }T
$a9a9 >PC $44 >S $a5 >A $c4 >X $46 >Y $6a >P $007e $0b >M $a9a9 $e4 >M $a9aa $7e >M $a9ab $68 >M
T{ op PC S A X Y P -> $a9ab $44 $a5 $c4 $46 $e9 }T T{ $007e M $a9a9 M $a9aa M $a9ab M -> $0b $e4 $7e $68 }T
$ada6 >PC $b6 >S $55 >A $7d >X $50 >Y $eb >P $0039 $0e >M $ada6 $e4 >M $ada7 $39 >M $ada8 $b9 >M
T{ op PC S A X Y P -> $ada8 $b6 $55 $7d $50 $69 }T T{ $0039 M $ada6 M $ada7 M $ada8 M -> $0e $e4 $39 $b9 }T
$02aa >PC $5d >S $ea >A $94 >X $e7 >Y $6e >P $00d2 $14 >M $02aa $e4 >M $02ab $d2 >M $02ac $9f >M
T{ op PC S A X Y P -> $02ac $5d $ea $94 $e7 $ed }T T{ $00d2 M $02aa M $02ab M $02ac M -> $14 $e4 $d2 $9f }T
$c23c >PC $cb >S $30 >A $02 >X $82 >Y $e2 >P $00f2 $47 >M $c23c $e4 >M $c23d $f2 >M $c23e $ca >M
T{ op PC S A X Y P -> $c23e $cb $30 $02 $82 $e0 }T T{ $00f2 M $c23c M $c23d M $c23e M -> $47 $e4 $f2 $ca }T
$fa77 >PC $99 >S $20 >A $81 >X $d5 >Y $a7 >P $0086 $5a >M $fa77 $e4 >M $fa78 $86 >M $fa79 $e4 >M
T{ op PC S A X Y P -> $fa79 $99 $20 $81 $d5 $25 }T T{ $0086 M $fa77 M $fa78 M $fa79 M -> $5a $e4 $86 $e4 }T
$64d3 >PC $20 >S $bc >A $34 >X $21 >Y $aa >P $0037 $6c >M $64d3 $e4 >M $64d4 $37 >M $64d5 $52 >M
T{ op PC S A X Y P -> $64d5 $20 $bc $34 $21 $a8 }T T{ $0037 M $64d3 M $64d4 M $64d5 M -> $6c $e4 $37 $52 }T
$f855 >PC $60 >S $c5 >A $c3 >X $74 >Y $27 >P $004b $74 >M $f855 $e4 >M $f856 $4b >M $f857 $eb >M
T{ op PC S A X Y P -> $f857 $60 $c5 $c3 $74 $25 }T T{ $004b M $f855 M $f856 M $f857 M -> $74 $e4 $4b $eb }T
$8856 >PC $81 >S $53 >A $51 >X $78 >Y $62 >P $0005 $8d >M $8856 $e4 >M $8857 $05 >M $8858 $02 >M
T{ op PC S A X Y P -> $8858 $81 $53 $51 $78 $e0 }T T{ $0005 M $8856 M $8857 M $8858 M -> $8d $e4 $05 $02 }T
$be06 >PC $c9 >S $e5 >A $97 >X $fd >Y $e9 >P $0058 $bc >M $be06 $e4 >M $be07 $58 >M $be08 $fe >M
T{ op PC S A X Y P -> $be08 $c9 $e5 $97 $fd $e8 }T T{ $0058 M $be06 M $be07 M $be08 M -> $bc $e4 $58 $fe }T
$6312 >PC $08 >S $98 >A $32 >X $1a >Y $26 >P $0091 $bd >M $6312 $e4 >M $6313 $91 >M $6314 $53 >M
T{ op PC S A X Y P -> $6314 $08 $98 $32 $1a $24 }T T{ $0091 M $6312 M $6313 M $6314 M -> $bd $e4 $91 $53 }T
$fd6b >PC $9c >S $8c >A $7a >X $8a >Y $63 >P $005e $1d >M $fd6b $e4 >M $fd6c $5e >M $fd6d $76 >M
T{ op PC S A X Y P -> $fd6d $9c $8c $7a $8a $61 }T T{ $005e M $fd6b M $fd6c M $fd6d M -> $1d $e4 $5e $76 }T
$b5a3 >PC $0d >S $56 >A $23 >X $ad >Y $66 >P $00e7 $4f >M $b5a3 $e4 >M $b5a4 $e7 >M $b5a5 $87 >M
T{ op PC S A X Y P -> $b5a5 $0d $56 $23 $ad $e4 }T T{ $00e7 M $b5a3 M $b5a4 M $b5a5 M -> $4f $e4 $e7 $87 }T
( e5 )
$5f30 >PC $25 >S $a6 >A $e9 >X $cc >Y $35 >P $0081 $4b >M $5f30 $e5 >M $5f31 $81 >M
T{ op PC S A X Y P -> $5f32 $25 $5b $e9 $cc $75 }T T{ $0081 M $5f30 M $5f31 M -> $4b $e5 $81 }T
$6c70 >PC $10 >S $ac >A $94 >X $08 >Y $f7 >P $00c8 $23 >M $6c70 $e5 >M $6c71 $c8 >M
T{ op PC S A X Y P -> $6c72 $10 $89 $94 $08 $b5 }T T{ $00c8 M $6c70 M $6c71 M -> $23 $e5 $c8 }T
$ca58 >PC $bf >S $08 >A $af >X $b3 >Y $bb >P $004f $0c >M $ca58 $e5 >M $ca59 $4f >M
T{ op PC S A X Y P -> $ca5a $bf $96 $af $b3 $b8 }T T{ $004f M $ca58 M $ca59 M -> $0c $e5 $4f }T
$ed64 >PC $ac >S $98 >A $5f >X $c4 >Y $73 >P $0000 $54 >M $ed64 $e5 >M $ed65 $00 >M
T{ op PC S A X Y P -> $ed66 $ac $44 $5f $c4 $71 }T T{ $0000 M $ed64 M $ed65 M -> $54 $e5 $00 }T
$261f >PC $b3 >S $8f >A $9b >X $b5 >Y $bd >P $0039 $aa >M $261f $e5 >M $2620 $39 >M
T{ op PC S A X Y P -> $2621 $b3 $85 $9b $b5 $bc }T T{ $0039 M $261f M $2620 M -> $aa $e5 $39 }T
$54e9 >PC $46 >S $2c >A $45 >X $20 >Y $b6 >P $0095 $26 >M $54e9 $e5 >M $54ea $95 >M
T{ op PC S A X Y P -> $54eb $46 $05 $45 $20 $35 }T T{ $0095 M $54e9 M $54ea M -> $26 $e5 $95 }T
$7e1d >PC $5f >S $dc >A $09 >X $12 >Y $bc >P $0072 $12 >M $7e1d $e5 >M $7e1e $72 >M
T{ op PC S A X Y P -> $7e1f $5f $c9 $09 $12 $bd }T T{ $0072 M $7e1d M $7e1e M -> $12 $e5 $72 }T
$5f90 >PC $c3 >S $45 >A $69 >X $fd >Y $f0 >P $0027 $6b >M $5f90 $e5 >M $5f91 $27 >M
T{ op PC S A X Y P -> $5f92 $c3 $d9 $69 $fd $b0 }T T{ $0027 M $5f90 M $5f91 M -> $6b $e5 $27 }T
$688f >PC $0b >S $19 >A $b8 >X $50 >Y $b1 >P $00b1 $07 >M $688f $e5 >M $6890 $b1 >M
T{ op PC S A X Y P -> $6891 $0b $12 $b8 $50 $31 }T T{ $00b1 M $688f M $6890 M -> $07 $e5 $b1 }T
$a22c >PC $d9 >S $7f >A $a4 >X $d9 >Y $fd >P $00d9 $ce >M $a22c $e5 >M $a22d $d9 >M
T{ op PC S A X Y P -> $a22e $d9 $51 $a4 $d9 $7c }T T{ $00d9 M $a22c M $a22d M -> $ce $e5 $d9 }T
$680f >PC $fb >S $d7 >A $44 >X $3e >Y $7d >P $008a $39 >M $680f $e5 >M $6810 $8a >M
T{ op PC S A X Y P -> $6811 $fb $98 $44 $3e $bd }T T{ $008a M $680f M $6810 M -> $39 $e5 $8a }T
$199a >PC $ed >S $a6 >A $fe >X $21 >Y $b7 >P $00ac $ac >M $199a $e5 >M $199b $ac >M
T{ op PC S A X Y P -> $199c $ed $fa $fe $21 $b4 }T T{ $00ac M $199a M $199b M -> $ac $e5 $ac }T
$88ef >PC $91 >S $3f >A $cf >X $61 >Y $72 >P $003c $b4 >M $88ef $e5 >M $88f0 $3c >M
T{ op PC S A X Y P -> $88f1 $91 $8a $cf $61 $f0 }T T{ $003c M $88ef M $88f0 M -> $b4 $e5 $3c }T
$11c2 >PC $2f >S $54 >A $91 >X $c7 >Y $3e >P $002d $e0 >M $11c2 $e5 >M $11c3 $2d >M
T{ op PC S A X Y P -> $11c4 $2f $13 $91 $c7 $3c }T T{ $002d M $11c2 M $11c3 M -> $e0 $e5 $2d }T
$878c >PC $4c >S $3f >A $0a >X $30 >Y $35 >P $007a $2d >M $878c $e5 >M $878d $7a >M
T{ op PC S A X Y P -> $878e $4c $12 $0a $30 $35 }T T{ $007a M $878c M $878d M -> $2d $e5 $7a }T
$0dbc >PC $28 >S $16 >A $23 >X $00 >Y $b1 >P $0030 $15 >M $0dbc $e5 >M $0dbd $30 >M
T{ op PC S A X Y P -> $0dbe $28 $01 $23 $00 $31 }T T{ $0030 M $0dbc M $0dbd M -> $15 $e5 $30 }T
( e6 )
$846b >PC $9a >S $48 >A $52 >X $04 >Y $ed >P $00bc $24 >M $846b $e6 >M $846c $bc >M $846d $b2 >M
T{ op PC S A X Y P -> $846d $9a $48 $52 $04 $6d }T T{ $00bc M $846b M $846c M $846d M -> $25 $e6 $bc $b2 }T
$1c42 >PC $2c >S $32 >A $8f >X $94 >Y $6c >P $0080 $1b >M $1c42 $e6 >M $1c43 $80 >M $1c44 $9f >M
T{ op PC S A X Y P -> $1c44 $2c $32 $8f $94 $6c }T T{ $0080 M $1c42 M $1c43 M $1c44 M -> $1c $e6 $80 $9f }T
$0e24 >PC $15 >S $42 >A $e3 >X $7d >Y $63 >P $006a $97 >M $0e24 $e6 >M $0e25 $6a >M $0e26 $cb >M
T{ op PC S A X Y P -> $0e26 $15 $42 $e3 $7d $e1 }T T{ $006a M $0e24 M $0e25 M $0e26 M -> $98 $e6 $6a $cb }T
$39b1 >PC $31 >S $f5 >A $f4 >X $d2 >Y $6d >P $0001 $70 >M $39b1 $e6 >M $39b2 $01 >M $39b3 $ab >M
T{ op PC S A X Y P -> $39b3 $31 $f5 $f4 $d2 $6d }T T{ $0001 M $39b1 M $39b2 M $39b3 M -> $71 $e6 $01 $ab }T
$6007 >PC $df >S $22 >A $25 >X $53 >Y $af >P $0031 $84 >M $6007 $e6 >M $6008 $31 >M $6009 $49 >M
T{ op PC S A X Y P -> $6009 $df $22 $25 $53 $ad }T T{ $0031 M $6007 M $6008 M $6009 M -> $85 $e6 $31 $49 }T
$b7ee >PC $88 >S $ae >A $2b >X $30 >Y $ae >P $001e $b9 >M $b7ee $e6 >M $b7ef $1e >M $b7f0 $cd >M
T{ op PC S A X Y P -> $b7f0 $88 $ae $2b $30 $ac }T T{ $001e M $b7ee M $b7ef M $b7f0 M -> $ba $e6 $1e $cd }T
$cc27 >PC $e1 >S $e9 >A $af >X $cc >Y $ab >P $0049 $0f >M $cc27 $e6 >M $cc28 $49 >M $cc29 $4d >M
T{ op PC S A X Y P -> $cc29 $e1 $e9 $af $cc $29 }T T{ $0049 M $cc27 M $cc28 M $cc29 M -> $10 $e6 $49 $4d }T
$6d74 >PC $2d >S $b4 >A $59 >X $ad >Y $60 >P $00a0 $04 >M $6d74 $e6 >M $6d75 $a0 >M $6d76 $ce >M
T{ op PC S A X Y P -> $6d76 $2d $b4 $59 $ad $60 }T T{ $00a0 M $6d74 M $6d75 M $6d76 M -> $05 $e6 $a0 $ce }T
$ab94 >PC $91 >S $a5 >A $d0 >X $50 >Y $22 >P $005b $cc >M $ab94 $e6 >M $ab95 $5b >M $ab96 $18 >M
T{ op PC S A X Y P -> $ab96 $91 $a5 $d0 $50 $a0 }T T{ $005b M $ab94 M $ab95 M $ab96 M -> $cd $e6 $5b $18 }T
$6bda >PC $1f >S $5c >A $ee >X $2f >Y $25 >P $0098 $3b >M $6bda $e6 >M $6bdb $98 >M $6bdc $38 >M
T{ op PC S A X Y P -> $6bdc $1f $5c $ee $2f $25 }T T{ $0098 M $6bda M $6bdb M $6bdc M -> $3c $e6 $98 $38 }T
$d6df >PC $72 >S $ce >A $0a >X $71 >Y $22 >P $00fe $55 >M $d6df $e6 >M $d6e0 $fe >M $d6e1 $2b >M
T{ op PC S A X Y P -> $d6e1 $72 $ce $0a $71 $20 }T T{ $00fe M $d6df M $d6e0 M $d6e1 M -> $56 $e6 $fe $2b }T
$9619 >PC $a1 >S $96 >A $3c >X $00 >Y $6e >P $0082 $0c >M $9619 $e6 >M $961a $82 >M $961b $37 >M
T{ op PC S A X Y P -> $961b $a1 $96 $3c $00 $6c }T T{ $0082 M $9619 M $961a M $961b M -> $0d $e6 $82 $37 }T
$a885 >PC $d4 >S $eb >A $43 >X $a6 >Y $6d >P $00c8 $11 >M $a885 $e6 >M $a886 $c8 >M $a887 $d5 >M
T{ op PC S A X Y P -> $a887 $d4 $eb $43 $a6 $6d }T T{ $00c8 M $a885 M $a886 M $a887 M -> $12 $e6 $c8 $d5 }T
$1bc2 >PC $b9 >S $b9 >A $dd >X $3d >Y $2f >P $0036 $74 >M $1bc2 $e6 >M $1bc3 $36 >M $1bc4 $aa >M
T{ op PC S A X Y P -> $1bc4 $b9 $b9 $dd $3d $2d }T T{ $0036 M $1bc2 M $1bc3 M $1bc4 M -> $75 $e6 $36 $aa }T
$80b2 >PC $01 >S $19 >A $81 >X $2b >Y $ab >P $0052 $96 >M $80b2 $e6 >M $80b3 $52 >M $80b4 $27 >M
T{ op PC S A X Y P -> $80b4 $01 $19 $81 $2b $a9 }T T{ $0052 M $80b2 M $80b3 M $80b4 M -> $97 $e6 $52 $27 }T
$f23e >PC $11 >S $09 >A $d5 >X $39 >Y $e4 >P $0099 $0d >M $f23e $e6 >M $f23f $99 >M $f240 $d6 >M
T{ op PC S A X Y P -> $f240 $11 $09 $d5 $39 $64 }T T{ $0099 M $f23e M $f23f M $f240 M -> $0e $e6 $99 $d6 }T
( e7 )
$9147 >PC $d8 >S $5d >A $01 >X $1d >Y $68 >P $0018 $d2 >M $9147 $e7 >M $9148 $18 >M $9149 $b5 >M
T{ op PC S A X Y P -> $9149 $d8 $5d $01 $1d $68 }T T{ $0018 M $9147 M $9148 M $9149 M -> $d2 $e7 $18 $b5 }T
$7605 >PC $da >S $f5 >A $8b >X $56 >Y $60 >P $00ec $67 >M $7605 $e7 >M $7606 $ec >M $7607 $28 >M
T{ op PC S A X Y P -> $7607 $da $f5 $8b $56 $60 }T T{ $00ec M $7605 M $7606 M $7607 M -> $67 $e7 $ec $28 }T
$8d38 >PC $30 >S $7d >A $85 >X $55 >Y $2b >P $00f8 $72 >M $8d38 $e7 >M $8d39 $f8 >M $8d3a $13 >M
T{ op PC S A X Y P -> $8d3a $30 $7d $85 $55 $2b }T T{ $00f8 M $8d38 M $8d39 M $8d3a M -> $72 $e7 $f8 $13 }T
$2034 >PC $2c >S $d5 >A $12 >X $ef >Y $67 >P $005c $7c >M $2034 $e7 >M $2035 $5c >M $2036 $f5 >M
T{ op PC S A X Y P -> $2036 $2c $d5 $12 $ef $67 }T T{ $005c M $2034 M $2035 M $2036 M -> $7c $e7 $5c $f5 }T
$a7e7 >PC $a9 >S $ec >A $0e >X $ff >Y $ec >P $0073 $9b >M $a7e7 $e7 >M $a7e8 $73 >M $a7e9 $56 >M
T{ op PC S A X Y P -> $a7e9 $a9 $ec $0e $ff $ec }T T{ $0073 M $a7e7 M $a7e8 M $a7e9 M -> $db $e7 $73 $56 }T
$bb47 >PC $92 >S $90 >A $88 >X $17 >Y $ae >P $0093 $2e >M $bb47 $e7 >M $bb48 $93 >M $bb49 $f5 >M
T{ op PC S A X Y P -> $bb49 $92 $90 $88 $17 $ae }T T{ $0093 M $bb47 M $bb48 M $bb49 M -> $6e $e7 $93 $f5 }T
$65c8 >PC $0b >S $31 >A $e6 >X $a5 >Y $6d >P $0082 $04 >M $65c8 $e7 >M $65c9 $82 >M $65ca $38 >M
T{ op PC S A X Y P -> $65ca $0b $31 $e6 $a5 $6d }T T{ $0082 M $65c8 M $65c9 M $65ca M -> $44 $e7 $82 $38 }T
$9cae >PC $58 >S $f7 >A $8f >X $e7 >Y $6a >P $003f $aa >M $9cae $e7 >M $9caf $3f >M $9cb0 $2f >M
T{ op PC S A X Y P -> $9cb0 $58 $f7 $8f $e7 $6a }T T{ $003f M $9cae M $9caf M $9cb0 M -> $ea $e7 $3f $2f }T
$dc5d >PC $4b >S $6b >A $d5 >X $da >Y $ea >P $00af $c1 >M $dc5d $e7 >M $dc5e $af >M $dc5f $7a >M
T{ op PC S A X Y P -> $dc5f $4b $6b $d5 $da $ea }T T{ $00af M $dc5d M $dc5e M $dc5f M -> $c1 $e7 $af $7a }T
$0a8e >PC $32 >S $2a >A $0e >X $5f >Y $2e >P $0015 $82 >M $0a8e $e7 >M $0a8f $15 >M $0a90 $ed >M
T{ op PC S A X Y P -> $0a90 $32 $2a $0e $5f $2e }T T{ $0015 M $0a8e M $0a8f M $0a90 M -> $c2 $e7 $15 $ed }T
$c57f >PC $b2 >S $33 >A $c7 >X $d8 >Y $6f >P $0076 $44 >M $c57f $e7 >M $c580 $76 >M $c581 $e6 >M
T{ op PC S A X Y P -> $c581 $b2 $33 $c7 $d8 $6f }T T{ $0076 M $c57f M $c580 M $c581 M -> $44 $e7 $76 $e6 }T
$a0db >PC $03 >S $51 >A $1b >X $86 >Y $ae >P $00b9 $b8 >M $a0db $e7 >M $a0dc $b9 >M $a0dd $13 >M
T{ op PC S A X Y P -> $a0dd $03 $51 $1b $86 $ae }T T{ $00b9 M $a0db M $a0dc M $a0dd M -> $f8 $e7 $b9 $13 }T
$5cd1 >PC $be >S $8d >A $d7 >X $87 >Y $6e >P $00a6 $a8 >M $5cd1 $e7 >M $5cd2 $a6 >M $5cd3 $78 >M
T{ op PC S A X Y P -> $5cd3 $be $8d $d7 $87 $6e }T T{ $00a6 M $5cd1 M $5cd2 M $5cd3 M -> $e8 $e7 $a6 $78 }T
$20dd >PC $ff >S $20 >A $b5 >X $d7 >Y $ed >P $008e $c5 >M $20dd $e7 >M $20de $8e >M $20df $1c >M
T{ op PC S A X Y P -> $20df $ff $20 $b5 $d7 $ed }T T{ $008e M $20dd M $20de M $20df M -> $c5 $e7 $8e $1c }T
$8458 >PC $a7 >S $c3 >A $ce >X $5f >Y $aa >P $0016 $0b >M $8458 $e7 >M $8459 $16 >M $845a $5e >M
T{ op PC S A X Y P -> $845a $a7 $c3 $ce $5f $aa }T T{ $0016 M $8458 M $8459 M $845a M -> $4b $e7 $16 $5e }T
$5bbc >PC $15 >S $b9 >A $0e >X $e4 >Y $ae >P $005e $38 >M $5bbc $e7 >M $5bbd $5e >M $5bbe $af >M
T{ op PC S A X Y P -> $5bbe $15 $b9 $0e $e4 $ae }T T{ $005e M $5bbc M $5bbd M $5bbe M -> $78 $e7 $5e $af }T
( e8 )
$0229 >PC $ce >S $b9 >A $c9 >X $e9 >Y $a5 >P $0229 $e8 >M $022a $64 >M $022b $db >M
T{ op PC S A X Y P -> $022a $ce $b9 $ca $e9 $a5 }T T{ $0229 M $022a M $022b M -> $e8 $64 $db }T
$0c8f >PC $62 >S $30 >A $5f >X $d2 >Y $a9 >P $0c8f $e8 >M $0c90 $83 >M $0c91 $58 >M
T{ op PC S A X Y P -> $0c90 $62 $30 $60 $d2 $29 }T T{ $0c8f M $0c90 M $0c91 M -> $e8 $83 $58 }T
$1e26 >PC $29 >S $14 >A $4c >X $a6 >Y $29 >P $1e26 $e8 >M $1e27 $a3 >M $1e28 $7e >M
T{ op PC S A X Y P -> $1e27 $29 $14 $4d $a6 $29 }T T{ $1e26 M $1e27 M $1e28 M -> $e8 $a3 $7e }T
$47c3 >PC $b3 >S $ed >A $d7 >X $57 >Y $ef >P $47c3 $e8 >M $47c4 $09 >M $47c5 $59 >M
T{ op PC S A X Y P -> $47c4 $b3 $ed $d8 $57 $ed }T T{ $47c3 M $47c4 M $47c5 M -> $e8 $09 $59 }T
$5321 >PC $8e >S $36 >A $be >X $7c >Y $e5 >P $5321 $e8 >M $5322 $20 >M $5323 $e5 >M
T{ op PC S A X Y P -> $5322 $8e $36 $bf $7c $e5 }T T{ $5321 M $5322 M $5323 M -> $e8 $20 $e5 }T
$b516 >PC $49 >S $c8 >A $50 >X $63 >Y $a2 >P $b516 $e8 >M $b517 $2e >M $b518 $09 >M
T{ op PC S A X Y P -> $b517 $49 $c8 $51 $63 $20 }T T{ $b516 M $b517 M $b518 M -> $e8 $2e $09 }T
$ae73 >PC $dd >S $2f >A $da >X $b6 >Y $ac >P $ae73 $e8 >M $ae74 $30 >M $ae75 $36 >M
T{ op PC S A X Y P -> $ae74 $dd $2f $db $b6 $ac }T T{ $ae73 M $ae74 M $ae75 M -> $e8 $30 $36 }T
$3d18 >PC $2d >S $54 >A $ce >X $b4 >Y $29 >P $3d18 $e8 >M $3d19 $be >M $3d1a $eb >M
T{ op PC S A X Y P -> $3d19 $2d $54 $cf $b4 $a9 }T T{ $3d18 M $3d19 M $3d1a M -> $e8 $be $eb }T
$02b2 >PC $14 >S $e7 >A $f6 >X $75 >Y $ec >P $02b2 $e8 >M $02b3 $de >M $02b4 $15 >M
T{ op PC S A X Y P -> $02b3 $14 $e7 $f7 $75 $ec }T T{ $02b2 M $02b3 M $02b4 M -> $e8 $de $15 }T
$5356 >PC $eb >S $3a >A $36 >X $34 >Y $ee >P $5356 $e8 >M $5357 $98 >M $5358 $bd >M
T{ op PC S A X Y P -> $5357 $eb $3a $37 $34 $6c }T T{ $5356 M $5357 M $5358 M -> $e8 $98 $bd }T
$fbd0 >PC $27 >S $e5 >A $95 >X $a7 >Y $27 >P $fbd0 $e8 >M $fbd1 $6f >M $fbd2 $17 >M
T{ op PC S A X Y P -> $fbd1 $27 $e5 $96 $a7 $a5 }T T{ $fbd0 M $fbd1 M $fbd2 M -> $e8 $6f $17 }T
$7fab >PC $e9 >S $7f >A $e0 >X $35 >Y $6f >P $7fab $e8 >M $7fac $0c >M $7fad $47 >M
T{ op PC S A X Y P -> $7fac $e9 $7f $e1 $35 $ed }T T{ $7fab M $7fac M $7fad M -> $e8 $0c $47 }T
$fe56 >PC $4c >S $4f >A $67 >X $02 >Y $eb >P $fe56 $e8 >M $fe57 $b2 >M $fe58 $da >M
T{ op PC S A X Y P -> $fe57 $4c $4f $68 $02 $69 }T T{ $fe56 M $fe57 M $fe58 M -> $e8 $b2 $da }T
$5788 >PC $4e >S $91 >A $6b >X $a3 >Y $e6 >P $5788 $e8 >M $5789 $f7 >M $578a $58 >M
T{ op PC S A X Y P -> $5789 $4e $91 $6c $a3 $64 }T T{ $5788 M $5789 M $578a M -> $e8 $f7 $58 }T
$8e1e >PC $a3 >S $7f >A $aa >X $f6 >Y $22 >P $8e1e $e8 >M $8e1f $f0 >M $8e20 $16 >M
T{ op PC S A X Y P -> $8e1f $a3 $7f $ab $f6 $a0 }T T{ $8e1e M $8e1f M $8e20 M -> $e8 $f0 $16 }T
$dd4f >PC $a6 >S $4e >A $ee >X $0a >Y $6c >P $dd4f $e8 >M $dd50 $6c >M $dd51 $11 >M
T{ op PC S A X Y P -> $dd50 $a6 $4e $ef $0a $ec }T T{ $dd4f M $dd50 M $dd51 M -> $e8 $6c $11 }T
( e9 )
$0c6e >PC $24 >S $a5 >A $98 >X $ee >Y $ba >P $0000 $6c >M $0c6e $e9 >M $0c6f $83 >M
T{ op PC S A X Y P -> $0c70 $24 $21 $98 $ee $39 }T T{ $0000 M $0c6e M $0c6f M -> $6c $e9 $83 }T
$f5fb >PC $bc >S $c5 >A $6d >X $98 >Y $bb >P $0000 $c5 >M $f5fb $e9 >M $f5fc $d4 >M
T{ op PC S A X Y P -> $f5fd $bc $91 $6d $98 $b8 }T T{ $0000 M $f5fb M $f5fc M -> $c5 $e9 $d4 }T
$cead >PC $d0 >S $a7 >A $df >X $ef >Y $bf >P $0000 $ec >M $cead $e9 >M $ceae $3f >M
T{ op PC S A X Y P -> $ceaf $d0 $62 $df $ef $7d }T T{ $0000 M $cead M $ceae M -> $ec $e9 $3f }T
$8d17 >PC $27 >S $e3 >A $bd >X $e0 >Y $fd >P $0000 $7a >M $8d17 $e9 >M $8d18 $b7 >M
T{ op PC S A X Y P -> $8d19 $27 $26 $bd $e0 $3d }T T{ $0000 M $8d17 M $8d18 M -> $7a $e9 $b7 }T
$f162 >PC $03 >S $bd >A $29 >X $55 >Y $76 >P $f162 $e9 >M $f163 $da >M
T{ op PC S A X Y P -> $f164 $03 $e2 $29 $55 $b4 }T T{ $f162 M $f163 M -> $e9 $da }T
$5ca7 >PC $b1 >S $d5 >A $53 >X $61 >Y $7c >P $0000 $ef >M $5ca7 $e9 >M $5ca8 $be >M
T{ op PC S A X Y P -> $5ca9 $b1 $10 $53 $61 $3d }T T{ $0000 M $5ca7 M $5ca8 M -> $ef $e9 $be }T
$ba08 >PC $48 >S $26 >A $45 >X $42 >Y $3d >P $0000 $30 >M $ba08 $e9 >M $ba09 $4a >M
T{ op PC S A X Y P -> $ba0a $48 $76 $45 $42 $3c }T T{ $0000 M $ba08 M $ba09 M -> $30 $e9 $4a }T
$ea6d >PC $f1 >S $19 >A $3a >X $79 >Y $f2 >P $ea6d $e9 >M $ea6e $98 >M
T{ op PC S A X Y P -> $ea6f $f1 $80 $3a $79 $f0 }T T{ $ea6d M $ea6e M -> $e9 $98 }T
$86fb >PC $c7 >S $ae >A $3b >X $7f >Y $7f >P $0000 $69 >M $86fb $e9 >M $86fc $84 >M
T{ op PC S A X Y P -> $86fd $c7 $2a $3b $7f $3d }T T{ $0000 M $86fb M $86fc M -> $69 $e9 $84 }T
$9a8f >PC $c2 >S $d2 >A $08 >X $2f >Y $78 >P $0000 $66 >M $9a8f $e9 >M $9a90 $62 >M
T{ op PC S A X Y P -> $9a91 $c2 $69 $08 $2f $79 }T T{ $0000 M $9a8f M $9a90 M -> $66 $e9 $62 }T
$3e12 >PC $a1 >S $53 >A $90 >X $d3 >Y $38 >P $0000 $e1 >M $3e12 $e9 >M $3e13 $cf >M
T{ op PC S A X Y P -> $3e14 $a1 $1d $90 $d3 $78 }T T{ $0000 M $3e12 M $3e13 M -> $e1 $e9 $cf }T
$cf01 >PC $fa >S $f3 >A $12 >X $61 >Y $79 >P $0000 $91 >M $cf01 $e9 >M $cf02 $14 >M
T{ op PC S A X Y P -> $cf03 $fa $d9 $12 $61 $b9 }T T{ $0000 M $cf01 M $cf02 M -> $91 $e9 $14 }T
$f84c >PC $6b >S $6d >A $34 >X $01 >Y $7d >P $0000 $c1 >M $f84c $e9 >M $f84d $f2 >M
T{ op PC S A X Y P -> $f84e $6b $1b $34 $01 $3c }T T{ $0000 M $f84c M $f84d M -> $c1 $e9 $f2 }T
$b49f >PC $31 >S $36 >A $1e >X $90 >Y $f3 >P $b49f $e9 >M $b4a0 $c0 >M
T{ op PC S A X Y P -> $b4a1 $31 $76 $1e $90 $30 }T T{ $b49f M $b4a0 M -> $e9 $c0 }T
$5671 >PC $1e >S $6c >A $1c >X $c7 >Y $30 >P $5671 $e9 >M $5672 $08 >M
T{ op PC S A X Y P -> $5673 $1e $63 $1c $c7 $31 }T T{ $5671 M $5672 M -> $e9 $08 }T
$3aaf >PC $06 >S $e7 >A $58 >X $39 >Y $74 >P $3aaf $e9 >M $3ab0 $b7 >M
T{ op PC S A X Y P -> $3ab1 $06 $2f $58 $39 $35 }T T{ $3aaf M $3ab0 M -> $e9 $b7 }T
( ea )
$1272 >PC $62 >S $87 >A $cc >X $9b >Y $e7 >P $1272 $ea >M $1273 $17 >M $1274 $36 >M
T{ op PC S A X Y P -> $1273 $62 $87 $cc $9b $e7 }T T{ $1272 M $1273 M $1274 M -> $ea $17 $36 }T
$b480 >PC $db >S $f3 >A $07 >X $44 >Y $2a >P $b480 $ea >M $b481 $08 >M $b482 $4f >M
T{ op PC S A X Y P -> $b481 $db $f3 $07 $44 $2a }T T{ $b480 M $b481 M $b482 M -> $ea $08 $4f }T
$9250 >PC $55 >S $f2 >A $c9 >X $c3 >Y $ab >P $9250 $ea >M $9251 $1f >M $9252 $77 >M
T{ op PC S A X Y P -> $9251 $55 $f2 $c9 $c3 $ab }T T{ $9250 M $9251 M $9252 M -> $ea $1f $77 }T
$2998 >PC $fc >S $72 >A $dc >X $96 >Y $60 >P $2998 $ea >M $2999 $6f >M $299a $85 >M
T{ op PC S A X Y P -> $2999 $fc $72 $dc $96 $60 }T T{ $2998 M $2999 M $299a M -> $ea $6f $85 }T
$e466 >PC $59 >S $e7 >A $60 >X $20 >Y $2c >P $e466 $ea >M $e467 $82 >M $e468 $69 >M
T{ op PC S A X Y P -> $e467 $59 $e7 $60 $20 $2c }T T{ $e466 M $e467 M $e468 M -> $ea $82 $69 }T
$171a >PC $eb >S $86 >A $d2 >X $7c >Y $af >P $171a $ea >M $171b $31 >M $171c $38 >M
T{ op PC S A X Y P -> $171b $eb $86 $d2 $7c $af }T T{ $171a M $171b M $171c M -> $ea $31 $38 }T
$e8aa >PC $13 >S $4a >A $a9 >X $a7 >Y $a0 >P $e8aa $ea >M $e8ab $54 >M $e8ac $07 >M
T{ op PC S A X Y P -> $e8ab $13 $4a $a9 $a7 $a0 }T T{ $e8aa M $e8ab M $e8ac M -> $ea $54 $07 }T
$5452 >PC $b0 >S $40 >A $8a >X $4f >Y $e2 >P $5452 $ea >M $5453 $20 >M $5454 $e7 >M
T{ op PC S A X Y P -> $5453 $b0 $40 $8a $4f $e2 }T T{ $5452 M $5453 M $5454 M -> $ea $20 $e7 }T
$7d85 >PC $80 >S $89 >A $8b >X $f3 >Y $2e >P $7d85 $ea >M $7d86 $5f >M $7d87 $da >M
T{ op PC S A X Y P -> $7d86 $80 $89 $8b $f3 $2e }T T{ $7d85 M $7d86 M $7d87 M -> $ea $5f $da }T
$300f >PC $46 >S $2a >A $bc >X $b4 >Y $a3 >P $300f $ea >M $3010 $39 >M $3011 $b3 >M
T{ op PC S A X Y P -> $3010 $46 $2a $bc $b4 $a3 }T T{ $300f M $3010 M $3011 M -> $ea $39 $b3 }T
$01af >PC $3f >S $3e >A $d2 >X $45 >Y $6f >P $01af $ea >M $01b0 $e1 >M $01b1 $0a >M
T{ op PC S A X Y P -> $01b0 $3f $3e $d2 $45 $6f }T T{ $01af M $01b0 M $01b1 M -> $ea $e1 $0a }T
$20ff >PC $b9 >S $eb >A $df >X $6d >Y $ad >P $20ff $ea >M $2100 $2e >M $2101 $36 >M
T{ op PC S A X Y P -> $2100 $b9 $eb $df $6d $ad }T T{ $20ff M $2100 M $2101 M -> $ea $2e $36 }T
$e1bb >PC $6d >S $c6 >A $20 >X $6b >Y $e9 >P $e1bb $ea >M $e1bc $57 >M $e1bd $84 >M
T{ op PC S A X Y P -> $e1bc $6d $c6 $20 $6b $e9 }T T{ $e1bb M $e1bc M $e1bd M -> $ea $57 $84 }T
$ae62 >PC $ae >S $9c >A $99 >X $4f >Y $e5 >P $ae62 $ea >M $ae63 $ca >M $ae64 $ab >M
T{ op PC S A X Y P -> $ae63 $ae $9c $99 $4f $e5 }T T{ $ae62 M $ae63 M $ae64 M -> $ea $ca $ab }T
$3697 >PC $ee >S $53 >A $83 >X $4b >Y $24 >P $3697 $ea >M $3698 $34 >M $3699 $bd >M
T{ op PC S A X Y P -> $3698 $ee $53 $83 $4b $24 }T T{ $3697 M $3698 M $3699 M -> $ea $34 $bd }T
$4255 >PC $09 >S $f2 >A $e8 >X $fc >Y $21 >P $4255 $ea >M $4256 $45 >M $4257 $09 >M
T{ op PC S A X Y P -> $4256 $09 $f2 $e8 $fc $21 }T T{ $4255 M $4256 M $4257 M -> $ea $45 $09 }T
( eb )
$8de8 >PC $f8 >S $18 >A $6f >X $89 >Y $ef >P $8de8 $eb >M $8de9 $56 >M $8dea $2d >M
T{ op PC S A X Y P -> $8de9 $f8 $18 $6f $89 $ef }T T{ $8de8 M $8de9 M $8dea M -> $eb $56 $2d }T
$be1f >PC $79 >S $1b >A $83 >X $fa >Y $23 >P $be1f $eb >M $be20 $52 >M $be21 $d2 >M
T{ op PC S A X Y P -> $be20 $79 $1b $83 $fa $23 }T T{ $be1f M $be20 M $be21 M -> $eb $52 $d2 }T
$b271 >PC $c2 >S $d6 >A $90 >X $65 >Y $ac >P $b271 $eb >M $b272 $0a >M $b273 $e7 >M
T{ op PC S A X Y P -> $b272 $c2 $d6 $90 $65 $ac }T T{ $b271 M $b272 M $b273 M -> $eb $0a $e7 }T
$aa50 >PC $dc >S $64 >A $aa >X $ce >Y $a3 >P $aa50 $eb >M $aa51 $b5 >M $aa52 $b5 >M
T{ op PC S A X Y P -> $aa51 $dc $64 $aa $ce $a3 }T T{ $aa50 M $aa51 M $aa52 M -> $eb $b5 $b5 }T
$338e >PC $c5 >S $2c >A $24 >X $ea >Y $24 >P $338e $eb >M $338f $c5 >M $3390 $88 >M
T{ op PC S A X Y P -> $338f $c5 $2c $24 $ea $24 }T T{ $338e M $338f M $3390 M -> $eb $c5 $88 }T
$9195 >PC $a4 >S $96 >A $3e >X $5f >Y $af >P $9195 $eb >M $9196 $08 >M $9197 $01 >M
T{ op PC S A X Y P -> $9196 $a4 $96 $3e $5f $af }T T{ $9195 M $9196 M $9197 M -> $eb $08 $01 }T
$89f9 >PC $03 >S $7c >A $ec >X $23 >Y $28 >P $89f9 $eb >M $89fa $e2 >M $89fb $c2 >M
T{ op PC S A X Y P -> $89fa $03 $7c $ec $23 $28 }T T{ $89f9 M $89fa M $89fb M -> $eb $e2 $c2 }T
$d6e5 >PC $b0 >S $a9 >A $ea >X $f7 >Y $ab >P $d6e5 $eb >M $d6e6 $42 >M $d6e7 $6f >M
T{ op PC S A X Y P -> $d6e6 $b0 $a9 $ea $f7 $ab }T T{ $d6e5 M $d6e6 M $d6e7 M -> $eb $42 $6f }T
$9510 >PC $8b >S $b3 >A $70 >X $a0 >Y $22 >P $9510 $eb >M $9511 $e0 >M $9512 $da >M
T{ op PC S A X Y P -> $9511 $8b $b3 $70 $a0 $22 }T T{ $9510 M $9511 M $9512 M -> $eb $e0 $da }T
$25ad >PC $bc >S $39 >A $d0 >X $9c >Y $66 >P $25ad $eb >M $25ae $a6 >M $25af $9a >M
T{ op PC S A X Y P -> $25ae $bc $39 $d0 $9c $66 }T T{ $25ad M $25ae M $25af M -> $eb $a6 $9a }T
$bdfc >PC $9e >S $63 >A $a0 >X $ec >Y $25 >P $bdfc $eb >M $bdfd $16 >M $bdfe $5a >M
T{ op PC S A X Y P -> $bdfd $9e $63 $a0 $ec $25 }T T{ $bdfc M $bdfd M $bdfe M -> $eb $16 $5a }T
$c4c7 >PC $b3 >S $5b >A $88 >X $d9 >Y $6a >P $c4c7 $eb >M $c4c8 $e3 >M $c4c9 $14 >M
T{ op PC S A X Y P -> $c4c8 $b3 $5b $88 $d9 $6a }T T{ $c4c7 M $c4c8 M $c4c9 M -> $eb $e3 $14 }T
$e8eb >PC $9c >S $5d >A $4f >X $fa >Y $e6 >P $e8eb $eb >M $e8ec $65 >M $e8ed $1c >M
T{ op PC S A X Y P -> $e8ec $9c $5d $4f $fa $e6 }T T{ $e8eb M $e8ec M $e8ed M -> $eb $65 $1c }T
$e4a6 >PC $3e >S $ed >A $39 >X $46 >Y $67 >P $e4a6 $eb >M $e4a7 $7c >M $e4a8 $31 >M
T{ op PC S A X Y P -> $e4a7 $3e $ed $39 $46 $67 }T T{ $e4a6 M $e4a7 M $e4a8 M -> $eb $7c $31 }T
$3f92 >PC $22 >S $24 >A $50 >X $1f >Y $6e >P $3f92 $eb >M $3f93 $07 >M $3f94 $d1 >M
T{ op PC S A X Y P -> $3f93 $22 $24 $50 $1f $6e }T T{ $3f92 M $3f93 M $3f94 M -> $eb $07 $d1 }T
$bb10 >PC $2a >S $ed >A $d4 >X $25 >Y $20 >P $bb10 $eb >M $bb11 $54 >M $bb12 $2b >M
T{ op PC S A X Y P -> $bb11 $2a $ed $d4 $25 $20 }T T{ $bb10 M $bb11 M $bb12 M -> $eb $54 $2b }T
( ec )
$af38 >PC $9c >S $61 >A $c7 >X $3a >Y $a8 >P $a260 $ec >M $af38 $ec >M $af39 $60 >M $af3a $a2 >M $af3b $1e >M
T{ op PC S A X Y P -> $af3b $9c $61 $c7 $3a $a8 }T T{ $a260 M $af38 M $af39 M $af3a M $af3b M -> $ec $ec $60 $a2 $1e }T
$b9e8 >PC $fa >S $04 >A $77 >X $95 >Y $a6 >P $b9e8 $ec >M $b9e9 $a8 >M $b9ea $eb >M $b9eb $05 >M $eba8 $be >M
T{ op PC S A X Y P -> $b9eb $fa $04 $77 $95 $a4 }T T{ $b9e8 M $b9e9 M $b9ea M $b9eb M $eba8 M -> $ec $a8 $eb $05 $be }T
$acf8 >PC $40 >S $95 >A $92 >X $c5 >Y $64 >P $4c6f $34 >M $acf8 $ec >M $acf9 $6f >M $acfa $4c >M $acfb $d7 >M
T{ op PC S A X Y P -> $acfb $40 $95 $92 $c5 $65 }T T{ $4c6f M $acf8 M $acf9 M $acfa M $acfb M -> $34 $ec $6f $4c $d7 }T
$feba >PC $97 >S $b0 >A $07 >X $8c >Y $63 >P $c420 $82 >M $feba $ec >M $febb $20 >M $febc $c4 >M $febd $1e >M
T{ op PC S A X Y P -> $febd $97 $b0 $07 $8c $e0 }T T{ $c420 M $feba M $febb M $febc M $febd M -> $82 $ec $20 $c4 $1e }T
$65b2 >PC $15 >S $e4 >A $28 >X $25 >Y $69 >P $17e8 $6f >M $65b2 $ec >M $65b3 $e8 >M $65b4 $17 >M $65b5 $08 >M
T{ op PC S A X Y P -> $65b5 $15 $e4 $28 $25 $e8 }T T{ $17e8 M $65b2 M $65b3 M $65b4 M $65b5 M -> $6f $ec $e8 $17 $08 }T
$8068 >PC $6f >S $bf >A $41 >X $ad >Y $a8 >P $7bba $ee >M $8068 $ec >M $8069 $ba >M $806a $7b >M $806b $45 >M
T{ op PC S A X Y P -> $806b $6f $bf $41 $ad $28 }T T{ $7bba M $8068 M $8069 M $806a M $806b M -> $ee $ec $ba $7b $45 }T
$c7d6 >PC $9b >S $11 >A $be >X $22 >Y $26 >P $26cd $6d >M $c7d6 $ec >M $c7d7 $cd >M $c7d8 $26 >M $c7d9 $ef >M
T{ op PC S A X Y P -> $c7d9 $9b $11 $be $22 $25 }T T{ $26cd M $c7d6 M $c7d7 M $c7d8 M $c7d9 M -> $6d $ec $cd $26 $ef }T
$c2a3 >PC $a7 >S $7d >A $25 >X $7c >Y $2c >P $51ce $b2 >M $c2a3 $ec >M $c2a4 $ce >M $c2a5 $51 >M $c2a6 $dd >M
T{ op PC S A X Y P -> $c2a6 $a7 $7d $25 $7c $2c }T T{ $51ce M $c2a3 M $c2a4 M $c2a5 M $c2a6 M -> $b2 $ec $ce $51 $dd }T
$c967 >PC $f2 >S $32 >A $90 >X $20 >Y $6a >P $c967 $ec >M $c968 $d7 >M $c969 $fc >M $c96a $82 >M $fcd7 $ec >M
T{ op PC S A X Y P -> $c96a $f2 $32 $90 $20 $e8 }T T{ $c967 M $c968 M $c969 M $c96a M $fcd7 M -> $ec $d7 $fc $82 $ec }T
$5421 >PC $5e >S $0b >A $76 >X $ed >Y $e1 >P $5421 $ec >M $5422 $a1 >M $5423 $6e >M $5424 $51 >M $6ea1 $d8 >M
T{ op PC S A X Y P -> $5424 $5e $0b $76 $ed $e0 }T T{ $5421 M $5422 M $5423 M $5424 M $6ea1 M -> $ec $a1 $6e $51 $d8 }T
$dd17 >PC $08 >S $2a >A $9e >X $ac >Y $2a >P $840c $d6 >M $dd17 $ec >M $dd18 $0c >M $dd19 $84 >M $dd1a $a5 >M
T{ op PC S A X Y P -> $dd1a $08 $2a $9e $ac $a8 }T T{ $840c M $dd17 M $dd18 M $dd19 M $dd1a M -> $d6 $ec $0c $84 $a5 }T
$8a53 >PC $90 >S $4c >A $02 >X $1d >Y $a2 >P $3c6d $f2 >M $8a53 $ec >M $8a54 $6d >M $8a55 $3c >M $8a56 $bb >M
T{ op PC S A X Y P -> $8a56 $90 $4c $02 $1d $20 }T T{ $3c6d M $8a53 M $8a54 M $8a55 M $8a56 M -> $f2 $ec $6d $3c $bb }T
$2f90 >PC $89 >S $c9 >A $b6 >X $2b >Y $e0 >P $2f90 $ec >M $2f91 $10 >M $2f92 $84 >M $2f93 $d5 >M $8410 $fc >M
T{ op PC S A X Y P -> $2f93 $89 $c9 $b6 $2b $e0 }T T{ $2f90 M $2f91 M $2f92 M $2f93 M $8410 M -> $ec $10 $84 $d5 $fc }T
$efd3 >PC $4c >S $0a >A $1d >X $2e >Y $ed >P $6bdc $6e >M $efd3 $ec >M $efd4 $dc >M $efd5 $6b >M $efd6 $11 >M
T{ op PC S A X Y P -> $efd6 $4c $0a $1d $2e $ec }T T{ $6bdc M $efd3 M $efd4 M $efd5 M $efd6 M -> $6e $ec $dc $6b $11 }T
$73a9 >PC $7a >S $d8 >A $74 >X $a0 >Y $a8 >P $73a9 $ec >M $73aa $95 >M $73ab $de >M $73ac $70 >M $de95 $62 >M
T{ op PC S A X Y P -> $73ac $7a $d8 $74 $a0 $29 }T T{ $73a9 M $73aa M $73ab M $73ac M $de95 M -> $ec $95 $de $70 $62 }T
$891c >PC $56 >S $da >A $83 >X $b8 >Y $2c >P $1011 $01 >M $891c $ec >M $891d $11 >M $891e $10 >M $891f $06 >M
T{ op PC S A X Y P -> $891f $56 $da $83 $b8 $ad }T T{ $1011 M $891c M $891d M $891e M $891f M -> $01 $ec $11 $10 $06 }T
( ed )
$525d >PC $4d >S $da >A $fa >X $11 >Y $7f >P $525d $ed >M $525e $4e >M $525f $64 >M $644e $cd >M
T{ op PC S A X Y P -> $5260 $4d $07 $fa $11 $3d }T T{ $525d M $525e M $525f M $644e M -> $ed $4e $64 $cd }T
$7948 >PC $9c >S $ce >A $f5 >X $73 >Y $3e >P $7948 $ed >M $7949 $63 >M $794a $99 >M $9963 $2d >M
T{ op PC S A X Y P -> $794b $9c $a0 $f5 $73 $bd }T T{ $7948 M $7949 M $794a M $9963 M -> $ed $63 $99 $2d }T
$ae69 >PC $bc >S $cd >A $39 >X $ac >Y $f6 >P $403d $cf >M $ae69 $ed >M $ae6a $3d >M $ae6b $40 >M
T{ op PC S A X Y P -> $ae6c $bc $fd $39 $ac $b4 }T T{ $403d M $ae69 M $ae6a M $ae6b M -> $cf $ed $3d $40 }T
$7ee8 >PC $52 >S $9d >A $36 >X $5a >Y $b4 >P $537b $93 >M $7ee8 $ed >M $7ee9 $7b >M $7eea $53 >M
T{ op PC S A X Y P -> $7eeb $52 $09 $36 $5a $35 }T T{ $537b M $7ee8 M $7ee9 M $7eea M -> $93 $ed $7b $53 }T
$40fc >PC $20 >S $fb >A $00 >X $e4 >Y $70 >P $40fc $ed >M $40fd $39 >M $40fe $44 >M $4439 $da >M
T{ op PC S A X Y P -> $40ff $20 $20 $00 $e4 $31 }T T{ $40fc M $40fd M $40fe M $4439 M -> $ed $39 $44 $da }T
$a176 >PC $42 >S $c2 >A $34 >X $26 >Y $b6 >P $a176 $ed >M $a177 $79 >M $a178 $d5 >M $d579 $16 >M
T{ op PC S A X Y P -> $a179 $42 $ab $34 $26 $b5 }T T{ $a176 M $a177 M $a178 M $d579 M -> $ed $79 $d5 $16 }T
$687b >PC $38 >S $44 >A $ae >X $b3 >Y $f5 >P $687b $ed >M $687c $78 >M $687d $b9 >M $b978 $ce >M
T{ op PC S A X Y P -> $687e $38 $76 $ae $b3 $34 }T T{ $687b M $687c M $687d M $b978 M -> $ed $78 $b9 $ce }T
$ed94 >PC $f7 >S $7e >A $71 >X $96 >Y $34 >P $ba84 $7f >M $ed94 $ed >M $ed95 $84 >M $ed96 $ba >M
T{ op PC S A X Y P -> $ed97 $f7 $fe $71 $96 $b4 }T T{ $ba84 M $ed94 M $ed95 M $ed96 M -> $7f $ed $84 $ba }T
$814d >PC $2f >S $f9 >A $fd >X $0a >Y $f3 >P $1e05 $06 >M $814d $ed >M $814e $05 >M $814f $1e >M
T{ op PC S A X Y P -> $8150 $2f $f3 $fd $0a $b1 }T T{ $1e05 M $814d M $814e M $814f M -> $06 $ed $05 $1e }T
$78e9 >PC $0d >S $a7 >A $da >X $de >Y $fc >P $4f32 $6c >M $78e9 $ed >M $78ea $32 >M $78eb $4f >M
T{ op PC S A X Y P -> $78ec $0d $34 $da $de $7d }T T{ $4f32 M $78e9 M $78ea M $78eb M -> $6c $ed $32 $4f }T
$6ff4 >PC $8f >S $bd >A $15 >X $34 >Y $f5 >P $4339 $15 >M $6ff4 $ed >M $6ff5 $39 >M $6ff6 $43 >M
T{ op PC S A X Y P -> $6ff7 $8f $a8 $15 $34 $b5 }T T{ $4339 M $6ff4 M $6ff5 M $6ff6 M -> $15 $ed $39 $43 }T
$097c >PC $2d >S $23 >A $88 >X $51 >Y $b0 >P $097c $ed >M $097d $e3 >M $097e $68 >M $68e3 $85 >M
T{ op PC S A X Y P -> $097f $2d $9d $88 $51 $f0 }T T{ $097c M $097d M $097e M $68e3 M -> $ed $e3 $68 $85 }T
$0e7b >PC $37 >S $9e >A $e9 >X $69 >Y $3c >P $0e7b $ed >M $0e7c $0f >M $0e7d $50 >M $500f $fb >M
T{ op PC S A X Y P -> $0e7e $37 $42 $e9 $69 $3c }T T{ $0e7b M $0e7c M $0e7d M $500f M -> $ed $0f $50 $fb }T
$101f >PC $14 >S $89 >A $bb >X $27 >Y $34 >P $0cf1 $50 >M $101f $ed >M $1020 $f1 >M $1021 $0c >M
T{ op PC S A X Y P -> $1022 $14 $38 $bb $27 $75 }T T{ $0cf1 M $101f M $1020 M $1021 M -> $50 $ed $f1 $0c }T
$f920 >PC $66 >S $26 >A $96 >X $16 >Y $b7 >P $427f $60 >M $f920 $ed >M $f921 $7f >M $f922 $42 >M
T{ op PC S A X Y P -> $f923 $66 $c6 $96 $16 $b4 }T T{ $427f M $f920 M $f921 M $f922 M -> $60 $ed $7f $42 }T
$a494 >PC $12 >S $08 >A $f6 >X $19 >Y $be >P $2340 $0b >M $a494 $ed >M $a495 $40 >M $a496 $23 >M
T{ op PC S A X Y P -> $a497 $12 $96 $f6 $19 $bc }T T{ $2340 M $a494 M $a495 M $a496 M -> $0b $ed $40 $23 }T
( ee )
$b602 >PC $28 >S $ac >A $42 >X $b3 >Y $24 >P $66ca $e2 >M $b602 $ee >M $b603 $ca >M $b604 $66 >M $b605 $78 >M
T{ op PC S A X Y P -> $b605 $28 $ac $42 $b3 $a4 }T T{ $66ca M $b602 M $b603 M $b604 M $b605 M -> $e3 $ee $ca $66 $78 }T
$5f49 >PC $dc >S $9a >A $3c >X $31 >Y $e7 >P $2f67 $84 >M $5f49 $ee >M $5f4a $67 >M $5f4b $2f >M $5f4c $ef >M
T{ op PC S A X Y P -> $5f4c $dc $9a $3c $31 $e5 }T T{ $2f67 M $5f49 M $5f4a M $5f4b M $5f4c M -> $85 $ee $67 $2f $ef }T
$a1e8 >PC $fc >S $a2 >A $ff >X $4a >Y $63 >P $3a18 $e6 >M $a1e8 $ee >M $a1e9 $18 >M $a1ea $3a >M $a1eb $76 >M
T{ op PC S A X Y P -> $a1eb $fc $a2 $ff $4a $e1 }T T{ $3a18 M $a1e8 M $a1e9 M $a1ea M $a1eb M -> $e7 $ee $18 $3a $76 }T
$c289 >PC $6b >S $a0 >A $3e >X $88 >Y $6c >P $c289 $ee >M $c28a $28 >M $c28b $ea >M $c28c $f4 >M $ea28 $39 >M
T{ op PC S A X Y P -> $c28c $6b $a0 $3e $88 $6c }T T{ $c289 M $c28a M $c28b M $c28c M $ea28 M -> $ee $28 $ea $f4 $3a }T
$6f5f >PC $52 >S $06 >A $c2 >X $39 >Y $e3 >P $4796 $8b >M $6f5f $ee >M $6f60 $96 >M $6f61 $47 >M $6f62 $ad >M
T{ op PC S A X Y P -> $6f62 $52 $06 $c2 $39 $e1 }T T{ $4796 M $6f5f M $6f60 M $6f61 M $6f62 M -> $8c $ee $96 $47 $ad }T
$9fc7 >PC $ee >S $7f >A $31 >X $d1 >Y $6c >P $9fc7 $ee >M $9fc8 $53 >M $9fc9 $a0 >M $9fca $5b >M $a053 $11 >M
T{ op PC S A X Y P -> $9fca $ee $7f $31 $d1 $6c }T T{ $9fc7 M $9fc8 M $9fc9 M $9fca M $a053 M -> $ee $53 $a0 $5b $12 }T
$f50d >PC $89 >S $cf >A $c5 >X $ea >Y $a0 >P $3db0 $bf >M $f50d $ee >M $f50e $b0 >M $f50f $3d >M $f510 $21 >M
T{ op PC S A X Y P -> $f510 $89 $cf $c5 $ea $a0 }T T{ $3db0 M $f50d M $f50e M $f50f M $f510 M -> $c0 $ee $b0 $3d $21 }T
$0145 >PC $c9 >S $10 >A $04 >X $c7 >Y $65 >P $0145 $ee >M $0146 $1b >M $0147 $ec >M $0148 $72 >M $ec1b $08 >M
T{ op PC S A X Y P -> $0148 $c9 $10 $04 $c7 $65 }T T{ $0145 M $0146 M $0147 M $0148 M $ec1b M -> $ee $1b $ec $72 $09 }T
$0c71 >PC $74 >S $2e >A $40 >X $95 >Y $29 >P $0c71 $ee >M $0c72 $93 >M $0c73 $52 >M $0c74 $58 >M $5293 $a7 >M
T{ op PC S A X Y P -> $0c74 $74 $2e $40 $95 $a9 }T T{ $0c71 M $0c72 M $0c73 M $0c74 M $5293 M -> $ee $93 $52 $58 $a8 }T
$d3e5 >PC $3e >S $92 >A $f0 >X $39 >Y $66 >P $231e $15 >M $d3e5 $ee >M $d3e6 $1e >M $d3e7 $23 >M $d3e8 $02 >M
T{ op PC S A X Y P -> $d3e8 $3e $92 $f0 $39 $64 }T T{ $231e M $d3e5 M $d3e6 M $d3e7 M $d3e8 M -> $16 $ee $1e $23 $02 }T
$8ea3 >PC $d5 >S $b8 >A $b2 >X $a1 >Y $a4 >P $8ea3 $ee >M $8ea4 $d0 >M $8ea5 $91 >M $8ea6 $f8 >M $91d0 $28 >M
T{ op PC S A X Y P -> $8ea6 $d5 $b8 $b2 $a1 $24 }T T{ $8ea3 M $8ea4 M $8ea5 M $8ea6 M $91d0 M -> $ee $d0 $91 $f8 $29 }T
$82fe >PC $87 >S $17 >A $a6 >X $8d >Y $66 >P $44c1 $f1 >M $82fe $ee >M $82ff $c1 >M $8300 $44 >M $8301 $9f >M
T{ op PC S A X Y P -> $8301 $87 $17 $a6 $8d $e4 }T T{ $44c1 M $82fe M $82ff M $8300 M $8301 M -> $f2 $ee $c1 $44 $9f }T
$6e6f >PC $dc >S $74 >A $07 >X $82 >Y $e9 >P $6e6f $ee >M $6e70 $95 >M $6e71 $c0 >M $6e72 $be >M $c095 $2a >M
T{ op PC S A X Y P -> $6e72 $dc $74 $07 $82 $69 }T T{ $6e6f M $6e70 M $6e71 M $6e72 M $c095 M -> $ee $95 $c0 $be $2b }T
$a451 >PC $a4 >S $88 >A $c7 >X $d5 >Y $2e >P $97bd $94 >M $a451 $ee >M $a452 $bd >M $a453 $97 >M $a454 $f6 >M
T{ op PC S A X Y P -> $a454 $a4 $88 $c7 $d5 $ac }T T{ $97bd M $a451 M $a452 M $a453 M $a454 M -> $95 $ee $bd $97 $f6 }T
$f488 >PC $3b >S $27 >A $b6 >X $26 >Y $a5 >P $eee2 $fc >M $f488 $ee >M $f489 $e2 >M $f48a $ee >M $f48b $a9 >M
T{ op PC S A X Y P -> $f48b $3b $27 $b6 $26 $a5 }T T{ $eee2 M $f488 M $f489 M $f48a M $f48b M -> $fd $ee $e2 $ee $a9 }T
$d95b >PC $98 >S $d1 >A $0b >X $02 >Y $af >P $c4b1 $74 >M $d95b $ee >M $d95c $b1 >M $d95d $c4 >M $d95e $39 >M
T{ op PC S A X Y P -> $d95e $98 $d1 $0b $02 $2d }T T{ $c4b1 M $d95b M $d95c M $d95d M $d95e M -> $75 $ee $b1 $c4 $39 }T
( ef )
$d8a0 >PC $bb >S $b6 >A $4f >X $5f >Y $a8 >P $000b $68 >M $d81c $f2 >M $d8a0 $ef >M $d8a1 $0b >M $d8a2 $79 >M $d91c $b0 >M
T{ op PC S A X Y P -> $d91c $bb $b6 $4f $5f $a8 }T T{ $000b M $d81c M $d8a0 M $d8a1 M $d8a2 M $d91c M -> $68 $f2 $ef $0b $79 $b0 }T
$7c30 >PC $42 >S $eb >A $38 >X $63 >Y $a2 >P $00f1 $c6 >M $7c23 $6a >M $7c30 $ef >M $7c31 $f1 >M $7c32 $f0 >M
T{ op PC S A X Y P -> $7c23 $42 $eb $38 $63 $a2 }T T{ $00f1 M $7c23 M $7c30 M $7c31 M $7c32 M -> $c6 $6a $ef $f1 $f0 }T
$04cd >PC $f9 >S $de >A $06 >X $07 >Y $a8 >P $00de $fb >M $04cc $02 >M $04cd $ef >M $04ce $de >M $04cf $fc >M
T{ op PC S A X Y P -> $04cc $f9 $de $06 $07 $a8 }T T{ $00de M $04cc M $04cd M $04ce M $04cf M -> $fb $02 $ef $de $fc }T
$a2f1 >PC $da >S $6f >A $16 >X $e4 >Y $a5 >P $0087 $63 >M $a2dd $24 >M $a2f1 $ef >M $a2f2 $87 >M $a2f3 $e9 >M
T{ op PC S A X Y P -> $a2dd $da $6f $16 $e4 $a5 }T T{ $0087 M $a2dd M $a2f1 M $a2f2 M $a2f3 M -> $63 $24 $ef $87 $e9 }T
$59cf >PC $35 >S $df >A $21 >X $29 >Y $a0 >P $003c $85 >M $59cf $ef >M $59d0 $3c >M $59d1 $2c >M $59d2 $ef >M $59fe $9d >M
T{ op PC S A X Y P -> $59d2 $35 $df $21 $29 $a0 }T T{ $003c M $59cf M $59d0 M $59d1 M $59d2 M $59fe M -> $85 $ef $3c $2c $ef $9d }T
$0bda >PC $ba >S $77 >A $e6 >X $3d >Y $6c >P $00ba $f9 >M $0b5b $8d >M $0bda $ef >M $0bdb $ba >M $0bdc $7e >M $0c5b $de >M
T{ op PC S A X Y P -> $0c5b $ba $77 $e6 $3d $6c }T T{ $00ba M $0b5b M $0bda M $0bdb M $0bdc M $0c5b M -> $f9 $8d $ef $ba $7e $de }T
$29c1 >PC $c8 >S $7d >A $2b >X $23 >Y $61 >P $009f $5b >M $295e $78 >M $29c1 $ef >M $29c2 $9f >M $29c3 $9a >M
T{ op PC S A X Y P -> $295e $c8 $7d $2b $23 $61 }T T{ $009f M $295e M $29c1 M $29c2 M $29c3 M -> $5b $78 $ef $9f $9a }T
$0125 >PC $71 >S $04 >A $f6 >X $5a >Y $62 >P $0022 $c1 >M $010d $12 >M $0125 $ef >M $0126 $22 >M $0127 $e5 >M
T{ op PC S A X Y P -> $010d $71 $04 $f6 $5a $62 }T T{ $0022 M $010d M $0125 M $0126 M $0127 M -> $c1 $12 $ef $22 $e5 }T
$9abe >PC $ce >S $51 >A $bf >X $c4 >Y $20 >P $004b $ee >M $9abe $ef >M $9abf $4b >M $9ac0 $0b >M $9acc $6e >M
T{ op PC S A X Y P -> $9acc $ce $51 $bf $c4 $20 }T T{ $004b M $9abe M $9abf M $9ac0 M $9acc M -> $ee $ef $4b $0b $6e }T
$7720 >PC $e8 >S $3e >A $aa >X $0a >Y $61 >P $0087 $d9 >M $7720 $ef >M $7721 $87 >M $7722 $7c >M $779f $fd >M
T{ op PC S A X Y P -> $779f $e8 $3e $aa $0a $61 }T T{ $0087 M $7720 M $7721 M $7722 M $779f M -> $d9 $ef $87 $7c $fd }T
$7d62 >PC $40 >S $42 >A $d6 >X $cc >Y $ac >P $008a $18 >M $7d62 $ef >M $7d63 $8a >M $7d64 $80 >M $7d65 $53 >M $7de5 $ff >M
T{ op PC S A X Y P -> $7d65 $40 $42 $d6 $cc $ac }T T{ $008a M $7d62 M $7d63 M $7d64 M $7d65 M $7de5 M -> $18 $ef $8a $80 $53 $ff }T
$0753 >PC $25 >S $c4 >A $90 >X $0b >Y $68 >P $00d9 $b0 >M $0726 $a2 >M $0753 $ef >M $0754 $d9 >M $0755 $d0 >M $0756 $d4 >M
T{ op PC S A X Y P -> $0756 $25 $c4 $90 $0b $68 }T T{ $00d9 M $0726 M $0753 M $0754 M $0755 M $0756 M -> $b0 $a2 $ef $d9 $d0 $d4 }T
$16c4 >PC $15 >S $17 >A $30 >X $1f >Y $2b >P $0033 $be >M $16a6 $56 >M $16c4 $ef >M $16c5 $33 >M $16c6 $df >M $16c7 $90 >M
T{ op PC S A X Y P -> $16c7 $15 $17 $30 $1f $2b }T T{ $0033 M $16a6 M $16c4 M $16c5 M $16c6 M $16c7 M -> $be $56 $ef $33 $df $90 }T
$e636 >PC $a5 >S $75 >A $8c >X $ff >Y $24 >P $003e $13 >M $e636 $ef >M $e637 $3e >M $e638 $49 >M $e639 $1a >M $e682 $ae >M
T{ op PC S A X Y P -> $e639 $a5 $75 $8c $ff $24 }T T{ $003e M $e636 M $e637 M $e638 M $e639 M $e682 M -> $13 $ef $3e $49 $1a $ae }T
$416d >PC $7a >S $59 >A $4c >X $83 >Y $22 >P $0048 $d6 >M $416d $ef >M $416e $48 >M $416f $14 >M $4184 $e8 >M
T{ op PC S A X Y P -> $4184 $7a $59 $4c $83 $22 }T T{ $0048 M $416d M $416e M $416f M $4184 M -> $d6 $ef $48 $14 $e8 }T
$2017 >PC $a7 >S $c2 >A $f2 >X $95 >Y $e6 >P $0069 $bf >M $2017 $ef >M $2018 $69 >M $2019 $8c >M $201a $94 >M $20a6 $a4 >M
T{ op PC S A X Y P -> $201a $a7 $c2 $f2 $95 $e6 }T T{ $0069 M $2017 M $2018 M $2019 M $201a M $20a6 M -> $bf $ef $69 $8c $94 $a4 }T
( f0 )
$228b >PC $0e >S $40 >A $1b >X $ac >Y $6f >P $2274 $ed >M $228b $f0 >M $228c $e7 >M $228d $67 >M
T{ op PC S A X Y P -> $2274 $0e $40 $1b $ac $6f }T T{ $2274 M $228b M $228c M $228d M -> $ed $f0 $e7 $67 }T
$5b03 >PC $6d >S $fb >A $83 >X $98 >Y $e5 >P $5b03 $f0 >M $5b04 $40 >M $5b05 $02 >M
T{ op PC S A X Y P -> $5b05 $6d $fb $83 $98 $e5 }T T{ $5b03 M $5b04 M $5b05 M -> $f0 $40 $02 }T
$5012 >PC $5f >S $01 >A $2d >X $5c >Y $2e >P $5012 $f0 >M $5013 $7b >M $5014 $62 >M $508f $76 >M
T{ op PC S A X Y P -> $508f $5f $01 $2d $5c $2e }T T{ $5012 M $5013 M $5014 M $508f M -> $f0 $7b $62 $76 }T
$c27d >PC $9f >S $12 >A $1f >X $a7 >Y $a4 >P $c27d $f0 >M $c27e $c9 >M $c27f $0f >M
T{ op PC S A X Y P -> $c27f $9f $12 $1f $a7 $a4 }T T{ $c27d M $c27e M $c27f M -> $f0 $c9 $0f }T
$dc9b >PC $1e >S $05 >A $2b >X $44 >Y $ac >P $dc9b $f0 >M $dc9c $7b >M $dc9d $3b >M
T{ op PC S A X Y P -> $dc9d $1e $05 $2b $44 $ac }T T{ $dc9b M $dc9c M $dc9d M -> $f0 $7b $3b }T
$2525 >PC $1f >S $83 >A $89 >X $c9 >Y $23 >P $251d $75 >M $2525 $f0 >M $2526 $f6 >M $2527 $c1 >M
T{ op PC S A X Y P -> $251d $1f $83 $89 $c9 $23 }T T{ $251d M $2525 M $2526 M $2527 M -> $75 $f0 $f6 $c1 }T
$6cba >PC $7e >S $a6 >A $c3 >X $ac >Y $ea >P $6ca9 $04 >M $6cba $f0 >M $6cbb $ed >M $6cbc $c1 >M
T{ op PC S A X Y P -> $6ca9 $7e $a6 $c3 $ac $ea }T T{ $6ca9 M $6cba M $6cbb M $6cbc M -> $04 $f0 $ed $c1 }T
$b78b >PC $6e >S $9e >A $19 >X $91 >Y $23 >P $b716 $e4 >M $b78b $f0 >M $b78c $89 >M $b78d $d1 >M
T{ op PC S A X Y P -> $b716 $6e $9e $19 $91 $23 }T T{ $b716 M $b78b M $b78c M $b78d M -> $e4 $f0 $89 $d1 }T
$0a73 >PC $0c >S $3c >A $66 >X $fd >Y $ef >P $0a02 $3d >M $0a73 $f0 >M $0a74 $8d >M $0a75 $70 >M
T{ op PC S A X Y P -> $0a02 $0c $3c $66 $fd $ef }T T{ $0a02 M $0a73 M $0a74 M $0a75 M -> $3d $f0 $8d $70 }T
$a534 >PC $b2 >S $e3 >A $83 >X $3c >Y $ac >P $a534 $f0 >M $a535 $87 >M $a536 $00 >M
T{ op PC S A X Y P -> $a536 $b2 $e3 $83 $3c $ac }T T{ $a534 M $a535 M $a536 M -> $f0 $87 $00 }T
$df0c >PC $5c >S $de >A $e2 >X $b4 >Y $ef >P $df0c $f0 >M $df0d $07 >M $df0e $50 >M $df15 $3d >M
T{ op PC S A X Y P -> $df15 $5c $de $e2 $b4 $ef }T T{ $df0c M $df0d M $df0e M $df15 M -> $f0 $07 $50 $3d }T
$382c >PC $6e >S $37 >A $e1 >X $40 >Y $e7 >P $382c $f0 >M $382d $62 >M $382e $bf >M $3890 $ba >M
T{ op PC S A X Y P -> $3890 $6e $37 $e1 $40 $e7 }T T{ $382c M $382d M $382e M $3890 M -> $f0 $62 $bf $ba }T
$dedc >PC $15 >S $7d >A $91 >X $33 >Y $ab >P $de05 $e0 >M $dedc $f0 >M $dedd $27 >M $dede $00 >M $df05 $28 >M
T{ op PC S A X Y P -> $df05 $15 $7d $91 $33 $ab }T T{ $de05 M $dedc M $dedd M $dede M $df05 M -> $e0 $f0 $27 $00 $28 }T
$ffa2 >PC $40 >S $cf >A $b5 >X $45 >Y $6b >P $ffa2 $f0 >M $ffa3 $23 >M $ffa4 $9a >M $ffc7 $04 >M
T{ op PC S A X Y P -> $ffc7 $40 $cf $b5 $45 $6b }T T{ $ffa2 M $ffa3 M $ffa4 M $ffc7 M -> $f0 $23 $9a $04 }T
$24b9 >PC $e8 >S $1f >A $1f >X $17 >Y $e5 >P $24b9 $f0 >M $24ba $1f >M $24bb $0c >M
T{ op PC S A X Y P -> $24bb $e8 $1f $1f $17 $e5 }T T{ $24b9 M $24ba M $24bb M -> $f0 $1f $0c }T
$bf53 >PC $fe >S $5d >A $37 >X $14 >Y $6c >P $bf53 $f0 >M $bf54 $45 >M $bf55 $ee >M
T{ op PC S A X Y P -> $bf55 $fe $5d $37 $14 $6c }T T{ $bf53 M $bf54 M $bf55 M -> $f0 $45 $ee }T
( f1 )
$c2dd >PC $cf >S $29 >A $69 >X $1b >Y $b4 >P $00af $c9 >M $00b0 $24 >M $24e4 $8f >M $c2dd $f1 >M $c2de $af >M
T{ op PC S A X Y P -> $c2df $cf $99 $69 $1b $f4 }T T{ $00af M $00b0 M $24e4 M $c2dd M $c2de M -> $c9 $24 $8f $f1 $af }T
$67d1 >PC $af >S $8e >A $ae >X $9a >Y $3f >P $0038 $4e >M $0039 $ab >M $67d1 $f1 >M $67d2 $38 >M $abe8 $05 >M
T{ op PC S A X Y P -> $67d3 $af $89 $ae $9a $bd }T T{ $0038 M $0039 M $67d1 M $67d2 M $abe8 M -> $4e $ab $f1 $38 $05 }T
$cadf >PC $96 >S $03 >A $01 >X $19 >Y $38 >P $00b0 $73 >M $00b1 $16 >M $168c $0e >M $cadf $f1 >M $cae0 $b0 >M
T{ op PC S A X Y P -> $cae1 $96 $8e $01 $19 $b8 }T T{ $00b0 M $00b1 M $168c M $cadf M $cae0 M -> $73 $16 $0e $f1 $b0 }T
$11a2 >PC $e0 >S $b1 >A $9b >X $15 >Y $30 >P $00c9 $28 >M $00ca $c3 >M $11a2 $f1 >M $11a3 $c9 >M $c33d $7f >M
T{ op PC S A X Y P -> $11a4 $e0 $31 $9b $15 $71 }T T{ $00c9 M $00ca M $11a2 M $11a3 M $c33d M -> $28 $c3 $f1 $c9 $7f }T
$fb30 >PC $b6 >S $6d >A $af >X $c1 >Y $3a >P $00a2 $4a >M $00a3 $c7 >M $c80b $72 >M $fb30 $f1 >M $fb31 $a2 >M
T{ op PC S A X Y P -> $fb32 $b6 $9a $af $c1 $b8 }T T{ $00a2 M $00a3 M $c80b M $fb30 M $fb31 M -> $4a $c7 $72 $f1 $a2 }T
$f9f3 >PC $47 >S $03 >A $b0 >X $c6 >Y $b2 >P $00a7 $3a >M $00a8 $92 >M $9300 $6e >M $f9f3 $f1 >M $f9f4 $a7 >M
T{ op PC S A X Y P -> $f9f5 $47 $94 $b0 $c6 $b0 }T T{ $00a7 M $00a8 M $9300 M $f9f3 M $f9f4 M -> $3a $92 $6e $f1 $a7 }T
$4d0f >PC $d4 >S $47 >A $62 >X $ca >Y $bb >P $003d $8e >M $003e $b9 >M $4d0f $f1 >M $4d10 $3d >M $ba58 $08 >M
T{ op PC S A X Y P -> $4d11 $d4 $39 $62 $ca $39 }T T{ $003d M $003e M $4d0f M $4d10 M $ba58 M -> $8e $b9 $f1 $3d $08 }T
$bae3 >PC $c4 >S $40 >A $31 >X $2f >Y $f0 >P $0098 $8d >M $0099 $fd >M $bae3 $f1 >M $bae4 $98 >M $fdbc $5b >M
T{ op PC S A X Y P -> $bae5 $c4 $e4 $31 $2f $b0 }T T{ $0098 M $0099 M $bae3 M $bae4 M $fdbc M -> $8d $fd $f1 $98 $5b }T
$bd52 >PC $8e >S $a0 >A $0d >X $ea >Y $3c >P $00a7 $e9 >M $00a8 $fb >M $bd52 $f1 >M $bd53 $a7 >M $fcd3 $29 >M
T{ op PC S A X Y P -> $bd54 $8e $70 $0d $ea $7d }T T{ $00a7 M $00a8 M $bd52 M $bd53 M $fcd3 M -> $e9 $fb $f1 $a7 $29 }T
$1712 >PC $47 >S $4f >A $01 >X $58 >Y $79 >P $0042 $b6 >M $0043 $f3 >M $1712 $f1 >M $1713 $42 >M $f40e $58 >M
T{ op PC S A X Y P -> $1714 $47 $97 $01 $58 $b8 }T T{ $0042 M $0043 M $1712 M $1713 M $f40e M -> $b6 $f3 $f1 $42 $58 }T
$f53e >PC $23 >S $de >A $ed >X $06 >Y $f7 >P $0092 $69 >M $0093 $d3 >M $d36f $fc >M $f53e $f1 >M $f53f $92 >M
T{ op PC S A X Y P -> $f540 $23 $e2 $ed $06 $b4 }T T{ $0092 M $0093 M $d36f M $f53e M $f53f M -> $69 $d3 $fc $f1 $92 }T
$3335 >PC $97 >S $0f >A $c5 >X $a3 >Y $f9 >P $0072 $b5 >M $0073 $71 >M $3335 $f1 >M $3336 $72 >M $7258 $9a >M
T{ op PC S A X Y P -> $3337 $97 $15 $c5 $a3 $38 }T T{ $0072 M $0073 M $3335 M $3336 M $7258 M -> $b5 $71 $f1 $72 $9a }T
$21e9 >PC $08 >S $96 >A $0d >X $73 >Y $30 >P $0028 $9a >M $0029 $08 >M $090d $b3 >M $21e9 $f1 >M $21ea $28 >M
T{ op PC S A X Y P -> $21eb $08 $e2 $0d $73 $b0 }T T{ $0028 M $0029 M $090d M $21e9 M $21ea M -> $9a $08 $b3 $f1 $28 }T
$da6e >PC $a3 >S $0d >A $82 >X $a0 >Y $3d >P $00fc $51 >M $00fd $d0 >M $d0f1 $95 >M $da6e $f1 >M $da6f $fc >M
T{ op PC S A X Y P -> $da70 $a3 $18 $82 $a0 $3c }T T{ $00fc M $00fd M $d0f1 M $da6e M $da6f M -> $51 $d0 $95 $f1 $fc }T
$30ae >PC $26 >S $51 >A $30 >X $45 >Y $31 >P $0075 $e5 >M $0076 $e8 >M $30ae $f1 >M $30af $75 >M $e92a $f9 >M
T{ op PC S A X Y P -> $30b0 $26 $58 $30 $45 $30 }T T{ $0075 M $0076 M $30ae M $30af M $e92a M -> $e5 $e8 $f1 $75 $f9 }T
$f56d >PC $49 >S $65 >A $33 >X $21 >Y $f9 >P $00b5 $b7 >M $00b6 $30 >M $30d8 $39 >M $f56d $f1 >M $f56e $b5 >M
T{ op PC S A X Y P -> $f56f $49 $26 $33 $21 $39 }T T{ $00b5 M $00b6 M $30d8 M $f56d M $f56e M -> $b7 $30 $39 $f1 $b5 }T
( f2 )
$6cf0 >PC $ff >S $4f >A $5c >X $5e >Y $bf >P $004a $9e >M $004b $7c >M $6cf0 $f2 >M $6cf1 $4a >M $7c9e $67 >M
T{ op PC S A X Y P -> $6cf2 $ff $88 $5c $5e $bc }T T{ $004a M $004b M $6cf0 M $6cf1 M $7c9e M -> $9e $7c $f2 $4a $67 }T
$1edf >PC $45 >S $70 >A $b7 >X $f4 >Y $b0 >P $00d4 $c7 >M $00d5 $e9 >M $1edf $f2 >M $1ee0 $d4 >M $e9c7 $13 >M
T{ op PC S A X Y P -> $1ee1 $45 $5c $b7 $f4 $31 }T T{ $00d4 M $00d5 M $1edf M $1ee0 M $e9c7 M -> $c7 $e9 $f2 $d4 $13 }T
$3cd2 >PC $9b >S $5f >A $3a >X $86 >Y $3a >P $0049 $56 >M $004a $42 >M $3cd2 $f2 >M $3cd3 $49 >M $4256 $3d >M
T{ op PC S A X Y P -> $3cd4 $9b $21 $3a $86 $39 }T T{ $0049 M $004a M $3cd2 M $3cd3 M $4256 M -> $56 $42 $f2 $49 $3d }T
$a41e >PC $35 >S $a2 >A $47 >X $6c >Y $78 >P $0047 $58 >M $0048 $bd >M $a41e $f2 >M $a41f $47 >M $bd58 $75 >M
T{ op PC S A X Y P -> $a420 $35 $26 $47 $6c $79 }T T{ $0047 M $0048 M $a41e M $a41f M $bd58 M -> $58 $bd $f2 $47 $75 }T
$6e21 >PC $12 >S $cb >A $5e >X $19 >Y $77 >P $00d4 $51 >M $00d5 $6b >M $6b51 $71 >M $6e21 $f2 >M $6e22 $d4 >M
T{ op PC S A X Y P -> $6e23 $12 $5a $5e $19 $75 }T T{ $00d4 M $00d5 M $6b51 M $6e21 M $6e22 M -> $51 $6b $71 $f2 $d4 }T
$a979 >PC $09 >S $3c >A $11 >X $d2 >Y $33 >P $0055 $a5 >M $0056 $65 >M $65a5 $7f >M $a979 $f2 >M $a97a $55 >M
T{ op PC S A X Y P -> $a97b $09 $bd $11 $d2 $b0 }T T{ $0055 M $0056 M $65a5 M $a979 M $a97a M -> $a5 $65 $7f $f2 $55 }T
$7b40 >PC $41 >S $04 >A $44 >X $d0 >Y $fc >P $0078 $c6 >M $0079 $63 >M $63c6 $62 >M $7b40 $f2 >M $7b41 $78 >M
T{ op PC S A X Y P -> $7b42 $41 $41 $44 $d0 $3c }T T{ $0078 M $0079 M $63c6 M $7b40 M $7b41 M -> $c6 $63 $62 $f2 $78 }T
$9405 >PC $9e >S $2b >A $7f >X $36 >Y $bb >P $006a $20 >M $006b $1a >M $1a20 $32 >M $9405 $f2 >M $9406 $6a >M
T{ op PC S A X Y P -> $9407 $9e $99 $7f $36 $b8 }T T{ $006a M $006b M $1a20 M $9405 M $9406 M -> $20 $1a $32 $f2 $6a }T
$c085 >PC $e3 >S $20 >A $8e >X $29 >Y $3b >P $0061 $71 >M $0062 $f3 >M $c085 $f2 >M $c086 $61 >M $f371 $bc >M
T{ op PC S A X Y P -> $c087 $e3 $fe $8e $29 $b8 }T T{ $0061 M $0062 M $c085 M $c086 M $f371 M -> $71 $f3 $f2 $61 $bc }T
$c26d >PC $7b >S $75 >A $76 >X $e0 >Y $b7 >P $0053 $37 >M $0054 $60 >M $6037 $69 >M $c26d $f2 >M $c26e $53 >M
T{ op PC S A X Y P -> $c26f $7b $0c $76 $e0 $35 }T T{ $0053 M $0054 M $6037 M $c26d M $c26e M -> $37 $60 $69 $f2 $53 }T
$3877 >PC $ec >S $8c >A $d3 >X $38 >Y $3a >P $0057 $95 >M $0058 $29 >M $2995 $1c >M $3877 $f2 >M $3878 $57 >M
T{ op PC S A X Y P -> $3879 $ec $69 $d3 $38 $79 }T T{ $0057 M $0058 M $2995 M $3877 M $3878 M -> $95 $29 $1c $f2 $57 }T
$6378 >PC $d6 >S $17 >A $fa >X $29 >Y $b7 >P $007e $cd >M $007f $c7 >M $6378 $f2 >M $6379 $7e >M $c7cd $fa >M
T{ op PC S A X Y P -> $637a $d6 $1d $fa $29 $34 }T T{ $007e M $007f M $6378 M $6379 M $c7cd M -> $cd $c7 $f2 $7e $fa }T
$0496 >PC $31 >S $4f >A $2d >X $95 >Y $3a >P $0056 $8e >M $0057 $2a >M $0496 $f2 >M $0497 $56 >M $2a8e $a5 >M
T{ op PC S A X Y P -> $0498 $31 $49 $2d $95 $78 }T T{ $0056 M $0057 M $0496 M $0497 M $2a8e M -> $8e $2a $f2 $56 $a5 }T
$a6a3 >PC $dd >S $c3 >A $41 >X $35 >Y $3b >P $0032 $c5 >M $0033 $74 >M $74c5 $12 >M $a6a3 $f2 >M $a6a4 $32 >M
T{ op PC S A X Y P -> $a6a5 $dd $b1 $41 $35 $b9 }T T{ $0032 M $0033 M $74c5 M $a6a3 M $a6a4 M -> $c5 $74 $12 $f2 $32 }T
$4f5f >PC $06 >S $06 >A $98 >X $c2 >Y $f6 >P $0082 $f0 >M $0083 $37 >M $37f0 $49 >M $4f5f $f2 >M $4f60 $82 >M
T{ op PC S A X Y P -> $4f61 $06 $bc $98 $c2 $b4 }T T{ $0082 M $0083 M $37f0 M $4f5f M $4f60 M -> $f0 $37 $49 $f2 $82 }T
$35b9 >PC $ef >S $bc >A $e9 >X $34 >Y $be >P $004a $e3 >M $004b $9d >M $35b9 $f2 >M $35ba $4a >M $9de3 $9e >M
T{ op PC S A X Y P -> $35bb $ef $17 $e9 $34 $3d }T T{ $004a M $004b M $35b9 M $35ba M $9de3 M -> $e3 $9d $f2 $4a $9e }T
( f3 )
$ba8b >PC $cc >S $53 >A $e6 >X $61 >Y $a3 >P $ba8b $f3 >M $ba8c $bc >M $ba8d $3e >M
T{ op PC S A X Y P -> $ba8c $cc $53 $e6 $61 $a3 }T T{ $ba8b M $ba8c M $ba8d M -> $f3 $bc $3e }T
$c392 >PC $61 >S $88 >A $4c >X $f4 >Y $65 >P $c392 $f3 >M $c393 $d1 >M $c394 $af >M
T{ op PC S A X Y P -> $c393 $61 $88 $4c $f4 $65 }T T{ $c392 M $c393 M $c394 M -> $f3 $d1 $af }T
$c94d >PC $23 >S $06 >A $78 >X $b0 >Y $a6 >P $c94d $f3 >M $c94e $f2 >M $c94f $b7 >M
T{ op PC S A X Y P -> $c94e $23 $06 $78 $b0 $a6 }T T{ $c94d M $c94e M $c94f M -> $f3 $f2 $b7 }T
$3adc >PC $c0 >S $e3 >A $cf >X $dc >Y $61 >P $3adc $f3 >M $3add $fb >M $3ade $b8 >M
T{ op PC S A X Y P -> $3add $c0 $e3 $cf $dc $61 }T T{ $3adc M $3add M $3ade M -> $f3 $fb $b8 }T
$47dd >PC $97 >S $58 >A $5d >X $21 >Y $af >P $47dd $f3 >M $47de $bd >M $47df $9a >M
T{ op PC S A X Y P -> $47de $97 $58 $5d $21 $af }T T{ $47dd M $47de M $47df M -> $f3 $bd $9a }T
$d178 >PC $ca >S $d8 >A $c8 >X $51 >Y $69 >P $d178 $f3 >M $d179 $00 >M $d17a $3a >M
T{ op PC S A X Y P -> $d179 $ca $d8 $c8 $51 $69 }T T{ $d178 M $d179 M $d17a M -> $f3 $00 $3a }T
$5107 >PC $38 >S $d0 >A $3d >X $00 >Y $6d >P $5107 $f3 >M $5108 $c2 >M $5109 $57 >M
T{ op PC S A X Y P -> $5108 $38 $d0 $3d $00 $6d }T T{ $5107 M $5108 M $5109 M -> $f3 $c2 $57 }T
$c4af >PC $56 >S $c3 >A $ea >X $6c >Y $6b >P $c4af $f3 >M $c4b0 $e1 >M $c4b1 $88 >M
T{ op PC S A X Y P -> $c4b0 $56 $c3 $ea $6c $6b }T T{ $c4af M $c4b0 M $c4b1 M -> $f3 $e1 $88 }T
$c339 >PC $43 >S $07 >A $62 >X $05 >Y $26 >P $c339 $f3 >M $c33a $79 >M $c33b $8e >M
T{ op PC S A X Y P -> $c33a $43 $07 $62 $05 $26 }T T{ $c339 M $c33a M $c33b M -> $f3 $79 $8e }T
$dd4c >PC $36 >S $e7 >A $81 >X $90 >Y $6a >P $dd4c $f3 >M $dd4d $47 >M $dd4e $57 >M
T{ op PC S A X Y P -> $dd4d $36 $e7 $81 $90 $6a }T T{ $dd4c M $dd4d M $dd4e M -> $f3 $47 $57 }T
$881b >PC $d0 >S $a8 >A $25 >X $2b >Y $60 >P $881b $f3 >M $881c $1b >M $881d $b2 >M
T{ op PC S A X Y P -> $881c $d0 $a8 $25 $2b $60 }T T{ $881b M $881c M $881d M -> $f3 $1b $b2 }T
$8d73 >PC $32 >S $a7 >A $fd >X $b4 >Y $69 >P $8d73 $f3 >M $8d74 $26 >M $8d75 $b1 >M
T{ op PC S A X Y P -> $8d74 $32 $a7 $fd $b4 $69 }T T{ $8d73 M $8d74 M $8d75 M -> $f3 $26 $b1 }T
$aa99 >PC $4b >S $37 >A $67 >X $cb >Y $69 >P $aa99 $f3 >M $aa9a $a7 >M $aa9b $fa >M
T{ op PC S A X Y P -> $aa9a $4b $37 $67 $cb $69 }T T{ $aa99 M $aa9a M $aa9b M -> $f3 $a7 $fa }T
$927d >PC $5a >S $50 >A $78 >X $e3 >Y $29 >P $927d $f3 >M $927e $82 >M $927f $5d >M
T{ op PC S A X Y P -> $927e $5a $50 $78 $e3 $29 }T T{ $927d M $927e M $927f M -> $f3 $82 $5d }T
$74b8 >PC $d9 >S $dc >A $5c >X $c3 >Y $27 >P $74b8 $f3 >M $74b9 $21 >M $74ba $cd >M
T{ op PC S A X Y P -> $74b9 $d9 $dc $5c $c3 $27 }T T{ $74b8 M $74b9 M $74ba M -> $f3 $21 $cd }T
$d120 >PC $60 >S $bb >A $3d >X $f1 >Y $aa >P $d120 $f3 >M $d121 $00 >M $d122 $66 >M
T{ op PC S A X Y P -> $d121 $60 $bb $3d $f1 $aa }T T{ $d120 M $d121 M $d122 M -> $f3 $00 $66 }T
( f4 )
$90ec >PC $69 >S $45 >A $7e >X $eb >Y $a2 >P $0032 $4a >M $00b4 $e8 >M $90ec $f4 >M $90ed $b4 >M $90ee $96 >M
T{ op PC S A X Y P -> $90ee $69 $45 $7e $eb $a2 }T T{ $0032 M $00b4 M $90ec M $90ed M $90ee M -> $4a $e8 $f4 $b4 $96 }T
$e4da >PC $c7 >S $d2 >A $32 >X $ec >Y $ea >P $003e $cc >M $0070 $ab >M $e4da $f4 >M $e4db $3e >M $e4dc $ac >M
T{ op PC S A X Y P -> $e4dc $c7 $d2 $32 $ec $ea }T T{ $003e M $0070 M $e4da M $e4db M $e4dc M -> $cc $ab $f4 $3e $ac }T
$be83 >PC $de >S $57 >A $37 >X $bc >Y $6d >P $00bd $fa >M $00f4 $b2 >M $be83 $f4 >M $be84 $bd >M $be85 $fa >M
T{ op PC S A X Y P -> $be85 $de $57 $37 $bc $6d }T T{ $00bd M $00f4 M $be83 M $be84 M $be85 M -> $fa $b2 $f4 $bd $fa }T
$bf3a >PC $32 >S $3e >A $01 >X $49 >Y $af >P $007e $86 >M $007f $b6 >M $bf3a $f4 >M $bf3b $7e >M $bf3c $77 >M
T{ op PC S A X Y P -> $bf3c $32 $3e $01 $49 $af }T T{ $007e M $007f M $bf3a M $bf3b M $bf3c M -> $86 $b6 $f4 $7e $77 }T
$e757 >PC $a2 >S $9e >A $fa >X $dc >Y $e1 >P $0033 $9e >M $0039 $28 >M $e757 $f4 >M $e758 $39 >M $e759 $7a >M
T{ op PC S A X Y P -> $e759 $a2 $9e $fa $dc $e1 }T T{ $0033 M $0039 M $e757 M $e758 M $e759 M -> $9e $28 $f4 $39 $7a }T
$4b68 >PC $e1 >S $4d >A $81 >X $d3 >Y $2f >P $001f $3e >M $009e $7a >M $4b68 $f4 >M $4b69 $9e >M $4b6a $cd >M
T{ op PC S A X Y P -> $4b6a $e1 $4d $81 $d3 $2f }T T{ $001f M $009e M $4b68 M $4b69 M $4b6a M -> $3e $7a $f4 $9e $cd }T
$d713 >PC $99 >S $2c >A $9e >X $23 >Y $28 >P $0027 $19 >M $00c5 $9e >M $d713 $f4 >M $d714 $27 >M $d715 $81 >M
T{ op PC S A X Y P -> $d715 $99 $2c $9e $23 $28 }T T{ $0027 M $00c5 M $d713 M $d714 M $d715 M -> $19 $9e $f4 $27 $81 }T
$1050 >PC $82 >S $09 >A $e0 >X $4b >Y $26 >P $0002 $2f >M $00e2 $71 >M $1050 $f4 >M $1051 $02 >M $1052 $3b >M
T{ op PC S A X Y P -> $1052 $82 $09 $e0 $4b $26 }T T{ $0002 M $00e2 M $1050 M $1051 M $1052 M -> $2f $71 $f4 $02 $3b }T
$283b >PC $ef >S $bd >A $ae >X $e0 >Y $65 >P $008c $5d >M $00de $fc >M $283b $f4 >M $283c $de >M $283d $39 >M
T{ op PC S A X Y P -> $283d $ef $bd $ae $e0 $65 }T T{ $008c M $00de M $283b M $283c M $283d M -> $5d $fc $f4 $de $39 }T
$2655 >PC $e4 >S $a1 >A $d7 >X $d7 >Y $a8 >P $00aa $9a >M $00d3 $6e >M $2655 $f4 >M $2656 $d3 >M $2657 $f8 >M
T{ op PC S A X Y P -> $2657 $e4 $a1 $d7 $d7 $a8 }T T{ $00aa M $00d3 M $2655 M $2656 M $2657 M -> $9a $6e $f4 $d3 $f8 }T
$0177 >PC $db >S $93 >A $4e >X $46 >Y $a1 >P $0042 $09 >M $0090 $8f >M $0177 $f4 >M $0178 $42 >M $0179 $9d >M
T{ op PC S A X Y P -> $0179 $db $93 $4e $46 $a1 }T T{ $0042 M $0090 M $0177 M $0178 M $0179 M -> $09 $8f $f4 $42 $9d }T
$6c9a >PC $4c >S $99 >A $83 >X $c9 >Y $a5 >P $006d $2e >M $00ea $b5 >M $6c9a $f4 >M $6c9b $ea >M $6c9c $ae >M
T{ op PC S A X Y P -> $6c9c $4c $99 $83 $c9 $a5 }T T{ $006d M $00ea M $6c9a M $6c9b M $6c9c M -> $2e $b5 $f4 $ea $ae }T
$0ce4 >PC $18 >S $ba >A $f9 >X $c0 >Y $21 >P $006b $d4 >M $0072 $e2 >M $0ce4 $f4 >M $0ce5 $72 >M $0ce6 $ca >M
T{ op PC S A X Y P -> $0ce6 $18 $ba $f9 $c0 $21 }T T{ $006b M $0072 M $0ce4 M $0ce5 M $0ce6 M -> $d4 $e2 $f4 $72 $ca }T
$8bf9 >PC $64 >S $10 >A $f9 >X $f4 >Y $2f >P $0000 $bd >M $00f9 $a3 >M $8bf9 $f4 >M $8bfa $00 >M $8bfb $e5 >M
T{ op PC S A X Y P -> $8bfb $64 $10 $f9 $f4 $2f }T T{ $0000 M $00f9 M $8bf9 M $8bfa M $8bfb M -> $bd $a3 $f4 $00 $e5 }T
$eea8 >PC $53 >S $aa >A $67 >X $23 >Y $2f >P $0047 $15 >M $00e0 $5e >M $eea8 $f4 >M $eea9 $e0 >M $eeaa $1b >M
T{ op PC S A X Y P -> $eeaa $53 $aa $67 $23 $2f }T T{ $0047 M $00e0 M $eea8 M $eea9 M $eeaa M -> $15 $5e $f4 $e0 $1b }T
$df73 >PC $02 >S $62 >A $a0 >X $bf >Y $66 >P $0066 $bd >M $00c6 $03 >M $df73 $f4 >M $df74 $c6 >M $df75 $57 >M
T{ op PC S A X Y P -> $df75 $02 $62 $a0 $bf $66 }T T{ $0066 M $00c6 M $df73 M $df74 M $df75 M -> $bd $03 $f4 $c6 $57 }T
( f5 )
$6a80 >PC $6b >S $45 >A $34 >X $3f >Y $76 >P $007f $76 >M $00b3 $c0 >M $6a80 $f5 >M $6a81 $7f >M
T{ op PC S A X Y P -> $6a82 $6b $84 $34 $3f $f4 }T T{ $007f M $00b3 M $6a80 M $6a81 M -> $76 $c0 $f5 $7f }T
$b16f >PC $fe >S $42 >A $78 >X $65 >Y $78 >P $0004 $1c >M $007c $d8 >M $b16f $f5 >M $b170 $04 >M
T{ op PC S A X Y P -> $b171 $fe $03 $78 $65 $38 }T T{ $0004 M $007c M $b16f M $b170 M -> $1c $d8 $f5 $04 }T
$872f >PC $00 >S $84 >A $b8 >X $83 >Y $f8 >P $0000 $34 >M $00b8 $42 >M $872f $f5 >M $8730 $00 >M
T{ op PC S A X Y P -> $8731 $00 $41 $b8 $83 $79 }T T{ $0000 M $00b8 M $872f M $8730 M -> $34 $42 $f5 $00 }T
$1242 >PC $a6 >S $ef >A $42 >X $2c >Y $b7 >P $0054 $2e >M $0096 $78 >M $1242 $f5 >M $1243 $54 >M
T{ op PC S A X Y P -> $1244 $a6 $77 $42 $2c $75 }T T{ $0054 M $0096 M $1242 M $1243 M -> $2e $78 $f5 $54 }T
$b27d >PC $3e >S $db >A $2d >X $b1 >Y $f3 >P $0018 $3c >M $00eb $56 >M $b27d $f5 >M $b27e $eb >M
T{ op PC S A X Y P -> $b27f $3e $9f $2d $b1 $b1 }T T{ $0018 M $00eb M $b27d M $b27e M -> $3c $56 $f5 $eb }T
$06c9 >PC $25 >S $39 >A $90 >X $ec >Y $b2 >P $003b $52 >M $00ab $32 >M $06c9 $f5 >M $06ca $ab >M
T{ op PC S A X Y P -> $06cb $25 $e6 $90 $ec $b0 }T T{ $003b M $00ab M $06c9 M $06ca M -> $52 $32 $f5 $ab }T
$0993 >PC $35 >S $b8 >A $2a >X $8e >Y $bd >P $0015 $9a >M $00eb $6c >M $0993 $f5 >M $0994 $eb >M
T{ op PC S A X Y P -> $0995 $35 $18 $2a $8e $3d }T T{ $0015 M $00eb M $0993 M $0994 M -> $9a $6c $f5 $eb }T
$5765 >PC $f7 >S $bd >A $c6 >X $62 >Y $31 >P $008a $27 >M $00c4 $d5 >M $5765 $f5 >M $5766 $c4 >M
T{ op PC S A X Y P -> $5767 $f7 $96 $c6 $62 $b1 }T T{ $008a M $00c4 M $5765 M $5766 M -> $27 $d5 $f5 $c4 }T
$bfc0 >PC $01 >S $b6 >A $59 >X $5f >Y $b0 >P $0011 $bf >M $006a $23 >M $bfc0 $f5 >M $bfc1 $11 >M
T{ op PC S A X Y P -> $bfc2 $01 $92 $59 $5f $b1 }T T{ $0011 M $006a M $bfc0 M $bfc1 M -> $bf $23 $f5 $11 }T
$d1ca >PC $8a >S $9c >A $f6 >X $d4 >Y $74 >P $002a $ff >M $0034 $e3 >M $d1ca $f5 >M $d1cb $34 >M
T{ op PC S A X Y P -> $d1cc $8a $9c $f6 $d4 $b4 }T T{ $002a M $0034 M $d1ca M $d1cb M -> $ff $e3 $f5 $34 }T
$6d7b >PC $44 >S $4d >A $83 >X $d0 >Y $f0 >P $005c $fd >M $00d9 $b8 >M $6d7b $f5 >M $6d7c $d9 >M
T{ op PC S A X Y P -> $6d7d $44 $4f $83 $d0 $30 }T T{ $005c M $00d9 M $6d7b M $6d7c M -> $fd $b8 $f5 $d9 }T
$2591 >PC $58 >S $f8 >A $f8 >X $74 >Y $34 >P $0063 $47 >M $006b $cd >M $2591 $f5 >M $2592 $6b >M
T{ op PC S A X Y P -> $2593 $58 $b0 $f8 $74 $b5 }T T{ $0063 M $006b M $2591 M $2592 M -> $47 $cd $f5 $6b }T
$4983 >PC $ab >S $f1 >A $9a >X $f5 >Y $7c >P $0047 $86 >M $00e1 $24 >M $4983 $f5 >M $4984 $47 >M
T{ op PC S A X Y P -> $4985 $ab $c6 $9a $f5 $bd }T T{ $0047 M $00e1 M $4983 M $4984 M -> $86 $24 $f5 $47 }T
$6371 >PC $ea >S $a9 >A $db >X $0f >Y $f2 >P $00d8 $64 >M $00fd $77 >M $6371 $f5 >M $6372 $fd >M
T{ op PC S A X Y P -> $6373 $ea $44 $db $0f $71 }T T{ $00d8 M $00fd M $6371 M $6372 M -> $64 $77 $f5 $fd }T
$c3be >PC $b9 >S $af >A $20 >X $4c >Y $b5 >P $00d5 $a3 >M $00f5 $67 >M $c3be $f5 >M $c3bf $d5 >M
T{ op PC S A X Y P -> $c3c0 $b9 $48 $20 $4c $75 }T T{ $00d5 M $00f5 M $c3be M $c3bf M -> $a3 $67 $f5 $d5 }T
$1502 >PC $40 >S $23 >A $ff >X $c5 >Y $bc >P $0073 $3a >M $0074 $c9 >M $1502 $f5 >M $1503 $74 >M
T{ op PC S A X Y P -> $1504 $40 $82 $ff $c5 $bc }T T{ $0073 M $0074 M $1502 M $1503 M -> $3a $c9 $f5 $74 }T
( f6 )
$2f55 >PC $4e >S $43 >A $c7 >X $7d >Y $e0 >P $008c $10 >M $00c5 $ba >M $2f55 $f6 >M $2f56 $c5 >M $2f57 $d9 >M
T{ op PC S A X Y P -> $2f57 $4e $43 $c7 $7d $60 }T T{ $008c M $00c5 M $2f55 M $2f56 M $2f57 M -> $11 $ba $f6 $c5 $d9 }T
$8798 >PC $23 >S $7f >A $7f >X $55 >Y $6c >P $007c $9d >M $00fb $18 >M $8798 $f6 >M $8799 $7c >M $879a $e0 >M
T{ op PC S A X Y P -> $879a $23 $7f $7f $55 $6c }T T{ $007c M $00fb M $8798 M $8799 M $879a M -> $9d $19 $f6 $7c $e0 }T
$0260 >PC $2a >S $ec >A $d2 >X $c7 >Y $ea >P $007f $c0 >M $00ad $9b >M $0260 $f6 >M $0261 $ad >M $0262 $48 >M
T{ op PC S A X Y P -> $0262 $2a $ec $d2 $c7 $e8 }T T{ $007f M $00ad M $0260 M $0261 M $0262 M -> $c1 $9b $f6 $ad $48 }T
$7c08 >PC $d0 >S $10 >A $71 >X $99 >Y $22 >P $005e $4e >M $00ed $6f >M $7c08 $f6 >M $7c09 $ed >M $7c0a $9b >M
T{ op PC S A X Y P -> $7c0a $d0 $10 $71 $99 $20 }T T{ $005e M $00ed M $7c08 M $7c09 M $7c0a M -> $4f $6f $f6 $ed $9b }T
$0902 >PC $5d >S $97 >A $78 >X $69 >Y $e2 >P $0009 $4a >M $0091 $25 >M $0902 $f6 >M $0903 $91 >M $0904 $07 >M
T{ op PC S A X Y P -> $0904 $5d $97 $78 $69 $60 }T T{ $0009 M $0091 M $0902 M $0903 M $0904 M -> $4b $25 $f6 $91 $07 }T
$70ec >PC $8f >S $0a >A $d3 >X $61 >Y $67 >P $005a $67 >M $0087 $52 >M $70ec $f6 >M $70ed $87 >M $70ee $3f >M
T{ op PC S A X Y P -> $70ee $8f $0a $d3 $61 $65 }T T{ $005a M $0087 M $70ec M $70ed M $70ee M -> $68 $52 $f6 $87 $3f }T
$3df0 >PC $53 >S $3e >A $1e >X $1a >Y $eb >P $00ce $90 >M $00ec $53 >M $3df0 $f6 >M $3df1 $ce >M $3df2 $8a >M
T{ op PC S A X Y P -> $3df2 $53 $3e $1e $1a $69 }T T{ $00ce M $00ec M $3df0 M $3df1 M $3df2 M -> $90 $54 $f6 $ce $8a }T
$816e >PC $4c >S $1f >A $7d >X $47 >Y $a5 >P $0063 $99 >M $00e0 $8d >M $816e $f6 >M $816f $63 >M $8170 $d5 >M
T{ op PC S A X Y P -> $8170 $4c $1f $7d $47 $a5 }T T{ $0063 M $00e0 M $816e M $816f M $8170 M -> $99 $8e $f6 $63 $d5 }T
$08b9 >PC $f0 >S $40 >A $2c >X $e6 >Y $ea >P $003b $29 >M $0067 $0b >M $08b9 $f6 >M $08ba $3b >M $08bb $32 >M
T{ op PC S A X Y P -> $08bb $f0 $40 $2c $e6 $68 }T T{ $003b M $0067 M $08b9 M $08ba M $08bb M -> $29 $0c $f6 $3b $32 }T
$c950 >PC $04 >S $6f >A $21 >X $5d >Y $2f >P $0009 $32 >M $00e8 $a6 >M $c950 $f6 >M $c951 $e8 >M $c952 $ee >M
T{ op PC S A X Y P -> $c952 $04 $6f $21 $5d $2d }T T{ $0009 M $00e8 M $c950 M $c951 M $c952 M -> $33 $a6 $f6 $e8 $ee }T
$8111 >PC $b3 >S $66 >A $53 >X $e1 >Y $64 >P $000c $45 >M $005f $a5 >M $8111 $f6 >M $8112 $0c >M $8113 $25 >M
T{ op PC S A X Y P -> $8113 $b3 $66 $53 $e1 $e4 }T T{ $000c M $005f M $8111 M $8112 M $8113 M -> $45 $a6 $f6 $0c $25 }T
$dd4b >PC $2c >S $17 >A $30 >X $2b >Y $e5 >P $0092 $8c >M $00c2 $5a >M $dd4b $f6 >M $dd4c $92 >M $dd4d $02 >M
T{ op PC S A X Y P -> $dd4d $2c $17 $30 $2b $65 }T T{ $0092 M $00c2 M $dd4b M $dd4c M $dd4d M -> $8c $5b $f6 $92 $02 }T
$80fa >PC $a9 >S $d8 >A $63 >X $45 >Y $a6 >P $0081 $99 >M $00e4 $9c >M $80fa $f6 >M $80fb $81 >M $80fc $9e >M
T{ op PC S A X Y P -> $80fc $a9 $d8 $63 $45 $a4 }T T{ $0081 M $00e4 M $80fa M $80fb M $80fc M -> $99 $9d $f6 $81 $9e }T
$963a >PC $ae >S $6b >A $f6 >X $d1 >Y $e8 >P $0076 $ff >M $0080 $cb >M $963a $f6 >M $963b $80 >M $963c $f1 >M
T{ op PC S A X Y P -> $963c $ae $6b $f6 $d1 $6a }T T{ $0076 M $0080 M $963a M $963b M $963c M -> $00 $cb $f6 $80 $f1 }T
$7cd8 >PC $cb >S $4a >A $f4 >X $15 >Y $6a >P $0018 $1c >M $0024 $49 >M $7cd8 $f6 >M $7cd9 $24 >M $7cda $7a >M
T{ op PC S A X Y P -> $7cda $cb $4a $f4 $15 $68 }T T{ $0018 M $0024 M $7cd8 M $7cd9 M $7cda M -> $1d $49 $f6 $24 $7a }T
$cb1f >PC $0a >S $43 >A $f1 >X $3a >Y $ec >P $0088 $69 >M $0097 $ae >M $cb1f $f6 >M $cb20 $97 >M $cb21 $11 >M
T{ op PC S A X Y P -> $cb21 $0a $43 $f1 $3a $6c }T T{ $0088 M $0097 M $cb1f M $cb20 M $cb21 M -> $6a $ae $f6 $97 $11 }T
( f7 )
$f3fd >PC $a6 >S $04 >A $88 >X $ca >Y $6d >P $0037 $e1 >M $f3fd $f7 >M $f3fe $37 >M $f3ff $71 >M
T{ op PC S A X Y P -> $f3ff $a6 $04 $88 $ca $6d }T T{ $0037 M $f3fd M $f3fe M $f3ff M -> $e1 $f7 $37 $71 }T
$bc97 >PC $ed >S $dd >A $e6 >X $0c >Y $26 >P $00a4 $34 >M $bc97 $f7 >M $bc98 $a4 >M $bc99 $45 >M
T{ op PC S A X Y P -> $bc99 $ed $dd $e6 $0c $26 }T T{ $00a4 M $bc97 M $bc98 M $bc99 M -> $b4 $f7 $a4 $45 }T
$41a1 >PC $42 >S $9d >A $68 >X $eb >Y $e4 >P $001a $ae >M $41a1 $f7 >M $41a2 $1a >M $41a3 $73 >M
T{ op PC S A X Y P -> $41a3 $42 $9d $68 $eb $e4 }T T{ $001a M $41a1 M $41a2 M $41a3 M -> $ae $f7 $1a $73 }T
$cdfb >PC $cf >S $48 >A $1f >X $c1 >Y $ee >P $0046 $b3 >M $cdfb $f7 >M $cdfc $46 >M $cdfd $cd >M
T{ op PC S A X Y P -> $cdfd $cf $48 $1f $c1 $ee }T T{ $0046 M $cdfb M $cdfc M $cdfd M -> $b3 $f7 $46 $cd }T
$07ce >PC $b3 >S $92 >A $fa >X $04 >Y $a2 >P $0029 $81 >M $07ce $f7 >M $07cf $29 >M $07d0 $df >M
T{ op PC S A X Y P -> $07d0 $b3 $92 $fa $04 $a2 }T T{ $0029 M $07ce M $07cf M $07d0 M -> $81 $f7 $29 $df }T
$901f >PC $69 >S $3d >A $ea >X $4b >Y $68 >P $00bc $ef >M $901f $f7 >M $9020 $bc >M $9021 $e8 >M
T{ op PC S A X Y P -> $9021 $69 $3d $ea $4b $68 }T T{ $00bc M $901f M $9020 M $9021 M -> $ef $f7 $bc $e8 }T
$f830 >PC $68 >S $a5 >A $e9 >X $23 >Y $e8 >P $00e2 $5d >M $f830 $f7 >M $f831 $e2 >M $f832 $3a >M
T{ op PC S A X Y P -> $f832 $68 $a5 $e9 $23 $e8 }T T{ $00e2 M $f830 M $f831 M $f832 M -> $dd $f7 $e2 $3a }T
$fd67 >PC $4f >S $74 >A $01 >X $e7 >Y $61 >P $00a0 $61 >M $fd67 $f7 >M $fd68 $a0 >M $fd69 $c7 >M
T{ op PC S A X Y P -> $fd69 $4f $74 $01 $e7 $61 }T T{ $00a0 M $fd67 M $fd68 M $fd69 M -> $e1 $f7 $a0 $c7 }T
$bdcb >PC $90 >S $d3 >A $77 >X $f5 >Y $e8 >P $00be $ef >M $bdcb $f7 >M $bdcc $be >M $bdcd $ad >M
T{ op PC S A X Y P -> $bdcd $90 $d3 $77 $f5 $e8 }T T{ $00be M $bdcb M $bdcc M $bdcd M -> $ef $f7 $be $ad }T
$c9c8 >PC $88 >S $18 >A $c1 >X $4e >Y $a3 >P $00be $38 >M $c9c8 $f7 >M $c9c9 $be >M $c9ca $33 >M
T{ op PC S A X Y P -> $c9ca $88 $18 $c1 $4e $a3 }T T{ $00be M $c9c8 M $c9c9 M $c9ca M -> $b8 $f7 $be $33 }T
$5311 >PC $17 >S $eb >A $68 >X $cf >Y $2e >P $00b1 $3d >M $5311 $f7 >M $5312 $b1 >M $5313 $8d >M
T{ op PC S A X Y P -> $5313 $17 $eb $68 $cf $2e }T T{ $00b1 M $5311 M $5312 M $5313 M -> $bd $f7 $b1 $8d }T
$cba2 >PC $9d >S $16 >A $c7 >X $9f >Y $63 >P $00f8 $d2 >M $cba2 $f7 >M $cba3 $f8 >M $cba4 $89 >M
T{ op PC S A X Y P -> $cba4 $9d $16 $c7 $9f $63 }T T{ $00f8 M $cba2 M $cba3 M $cba4 M -> $d2 $f7 $f8 $89 }T
$97b3 >PC $1f >S $54 >A $46 >X $b3 >Y $60 >P $003f $23 >M $97b3 $f7 >M $97b4 $3f >M $97b5 $ea >M
T{ op PC S A X Y P -> $97b5 $1f $54 $46 $b3 $60 }T T{ $003f M $97b3 M $97b4 M $97b5 M -> $a3 $f7 $3f $ea }T
$bd37 >PC $b6 >S $9a >A $b3 >X $f9 >Y $27 >P $00c6 $43 >M $bd37 $f7 >M $bd38 $c6 >M $bd39 $85 >M
T{ op PC S A X Y P -> $bd39 $b6 $9a $b3 $f9 $27 }T T{ $00c6 M $bd37 M $bd38 M $bd39 M -> $c3 $f7 $c6 $85 }T
$e63a >PC $ff >S $95 >A $97 >X $f0 >Y $66 >P $0068 $f0 >M $e63a $f7 >M $e63b $68 >M $e63c $b5 >M
T{ op PC S A X Y P -> $e63c $ff $95 $97 $f0 $66 }T T{ $0068 M $e63a M $e63b M $e63c M -> $f0 $f7 $68 $b5 }T
$df60 >PC $ae >S $ac >A $56 >X $6a >Y $ad >P $0025 $66 >M $df60 $f7 >M $df61 $25 >M $df62 $3f >M
T{ op PC S A X Y P -> $df62 $ae $ac $56 $6a $ad }T T{ $0025 M $df60 M $df61 M $df62 M -> $e6 $f7 $25 $3f }T
( f8 )
$be59 >PC $99 >S $97 >A $0a >X $4b >Y $e2 >P $be59 $f8 >M $be5a $f8 >M $be5b $0f >M
T{ op PC S A X Y P -> $be5a $99 $97 $0a $4b $ea }T T{ $be59 M $be5a M $be5b M -> $f8 $f8 $0f }T
$3d33 >PC $69 >S $78 >A $ad >X $26 >Y $63 >P $3d33 $f8 >M $3d34 $96 >M $3d35 $75 >M
T{ op PC S A X Y P -> $3d34 $69 $78 $ad $26 $6b }T T{ $3d33 M $3d34 M $3d35 M -> $f8 $96 $75 }T
$3bf0 >PC $d1 >S $03 >A $11 >X $39 >Y $24 >P $3bf0 $f8 >M $3bf1 $ae >M $3bf2 $e9 >M
T{ op PC S A X Y P -> $3bf1 $d1 $03 $11 $39 $2c }T T{ $3bf0 M $3bf1 M $3bf2 M -> $f8 $ae $e9 }T
$df1b >PC $59 >S $97 >A $33 >X $8a >Y $6f >P $df1b $f8 >M $df1c $03 >M $df1d $ac >M
T{ op PC S A X Y P -> $df1c $59 $97 $33 $8a $6f }T T{ $df1b M $df1c M $df1d M -> $f8 $03 $ac }T
$c976 >PC $39 >S $c9 >A $48 >X $e1 >Y $a4 >P $c976 $f8 >M $c977 $8e >M $c978 $80 >M
T{ op PC S A X Y P -> $c977 $39 $c9 $48 $e1 $ac }T T{ $c976 M $c977 M $c978 M -> $f8 $8e $80 }T
$4d4e >PC $e6 >S $90 >A $2c >X $4c >Y $e7 >P $4d4e $f8 >M $4d4f $c8 >M $4d50 $7f >M
T{ op PC S A X Y P -> $4d4f $e6 $90 $2c $4c $ef }T T{ $4d4e M $4d4f M $4d50 M -> $f8 $c8 $7f }T
$d7ea >PC $95 >S $4b >A $fd >X $ac >Y $28 >P $d7ea $f8 >M $d7eb $c9 >M $d7ec $ef >M
T{ op PC S A X Y P -> $d7eb $95 $4b $fd $ac $28 }T T{ $d7ea M $d7eb M $d7ec M -> $f8 $c9 $ef }T
$e0cf >PC $2f >S $0f >A $aa >X $4e >Y $2d >P $e0cf $f8 >M $e0d0 $8b >M $e0d1 $42 >M
T{ op PC S A X Y P -> $e0d0 $2f $0f $aa $4e $2d }T T{ $e0cf M $e0d0 M $e0d1 M -> $f8 $8b $42 }T
$2975 >PC $e4 >S $b0 >A $2c >X $c1 >Y $e2 >P $2975 $f8 >M $2976 $6f >M $2977 $59 >M
T{ op PC S A X Y P -> $2976 $e4 $b0 $2c $c1 $ea }T T{ $2975 M $2976 M $2977 M -> $f8 $6f $59 }T
$c815 >PC $09 >S $2d >A $4a >X $45 >Y $ef >P $c815 $f8 >M $c816 $26 >M $c817 $31 >M
T{ op PC S A X Y P -> $c816 $09 $2d $4a $45 $ef }T T{ $c815 M $c816 M $c817 M -> $f8 $26 $31 }T
$391b >PC $e5 >S $26 >A $e4 >X $34 >Y $2f >P $391b $f8 >M $391c $5c >M $391d $3e >M
T{ op PC S A X Y P -> $391c $e5 $26 $e4 $34 $2f }T T{ $391b M $391c M $391d M -> $f8 $5c $3e }T
$b59b >PC $cf >S $ca >A $bd >X $0c >Y $26 >P $b59b $f8 >M $b59c $77 >M $b59d $42 >M
T{ op PC S A X Y P -> $b59c $cf $ca $bd $0c $2e }T T{ $b59b M $b59c M $b59d M -> $f8 $77 $42 }T
$a6a9 >PC $40 >S $5c >A $70 >X $81 >Y $a8 >P $a6a9 $f8 >M $a6aa $88 >M $a6ab $3e >M
T{ op PC S A X Y P -> $a6aa $40 $5c $70 $81 $a8 }T T{ $a6a9 M $a6aa M $a6ab M -> $f8 $88 $3e }T
$835d >PC $5a >S $4f >A $82 >X $58 >Y $aa >P $835d $f8 >M $835e $cc >M $835f $b2 >M
T{ op PC S A X Y P -> $835e $5a $4f $82 $58 $aa }T T{ $835d M $835e M $835f M -> $f8 $cc $b2 }T
$ff90 >PC $e8 >S $10 >A $24 >X $f6 >Y $2c >P $ff90 $f8 >M $ff91 $0e >M $ff92 $92 >M
T{ op PC S A X Y P -> $ff91 $e8 $10 $24 $f6 $2c }T T{ $ff90 M $ff91 M $ff92 M -> $f8 $0e $92 }T
$48c4 >PC $8c >S $aa >A $97 >X $60 >Y $e1 >P $48c4 $f8 >M $48c5 $03 >M $48c6 $93 >M
T{ op PC S A X Y P -> $48c5 $8c $aa $97 $60 $e9 }T T{ $48c4 M $48c5 M $48c6 M -> $f8 $03 $93 }T
( f9 )
$77d1 >PC $f0 >S $f9 >A $81 >X $23 >Y $75 >P $3a3d $65 >M $77d1 $f9 >M $77d2 $1a >M $77d3 $3a >M
T{ op PC S A X Y P -> $77d4 $f0 $94 $81 $23 $b5 }T T{ $3a3d M $77d1 M $77d2 M $77d3 M -> $65 $f9 $1a $3a }T
$6198 >PC $c5 >S $a1 >A $8c >X $64 >Y $3e >P $3677 $88 >M $6198 $f9 >M $6199 $13 >M $619a $36 >M
T{ op PC S A X Y P -> $619b $c5 $12 $8c $64 $3d }T T{ $3677 M $6198 M $6199 M $619a M -> $88 $f9 $13 $36 }T
$e3ca >PC $1b >S $28 >A $9f >X $2a >Y $b0 >P $7e96 $a7 >M $e3ca $f9 >M $e3cb $6c >M $e3cc $7e >M
T{ op PC S A X Y P -> $e3cd $1b $80 $9f $2a $f0 }T T{ $7e96 M $e3ca M $e3cb M $e3cc M -> $a7 $f9 $6c $7e }T
$4be9 >PC $65 >S $36 >A $8c >X $19 >Y $3a >P $3434 $b7 >M $4be9 $f9 >M $4bea $1b >M $4beb $34 >M
T{ op PC S A X Y P -> $4bec $65 $18 $8c $19 $38 }T T{ $3434 M $4be9 M $4bea M $4beb M -> $b7 $f9 $1b $34 }T
$a47a >PC $cc >S $8c >A $3d >X $65 >Y $f6 >P $a47a $f9 >M $a47b $52 >M $a47c $d3 >M $d3b7 $45 >M
T{ op PC S A X Y P -> $a47d $cc $46 $3d $65 $75 }T T{ $a47a M $a47b M $a47c M $d3b7 M -> $f9 $52 $d3 $45 }T
$0854 >PC $d6 >S $0b >A $68 >X $45 >Y $f2 >P $0854 $f9 >M $0855 $56 >M $0856 $c9 >M $c99b $ec >M
T{ op PC S A X Y P -> $0857 $d6 $1e $68 $45 $30 }T T{ $0854 M $0855 M $0856 M $c99b M -> $f9 $56 $c9 $ec }T
$a318 >PC $79 >S $4f >A $f2 >X $d8 >Y $f7 >P $70f3 $6b >M $a318 $f9 >M $a319 $1b >M $a31a $70 >M
T{ op PC S A X Y P -> $a31b $79 $e4 $f2 $d8 $b4 }T T{ $70f3 M $a318 M $a319 M $a31a M -> $6b $f9 $1b $70 }T
$3bdc >PC $02 >S $76 >A $b6 >X $d1 >Y $79 >P $3bdc $f9 >M $3bdd $56 >M $3bde $c2 >M $c327 $94 >M
T{ op PC S A X Y P -> $3bdf $02 $82 $b6 $d1 $f8 }T T{ $3bdc M $3bdd M $3bde M $c327 M -> $f9 $56 $c2 $94 }T
$621b >PC $33 >S $e5 >A $23 >X $6d >Y $be >P $621b $f9 >M $621c $13 >M $621d $e6 >M $e680 $3f >M
T{ op PC S A X Y P -> $621e $33 $9f $23 $6d $bd }T T{ $621b M $621c M $621d M $e680 M -> $f9 $13 $e6 $3f }T
$8e55 >PC $09 >S $a2 >A $54 >X $b0 >Y $fa >P $8d14 $c3 >M $8e55 $f9 >M $8e56 $64 >M $8e57 $8c >M
T{ op PC S A X Y P -> $8e58 $09 $78 $54 $b0 $38 }T T{ $8d14 M $8e55 M $8e56 M $8e57 M -> $c3 $f9 $64 $8c }T
$9987 >PC $e5 >S $a5 >A $65 >X $47 >Y $b2 >P $1f6b $ce >M $9987 $f9 >M $9988 $24 >M $9989 $1f >M
T{ op PC S A X Y P -> $998a $e5 $d6 $65 $47 $b0 }T T{ $1f6b M $9987 M $9988 M $9989 M -> $ce $f9 $24 $1f }T
$3d5f >PC $d6 >S $00 >A $eb >X $01 >Y $32 >P $3d5f $f9 >M $3d60 $dc >M $3d61 $6c >M $6cdd $64 >M
T{ op PC S A X Y P -> $3d62 $d6 $9b $eb $01 $b0 }T T{ $3d5f M $3d60 M $3d61 M $6cdd M -> $f9 $dc $6c $64 }T
$c5c6 >PC $79 >S $ac >A $37 >X $7a >Y $f4 >P $a3c7 $ab >M $c5c6 $f9 >M $c5c7 $4d >M $c5c8 $a3 >M
T{ op PC S A X Y P -> $c5c9 $79 $00 $37 $7a $37 }T T{ $a3c7 M $c5c6 M $c5c7 M $c5c8 M -> $ab $f9 $4d $a3 }T
$903b >PC $00 >S $a0 >A $fa >X $3f >Y $b3 >P $5f1e $18 >M $903b $f9 >M $903c $df >M $903d $5e >M
T{ op PC S A X Y P -> $903e $00 $88 $fa $3f $b1 }T T{ $5f1e M $903b M $903c M $903d M -> $18 $f9 $df $5e }T
$f4ec >PC $9e >S $29 >A $c1 >X $d9 >Y $72 >P $c79b $d3 >M $f4ec $f9 >M $f4ed $c2 >M $f4ee $c6 >M
T{ op PC S A X Y P -> $f4ef $9e $55 $c1 $d9 $30 }T T{ $c79b M $f4ec M $f4ed M $f4ee M -> $d3 $f9 $c2 $c6 }T
$5e06 >PC $64 >S $db >A $dc >X $79 >Y $ff >P $379e $9b >M $5e06 $f9 >M $5e07 $25 >M $5e08 $37 >M
T{ op PC S A X Y P -> $5e09 $64 $40 $dc $79 $3d }T T{ $379e M $5e06 M $5e07 M $5e08 M -> $9b $f9 $25 $37 }T
( fa )
$9b65 >PC $a2 >S $b0 >A $8c >X $95 >Y $ee >P $01a2 $f9 >M $01a3 $1b >M $9b65 $fa >M $9b66 $d0 >M $9b67 $f4 >M
T{ op PC S A X Y P -> $9b66 $a3 $b0 $1b $95 $6c }T T{ $01a2 M $01a3 M $9b65 M $9b66 M $9b67 M -> $f9 $1b $fa $d0 $f4 }T
$4a26 >PC $85 >S $85 >A $7d >X $39 >Y $a6 >P $0185 $40 >M $0186 $dc >M $4a26 $fa >M $4a27 $7f >M $4a28 $2f >M
T{ op PC S A X Y P -> $4a27 $86 $85 $dc $39 $a4 }T T{ $0185 M $0186 M $4a26 M $4a27 M $4a28 M -> $40 $dc $fa $7f $2f }T
$338b >PC $52 >S $bf >A $e8 >X $d0 >Y $61 >P $0152 $fb >M $0153 $dc >M $338b $fa >M $338c $6b >M $338d $99 >M
T{ op PC S A X Y P -> $338c $53 $bf $dc $d0 $e1 }T T{ $0152 M $0153 M $338b M $338c M $338d M -> $fb $dc $fa $6b $99 }T
$e00e >PC $9b >S $74 >A $4e >X $e1 >Y $a1 >P $019b $68 >M $019c $2d >M $e00e $fa >M $e00f $db >M $e010 $dc >M
T{ op PC S A X Y P -> $e00f $9c $74 $2d $e1 $21 }T T{ $019b M $019c M $e00e M $e00f M $e010 M -> $68 $2d $fa $db $dc }T
$9df4 >PC $82 >S $36 >A $b5 >X $b6 >Y $2b >P $0182 $d7 >M $0183 $e5 >M $9df4 $fa >M $9df5 $80 >M $9df6 $6c >M
T{ op PC S A X Y P -> $9df5 $83 $36 $e5 $b6 $a9 }T T{ $0182 M $0183 M $9df4 M $9df5 M $9df6 M -> $d7 $e5 $fa $80 $6c }T
$44da >PC $bb >S $c7 >A $bd >X $ea >Y $6c >P $01bb $25 >M $01bc $ae >M $44da $fa >M $44db $aa >M $44dc $23 >M
T{ op PC S A X Y P -> $44db $bc $c7 $ae $ea $ec }T T{ $01bb M $01bc M $44da M $44db M $44dc M -> $25 $ae $fa $aa $23 }T
$a26f >PC $b3 >S $13 >A $c6 >X $31 >Y $20 >P $01b3 $08 >M $01b4 $a8 >M $a26f $fa >M $a270 $ad >M $a271 $2e >M
T{ op PC S A X Y P -> $a270 $b4 $13 $a8 $31 $a0 }T T{ $01b3 M $01b4 M $a26f M $a270 M $a271 M -> $08 $a8 $fa $ad $2e }T
$51f6 >PC $fa >S $01 >A $c7 >X $9e >Y $a9 >P $01fa $1c >M $01fb $2a >M $51f6 $fa >M $51f7 $4d >M $51f8 $ec >M
T{ op PC S A X Y P -> $51f7 $fb $01 $2a $9e $29 }T T{ $01fa M $01fb M $51f6 M $51f7 M $51f8 M -> $1c $2a $fa $4d $ec }T
$62ce >PC $47 >S $cb >A $ea >X $aa >Y $e5 >P $0147 $88 >M $0148 $51 >M $62ce $fa >M $62cf $7a >M $62d0 $3d >M
T{ op PC S A X Y P -> $62cf $48 $cb $51 $aa $65 }T T{ $0147 M $0148 M $62ce M $62cf M $62d0 M -> $88 $51 $fa $7a $3d }T
$fa21 >PC $9e >S $4a >A $74 >X $90 >Y $23 >P $019e $0c >M $019f $e8 >M $fa21 $fa >M $fa22 $b9 >M $fa23 $29 >M
T{ op PC S A X Y P -> $fa22 $9f $4a $e8 $90 $a1 }T T{ $019e M $019f M $fa21 M $fa22 M $fa23 M -> $0c $e8 $fa $b9 $29 }T
$e80d >PC $58 >S $95 >A $76 >X $d7 >Y $62 >P $0158 $ab >M $0159 $de >M $e80d $fa >M $e80e $26 >M $e80f $5a >M
T{ op PC S A X Y P -> $e80e $59 $95 $de $d7 $e0 }T T{ $0158 M $0159 M $e80d M $e80e M $e80f M -> $ab $de $fa $26 $5a }T
$25b7 >PC $8b >S $e0 >A $e7 >X $a0 >Y $66 >P $018b $9a >M $018c $e0 >M $25b7 $fa >M $25b8 $a9 >M $25b9 $2f >M
T{ op PC S A X Y P -> $25b8 $8c $e0 $e0 $a0 $e4 }T T{ $018b M $018c M $25b7 M $25b8 M $25b9 M -> $9a $e0 $fa $a9 $2f }T
$b900 >PC $ea >S $f4 >A $de >X $a2 >Y $2a >P $01ea $4c >M $01eb $ca >M $b900 $fa >M $b901 $0b >M $b902 $02 >M
T{ op PC S A X Y P -> $b901 $eb $f4 $ca $a2 $a8 }T T{ $01ea M $01eb M $b900 M $b901 M $b902 M -> $4c $ca $fa $0b $02 }T
$0d04 >PC $dd >S $e6 >A $bc >X $97 >Y $27 >P $01dd $ea >M $01de $61 >M $0d04 $fa >M $0d05 $ed >M $0d06 $fa >M
T{ op PC S A X Y P -> $0d05 $de $e6 $61 $97 $25 }T T{ $01dd M $01de M $0d04 M $0d05 M $0d06 M -> $ea $61 $fa $ed $fa }T
$7410 >PC $fd >S $01 >A $97 >X $e6 >Y $65 >P $01fd $38 >M $01fe $9c >M $7410 $fa >M $7411 $4f >M $7412 $b4 >M
T{ op PC S A X Y P -> $7411 $fe $01 $9c $e6 $e5 }T T{ $01fd M $01fe M $7410 M $7411 M $7412 M -> $38 $9c $fa $4f $b4 }T
$4668 >PC $fe >S $98 >A $b8 >X $6c >Y $ad >P $01fe $71 >M $01ff $9e >M $4668 $fa >M $4669 $89 >M $466a $23 >M
T{ op PC S A X Y P -> $4669 $ff $98 $9e $6c $ad }T T{ $01fe M $01ff M $4668 M $4669 M $466a M -> $71 $9e $fa $89 $23 }T
( fb )
$b6cf >PC $85 >S $0b >A $a3 >X $ce >Y $a7 >P $b6cf $fb >M $b6d0 $40 >M $b6d1 $05 >M
T{ op PC S A X Y P -> $b6d0 $85 $0b $a3 $ce $a7 }T T{ $b6cf M $b6d0 M $b6d1 M -> $fb $40 $05 }T
$8d93 >PC $aa >S $ad >A $b0 >X $fd >Y $ef >P $8d93 $fb >M $8d94 $e7 >M $8d95 $4e >M
T{ op PC S A X Y P -> $8d94 $aa $ad $b0 $fd $ef }T T{ $8d93 M $8d94 M $8d95 M -> $fb $e7 $4e }T
$d290 >PC $ff >S $11 >A $50 >X $d7 >Y $66 >P $d290 $fb >M $d291 $b9 >M $d292 $01 >M
T{ op PC S A X Y P -> $d291 $ff $11 $50 $d7 $66 }T T{ $d290 M $d291 M $d292 M -> $fb $b9 $01 }T
$3c91 >PC $42 >S $8c >A $50 >X $12 >Y $23 >P $3c91 $fb >M $3c92 $a9 >M $3c93 $4f >M
T{ op PC S A X Y P -> $3c92 $42 $8c $50 $12 $23 }T T{ $3c91 M $3c92 M $3c93 M -> $fb $a9 $4f }T
$ef9b >PC $65 >S $3d >A $16 >X $44 >Y $a9 >P $ef9b $fb >M $ef9c $1c >M $ef9d $b5 >M
T{ op PC S A X Y P -> $ef9c $65 $3d $16 $44 $a9 }T T{ $ef9b M $ef9c M $ef9d M -> $fb $1c $b5 }T
$baae >PC $69 >S $89 >A $30 >X $03 >Y $28 >P $baae $fb >M $baaf $ad >M $bab0 $46 >M
T{ op PC S A X Y P -> $baaf $69 $89 $30 $03 $28 }T T{ $baae M $baaf M $bab0 M -> $fb $ad $46 }T
$3f62 >PC $4e >S $3d >A $17 >X $a7 >Y $a8 >P $3f62 $fb >M $3f63 $52 >M $3f64 $0b >M
T{ op PC S A X Y P -> $3f63 $4e $3d $17 $a7 $a8 }T T{ $3f62 M $3f63 M $3f64 M -> $fb $52 $0b }T
$8874 >PC $4c >S $14 >A $4f >X $f7 >Y $ad >P $8874 $fb >M $8875 $d3 >M $8876 $d8 >M
T{ op PC S A X Y P -> $8875 $4c $14 $4f $f7 $ad }T T{ $8874 M $8875 M $8876 M -> $fb $d3 $d8 }T
$bcc7 >PC $81 >S $4e >A $b0 >X $ab >Y $e6 >P $bcc7 $fb >M $bcc8 $72 >M $bcc9 $50 >M
T{ op PC S A X Y P -> $bcc8 $81 $4e $b0 $ab $e6 }T T{ $bcc7 M $bcc8 M $bcc9 M -> $fb $72 $50 }T
$0ec1 >PC $57 >S $48 >A $02 >X $85 >Y $ac >P $0ec1 $fb >M $0ec2 $06 >M $0ec3 $1e >M
T{ op PC S A X Y P -> $0ec2 $57 $48 $02 $85 $ac }T T{ $0ec1 M $0ec2 M $0ec3 M -> $fb $06 $1e }T
$05ff >PC $a1 >S $5c >A $76 >X $32 >Y $e9 >P $05ff $fb >M $0600 $b6 >M $0601 $9e >M
T{ op PC S A X Y P -> $0600 $a1 $5c $76 $32 $e9 }T T{ $05ff M $0600 M $0601 M -> $fb $b6 $9e }T
$d4b0 >PC $64 >S $55 >A $e5 >X $56 >Y $6e >P $d4b0 $fb >M $d4b1 $24 >M $d4b2 $03 >M
T{ op PC S A X Y P -> $d4b1 $64 $55 $e5 $56 $6e }T T{ $d4b0 M $d4b1 M $d4b2 M -> $fb $24 $03 }T
$0f11 >PC $68 >S $da >A $fa >X $51 >Y $a9 >P $0f11 $fb >M $0f12 $09 >M $0f13 $97 >M
T{ op PC S A X Y P -> $0f12 $68 $da $fa $51 $a9 }T T{ $0f11 M $0f12 M $0f13 M -> $fb $09 $97 }T
$d0d3 >PC $30 >S $7c >A $ad >X $00 >Y $e3 >P $d0d3 $fb >M $d0d4 $0f >M $d0d5 $6a >M
T{ op PC S A X Y P -> $d0d4 $30 $7c $ad $00 $e3 }T T{ $d0d3 M $d0d4 M $d0d5 M -> $fb $0f $6a }T
$9ddc >PC $23 >S $f6 >A $27 >X $0a >Y $a1 >P $9ddc $fb >M $9ddd $c0 >M $9dde $12 >M
T{ op PC S A X Y P -> $9ddd $23 $f6 $27 $0a $a1 }T T{ $9ddc M $9ddd M $9dde M -> $fb $c0 $12 }T
$cf14 >PC $97 >S $0f >A $9b >X $25 >Y $6f >P $cf14 $fb >M $cf15 $ef >M $cf16 $ba >M
T{ op PC S A X Y P -> $cf15 $97 $0f $9b $25 $6f }T T{ $cf14 M $cf15 M $cf16 M -> $fb $ef $ba }T
( fc )
$bfa8 >PC $5f >S $46 >A $a5 >X $30 >Y $23 >P $bfa8 $fc >M $bfa9 $d2 >M $bfaa $de >M $bfab $5b >M
T{ op PC S A X Y P -> $bfab $5f $46 $a5 $30 $23 }T T{ $bfa8 M $bfa9 M $bfaa M $bfab M -> $fc $d2 $de $5b }T
$db3e >PC $85 >S $79 >A $f1 >X $f2 >Y $20 >P $db3e $fc >M $db3f $ab >M $db40 $55 >M $db41 $33 >M
T{ op PC S A X Y P -> $db41 $85 $79 $f1 $f2 $20 }T T{ $db3e M $db3f M $db40 M $db41 M -> $fc $ab $55 $33 }T
$386e >PC $11 >S $c2 >A $ce >X $85 >Y $6d >P $386e $fc >M $386f $52 >M $3870 $7b >M $3871 $42 >M
T{ op PC S A X Y P -> $3871 $11 $c2 $ce $85 $6d }T T{ $386e M $386f M $3870 M $3871 M -> $fc $52 $7b $42 }T
$f27c >PC $f9 >S $68 >A $c1 >X $f2 >Y $61 >P $f27c $fc >M $f27d $3a >M $f27e $ce >M $f27f $fb >M
T{ op PC S A X Y P -> $f27f $f9 $68 $c1 $f2 $61 }T T{ $f27c M $f27d M $f27e M $f27f M -> $fc $3a $ce $fb }T
$9d63 >PC $5d >S $48 >A $4d >X $df >Y $a2 >P $9d63 $fc >M $9d64 $62 >M $9d65 $91 >M $9d66 $50 >M
T{ op PC S A X Y P -> $9d66 $5d $48 $4d $df $a2 }T T{ $9d63 M $9d64 M $9d65 M $9d66 M -> $fc $62 $91 $50 }T
$e802 >PC $e5 >S $6a >A $70 >X $c4 >Y $60 >P $e802 $fc >M $e803 $ab >M $e804 $a0 >M $e805 $ee >M
T{ op PC S A X Y P -> $e805 $e5 $6a $70 $c4 $60 }T T{ $e802 M $e803 M $e804 M $e805 M -> $fc $ab $a0 $ee }T
$395a >PC $e4 >S $7a >A $82 >X $9a >Y $21 >P $395a $fc >M $395b $8e >M $395c $f5 >M $395d $79 >M
T{ op PC S A X Y P -> $395d $e4 $7a $82 $9a $21 }T T{ $395a M $395b M $395c M $395d M -> $fc $8e $f5 $79 }T
$60a1 >PC $97 >S $bc >A $ce >X $e3 >Y $e6 >P $60a1 $fc >M $60a2 $a4 >M $60a3 $d1 >M $60a4 $af >M
T{ op PC S A X Y P -> $60a4 $97 $bc $ce $e3 $e6 }T T{ $60a1 M $60a2 M $60a3 M $60a4 M -> $fc $a4 $d1 $af }T
$54f2 >PC $77 >S $ad >A $3e >X $c4 >Y $ef >P $54f2 $fc >M $54f3 $f8 >M $54f4 $37 >M $54f5 $84 >M
T{ op PC S A X Y P -> $54f5 $77 $ad $3e $c4 $ef }T T{ $54f2 M $54f3 M $54f4 M $54f5 M -> $fc $f8 $37 $84 }T
$593e >PC $af >S $e1 >A $e8 >X $3f >Y $25 >P $593e $fc >M $593f $a1 >M $5940 $20 >M $5941 $7f >M
T{ op PC S A X Y P -> $5941 $af $e1 $e8 $3f $25 }T T{ $593e M $593f M $5940 M $5941 M -> $fc $a1 $20 $7f }T
$008e >PC $da >S $7b >A $7a >X $ed >Y $2d >P $008e $fc >M $008f $d5 >M $0090 $90 >M $0091 $e0 >M
T{ op PC S A X Y P -> $0091 $da $7b $7a $ed $2d }T T{ $008e M $008f M $0090 M $0091 M -> $fc $d5 $90 $e0 }T
$3963 >PC $f9 >S $5a >A $3a >X $fb >Y $ed >P $3963 $fc >M $3964 $34 >M $3965 $84 >M $3966 $87 >M
T{ op PC S A X Y P -> $3966 $f9 $5a $3a $fb $ed }T T{ $3963 M $3964 M $3965 M $3966 M -> $fc $34 $84 $87 }T
$cd43 >PC $92 >S $59 >A $b2 >X $a8 >Y $a4 >P $cd43 $fc >M $cd44 $d5 >M $cd45 $cf >M $cd46 $d2 >M
T{ op PC S A X Y P -> $cd46 $92 $59 $b2 $a8 $a4 }T T{ $cd43 M $cd44 M $cd45 M $cd46 M -> $fc $d5 $cf $d2 }T
$f078 >PC $39 >S $da >A $b6 >X $84 >Y $a0 >P $f078 $fc >M $f079 $bf >M $f07a $69 >M $f07b $a3 >M
T{ op PC S A X Y P -> $f07b $39 $da $b6 $84 $a0 }T T{ $f078 M $f079 M $f07a M $f07b M -> $fc $bf $69 $a3 }T
$2741 >PC $bc >S $1d >A $6e >X $a5 >Y $64 >P $2741 $fc >M $2742 $8a >M $2743 $3a >M $2744 $ce >M
T{ op PC S A X Y P -> $2744 $bc $1d $6e $a5 $64 }T T{ $2741 M $2742 M $2743 M $2744 M -> $fc $8a $3a $ce }T
$0b6c >PC $db >S $f6 >A $78 >X $4c >Y $23 >P $0b6c $fc >M $0b6d $bf >M $0b6e $d6 >M $0b6f $ba >M
T{ op PC S A X Y P -> $0b6f $db $f6 $78 $4c $23 }T T{ $0b6c M $0b6d M $0b6e M $0b6f M -> $fc $bf $d6 $ba }T
( fd )
$f0da >PC $b5 >S $32 >A $a2 >X $8a >Y $b2 >P $872e $89 >M $f0da $fd >M $f0db $8c >M $f0dc $86 >M
T{ op PC S A X Y P -> $f0dd $b5 $a8 $a2 $8a $f0 }T T{ $872e M $f0da M $f0db M $f0dc M -> $89 $fd $8c $86 }T
$c836 >PC $4a >S $8e >A $2b >X $db >Y $3a >P $6ca1 $4b >M $c836 $fd >M $c837 $76 >M $c838 $6c >M
T{ op PC S A X Y P -> $c839 $4a $42 $2b $db $79 }T T{ $6ca1 M $c836 M $c837 M $c838 M -> $4b $fd $76 $6c }T
$d798 >PC $d7 >S $14 >A $e6 >X $8d >Y $7e >P $5ff3 $20 >M $d798 $fd >M $d799 $0d >M $d79a $5f >M
T{ op PC S A X Y P -> $d79b $d7 $93 $e6 $8d $bc }T T{ $5ff3 M $d798 M $d799 M $d79a M -> $20 $fd $0d $5f }T
$51c1 >PC $45 >S $fe >A $11 >X $8b >Y $ff >P $51c1 $fd >M $51c2 $d4 >M $51c3 $f5 >M $f5e5 $53 >M
T{ op PC S A X Y P -> $51c4 $45 $ab $11 $8b $bd }T T{ $51c1 M $51c2 M $51c3 M $f5e5 M -> $fd $d4 $f5 $53 }T
$10ed >PC $f9 >S $f5 >A $77 >X $82 >Y $b8 >P $0344 $ee >M $10ed $fd >M $10ee $cd >M $10ef $02 >M
T{ op PC S A X Y P -> $10f0 $f9 $00 $77 $82 $3b }T T{ $0344 M $10ed M $10ee M $10ef M -> $ee $fd $cd $02 }T
$61dd >PC $d3 >S $68 >A $e8 >X $7c >Y $f8 >P $61dd $fd >M $61de $41 >M $61df $ea >M $eb29 $7d >M
T{ op PC S A X Y P -> $61e0 $d3 $84 $e8 $7c $b8 }T T{ $61dd M $61de M $61df M $eb29 M -> $fd $41 $ea $7d }T
$b6b6 >PC $a6 >S $5f >A $37 >X $e0 >Y $fc >P $681b $57 >M $b6b6 $fd >M $b6b7 $e4 >M $b6b8 $67 >M
T{ op PC S A X Y P -> $b6b9 $a6 $07 $37 $e0 $3d }T T{ $681b M $b6b6 M $b6b7 M $b6b8 M -> $57 $fd $e4 $67 }T
$383f >PC $f2 >S $b2 >A $88 >X $c8 >Y $f9 >P $383f $fd >M $3840 $19 >M $3841 $f9 >M $f9a1 $5e >M
T{ op PC S A X Y P -> $3842 $f2 $4e $88 $c8 $79 }T T{ $383f M $3840 M $3841 M $f9a1 M -> $fd $19 $f9 $5e }T
$389c >PC $06 >S $b6 >A $d4 >X $65 >Y $74 >P $1cab $10 >M $389c $fd >M $389d $d7 >M $389e $1b >M
T{ op PC S A X Y P -> $389f $06 $a5 $d4 $65 $b5 }T T{ $1cab M $389c M $389d M $389e M -> $10 $fd $d7 $1b }T
$69c9 >PC $91 >S $da >A $72 >X $49 >Y $78 >P $69c9 $fd >M $69ca $b1 >M $69cb $df >M $e023 $7f >M
T{ op PC S A X Y P -> $69cc $91 $54 $72 $49 $79 }T T{ $69c9 M $69ca M $69cb M $e023 M -> $fd $b1 $df $7f }T
$c6b6 >PC $0d >S $fb >A $05 >X $6b >Y $3b >P $2d23 $2a >M $c6b6 $fd >M $c6b7 $1e >M $c6b8 $2d >M
T{ op PC S A X Y P -> $c6b9 $0d $d1 $05 $6b $b9 }T T{ $2d23 M $c6b6 M $c6b7 M $c6b8 M -> $2a $fd $1e $2d }T
$ee32 >PC $a4 >S $8a >A $39 >X $61 >Y $3d >P $8e9a $8e >M $ee32 $fd >M $ee33 $61 >M $ee34 $8e >M
T{ op PC S A X Y P -> $ee35 $a4 $96 $39 $61 $bc }T T{ $8e9a M $ee32 M $ee33 M $ee34 M -> $8e $fd $61 $8e }T
$e3b0 >PC $ba >S $f6 >A $5d >X $91 >Y $f6 >P $e3b0 $fd >M $e3b1 $67 >M $e3b2 $fa >M $fac4 $d4 >M
T{ op PC S A X Y P -> $e3b3 $ba $21 $5d $91 $35 }T T{ $e3b0 M $e3b1 M $e3b2 M $fac4 M -> $fd $67 $fa $d4 }T
$fd61 >PC $6c >S $18 >A $e0 >X $0b >Y $33 >P $d7cc $a5 >M $fd61 $fd >M $fd62 $ec >M $fd63 $d6 >M
T{ op PC S A X Y P -> $fd64 $6c $73 $e0 $0b $30 }T T{ $d7cc M $fd61 M $fd62 M $fd63 M -> $a5 $fd $ec $d6 }T
$d5c4 >PC $c3 >S $e7 >A $5b >X $d2 >Y $7e >P $b404 $ae >M $d5c4 $fd >M $d5c5 $a9 >M $d5c6 $b3 >M
T{ op PC S A X Y P -> $d5c7 $c3 $32 $5b $d2 $3d }T T{ $b404 M $d5c4 M $d5c5 M $d5c6 M -> $ae $fd $a9 $b3 }T
$31da >PC $7d >S $46 >A $4c >X $12 >Y $bc >P $31da $fd >M $31db $f4 >M $31dc $75 >M $7640 $78 >M
T{ op PC S A X Y P -> $31dd $7d $67 $4c $12 $3c }T T{ $31da M $31db M $31dc M $7640 M -> $fd $f4 $75 $78 }T
( fe )
$374b >PC $7c >S $b0 >A $63 >X $ef >Y $65 >P $374b $fe >M $374c $d5 >M $374d $ce >M $374e $b9 >M $cf38 $9b >M
T{ op PC S A X Y P -> $374e $7c $b0 $63 $ef $e5 }T T{ $374b M $374c M $374d M $374e M $cf38 M -> $fe $d5 $ce $b9 $9c }T
$1543 >PC $62 >S $11 >A $fa >X $97 >Y $e4 >P $1543 $fe >M $1544 $33 >M $1545 $a2 >M $1546 $4d >M $a32d $ab >M
T{ op PC S A X Y P -> $1546 $62 $11 $fa $97 $e4 }T T{ $1543 M $1544 M $1545 M $1546 M $a32d M -> $fe $33 $a2 $4d $ac }T
$92e1 >PC $39 >S $f3 >A $c5 >X $17 >Y $28 >P $5b5c $c3 >M $92e1 $fe >M $92e2 $97 >M $92e3 $5a >M $92e4 $48 >M
T{ op PC S A X Y P -> $92e4 $39 $f3 $c5 $17 $a8 }T T{ $5b5c M $92e1 M $92e2 M $92e3 M $92e4 M -> $c4 $fe $97 $5a $48 }T
$6ddf >PC $d4 >S $40 >A $fd >X $09 >Y $28 >P $6ddf $fe >M $6de0 $1d >M $6de1 $8d >M $6de2 $53 >M $8e1a $9f >M
T{ op PC S A X Y P -> $6de2 $d4 $40 $fd $09 $a8 }T T{ $6ddf M $6de0 M $6de1 M $6de2 M $8e1a M -> $fe $1d $8d $53 $a0 }T
$dafb >PC $72 >S $ba >A $13 >X $f6 >Y $ec >P $00cc $45 >M $dafb $fe >M $dafc $b9 >M $dafd $00 >M $dafe $84 >M
T{ op PC S A X Y P -> $dafe $72 $ba $13 $f6 $6c }T T{ $00cc M $dafb M $dafc M $dafd M $dafe M -> $46 $fe $b9 $00 $84 }T
$98cc >PC $2c >S $14 >A $c9 >X $4e >Y $a9 >P $0f6f $bc >M $98cc $fe >M $98cd $a6 >M $98ce $0e >M $98cf $ed >M
T{ op PC S A X Y P -> $98cf $2c $14 $c9 $4e $a9 }T T{ $0f6f M $98cc M $98cd M $98ce M $98cf M -> $bd $fe $a6 $0e $ed }T
$568a >PC $b6 >S $a5 >A $ef >X $19 >Y $6c >P $568a $fe >M $568b $13 >M $568c $79 >M $568d $90 >M $7a02 $dd >M
T{ op PC S A X Y P -> $568d $b6 $a5 $ef $19 $ec }T T{ $568a M $568b M $568c M $568d M $7a02 M -> $fe $13 $79 $90 $de }T
$55df >PC $ca >S $63 >A $60 >X $20 >Y $20 >P $55df $fe >M $55e0 $ed >M $55e1 $d2 >M $55e2 $e9 >M $d34d $d5 >M
T{ op PC S A X Y P -> $55e2 $ca $63 $60 $20 $a0 }T T{ $55df M $55e0 M $55e1 M $55e2 M $d34d M -> $fe $ed $d2 $e9 $d6 }T
$f3d4 >PC $59 >S $2a >A $bc >X $49 >Y $6a >P $6fb6 $99 >M $f3d4 $fe >M $f3d5 $fa >M $f3d6 $6e >M $f3d7 $da >M
T{ op PC S A X Y P -> $f3d7 $59 $2a $bc $49 $e8 }T T{ $6fb6 M $f3d4 M $f3d5 M $f3d6 M $f3d7 M -> $9a $fe $fa $6e $da }T
$0a49 >PC $07 >S $28 >A $17 >X $7b >Y $e3 >P $0a49 $fe >M $0a4a $7e >M $0a4b $ac >M $0a4c $68 >M $ac95 $4e >M
T{ op PC S A X Y P -> $0a4c $07 $28 $17 $7b $61 }T T{ $0a49 M $0a4a M $0a4b M $0a4c M $ac95 M -> $fe $7e $ac $68 $4f }T
$e741 >PC $4f >S $94 >A $75 >X $ce >Y $2f >P $6c83 $06 >M $e741 $fe >M $e742 $0e >M $e743 $6c >M $e744 $74 >M
T{ op PC S A X Y P -> $e744 $4f $94 $75 $ce $2d }T T{ $6c83 M $e741 M $e742 M $e743 M $e744 M -> $07 $fe $0e $6c $74 }T
$f4c9 >PC $e8 >S $e4 >A $a8 >X $80 >Y $2d >P $d591 $52 >M $f4c9 $fe >M $f4ca $e9 >M $f4cb $d4 >M $f4cc $37 >M
T{ op PC S A X Y P -> $f4cc $e8 $e4 $a8 $80 $2d }T T{ $d591 M $f4c9 M $f4ca M $f4cb M $f4cc M -> $53 $fe $e9 $d4 $37 }T
$20ee >PC $5b >S $a3 >A $98 >X $e1 >Y $e1 >P $20ee $fe >M $20ef $03 >M $20f0 $85 >M $20f1 $a6 >M $859b $cf >M
T{ op PC S A X Y P -> $20f1 $5b $a3 $98 $e1 $e1 }T T{ $20ee M $20ef M $20f0 M $20f1 M $859b M -> $fe $03 $85 $a6 $d0 }T
$a374 >PC $61 >S $2b >A $ea >X $08 >Y $a5 >P $2980 $17 >M $a374 $fe >M $a375 $96 >M $a376 $28 >M $a377 $93 >M
T{ op PC S A X Y P -> $a377 $61 $2b $ea $08 $25 }T T{ $2980 M $a374 M $a375 M $a376 M $a377 M -> $18 $fe $96 $28 $93 }T
$c6b4 >PC $06 >S $a2 >A $52 >X $7e >Y $ec >P $92c1 $d9 >M $c6b4 $fe >M $c6b5 $6f >M $c6b6 $92 >M $c6b7 $48 >M
T{ op PC S A X Y P -> $c6b7 $06 $a2 $52 $7e $ec }T T{ $92c1 M $c6b4 M $c6b5 M $c6b6 M $c6b7 M -> $da $fe $6f $92 $48 }T
$cfaa >PC $4b >S $07 >A $02 >X $bb >Y $69 >P $a89f $40 >M $cfaa $fe >M $cfab $9d >M $cfac $a8 >M $cfad $39 >M
T{ op PC S A X Y P -> $cfad $4b $07 $02 $bb $69 }T T{ $a89f M $cfaa M $cfab M $cfac M $cfad M -> $41 $fe $9d $a8 $39 }T
( ff )
$f159 >PC $cb >S $e2 >A $04 >X $8d >Y $20 >P $00f5 $fa >M $f156 $b6 >M $f159 $ff >M $f15a $f5 >M $f15b $fa >M
T{ op PC S A X Y P -> $f156 $cb $e2 $04 $8d $20 }T T{ $00f5 M $f156 M $f159 M $f15a M $f15b M -> $fa $b6 $ff $f5 $fa }T
$156d >PC $3e >S $ad >A $0e >X $d8 >Y $e8 >P $00ec $5c >M $1503 $f4 >M $156d $ff >M $156e $ec >M $156f $93 >M $1570 $b8 >M
T{ op PC S A X Y P -> $1570 $3e $ad $0e $d8 $e8 }T T{ $00ec M $1503 M $156d M $156e M $156f M $1570 M -> $5c $f4 $ff $ec $93 $b8 }T
$13a4 >PC $49 >S $70 >A $58 >X $9b >Y $ae >P $0036 $b9 >M $13a4 $ff >M $13a5 $36 >M $13a6 $45 >M $13ec $5b >M
T{ op PC S A X Y P -> $13ec $49 $70 $58 $9b $ae }T T{ $0036 M $13a4 M $13a5 M $13a6 M $13ec M -> $b9 $ff $36 $45 $5b }T
$2e97 >PC $1d >S $c2 >A $44 >X $29 >Y $28 >P $0023 $9e >M $2e97 $ff >M $2e98 $23 >M $2e99 $12 >M $2eac $30 >M
T{ op PC S A X Y P -> $2eac $1d $c2 $44 $29 $28 }T T{ $0023 M $2e97 M $2e98 M $2e99 M $2eac M -> $9e $ff $23 $12 $30 }T
$fc8c >PC $10 >S $64 >A $4b >X $16 >Y $e2 >P $004b $ae >M $fc3c $fc >M $fc8c $ff >M $fc8d $4b >M $fc8e $ad >M
T{ op PC S A X Y P -> $fc3c $10 $64 $4b $16 $e2 }T T{ $004b M $fc3c M $fc8c M $fc8d M $fc8e M -> $ae $fc $ff $4b $ad }T
$0e18 >PC $6e >S $25 >A $09 >X $0a >Y $68 >P $00c2 $57 >M $0e18 $ff >M $0e19 $c2 >M $0e1a $28 >M $0e1b $f1 >M $0e43 $d7 >M
T{ op PC S A X Y P -> $0e1b $6e $25 $09 $0a $68 }T T{ $00c2 M $0e18 M $0e19 M $0e1a M $0e1b M $0e43 M -> $57 $ff $c2 $28 $f1 $d7 }T
$11fd >PC $58 >S $3b >A $39 >X $96 >Y $60 >P $0087 $ab >M $118e $12 >M $11fd $ff >M $11fe $87 >M $11ff $8e >M $128e $dd >M
T{ op PC S A X Y P -> $118e $58 $3b $39 $96 $60 }T T{ $0087 M $118e M $11fd M $11fe M $11ff M $128e M -> $ab $12 $ff $87 $8e $dd }T
$b3ff >PC $76 >S $c3 >A $a2 >X $37 >Y $62 >P $0049 $be >M $b386 $0c >M $b3ff $ff >M $b400 $49 >M $b401 $84 >M $b486 $62 >M
T{ op PC S A X Y P -> $b386 $76 $c3 $a2 $37 $62 }T T{ $0049 M $b386 M $b3ff M $b400 M $b401 M $b486 M -> $be $0c $ff $49 $84 $62 }T
$3e9a >PC $8f >S $5c >A $ad >X $44 >Y $ef >P $008b $94 >M $3e9a $ff >M $3e9b $8b >M $3e9c $06 >M $3ea3 $6d >M
T{ op PC S A X Y P -> $3ea3 $8f $5c $ad $44 $ef }T T{ $008b M $3e9a M $3e9b M $3e9c M $3ea3 M -> $94 $ff $8b $06 $6d }T
$fafb >PC $84 >S $f3 >A $3a >X $56 >Y $a9 >P $0059 $7f >M $fadf $40 >M $fafb $ff >M $fafc $59 >M $fafd $e1 >M $fafe $83 >M
T{ op PC S A X Y P -> $fafe $84 $f3 $3a $56 $a9 }T T{ $0059 M $fadf M $fafb M $fafc M $fafd M $fafe M -> $7f $40 $ff $59 $e1 $83 }T
$a469 >PC $fc >S $b5 >A $39 >X $08 >Y $ed >P $00e8 $8b >M $a469 $ff >M $a46a $e8 >M $a46b $45 >M $a4b1 $e4 >M
T{ op PC S A X Y P -> $a4b1 $fc $b5 $39 $08 $ed }T T{ $00e8 M $a469 M $a46a M $a46b M $a4b1 M -> $8b $ff $e8 $45 $e4 }T
$b923 >PC $62 >S $93 >A $39 >X $d7 >Y $2f >P $008a $2d >M $b923 $ff >M $b924 $8a >M $b925 $40 >M $b926 $6f >M $b966 $cc >M
T{ op PC S A X Y P -> $b926 $62 $93 $39 $d7 $2f }T T{ $008a M $b923 M $b924 M $b925 M $b926 M $b966 M -> $2d $ff $8a $40 $6f $cc }T
$e322 >PC $6b >S $83 >A $0d >X $bb >Y $2e >P $0032 $12 >M $e304 $28 >M $e322 $ff >M $e323 $32 >M $e324 $df >M $e325 $b7 >M
T{ op PC S A X Y P -> $e325 $6b $83 $0d $bb $2e }T T{ $0032 M $e304 M $e322 M $e323 M $e324 M $e325 M -> $12 $28 $ff $32 $df $b7 }T
$c109 >PC $dd >S $6f >A $32 >X $ec >Y $6e >P $001f $9f >M $c109 $ff >M $c10a $1f >M $c10b $10 >M $c11c $f2 >M
T{ op PC S A X Y P -> $c11c $dd $6f $32 $ec $6e }T T{ $001f M $c109 M $c10a M $c10b M $c11c M -> $9f $ff $1f $10 $f2 }T
$a0d2 >PC $34 >S $99 >A $0b >X $73 >Y $ae >P $00a5 $a6 >M $a08c $a4 >M $a0d2 $ff >M $a0d3 $a5 >M $a0d4 $b7 >M
T{ op PC S A X Y P -> $a08c $34 $99 $0b $73 $ae }T T{ $00a5 M $a08c M $a0d2 M $a0d3 M $a0d4 M -> $a6 $a4 $ff $a5 $b7 }T
$61d8 >PC $24 >S $37 >A $d1 >X $b4 >Y $ec >P $0082 $3a >M $61a4 $ef >M $61d8 $ff >M $61d9 $82 >M $61da $c9 >M $61db $ff >M
T{ op PC S A X Y P -> $61db $24 $37 $d1 $b4 $ec }T T{ $0082 M $61a4 M $61d8 M $61d9 M $61da M $61db M -> $3a $ef $ff $82 $c9 $ff }T

bye
