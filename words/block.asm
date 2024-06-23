; ## BLK ( -- addr ) "Push address of block being interpreted"
; ## "block"  auto  ANS block
        ; """https://forth-standard.org/standard/block/BLK"""
xt_blk:
w_blk:
                lda #blk_offset
                jmp push_upvar_tos
z_blk:



; ## BLKBUFFER ( -- addr ) "Push address of block buffer"
; ## "blkbuffer"  auto  Tali block
xt_blkbuffer:
w_blkbuffer:
                ; blkbuffer address is at UP + blkbuffer_offset.
                ; Unlike some of the other user variables, we actually
                ; want to push the address stored here, which will
                ; point to somewhere outside of the user variables.
                dex
                dex
                ; Put the address on the stack.
                ldy #blkbuffer_offset
                lda (up),y
                sta 0,x
                iny             ; Move along to the next byte
                lda (up),y
                sta 1,x

z_blkbuffer:    rts



; ## BLOCK ( u -- a-addr ) "Fetch a block into a buffer"
; ## "block"  auto  ANS block
        ; """https://forth-standard.org/standard/block/BLOCK"""
xt_block:
w_block:
                ; See if the block requested is the same as the one we
                ; currently have in the buffer. Check the LSB.
                ldy #buffblocknum_offset
                lda (up),y
                cmp 0,x
                bne _not_in_buffer

                ; Check the MSB.
                iny
                lda (up),y
                cmp 1,x
                bne _not_in_buffer

                ; The block is in the buffer. See if the buffer is in use.
                ldy #buffstatus_offset
                lda (up),y
                and #1          ; Check the in-use flag (bit 0)
                bne _done       ; It's already in the buffer and in use.
                                ; _done will replace the block# with the
                                ; buffer address.
_not_in_buffer:
                ; Check the buffer status
                ldy #buffstatus_offset
                lda (up),y      ; Only bits 0 and 1 are used, so only
                cmp #3          ; LSB is needed.
                bne _buffer_available ; Unused or not dirty = available

                ; We need to save the block.
                jsr w_blkbuffer
                jsr w_buffblocknum
                jsr w_fetch
                jsr w_block_write

_buffer_available:
                ; Save the block number.
                ldy #buffblocknum_offset
                lda 0,x
                sta (up),y
                iny
                lda 1,x
                sta (up),y

                ; Get the requested block.
                jsr w_blkbuffer
                jsr w_swap
                jsr w_block_read

                ; Mark the buffer as clean and in-use.
                lda #1
                ldy #buffstatus_offset
                sta (up),y

                ; Make room on the stack for the return address.
                dex
                dex

_done:
                ; It's in the buffer. Return the buffer address.
                ldy #blkbuffer_offset
                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x

z_block:        rts

.if "block" in TALI_OPTIONAL_WORDS

; ## BLOCK_C65_INIT ( -- f ) "Initialize c65 simulator block storage"
; ## "block-c65-init"  auto  Tali block
        ; """Set up block IO to read/write to/from c65 block file.
        ; Run simulator with a writable block file option
        ; e.g. `touch blocks.dat; c65/c65 -b blocks.dat -r taliforth-py65mon.bin`
        ; Returns true if c65 block storage is available and false otherwise."""

.weak
; These labels allow this to assemble even if c65 is not the target platform.
; Because they are weak, they will be replaced when c65 is the target platform.
io_blk_status = 0
io_blk_action = 0
io_blk_number = 0
io_blk_buffer = 0
.endweak
xt_block_c65_init:
w_block_c65_init:
                lda #$ff
                sta io_blk_status
                lda #$0
                sta io_blk_action
                lda io_blk_status      ; $0 if OK, $ff otherwise
                eor #$ff            ; invert to forth true/false
                dex
                dex
                sta 0,x             ; true ($ff) if OK, false (0) otherwise
                sta 1,x
                dex
                dex
                lda #<c65_blk_read
                sta 0,x
                lda #>c65_blk_read
                sta 1,x
                jsr w_block_read_vector
                jsr w_store
                dex
                dex
                lda #<c65_blk_write
                sta 0,x
                lda #>c65_blk_write
                sta 1,x
                jsr w_block_write_vector
                jsr w_store
z_block_c65_init:
                rts

c65_blk_write:  ldy #2
                bra c65_blk_rw
c65_blk_read:   ldy #1
c65_blk_rw:     lda 0,x                 ; ( addr blk# )
                sta io_blk_number
                lda 1,x
                sta io_blk_number+1
                lda 2,x
                sta io_blk_buffer
                lda 3,x
                sta io_blk_buffer+1
                sty io_blk_action       ; trigger the r/w
                inx                     ; clean up stack
                inx
                inx
                inx
                rts
.endif



.if "ramdrive" in TALI_OPTIONAL_WORDS
; ## BLOCK_RAMDRIVE_INIT ( u -- ) "Create a ramdrive for blocks"
; ## "block-ramdrive-init"  auto  Tali block
        ; """Create a RAM drive, with the given number of
        ; blocks, in the dictionary along with setting up the block words to
        ; use it.  The read/write routines do not provide bounds checking.
        ; Expected use: `4 block-ramdrive-init` ( to create blocks 0-3 )
        ; """
xt_block_ramdrive_init:
                jsr underflow_1
w_block_ramdrive_init:
                ; Store the string to run here as a string literal.
                ; See SLITERAL for the format information. This way, we
                ; don't have the words defined below in the Dictionary until
                ; we really use them.
                jsr sliteral_runtime
                .word ramdrive_code, ramdrive_code_end-ramdrive_code

                ; The address and length of the ramdrive code is now on the
                ; stack. Call EVALUATE to run it.
                jsr w_evaluate

z_block_ramdrive_init:
                rts

ramdrive_code:
        .text "base @ swap decimal"
        .text " 1024 *" ; ( Calculate how many bytes are needed for numblocks blocks )
        .text " dup"    ; ( Save a copy for formatting it at the end )
        .text " buffer: ramdrive" ; ( Create ramdrive )
        ; ( These routines just copy between the buffer and the ramdrive blocks )
        .text " : block-read-ramdrive"  ; ( addr u -- )
        .text " ramdrive swap 1024 * + swap 1024 move ;"
        .text " : block-write-ramdrive" ; ( addr u -- )
        .text " ramdrive swap 1024 * + 1024 move ;"
        .text " ' block-read-ramdrive block-read-vector !" ; ( Replace I/O vectors )
        .text " ' block-write-ramdrive block-write-vector !"
        .text " ramdrive swap blank base !"
ramdrive_code_end:

.endif



; ## BLOCK_READ ( addr u -- ) "Read a block from storage (deferred word)"
; ## "block-read"  auto  Tali block
        ; """BLOCK-READ is a vectored word that the user needs to override
        ; with their own version to read a block from storage.
        ; The stack parameters are ( buffer_address block# -- ).
        ; """
xt_block_read:
w_block_read:
                ; Execute the BLOCK-READ-VECTOR
                ldy #blockread_offset
                lda (up),y
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1

                jmp (tmp1)

z_block_read:   ; No RTS needed



; ## BLOCK_READ_VECTOR ( -- addr ) "Address of the block-read vector"
; ## "block-read-vector"  auto  Tali block
        ; """BLOCK-READ is a vectored word that the user needs to override
        ; with their own version to read a block from storage.
        ; This word gives the address of the vector so it can be replaced.
        ; """
xt_block_read_vector:
w_block_read_vector:
                ; Get the BLOCK-READ-VECTOR address
                lda #blockread_offset
                jmp push_upvar_tos
z_block_read_vector:



; This is the default error message the vectored words BLOCK-READ and
; BLOCK-WRITE start with. This word is not included in the dictionary.
xt_block_word_error:
w_block_word_error:
                lda #err_blockwords
                jmp error       ; no RTS needed



; ## BLOCK_WRITE ( addr u -- ) "Write a block to storage (deferred word)"
; ## "block-write"  auto  Tali block
        ; """BLOCK-WRITE is a vectored word that the user needs to override
        ; with their own version to write a block to storage.
        ; The stack parameters are ( buffer_address block# -- ).
        ; """
xt_block_write:
w_block_write:
                ; Execute the BLOCK-READ-VECTOR
                ldy #blockwrite_offset
                lda (up),y
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1
                jmp (tmp1)

z_block_write:  ; No RTS needed



; ## BLOCK_WRITE_VECTOR ( -- addr ) "Address of the block-write vector"
; ## "block-write-vector"  auto  Tali block
        ; """BLOCK-WRITE is a vectored word that the user needs to override
        ; with their own version to write a block to storage.
        ; This word gives the address of the vector so it can be replaced.
        ; """
xt_block_write_vector:
w_block_write_vector:
                ; Get the BLOCK-WRITE-VECTOR address
                lda #blockwrite_offset
                jmp push_upvar_tos
z_block_write_vector:



; ## BUFFBLOCKNUM ( -- addr ) "Push address of variable holding block in buffer"
; ## "buffblocknum"  auto  Tali block
xt_buffblocknum:
w_buffblocknum:
                ; BUFFBLOCKNUM is at UP + buffblocknum_offset
                lda #buffblocknum_offset
                jmp push_upvar_tos
z_buffblocknum:



; ## BUFFER ( u -- a-addr ) "Get a buffer for a block"
; ## "buffer"  auto  ANS block
        ; """https://forth-standard.org/standard/block/BUFFER"""
xt_buffer:
w_buffer:
                ; Check the buffer status
                ldy #buffstatus_offset
                lda (up),y      ; Only bits 0 and 1 are used, so only
                cmp #3          ; LSB is needed.
                bne _buffer_available ; Unused or not dirty = available

                ; We need to save the block.
                jsr w_blkbuffer
                jsr w_buffblocknum
                jsr w_fetch
                jsr w_block_write

_buffer_available:
                ; Save the block number.
                ldy #buffblocknum_offset
                lda 0,x
                sta (up),y
                iny
                lda 1,x
                sta (up),y

                ; Mark the buffer as clean and in-use.
                lda #1
                ldy #buffstatus_offset
                sta (up),y

                ; Return the buffer address.
                ldy #blkbuffer_offset
                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x

z_buffer:       rts



; ## BUFFSTATUS ( -- addr ) "Push address of variable holding buffer status"
; ## "buffstatus"  auto  Tali block
xt_buffstatus:
w_buffstatus:
                lda #buffstatus_offset
                jmp push_upvar_tos
z_buffstatus:



; ## EMPTY_BUFFERS ( -- ) "Empty all buffers without saving"
; ## "empty-buffers"  tested  ANS block ext
        ; """https://forth-standard.org/standard/block/EMPTY-BUFFERS"""
xt_empty_buffers:
w_empty_buffers:
                ; Set the buffer status to empty.
                ldy #buffstatus_offset
                lda #0
                sta (up),y      ; Only LSB is used.
z_empty_buffers:
                rts



; ## FLUSH ( -- ) "Save dirty buffers and empty buffers"
; ## "flush"  auto  ANS block
        ; """https://forth-standard.org/standard/block/FLUSH"""
xt_flush:
w_flush:
                jsr w_save_buffers

                ; Set the buffer status to empty.
                ldy #buffstatus_offset
                lda #0
                sta (up),y      ; Only LSB is used.
z_flush:
                rts



.if "editor" in TALI_OPTIONAL_WORDS
; ## LIST ( scr# -- ) "List the given screen"
; ## "list"  tested  ANS block ext
        ; """https://forth-standard.org/standard/block/LIST"""

xt_list:
                jsr underflow_1
w_list:
                ; Save the screen number in SCR
                jsr w_scr
                jsr w_store

                ; Use L from the editor-wordlist to display the screen.
                jsr w_editor_l

z_list:         rts
.endif



; ## LOAD ( scr# -- ) "Load the Forth code in a screen/block"
; ## "load"  auto  ANS block
        ; """https://forth-standard.org/standard/block/LOAD
        ;
        ; Note: LOAD current works because there is only one buffer.
        ; If/when multiple buffers are supported, we'll have to deal
        ; with the fact that it might re-load the old block into a
        ; different buffer.
        ; """

xt_load:
                jsr underflow_1
w_load:
                ; Save the current value of BLK on the return stack.
                ldy #blk_offset+1
                lda (up),y
                pha
                dey
                lda (up),y
                pha

                ; Set BLK to the given block/screen number.
                lda 0,x
                sta (up),y
                iny
                lda 1,x
                sta (up),y

                ; Load that block into a buffer
                jsr w_block

                ; Put 1024 on the stack for the screen length.
                dex
                dex
                lda #4
                sta 1,x
                stz 0,x

                ; Jump to a special evluate target. This bypasses the underflow
                ; check and skips the zeroing of BLK.
                jsr load_evaluate

                ; Restore the value of BLK from before the LOAD command.
                ldy #blk_offset
                pla
                sta (up),y
                iny
                pla
                sta (up),y

                ; If BLK is not zero, read it back into the buffer.
                ; A still has MSB
                dey
                ora (up),y
                beq _done

                ; The block needs to be read back into the buffer.
                dex
                dex
                ldy #blk_offset
                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x
                jsr w_block

                ; Drop the buffer address.
                inx
                inx

_done:
z_load:         rts



; ## SAVE_BUFFERS ( -- ) "Save all dirty buffers to storage"
; ## "save-buffers"  tested  ANS block
        ; """https://forth-standard.org/standard/block/SAVE-BUFFERS"""

xt_save_buffers:
w_save_buffers:
                ; Check the buffer status
                ldy #buffstatus_offset
                lda (up),y      ; Only bits 0 and 1 are used, so only
                cmp #3          ; LSB is needed.
                bne _done       ; Either not used or not dirty = done!

                ; We need to save the block.
                jsr w_blkbuffer
                jsr w_buffblocknum
                jsr w_fetch
                jsr w_block_write

                ; Mark the buffer as clean now.
                lda #1
                ldy #buffstatus_offset
                sta (up),y

_done:
z_save_buffers: rts



; ## SCR ( -- addr ) "Push address of variable holding last screen listed"
; ## "scr"  auto  ANS block ext
        ; """https://forth-standard.org/standard/block/SCR"""
xt_scr:
w_scr:
                lda #scr_offset
                jmp push_upvar_tos
z_scr:



; ## THRU ( scr# scr# -- ) "Load screens in the given range"
; ## "thru"  tested  ANS block ext
        ; """https://forth-standard.org/standard/block/THRU"""

xt_thru:
                jsr underflow_2
w_thru:
                ; We need to loop here, and can't use the data stack
                ; because the LOADed screens might use it.  We'll
                ; need to use the same trick that DO loops use, holding
                ; the limit and current index on the return stack.

                ; Put the ending screen number on the return stack
                lda 1,x
                pha
                lda 0,x
                pha
                inx
                inx
_thru_loop:
                ; Put the starting screen number on the stack,
                ; but keep a copy
                lda 1,x
                pha
                lda 0,x
                pha

                ; Load this screen.
                jsr w_load

                ; Get the number and limit back off the stack.  Rather than
                ; waste time making room on the stack, just use tmp1 and tmp2.

                ; Get the screen we just loaded.
                pla
                sta tmp1
                pla
                sta tmp1+1

                ; Get the ending screen.
                pla
                sta tmp2
                pla
                sta tmp2+1

                ; See if we just loaded the last screen.
                ; A already has the MSB of the last screen in it.
                cmp tmp1+1
                bne _next_screen
                lda tmp2        ; Compare the LSB
                cmp tmp1
                bne _next_screen
                bra _done       ; We just did the last screen.

_next_screen:
                ; Put the ending screen back on the data stack.
                lda tmp2+1
                pha
                lda tmp2
                pha

                ; Increment the current screen.
                inc tmp1
                bne +
                inc tmp1+1
+
                ; Put the current screen on the stack to prepare for
                ; the next loop.
                dex
                dex
                lda tmp1
                sta 0,x
                lda tmp1+1
                sta 1,x
                bra _thru_loop
_done:
z_thru:         rts



; ## UPDATE ( -- ) "Mark current block as dirty"
; ## "update"  auto  ANS block
        ; """https://forth-standard.org/standard/block/UPDATE"""
xt_update:
w_update:
                ; Turn on the dirty bit. We can't use TSB here because it only
                ; has Absolute and Direct Pages addressing modes
                ldy #buffstatus_offset
                lda (up),y
                ora #2          ; Turn on dirty flag (bit 2)
                sta (up),y

z_update:       rts
