; Default kernel file for Tali Forth 2 shared by the py65mon and c65 platforms
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; First version: 19. Jan 2014
; This version: 01. Jun 2024


; =====================================================================
; Include Tali Forth 2 code
; Make sure the above options are set BEFORE this include.

.include "../taliforth.asm" ; zero page variables, definitions

; Now we've got all of Tali's native code.  This requires about 24Kb
; with all options, or as little as 12Kb for a minimal build.
; In the default configuraiton, we've filled ROM from $8000
; to about $dfff, leaving about 8Kb.

; Both py65mon and c65 use $f000-$f010 as their default IO interface
; and we don't want to change that because it would make it harder to
; use out of the box, so we just advance past the virtual hardware addresses.
; (We could also choose to put our kernel routines before the IO addresses.)

io_start = $f000                ; virtual hardware addresses for the simulators

* = io_start

; Define the c65 / py65mon magic IO addresses relative to $f000
                .byte ?
io_putc:        .byte ?         ; $f001     write byte to stdout
                .byte ?
io_kbhit:       .byte ?         ; $f003     read non-zero on key ready (c65 only)
io_getc:        .byte ?         ; $f004     non-blocking read input character (0 if no key)
io_clk_start:   .byte ?         ; $f006     *read* to start cycle counter
io_clk_stop:    .byte ?         ; $f007     *read* to stop the cycle counter
io_clk_cycles:  .word ?,?       ; $f008-b   32-bit cycle count in NUXI order
                .word ?,?

; These magic block IO addresses are only implemented by c65 (not py65mon)
; see c65/README.md for more detail

io_blk_action:  .byte ?     ; $f010     Write to act (status=0 read=1 write=2)
io_blk_status:  .byte ?     ; $f011     Read action result (OK=0)
io_blk_number:  .word ?     ; $f012     Little endian block number 0-ffff
io_blk_buffer:  .word ?     ; $f014     Little endian memory address

io_end:

; Now we're safe to inject the required kernel routines to interface to
; our "hardware" which is a simulator in this configuration.

kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have the 6502 stack ready yet. With
        ; py65mon, of course, this is really easy. At the end, we JMP
        ; back to the label `forth` to start the Forth system.
        ; This will also typically be the target of the reset vector.
        ; """
                ; Since the default case for Tali is the py65mon emulator, we
                ; have no use for interrupts. If you are going to include
                ; them in your system in any way, you're going to have to
                ; do it from scratch. Sorry.
                sei             ; Disable interrupts

                ; We've successfully set everything up, so print the kernel
                ; string
                ldx #0
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc
                inx
                bra -
_done:
                jmp forth

kernel_bye:
        ; """Forth shutdown called from BYE"""
                brk

kernel_putc:
        ; """Print a single character to the console.
        ;
        ; Note this routine must preserve X, Y but that's easy here.
        ; If your code is more complex, wrap it with PHX, PHY ... PLY, PHX
        ; """
                sta io_putc
                rts

; c65 and py65mon have different implementations of kernel_getc and kernel_kbhit

