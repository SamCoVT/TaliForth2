; platform_words.asm
; PC Engine Specific Assembly Words

; =========================================================================
; This file contains assembly-language definitions for the PC Engine.
; The headers are created in reverse order; prev_nt := 0 marks the end
; of this specific wordlist section.
; =========================================================================

prev_nt := 0

; ==== HEADERS ====
; Use the #nt_header macro here for new words.
; Format: #nt_header forth_name
; Or:     #nt_header label_name, "forth_name", flags


; ==== WORDS ====
; Define the execution tokens (xt_...) and end labels (z_...) here.
; Example structure:
; xt_myword:
;     ... assembly ...
; z_myword:
;     rts


.include "./dev/pce_hardware.asm"
.include "./dev/memory_util.asm"
;.include "./dev/video_util.asm"
;.include "./dev/joy_util.asm"
