; Register allocations:
; A - scratch
; X - data stack, base address: stack
; Y - scratch
; S - return stack pointer, base address $0100
;
; Vocabulary entry:
;  -N   name
;   0   previous header
;   2   name length and flags
;   3   code address
;


    include_blocks = 1      ; include blocks.compressed in binary?
    small_code = 1          ; optimize for size?

    word_header_last    = 0
    word_header_flags   = 2
    word_header_code    = 3

    flag_immediate      = $80

    state_interpret     = $00
    state_compile       = $ff

    ; flags for zp_optimize
    optimize_tail       = $80

    !address zp_here        = $02       ; HERE, top of heap
    !address zp_last        = $04       ; last word defined
    !address zp_input       = $06       ; pointer to next character to read
    !address zp_end         = $08       ; last byte to read + 1
    !address zp_header      = $0a       ; temporary pointer to word header
    !address zp_state       = $0c       ; state_interpret or state_compile
    !address zp_optimize    = $0d
                            ; $0e available
                                        ; $12 used by BASIC FP routines
    !address zp_temp        = $13       ; 10 temp bytes reserved
    !address zp_save        = $1e       ; temp pointer for kernal SAVE

    word_buffer_size        = $20       ; size of word buffer excluding count

    !address hwstack        = $0100     ; 256 bytes
    !address stack          = $ce00     ; 256 bytes
    !address sbox           = $cd00     ; 256 bytes
    !address hash_table     = $cb00     ; 512 bytes
    !address free_list      = $ca00     ; 256 bytes
    !address text_buffer    = $c700     ; 768 bytes
    !address ram_drive      = $c600     ; 256 BYTES
    !address pair_start     = $c400
    !address pair_len       = $c300
    first_free_page         = $60       ; comfortably above the heap
    last_free_page          = $c2       ; must be synced with the above!
    stack_init              = $fc       ; initial value of stack pointers


!to "forth.prg", cbm
    * = $0801
    !word basic_end, 2026
    !byte $9e, $20
    !byte '0' + entry % 10000 / 1000
    !byte '0' + entry %  1000 /  100
    !byte '0' + entry %   100 /   10
    !byte '0' + entry %    10
    !byte $00       ; end of line
basic_end:
    !byte $00, $00  ; end of basic


    !set last_core_header = 0
    !macro create_word_header .name, .flags {
.name_start:
        !pet .name
.name_end:
        !word last_core_header
        !set last_core_header = *-2
        !byte .flags + (.name_end-.name_start)
    }

    !macro counted_string .value {
        !byte (.end - .start)
.start:
        !text .value
.end:
    }

    !macro sety .old_value, .new_value {
        !if .new_value = .old_value+1 {
            iny
        } else if .new_value = .old_value-1 {
            dey
        } else {
            ldy #.new_value
        }
    }

    !macro incw .address {
    inc .address
    bne +
    inc .address+1
+
    }

    !macro adcw .address {
    adc .address
    sta .address
    bcc +
    inc .address+1
+
    }

    !macro push_literal .value {
        !if small_code = 1 {
    jsr code_dolit
    !byte .value
        } else {
    lda #.value
    dex
    sta stack, x
        }
    }

    !macro drop {
    inx
    }


compute_sbox:
    !zone {
    lda #1
    sta zp_temp+0
    sta zp_temp+1
.next_value:
    ; temp0 = p, temp1 = q
    lda zp_temp+0
    asl
    eor zp_temp+0
    bcc +
    eor #$1b
+   sta zp_temp+0           ; p = p ^ (p << 1) ^ (p & 0x80 ? 0x1B : 0)

    lda zp_temp+1
    asl
    eor zp_temp+1
    sta zp_temp+1           ; q ^= q << 1
    asl
    asl
    eor zp_temp+1
    sta zp_temp+1           ; q ^= q << 2
    asl
    asl
    asl
    asl
    eor zp_temp+1
    sta zp_temp+1           ; q ^= q << 4
    bpl +
    eor #$09
+   sta zp_temp+1           ; q ^= q & 0x80 ? 0x09 : 0

    sta zp_temp+2           ; temp2 = q (xor sum so far)
    sta zp_temp+3           ; temp3 = q (rotated q)
    ldy #4
-   asl                     ; A = current rotated q
    rol zp_temp+3           ; temp3 = q rotated 1
    lda zp_temp+3
    eor zp_temp+2
    sta zp_temp+2           ; temp2 = sum xor rotated q
    lda zp_temp+3           ; prepare to rotate once more
    dey
    bne -

    lda #$63
    sta sbox                ; done 255 times, but saves bytes
    eor zp_temp+2
    ldy zp_temp+0
    sta sbox, y             ; sbox[p] = xformed ^ $63

    cpy #1
    bne .next_value
    rts
    }


    +create_word_header "(", flag_immediate
code_paren:
-   jsr read_byte
    bcs +
    cmp #')'
    bne -
+   rts


    +create_word_header ".(", flag_immediate
code_dotparen:
-   jsr read_byte
    bcs +
    cmp #')'
    beq +
    jsr $ffd2
    jmp -
+   lda #$0d
    jsr $ffd2
    rts


    +create_word_header "char", 0
code_char:
    jsr read_byte
    dex
    sta stack, x
    rts


    +create_word_header ",", 0
    ; Destroys: A, Y
code_comma:
    lda stack, x
    inx
comma_a:
    ; Destroys: Y
    ldy #0
    sty zp_optimize         ; clear all optimization flags
    sta (zp_here), y
    +incw zp_here
    rts


    +create_word_header "[", flag_immediate
code_lbracket:
    lda #state_interpret
    sta zp_state
    rts


    ; MOVE> ( W:SRC W:TRG W:N -- )
    +create_word_header "move>", 0
code_mover:
    !zone {
    clc
    lda stack+2, x
    adc stack+0, x
    sta zp_temp+0
    lda stack+3, x
    adc stack+1, x
    sta zp_temp+1
    dec zp_temp+1

    clc
    lda stack+4, x
    adc stack+0, x
    sta zp_temp+2
    lda stack+5, x
    adc stack+1, x
    sta zp_temp+3
    dec zp_temp+3

    lda stack+0, x
    beq .skip_bytes

    ldy #$ff
.copy_bytes:
    lda (zp_temp+2), y
    sta (zp_temp+0), y
    dey
    dec stack+0, x
    bne .copy_bytes

    iny

    tya
    clc
    adc zp_temp+2
    sta zp_temp+2
    bcs +
    dec zp_temp+3
+

    tya
    clc
    adc zp_temp+0
    sta zp_temp+0
    bcs +
    dec zp_temp+1
+

.skip_bytes:
    lda stack+1, x
    beq .skip_page

    ldy #$ff
.copy_page:
    lda (zp_temp+2), y
    sta (zp_temp+0), y
    dey
    bne .copy_page

    lda (zp_temp+2), y
    sta (zp_temp+0), y
    dey

    dec zp_temp+1
    dec zp_temp+3

    dec stack+1, x
    bne .copy_page
.skip_page:

    inx
    inx
    inx
    inx
    inx
    inx
    rts
    }

    ; MOVE-SHADOW ( W:SRC W:TRG W:N -- )
    +create_word_header "<move-shadow", 0
code_lmove_shadow:
    sei
    lda #$30
    sta 1
    jsr code_lmove
    lda #$36
    sta 1
    cli
    rts

    ; <MOVE ( W:SRC W:TRG W:N -- )
    +create_word_header "<move", 0
code_lmove:
    !zone {
    lda stack+2, x
    sta zp_temp+0
    lda stack+3, x
    sta zp_temp+1
    lda stack+4, x
    sta zp_temp+2
    lda stack+5, x
    sta zp_temp+3

    ldy #0
    lda stack, x
    beq .skip_bytes

.copy_bytes:
    lda (zp_temp+2), y
    sta (zp_temp+0), y
    iny
    dec stack, x
    bne .copy_bytes

    tya
    clc
    adc zp_temp+0
    sta zp_temp+0
    bcc +
    inc zp_temp+1
+
    tya
    clc
    adc zp_temp+2
    sta zp_temp+2
    bcc +
    inc zp_temp+3
+
.skip_bytes:
    lda stack+1, x
    beq .done
    ldy #0
.copy_page:
    lda (zp_temp+2), y
    sta (zp_temp+0), y
    iny
    bne .copy_page
    inc zp_temp+1
    inc zp_temp+3
    dec stack+1, x
    bne .copy_page

.done:
    inx
    inx
    inx
    inx
    inx
    inx
    rts
    }


    +create_word_header "]", 0
code_rbracket:
    lda #state_compile
    sta zp_state
    rts


    +create_word_header ":", 0
code_colon:
    jsr code_create

    sec                     ; remove JSR DOVAR
    lda zp_here+0
    sbc #3
    sta zp_here+0
    bcs code_rbracket
    dec zp_here+1
+   jmp code_rbracket


    +create_word_header "rts", 0
code_rts:
    !zone {
    ; are we at a position where tail call elimination is possible?
    lda zp_optimize
    and #optimize_tail
    beq .no_tail_call

    and #($ff XOR optimize_tail)
    sta zp_optimize

    ; yes, change JSR to JMP by first moving back...
    sec
    lda zp_here+0
    sbc #3
    sta zp_here+0
    bcs +
    dec zp_here+1
+

    ; ...then changing the opcode...
    ldy #0
    lda #$4c
    sta (zp_here), y

    ; ...and finally moving HERE forward again
    clc
    lda zp_here+0
    adc #3
    sta zp_here+0
    bcc +
    inc zp_here+1
+
    ; then skip compiling RTS
    rts

.no_tail_call:
    lda #$60
    jmp comma_a
    }


    +create_word_header ";", flag_immediate
code_semicolon:
    jsr code_rts
    jmp code_lbracket


    +create_word_header "+", 0
code_plus:
    lda stack, x
    inx
    clc
    adc stack, x
    sta stack, x
    rts


    +create_word_header "'", 0
run_tick:
    jsr read_word
    bcc +
    jmp error_eof
+   jsr lookup
    bcc +
    jmp unknown_word
+   lda zp_header+1
    dex
    sta stack, x
    lda zp_header+0
    dex
    sta stack, x
    rts


    +create_word_header "dovar", 0
code_dovar:
    !zone {
    pla
    dex
    dex
    sta stack, x
    pla
    sta stack+1, x
    inc stack, x
    bne +
    inc stack+1, x
+   rts
    }


    +create_word_header "create", 0
    ; Destroys: A, Y
code_create:
    !zone {
    jsr read_word
    bcc +
    jmp error_eof

+
    jsr hash_word
    sty zp_temp+0           ; temp0 = word hash

    ldy #0                  ; write word name to header
-   lda word_buffer, y
    sta (zp_here), y
    iny
    cpy word_buffer_len
    bne -

    tya                     ; add length of name to HERE
    clc
    +adcw zp_here

    pha
    lda zp_here+1
    pha                     ; push new word header to return stack

    ldy zp_temp+0
    lda hash_table, y
    jsr comma_a

    ldy zp_temp+0
    lda hash_table+$100, y
    jsr comma_a             ; write pointer to previous head of hash slot

;    lda zp_last+0           ; write pointer to old head of vocabulary
;    jsr comma_a
;    lda zp_last+1
;    jsr comma_a

    ldy zp_temp+0

    pla
    sta hash_table+$100, y
    sta zp_last+1
    pla
    sta hash_table, y       ; the new word becomes the new head of hash slot
    sta zp_last+0           ; and also the most recently defined word

;    pla                     ; the new word becomes the new head of vocabulary
;    sta zp_last+1
;    pla
;    sta zp_last+0

    lda word_buffer_len
    jsr comma_a             ; write name length with no flags set

    lda #$20                ; compile JSR DOVAR
    jsr comma_a
    lda #<code_dovar
    jsr comma_a
    lda #>code_dovar
    jmp comma_a
    }


    +create_word_header "lit", 0
code_dolit:
    stx zp_temp+2
    tsx
    inc hwstack+1, x
    bne +
    inc hwstack+2, x
+   lda hwstack+1, x
    sta zp_temp+0
    lda hwstack+2, x
    sta zp_temp+1
    ldy #0
    lda (zp_temp), y
    ldx zp_temp+2
    dex
    sta stack, x
    rts


    +create_word_header "literal", flag_immediate
code_literal:
!if small_code = 1 {
    lda #$20                ; JSR
    dex
    sta stack, x
    jsr code_comma
    lda #<code_dolit
    jsr comma_a
    lda #>code_dolit
    jsr comma_a
    jmp code_comma          ; write byte value, and we are done
} else {
    lda #$a9                ; LDA immediate
    jsr comma_a
    jsr code_comma          ; A = value on stack
    lda #$ca                ; DEX
    jsr comma_a
    lda #$9d                ; STA stack, x
    jsr comma_a
    lda #<stack
    jsr comma_a
    lda #>stack
    jmp comma_a
}


    +create_word_header "type", 0
    ; Destroys: A, Y, temp[0..1]
code_type:
    !zone {
    lda stack+1, x
    sta zp_temp+0
    lda stack+2, x
    sta zp_temp+1           ; temp[0..1] = address
    lda stack, x
    beq .empty
    ldy #0
.print_char:
    lda (zp_temp+0), y
    iny
    jsr $ffd2
    dec stack, x
    bne .print_char
.empty:
    +drop
    +drop
    +drop
    rts
    }


    +create_word_header "word", 0
code_word:
    !zone {
    jsr read_word
    bcs +
    +push_literal >word_buffer_len
    +push_literal <word_buffer_len
    rts
+   lda #0
    dex
    sta stack, x
    dex
    sta stack, x
    rts
    }


code_error_a:
    dex
    sta stack, x
    jmp code_error

    +create_word_header "error", 0
code_error:                 ; to be overwritten
    lda #$30
    clc
    adc stack, x
    jsr $ffd2
    lda #'E'
    jsr $ffd2
    lda #'R'
    jsr $ffd2
    jsr $ffd2
-   jmp -

    +create_word_header "1-w", 0
code_1minusw:
    lda stack+0, x
    bne +
    dec stack+1, x
+   dec stack+0, x
    rts

    
    +create_word_header "sbox-page", 0
    +push_literal >sbox
    rts


    !if include_blocks = 1 {
    +create_word_header "rd-page", 0
    +push_literal >ram_drive
    rts

create_ram_drive:
    !zone {
    ldy #0
    tya
-   sta ram_drive, y
    iny
    bne -
    +push_literal >builtin_blocks
    +push_literal <builtin_blocks
    +push_literal >ram_drive
    +push_literal <ram_drive
    jmp code_rd_import
    }

load_ram_drive:
    !zone {
    ldy #0
.load_block:
    tya
    pha
    lda ram_drive, y
    ;; 0 indicates block is not available in RAM drive
    beq .skip_block
    dex
    dex
    sta stack+1, x
    lda #0
    sta stack+0, x
    jsr code_text_buffer
    +push_literal >(text_buffer+$2ff)
    +push_literal <(text_buffer+$2ff)
    jsr code_uncompress

    dex
    dex
    lda stack+2, x
    sta stack+0, x
    lda stack+3, x
    sta stack+1, x
    lda #<text_buffer
    sta stack+2, x
    lda #>text_buffer
    sta stack+3, x
    jsr code_interpret

    lda #'.'
    jsr $ffd2
.skip_block:
    pla
    tay
    iny
    bne .load_block
.done:
    rts
    }


    +create_word_header "text-buffer", 0
code_text_buffer:
    +push_literal >text_buffer
    +push_literal <text_buffer
    rts

min_compress_symbol = $60


ascii_to_petscii:
    !zone {
    cmp #$41
    bcc .no_change
    cmp #$5b
    bcc .bit7
    cmp #$61
    bcc .no_change
    cmp #$7b
    bcs .no_change
.bit5:
    eor #$20
    rts
.bit7:
    eor #$80
.no_change:
    rts
    }

petscii_to_ascii:
    !zone {
    cmp #$41
    bcc .no_change
    cmp #$5b
    bcc .bit5
    cmp #$c1
    bcc .no_change
    cmp #$db
    bcs .no_change
.bit7:
    eor #$80
    rts
.bit5:
    eor #$20
.no_change:
    rts
    }


best_len:
    !byte 0
best_offset:
    !byte 0
best_start:
    !word 0
bytes_left:
    !byte 0

    ; COMPRESS ( W:SRC W:SRC-END TRG-PAGE -- ERRCODE )
    ; error codes:
    ;  0 - success
    ;  1 - output buffer overflow
    ;  2 - invalid symbol
    ; Speed is about 250 bytes/s for Forth code, or 2 seconds for ~500 bytes
    ; being compressed into ~250 bytes
    +create_word_header "compress", 0
code_compress:
    !zone {
    candidate_offset = zp_temp+6
    candidate_len = zp_temp+7
    candidates_left = zp_temp+8
    compress_ascii_min = $1f
    compress_ascii_max = $7e
    compress_max_output = $fe
    compress_atoms = 1 + (compress_ascii_max - compress_ascii_min)
    compress_max_offset = ($ff - compress_atoms)

    ; temp+0 = input pointer
    lda stack+3, x
    sta zp_temp+0
    lda stack+4, x
    sta zp_temp+1

    ; temp+2 = output buffer (one page)
    lda stack+0, x
    sta zp_temp+3
    lda #0
    sta zp_temp+2

    ; output page will be padded with $ff
    tay
    lda #$ff
.fill_page:
    sta (zp_temp+2), y
    iny
    bne .fill_page

.next_byte:
    lda zp_temp+0
    cmp stack+1, x
    bne .not_done
    lda zp_temp+1
    cmp stack+2, x
    bne .not_done
    jmp .success
.not_done:
    ; the first two bytes must be atoms
    lda zp_temp+2
    cmp #2
    bcs .consider_pair
    jmp .encode_atom

    ; there is more input data, first see if there is a pair we can repeat in order to 
.consider_pair:
    ; compression tables
    ;
    ; pair_len, y -- one page, number of bytes encoded by pair at
    ;                position y and y+1 in compressed block
    ; pair_start, y -- two pages, high/low pointer to input buffer starting
    ;                  at position y in compressed block
    ;
    ; after the length of data that each symbol encodes, the following is done:
    ; pair_len[(offset-1) & $ff] += data length
    ; pair_len[offset] = data_length

    ; compute number of bytes left in the input, with a maximum of $ff
    ; we will not be able to consider pairs generating longer sequences than this
    lda stack+1, x
    sec
    sbc zp_temp+0
    sta bytes_left
    lda stack+2, x
    sbc zp_temp+1
    beq +
    lda #$ff
    sta bytes_left
+

    lda #1
    sta best_len
    lda #$ff
    sta best_offset
    ; find output buffer offset to start searching from
    ; in the loop below, Y will go up to output pointer - 2
    ; it starts at output pointer - 2 - compress_max_offset
    ; with a floor value of 0.
    ; otuput pointer is stored at zp_temp+2
    lda zp_temp+2
    sec
    sbc #compress_max_offset-2
    bcs +
    lda #0
+   sta candidate_offset

    ; compute how many candidate pairs to consider, store in candidates_left
    ; that is: output pointer - first_candidate - 1
    lda zp_temp+2
    clc
    sbc candidate_offset
    sta candidates_left

    ; before optimizations: $8e jiffys for block 0 (test2)
    ; $88 with variables moved to zero page
    ; $82 with candidates_left counter
    ; $77 with pair_len storing sums over pairs
.find_best_offset:
    ; find the best pair offset
    ldy candidate_offset

    ; bytes encoded by pair starting at this candidate symbol (y)
    lda pair_len, y
    ; in order to proceed, this must be greater than the best pair so far
    cmp best_len
    bcc .test_next_offset
    beq .test_next_offset
    ; it must *not* be greater than the number of input bytes left
    cmp bytes_left
    bcs .test_next_offset

    ; number of bytes encoded by candidate pair
    sta candidate_len
    ; zp_temp+4 = candidate location in input buffer to encode
    lda pair_start, y
    sta zp_temp+4
    lda pair_start+$100, y
    sta zp_temp+5

    ; compare candidate symbol's generation with upcoming data in the input buffer
    ldy candidate_len
    dey
.compare_byte:
    lda (zp_temp+0), y
    cmp (zp_temp+4), y
    bne .no_match
    dey
    bpl .compare_byte
.match:
    lda candidate_len
    sta best_len
    lda candidate_offset
    sta best_offset
.no_match:
.test_next_offset:
    inc candidate_offset
    dec candidates_left
    bne .find_best_offset

    lda best_len
    cmp #2
    bcc .encode_atom

    lda zp_temp+2
    sec
    sbc best_offset
    clc
    adc #(compress_atoms-2)

    ; write symbol to output buffer
    ldy #0
    sta (zp_temp+2),y

    ; the current input pointer contains the offset of the symbol just written
    ; to the output buffer, so we store this to the pair_start entry corresponding
    ; to the current output offset
    ldy zp_temp+2
    lda zp_temp+0 ; best_start
    sta pair_start, y
    lda zp_temp+1 ; best_start+1
    sta pair_start+$100, y

    ; advance the input pointer by the number of bytes encoded by the last symbol
    lda best_len
    clc
    adc zp_temp+0
    sta zp_temp+0
    bcc +
    inc zp_temp+1
+

    ; store the length of the symbol just written to the output buffer
    ldy zp_temp+2
    lda best_len
    sta pair_len, y
    dey
    clc
    adc pair_len, y
    sta pair_len, y
    ; iny -- not used, can discard Y

    ; advance output buffer, abort with failure if we go beyond the page limit
    inc zp_temp+2
    lda #(compress_max_output+1)
    cmp zp_temp+2
    beq .failure_overflow
    jmp .next_byte

.encode_atom:
    ldy zp_temp+2
    lda #1
    sta pair_len, y
    dey
    clc
    adc pair_len, y
    sta pair_len, y
    iny

    lda zp_temp+0
    sta pair_start, y
    lda zp_temp+1
    sta pair_start+$100, y

    ldy #0
    lda (zp_temp+0), y
    cmp #$0d
    beq .byte_cr

    jsr petscii_to_ascii

    cmp #compress_ascii_min+1
    bcc .failure_invalid
    cmp #compress_ascii_max+1
    bcs .failure_invalid
    sec
    sbc #compress_ascii_min
.store_symbol:
    ; ldy #0
    sta (zp_temp+2),y

    +incw zp_temp+0
    inc zp_temp+2
    lda #(compress_max_output+1)
    cmp zp_temp+2
    beq .failure_overflow

    jmp .next_byte

.byte_cr:
    lda #0
    beq .store_symbol

.failure_overflow:
    lda #1
    bne .done
.failure_invalid:
    lda #2
    bne .done
.success:
    lda #0
.done:
    inx
    inx
    inx
    inx
    sta stack+0, x
    rts
    }


    ; UNCOMPRESS ( W:SRC W:TRG W:TRG-END -- W:TRG' )
    ; If return value is equal to TRG-END there was an overflow
    ; the final byte of the buffer is then corrupted
    +create_word_header "uncompress", 0
code_uncompress:
    !zone {
    ; address of last byte of target buffer
    lda stack+0, x
    sta zp_temp+2
    lda stack+1, x
    sta zp_temp+3

    ; target address
    lda stack+2, x
    sta .store_address+0
    lda stack+3, x
    sta .store_address+1

    ; zp_temp+0 = src
    lda stack+4, x
    sta zp_temp+0
    lda stack+5, x
    sta zp_temp+1

    ; y = offset in src
    ldy #0

.decode_loop:
    lda (zp_temp+0), y
    cmp #$ff
    beq .decode_finished
    jsr .decode_symbol
    iny
    bne .decode_loop
    ; TODO: should this be an error condition?
.decode_finished:
    inx
    inx
    inx
    inx
    lda .store_address+0
    sta stack+0, x
    lda .store_address+1
    sta stack+1, x
    rts
.decode_symbol:
    ; A = symbol to decode
    ; Y = src offset
    cmp #min_compress_symbol
    bcc .decode_atom
    sec
    sbc #min_compress_symbol-2
    ; A = offset to subtract from src offset
    sta zp_temp+4
    tya
    pha
    sec
    sbc zp_temp+4
    ; A = new src offset (first byte)
    tay
    lda (zp_temp+0), y
    jsr .decode_symbol
    iny
    lda (zp_temp+0), y
    jsr .decode_symbol
    pla
    tay
    rts
.decode_atom:
    clc
    adc #$1f
    cmp #$1f
    bne .decode_atom_petscii
    ; newline, special case
    lda #$0d
.decode_atom_petscii:
    jsr ascii_to_petscii

.store_address = *+1
    sta $ff00
    lda .store_address+0
    cmp zp_temp+2
    bne .no_overflow
    lda .store_address+1
    cmp zp_temp+3
    beq .overflow
.no_overflow:
    +incw .store_address
.overflow:
    rts
    }

    ; FREE-PAGE ( PAGE -- ERR? )
    +create_word_header "free-page", 0
code_free_page:
    !zone {
    lda stack+0, x
    tay
    lda free_list, y
    bne .error
    lda free_list
    sta free_list, y
    sty free_list
    lda #$ff
    sta stack+0, x
    rts
.error:
    lda #0
    sta stack+0, x
    rts
    }


    ;; TODO: unless we really need to be able to create multiple
    ;; RAM drives, consider removing the second argument to make
    ;; this code simpler by using the hardocoded ramdrive address
    ;; RD-IMPORT ( W:BLOCK-DATA W:RAM-DRIVE -- )
    +create_word_header "rd-import", 0
code_rd_import:
    !zone {
    ;; zp_temp+4 points to RAM-DRIVE structure
    lda stack+0, x
    sta zp_temp+4
    lda stack+1, x
    sta zp_temp+5

    ; zp_temp+2 points to serialized block to load next
    lda stack+2, x
    sta zp_temp+2
    lda stack+3, x
    sta zp_temp+3

.create_block:
    ldy #1
    lda (zp_temp+2), y
    cmp #$ff
    beq .eof

    ; allocate a page for copying buffer, store pointer at zp_temp+0
    jsr code_alloc_page
    lda stack, x
    inx
    sta zp_temp+1
    lda #0
    sta zp_temp+0

.copy_block:
    ; first erase buffer page
    lda #$ff
    ldy #0
.erase_byte:
    sta (zp_temp+0), y
    iny
    bne .erase_byte

    ldy #0
    lda (zp_temp+2), y
    tay
    ;; Y = low byte of block number
    lda zp_temp+1
    ;; A = newly allocated page
    ;; insert into right page of RAM drive
    ;; verify high byte = 0
    sta (zp_temp+4), y

    ldy #2
    ;; load size of block in bytes (but Y = 0 means size is $100)
    lda (zp_temp+2), y
    pha

    ;; add 2+1 = 3 to zp_temp+2 so it points to block data
    tya
    sec
    adc zp_temp+2
    sta zp_temp+2
    bcc +
    inc zp_temp+3
+

    pla
    tay
    pha
    ;; Y is now the number of bytes stored of this block
.copy_block_byte:
    dey
    lda (zp_temp+2), y
    sta (zp_temp+0), y
    cpy #0
    bne .copy_block_byte

    pla
    tay
    ;; special case: size 0 means $100
    bne +
    inc zp_temp+3
+
    clc
    adc zp_temp+2
    sta zp_temp+2
    bcc +
    inc zp_temp+3
+
    jmp .create_block

.eof:
    inx
    inx
    inx
    inx
    rts
    }
    }

init_memory:
    !zone {
    lda #2
    ldy #0
    ; by default, make everything unavailable
.fill_unavailable:
    sta free_list, y
    iny
    bne .fill_unavailable

    lda #first_free_page
    ; zp_temp+0 = first free page
    sta zp_temp+0

    ldy #last_free_page
    sty free_list
.fill_free:
    dey
    tya
    sta free_list+1, y
    cpy zp_temp+0
    bne .fill_free

    lda #1
    sta free_list, y
    rts
    }

    ; ALLOC-PAGE ( -- PAGE )
    ; page 0 indicates error
    +create_word_header "alloc-page", 0
code_alloc_page:
    !zone {
    ldy free_list
    ; value of 1 indicates end of free list, so empty
    cpy #1
    beq .out_of_memory
    lda free_list, y
    sta free_list
    ; mark page as allocated
    lda #0
    sta free_list, y
    tya
.done:
    dex
    sta stack+0, x
    rts
.out_of_memory:
    lda #0
    beq .done
    }

    ; C>HEX ( C -- N )
    ; N is $ff on error, otherwise a value 0--15
    +create_word_header "c>hex", 0
    lda stack, x
    jsr hexdigit
    sta stack, x
    rts

    ; INTERPRET ( W:FROM W:TO -- )
    +create_word_header "interpret", 0
code_interpret:
    lda zp_input
    pha
    lda zp_input+1
    pha
    lda zp_end
    pha
    lda zp_end+1
    pha

    lda stack, x
    sta zp_end
    inx
    lda stack, x
    sta zp_end+1
    inx
    lda stack, x
    sta zp_input
    inx
    lda stack, x
    sta zp_input+1
    inx

    jsr interpret

    pla
    sta zp_end+1
    pla
    sta zp_end
    pla
    sta zp_input+1
    pla
    sta zp_input
    rts


entry:
    !zone {

    lda #0
    tay
.clear_tables:
    sta stack, y
    sta hash_table, y
    sta hash_table+$100, y
    iny
    bne .clear_tables

    lda #$36
    sta 1                   ; disable BASIC ROM, keep KERNAL ROM

    jsr compute_sbox        ; initialize AES S-box (used for hashing)

    jsr init_memory         ; initialize memory page allocator

    ; both stacks have 4 reserved bytes at the top/bottom to decrease the risk
    ; of over/underflow going undetected
    ldx #stack_init         ; set up data stack
    txs                     ; set up return stack

    lda #<heap_start        ; initialize HERE
    sta zp_here+0
    lda #>heap_start
    sta zp_here+1

    lda #state_interpret
    sta zp_state

    ; initialize head of dictionary pointer
    lda #<last_core_header
    sta zp_last+0
    sta zp_header+0
    lda #>last_core_header
    sta zp_last+1
    sta zp_header+1

    ; insert core headers into hash table
    ; zp_header points to current header
.insert_next_word:
    ldy #1
    lda (zp_header), y
    pha
    dey
    lda (zp_header), y
    pha                     ; push next header in core list

    jsr hash_header         ; Y = hash of current header
    sty zp_temp+0
    lda hash_table, y
    pha
    lda hash_table+$100, y
    ldy #1
    sta (zp_header), y
    dey
    pla
    sta (zp_header), y      ; write old hash slot head to current header

    ldy zp_temp+0           ; Y = hash

    lda zp_header+0
    sta hash_table, y
    lda zp_header+1
    sta hash_table+$100, y  ; hash slot head point to current header

    pla
    sta zp_header+0
    pla
    sta zp_header+1         ; advance to next header in core word list
    ora zp_header+0
    bne .insert_next_word   ; until we reach a null pointer
    }

    +push_literal >boot_text
    +push_literal <boot_text
    +push_literal <(boot_text_end-boot_text)
    jsr code_type

!if include_blocks = 1 {
    ldx #stack_init
    jsr create_ram_drive
    jsr load_ram_drive
    lda #2
    jmp code_error_a
}

interpret:
    !zone {
    jsr read_word
    bcc +
    rts
+
    jsr lookup
    bcc .word_found         ; word in dictionary?

    jsr hexbyte             ; no, see if it is a number
    beq unknown_word        ; if not, this is an error

    dex                     ; if yes, push number to stack
    sta stack, x

    lda zp_state            ; interpreting? (then this is zero)
    beq interpret           ; yes, then we are done

    jsr code_literal        ; no, compile literal
    jmp interpret           ; and then we are done

;; Previously, we tested for numerals *first* for speed reasons.
;; That is generally risky, and also does not allow for optimizing
;; Using : 0 00 ; and : 1 01 ;

;;     jsr hexbyte             ; is the word a valid number?
;;     beq .non_numeral

;;     dex                     ; yes, push number to stack
;;     sta stack, x

;;     lda zp_state            ; interpreting? (then this is zero)
;;     beq interpret           ; yes, then we are done

;;     jsr code_literal        ; no, compile literal
;;     jmp interpret           ; and then we are done

;; .non_numeral:
;;     jsr lookup
;;     bcs unknown_word

.word_found:
    clc                     ; compute code address
    lda #word_header_code
    adc zp_header+0
    sta .call+1
    lda #0
    adc zp_header+1
    sta .call+2

    ldy #word_header_flags
    lda (zp_header), y
    bmi .execute_word       ; immediate flag set? then always execute

    lda zp_state            ; interpreting? (then this is zero)
    bne .compile_word

.execute_word:
.call:
    jsr $ff00
    jmp interpret

.compile_word:              ; we are in compilation mode, word not immediate
    lda #$20                ; JSR
    jsr comma_a
    lda .call+1
    jsr comma_a
    lda .call+2
    jsr comma_a
    lda zp_optimize         ; eligible for tail call elimination
    ora #optimize_tail
    sta zp_optimize
    jmp interpret

unknown_word:
    +push_literal >word_buffer
    +push_literal <word_buffer
    lda word_buffer_len
    dex
    sta stack, x
    jsr code_type
    lda #'?'
    jsr $ffd2
    lda #$0d
    jsr $ffd2
    rts
    }

error_eof:
    lda #1
    jmp code_error_a


    ; Read one byte from the input buffer
    ; outputs: A = byte, C = 0 if OK, C = 1 if EOF
    ;          on EOF, A = 0
    ; destroys: Y
read_byte:
    !zone {
    lda zp_input+0
    cmp zp_end+0
    bne .bytes_left
    lda zp_input+1
    cmp zp_end+1
    beq .end_of_buffer
.bytes_left:
    ldy #0
    lda (zp_input), y
    +incw zp_input
    clc
    rts
.end_of_buffer:
    lda #0
    sec
    rts
    }


    ; Read until a non-blank character is found
    ; Returns: A = first non-blank character, C = 0 if OK, 1 if EOF
    ; Destroys: Y
skip_blanks:
    !zone {
    jsr read_byte
    bcc +
    rts
+   cmp #$21
    bcc skip_blanks
    clc
    rts
    }


    ; Compute hash sum of word header pointed to by zp_header
    ; This is currently used only during initialization.
    ; Returns: Y = hash
    ; Destroys: A
hash_header:
    !zone {
    ldy #word_header_flags
    lda (zp_header), y
    and #$1f
    sta zp_temp+2

    lda zp_header+0
    clc
    sbc zp_temp+2
    sta zp_temp+0
    lda zp_header+1
    sbc #0
    sta zp_temp+1           ; zp_temp[0..1] = pointer to start of name - 1

    txa
    pha

    ldy zp_temp+2           ; y = length
    lda sbox, y
    tax
.add_byte:
    lda (zp_temp+0), y
    eor sbox, x
    tax
    dey
    bne .add_byte

    txa
    tay

    pla
    tax
    rts
    }


    ; Compute hash sum of word_buffer
    ; Returns: Y = hash
    ; Destroys: A
hash_word:
    !zone {
    txa
    pha

    ldx word_buffer_len
    lda sbox, x
    tay
.add_byte:
    lda word_buffer_len, x
    eor sbox, y
    tay
    dex
    bne .add_byte

    pla
    tax
    rts
    }


    ; Read a word to word_buffer (counted string)
    ; Returns: C = 0 if OK, 1 if EOF or overflow
    ;          C = 1 and word_buffer_len = #word_buffer_size signals overflow
    ; Destroys: A, Y
read_word:
    !zone {
    lda #0
    sta word_buffer_len     ; current length

    jsr skip_blanks
    bcs .eof_empty

    sta word_buffer         ; got first byte, store it
.read_next:
    inc word_buffer_len
    jsr read_byte
    bcs .eof
    cmp #$21
    bcc .done
    ldy word_buffer_len
    cpy #word_buffer_size
    beq .overflow
    sta word_buffer, y
    jmp .read_next
.eof:                       ; EOF is OK if we have read something
.done:
    clc                     ; indicate success
    rts
.eof_empty:
.overflow:
    sec                     ; indicate failure
    rts
    }


    ; Convert hex string in word_buffer to byte
    ;
    ; Returns: A = value, Y = $ff on success, 0 on failure
    ;          Z = 0 on success, 1 on failure
    ; Destroys: temp0
hexbyte:
    !zone {
    ldy word_buffer_len
    dey
    bmi .invalid
    lda word_buffer
    jsr hexdigit
    bmi .invalid
    dey
    bmi .done
    asl
    asl
    asl
    asl
    sta zp_temp+0
    lda word_buffer+1
    jsr hexdigit
    bmi .invalid
    ora zp_temp+0
    dey
    bpl .invalid
.done:
    ldy #$ff
    rts
.invalid:
    ldy #0
    rts
    }


    ; input: A = PETSCII hex digit
    ; output: A = 0-15 or $ff on error
hexdigit:
    !zone {
    cmp #'0'
    bcc .fail
    cmp #'9'+1
    bcs .maybe_lower
    sbc #'0'-1
    rts
.maybe_lower:
    cmp #$41
    bcc .fail
    cmp #$46+1
    bcs .maybe_upper
    sbc #$41-10-1
    rts
.maybe_upper:
    cmp #$c1
    bcc .fail
    cmp #$c6+1
    bcs .fail
    sbc #$c1-10-1
    rts
.fail:
    lda #$ff
    rts
    }


    ; Find the header with the name in word_buffer
    ; Returns: zp_header contains first matching header or 0 on failure
    ;          C=0 on success, C=1 on failure
    ; Destroys: A, Y, temp[0..1]
lookup:
    !zone {

    jsr hash_word
    lda hash_table, y
    sta zp_header+0
    lda hash_table+$100, y
    sta zp_header+1
    
;    lda zp_last+1
;    sta zp_header+1
;    lda zp_last+0
;    sta zp_header+0

.check_word:
    ora zp_header+1
    beq .failed

    ldy #word_header_flags
    lda (zp_header), y
    and #$1f
    cmp word_buffer_len
    bne .not_equal          ; lengths don't match

    tay                     ; Y = length of word name (and str)

    lda zp_header+0
    clc
    sbc word_buffer_len     ; subtract length + 1 from header
    sta zp_temp+0
    lda zp_header+1
    sbc #0
    sta zp_temp+1           ; temp[0..1] = one before start of word name

.check_char:
    lda (zp_temp+0), y
    cmp word_buffer-1, y
    bne .not_equal
    dey
    bne .check_char

    clc
    rts                     ; return with C=0 on success

.failed:
    sec
    rts                     ; return with C=1 on failure

.not_equal:
    ldy #word_header_last
    lda (zp_header), y
    pha
    iny
    lda (zp_header), y
    sta zp_header+1
    pla
    sta zp_header+0
    jmp .check_word
    }

boot_text:
    !byte $0e, $05, $93
    !pet "Loading"
boot_text_end:

word_buffer_len:
!address word_buffer = *+1
!address heap_start = *+word_buffer_size+1

builtin_blocks:
!if include_blocks = 1 {
    ; asumed to start with 2 bytes indicating total size
    !bin "blocks.compressed"
}

end_of_prg:
