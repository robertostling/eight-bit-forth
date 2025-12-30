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

    include_kernel = 1      ; include kernel.fth in binary?
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
    !address kernel_source  = $7000
    !address stack          = $ce00     ; 256 bytes
    !address sbox           = $cd00     ; 256 bytes
    !address hash_table     = $cb00     ; 512 bytes
    stack_init              = $fc       ; initial value of stack pointers

!to "forth.prg", cbm
    * = $0801
    !word basic_end, 2025
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
        !text .name
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


    +create_word_header "CHAR", 0
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
    +create_word_header "MOVE>", 0
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


    ; <MOVE ( W:SRC W:TRG W:N -- )
    +create_word_header "<MOVE", 0
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


    +create_word_header "RTS", 0
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


    +create_word_header "DOVAR", 0
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


    +create_word_header "CREATE", 0
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


    +create_word_header "LIT", 0
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


    +create_word_header "LITERAL", flag_immediate
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


    +create_word_header "TYPE", 0
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


;    ; LOAD-FILE ( DRIVE W:NAME NAME-LEN W:START -- )
;    +create_word_header "LOAD-FILE", 0
;code_load:
;    !zone {
;    lda stack, x            ; temp+0 points to start address
;    sta zp_temp+0
;    inx
;    lda stack, x
;    sta zp_temp+1
;    inx
;
;    lda stack, x            ; temp+4 is name length
;    sta zp_temp+4
;    inx
;
;    lda stack, x            ; temp+2 points to name (counted string)
;    sta zp_temp+2
;    inx
;
;    txa                     ; will return 2 items
;    pha
;
;    lda stack, x
;    sta zp_temp+3
;    inx
;
;    lda stack, x            ; A = device
;    tax
;    lda #1
;    ldy #0
;    jsr $ffb1               ; SETLFS(logical=1, device, secondary=0)
;
;    lda zp_temp+4
;    ldx zp_temp+2
;    ldy zp_temp+3
;    jsr $ffbd               ; SETNAM(W:NAME, NAME-LEN)
;
;    lda #0
;    ldx zp_temp+0
;    ldy zp_temp+1
;    sty zp_firstblock
;    jsr $ffd5               ; LOAD(load, W:START)
;
;    sta zp_temp+2
;
;    bcs .error
;
;    inx
;    bne +
;    iny
;+
;    stx zp_temp+0
;    sty zp_temp+1
;
;    sty zp_lastblock
;
;    pla
;    tax
;
;    lda zp_temp+1
;    sta stack+1, x
;    lda zp_temp+0
;    sta stack, x
;
;    lda #0
;    dex
;    sta stack, x
;
;    rts
;.error:
;    pla
;    tax
;    lda zp_temp+2
;    sta stack+1, x
;    lda #$ff
;    sta stack, x
;    rts
;    }


    +create_word_header "WORD", 0
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

    +create_word_header "ERROR", 0
code_error:                 ; to be overwritten
    jmp -


    +create_word_header "KERNEL-SOURCE", 0
code_kernel_source:
    jsr code_dovar
kernel_source_struct:
    !word kernel_source     ; start
    !word 0                 ; end
    !word kernel_source     ; current position


    +create_word_header "1-W", 0
code_1minusw:
    lda stack+0, x
    bne +
    dec stack+1, x
+   dec stack+0, x
    rts


    ; UNCOMPRESS ( W:SRC W:TRG W:N -- W:TRG' )
    +create_word_header "UNCOMPRESS", 0
code_uncompress:
    !zone {
    ; zp_temp+0 = src
    lda stack+2, x
    sta zp_temp+0
    lda stack+3, x
    sta zp_temp+1

    ; zp_temp+2 = trg
    lda stack+4, x
    sta zp_temp+2
    lda stack+5, x
    sta zp_temp+3

.decode_symbol:
    lda stack+1, x
    bmi .no_data            ; safety check
    ora stack+0, x
    bne .has_data

.no_data:
    inx
    inx
    inx
    inx
    lda zp_temp+0
    sta stack+0, x
    lda zp_temp+1
    sta stack+1, x
    rts

.has_data:
    jsr code_1minusw

    ldy #0
    lda (zp_temp+2), y
    +incw zp_temp+2
    cmp #$53
    bcs .sequence
    ; decode single symbol
    ; C = 0, add minimum PETSCII symbol
    adc #$0d
    sta (zp_temp+0), y
    +incw zp_temp+0
    jmp .decode_symbol

.sequence:
    ; decode sequence
    ; A - $53 contains: OOOOOLLL
    ; where LLL + 3 is the length of the sequence (range 3 to 10)
    ; OOOOO are the 5 most significant bits of the offset
    ; the following byte -- (zp_temp+2),y -- contains low 8 bits
    ; of offset, note that zp_temp+2 has already been increased so Y = 0
    sbc #$53
    pha
    and #7
    clc
    adc #3
    sta zp_temp+6           ; zp_temp+6 = sequence length
    pla
    lsr
    lsr
    lsr
    sta zp_temp+5
    sec
    lda zp_temp+0
    sbc (zp_temp+2), y      ; subtract offset from target pointer
    sta zp_temp+4           ; zp_temp+4 = sequence address
    lda zp_temp+1
    sbc zp_temp+5
    sta zp_temp+5
    ; now we need to copy zp_temp+6 bytes from zp_temp+4 to zp_temp+0
    ; and then increase zp_temp+0
    ldy #0
.copy_byte:
    lda (zp_temp+4), y
    sta (zp_temp+0), y
    iny
    cpy zp_temp+6
    bne .copy_byte

    clc
    tya
    adc zp_temp+0
    sta zp_temp+0
    bcc +
    inc zp_temp+1
+
    +incw zp_temp+2
    jsr code_1minusw
    jmp .decode_symbol
    }


    ; INTERPRET ( W:FROM W:TO -- )
    +create_word_header "INTERPRET", 0
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

    jsr compute_sbox

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

!if include_kernel = 1 {
    +push_literal >boot_text
    +push_literal <boot_text
    +push_literal <(boot_text_end-boot_text)
    jsr code_type

    +push_literal >compressed_code
    +push_literal <compressed_code
    +push_literal >kernel_source
    +push_literal <kernel_source
    +push_literal >(compressed_code_end-compressed_code)
    +push_literal <(compressed_code_end-compressed_code)
    jsr code_uncompress
    lda stack+0, x
    sta kernel_source_struct+2
    sta zp_end+0
    lda stack+1, x
    sta kernel_source_struct+3
    sta zp_end+1
    inx
    inx

    lda #<kernel_source
    sta zp_input+0
    lda #>kernel_source
    sta zp_input+1

    jsr interpret
    lda #2
    jmp code_error_a

boot_text:
    !text "UNCOMPRESSING\r"
boot_text_end:

} else {
    lda #1
    ldx #8
    ldy #0
    jsr $ffba

    lda #kernel_name_end-kernel_name
    ldx #<kernel_name
    ldy #>kernel_name
    jsr $ffbd

    lda #0
    ldx #<kernel_source
    ldy #>kernel_source
    stx zp_input+0
    sty zp_input+1
    jsr $ffd5

    inx
    bne +
    iny
+

    stx zp_end+0
    sty zp_end+1

    stx kernel_source_struct+2
    sty kernel_source_struct+3

    ldx #stack_init

    jsr interpret
    lda #2
    jmp code_error_a

kernel_name:
    !text "KERNEL"
kernel_name_end:
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
    bcs .maybe_alpha
    sbc #'0'-1
    rts
.maybe_alpha:
    cmp #'A'
    bcc .fail
    cmp #'F'+1
    bcs .fail
    sbc #'A'-10-1
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

word_buffer_len:
!address word_buffer = *+1
!address heap_start = *+word_buffer_size+1

!if include_kernel = 1 {
; NOTE: must compress before using either heap or word_buffer
compressed_code:
    !bin "kernel-cr.compressed"
compressed_code_end:
}


