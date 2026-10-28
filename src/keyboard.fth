# code
( Keymap management )
variablew keymap
: choose-keymap [
  2 8d lda adr
  1 anda imm
  2 beq
  41 lda imm
  clc
  keymap adc adr
  f5 sta zp
  0 lda imm
  keymap 1+w adc adr
  f6 sta zp
  ea e0 jmp
: init-keymap
  ['] choose-keymap >code 2 8f !w ;
