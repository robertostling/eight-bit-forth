# code
( REPL and utils )
: interpret-line
  countw overw +w interpret ;
: repl begin
    check-stack
    ."  ok" cr
    kernal-read-line
    18 0 cursor! cr
    kernal-line-buf interpret-line
  again ;
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
# code
( Startup code )

vps-block0 blk !block
10 blk7 !
: alloc-load-block ( -- [w:a] err? )
  block ?dup skip
  alloc-page 0  dup>rw  1 0  <move
  r>w false ;
: main
  alloc-load-block
  if ." fail" exit then
  keymap !w
  init-keymap ;

cr status

serial-init
cr .( initialized rs-232) cr
sunet-ip ping
sunet-ip ping

repl
