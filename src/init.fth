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
