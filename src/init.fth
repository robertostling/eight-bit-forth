# code
cr status
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
# cr status

# .( init checksums:)
# debug-info key drop

serial-init
cr .( initialized rs-232) cr
sunet-ip ping
sunet-ip ping

repl
