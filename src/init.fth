: interpret-line
  countw overw +w interpret ;
: repl begin
    ."  ok" cr
    kernal-read-line
    18 0 cursor! cr
    kernal-line-buf interpret-line
  again ;

serial-init
cr .( initialized rs-232) cr
sunet-ip ping
sunet-ip ping
repl
