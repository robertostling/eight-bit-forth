: interpret-line
  countw overw +w interpret ;
: repl begin
    ."  ok" cr
    kernal-read-line
    18 0 cursor! cr
    kernal-line-buf interpret-line
  again ;

status

# before reducing macros: 3e66 bytes, time 211
# with if -> ?0branch: 3e26 bytes, time 219

serial-init
cr .( initialized rs-232) cr
sunet-ip ping
sunet-ip ping

repl
