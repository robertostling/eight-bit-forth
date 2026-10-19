: interpret-line
  countw overw +w interpret ;
: repl begin
    ."  ok" cr
    kernal-read-line
    18 0 cursor! cr
    ( ." <" kernal-line-buf count type ." >" cr )
    kernal-line-buf interpret-line
  again ;

# serial-init
# cr .( initialized rs-232) cr
# sunet-ip ping
# sunet-ip ping
alloc-page var buf-page
s" test test test" varw test-string
: compress-test
  test-string count 0 swap text-buffer swapw <move
  test-string count 0 swap tow text-size dropw
  ." before compression:" cr
  p cr
  ." hex: " text-buffer text-size dumpw cr
  ." checksum: "
  text-buffer text-size .checksum cr 
  ." compressing to: " buf-page 0 .w cr
  text-buffer dupw text-size +w
  buf-page compress
  ." compressed: " buf-page 0 20 dump cr
  buf-page 0 uncompress-text
  ." decompressed:" cr
  p cr
  ." hex: " text-buffer text-size dumpw cr
  ." checksum: "
  text-buffer text-size .checksum cr ;

compress-test
repl
