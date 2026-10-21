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
s" xxxxxxxxxxxyyyyyyyyxyxyxyxy" varw test-string
: test-text
  test-string count 0 swap text-buffer swapw <move
  test-string count 0 swap tow text-size dropw ;

# TODO: worked before go# 000000000000000  but not after
: compress-test
  test-text
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

: test2
  ." checksum: "
  text-buffer text-size .checksum cr 
  ." compressing to: " buf-page 0 .w cr
  text-buffer dupw text-size +w
  buf-page compress
  if ." compression error" exit then
  buf-page 0 uncompress-text
  ." checksum: "
  text-buffer text-size .checksum cr ;

# XXX for testing
scr block# 0000000000000000
repl
