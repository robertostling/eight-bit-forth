# block addresses are big-endian
# byte 0: 0 = byte 1 indicates device
#   otherwise ipv4 address bytes 0-3
# byte 1: 0 = ram drive
: block-variable  create 8 allot ;
: !block ( w:src w:trg -- ) 0 8 <move ;

block-variable scr
block-variable blk
blk 0 4 +w varw blk4
blk 0 7 +w varw blk7
scr 0 6 +w varw scr6
scr 0 7 +w varw scr7

false var scr-valid
create bs-key 10 allot

: swapd ( w:a -- )
  dup>rw  @w  r@w 1+w 1+w @w
  swap r@w !w  swap r>w 1+w 1+w !w ;

# todo: elsewhere?
: add-shadow-ram ( -- )
  ff d0 do  i free-page drop  1 +loop ;
add-shadow-ram

: rd-block-page ( -- w:a )
  blk7 @  rd-page swap ;
: rd-read ( w:a -- err? )
  rd-block-page @ 0  swapw
  1 0 <move-shadow
  false ;
: rd-write ( w:a -- err? )
  rd-block-page @
  0= if
    # todo: handle oom
    alloc-page rd-block-page !
  then
  rd-block-page @ 0  1 0 <move-shadow
  false ;

19 66 varw bs-port

: bs-load-req ( -- )
  blk4  0 4  0 1  udp-copy-payload
  packet-write-buf 0 1d +w swapd
  a0 packet-write-buf 0 1c +w !
  my-ip  blk  ab cd  bs-port  0 5
  udp-create-header
  slip-write-packet ;
: bs-load-rx ( w:buf -- err? )
  dupw  ff  1 00  fillw
  slip-rx
  slip-rx-state slip-state-done <> if
    true exit
  then
  # todo: verify load response
  packet-read-buf 0 21 +w
  swapw
  packet-read-size 0 21 -w
  <move
  false ;

: bs-pack-size ( -- w:size )
  0 0   1 0 forw
    packet-write-buf 0 31 +w
    1 0 iw -w  +w @
    ff <> if
      dropw 1 1 iw -w
    then
  nextw ;
: bs-store-req ( w:a -- )
  1 0  0 15  udp-copy-payload
  blk4  0 4  0 11 udp-copy-payload
  packet-write-buf 0 2d +w swapd
  bs-key  0 10  0 1  udp-copy-payload
  a2  packet-write-buf 0 1c +w  !
  my-ip  blk  ab cd  bs-port
  bs-pack-size  0 15  +w
  udp-create-header
  slip-write-packet ;
: bs-store-rx ( -- err? )
  slip-rx
  slip-rx-state slip-state-done <> if
    true exit
  then
  packet-read-buf 0 1c +w @ a3 <> if
    true exit
  then
  # todo: verify block# and ip
  false ;

: bs-read ( w:a -- err? )
  bs-load-req  bs-load-rx ;
: bs-write ( w:a -- err? )
  bs-store-req bs-store-rx ;

create rd-struct
  ' rd-read ,2
  ' rd-write ,2

create bs-struct
  ' bs-read ,2
  ' bs-write ,2

create block-devices
  rd-struct ,2
  # todo: more devices

1 var max-block-devices
0 0 varw fn-read
0 2 varw fn-write

: blk>dev ( -- w:dev )
  blk @ 0<> if  bs-struct exit  then
  blk 1+w @  max-block-devices >= if
    ." invalid blk: " blk 8 dump  cr
    0 0  exit
  then
  0  blk 1+w @  2*
  block-devices +w @w ;
: block-fn ( w:a w:idx -- err? )
  blk>dev  0=w if
    dropw dropw true exit
  then
  blk>dev +w @w execute ;
: block-read ( w:a -- err? )
  fn-read block-fn ;
: block-write ( w:a -- err? )
  fn-write block-fn ;

4 var checksum-size
create checksum checksum-size allot
# TODO: not deterministic! debug...
: sbox-mix ( c -- c' )  sbox-page swap @ ;
: checksum-mix ( c -- )
  2 for
    checksum-size for
      sbox-mix
      0 i 1- checksum +w +!
      0 i 1- checksum +w @
    next
  next drop ;
: compute-checksum ( w:a w:n -- )
  sbox-page 0  checksum  0 checksum-size <move
  forw  @+ checksum-mix  nextw dropw ;
: mix-word ( w:x -- w:x' )
  4 for  sbox-mix over xor swap  next ;
: checksum-word ( w:a w:n -- w:x )
  13 37 swapw forw  # a x
    swapw  @+ 0 swap  # x a+1 t
    swapw >rw +w  r>w swapw
    mix-word
  nextw swapw dropw ;
: .checksum ( w:a w:n -- )
  checksum-word .w ;
  # compute-checksum
  # checksum checksum-size dump ;

: uncompress-text ( w:a -- )
  text-buffer dupw 2 ff +w uncompress
  text-buffer -w  tow text-size ;
: compress-text ( page -- err? )
  >r  text-buffer dupw text-size +w
  r> compress ;
: compress-text-verbose ( page -- err? )
  ." Checksum before compression: "
  text-buffer text-size .checksum cr
  dup >r compress-text
  r> 0 uncompress-text
  ." Checksum after decompression: "
  text-buffer text-size .checksum cr ;
  

: save ( -- )
  scr-valid not if
    ." no screen open" cr exit
  then
  text-modified so
  alloc-page dup
  compress-text-verbose if
    free-page
    ." compress error" cr exit
  then
  dup 0
  scr blk !block
  block-write if
    free-page
    ." write error" cr exit
  then
  free-page ;
: cache ( -- )
  false to text-modified
  alloc-page
  dup 0
  scr blk !block
  block-read if
    free-page ." read error" exit
  then
  dup 0 uncompress-text
  free-page
  true to scr-valid ;
: +scr ( n -- )
  0 swap  scr6 @w
  swap +w swap  scr6 !w ;
: -scr ( n -- )
  0 swap  scr6 @w
  swap swapw -w swap  scr6 !w ;
: discard ( -- )
  false to text-modified
  false to scr-valid
  0 0 tow text-size ;
: new ( -- )
  0 0 tow text-size
  true to scr-valid
  false to text-modified ;
: run ( -- )
  text-buffer  dupw text-size +w
  interpret ;
: loads ( n -- )
  for
    discard cache run
    1 +scr
  next ;
: hex ( w:a n -- )
  for
    char c>hex 4 <<  char c>hex  +  !+
  next dropw ;
: block# ( w:a -- ) 8 hex ;
: go ( -- ) cache p ;
: go# ( -- ) discard  scr block#  go ;
: go> ( n -- ) discard  +scr  go ;
: <go ( n -- ) discard  -scr  go ;
: go* ( n -- ) discard  scr7 !  go ;
