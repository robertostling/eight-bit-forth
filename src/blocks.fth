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
19 66 varw bs-port

: swapd ( w:a -- )
  dup>rw  @w  r@w 1+w 1+w @w
  swap r@w !w  swap r>w 1+w 1+w !w ;

# : add-shadow-ram ( -- )
#   ff d0 do  i free-page drop  1 +loop ;
# add-shadow-ram

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

1 var max-block-devices
create block-devices
  rd-struct ,2

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

# entry in cache-table (10 bytes):
# state (1 byte)
# RAM page (1 byte)
# block# (8 bytes)

0 var cache-state-free
1 var cache-state-used
2 var cache-state-dirty
8 var cache-entries  # max $19
0 var cache-used
: a*  dup 2* 2* + 2* ;
create cache-table
  cache-entries a* allot

: cache-lookup ( -- [w:header] err? )
  cache-entries for
    0 i 1- a* cache-table +w
    dupw @ 0<> if
      dupw 1+w 1+w  blk  8 mem= if
        r>drop false exit
      then
    then
    dropw next
    true ;
: cache-write ( w:header -- err? )
  dupw @ cache-state-dirty <> if
    dropw  false exit
  then
  dupw 1+w  dupw 1+w  blk  0 8 <move
  @ 0  block-write  if
    dropw true exit
  then
  cache-state-used !  false ;
: cache-free ( w:header -- err? )
  dupw cache-write
  if  dropw true exit  then
  dupw 1+w @ free-page
  cache-state-free -rot !
  false ;
: cache-new-entry ( -- [w:header] err? )
  cache-used cache-entries = if
    cache-table cache-free ?dup skip
    cache-used 1- to cache-used
    cache-table dupw 0 a +w swapw
    0 cache-used a* <move
  then
  cache-table  0 cache-used a* +w
  cache-used 1+ to cache-used
  false ;
: cache-allocate ( w:header -- w:a )
  dupw cache-state-used -rot !
  1+w dupw alloc-page dup >r -rot !
  1+w  blk swapw  0 8 <move
  r> 0 ;
: empty-buffers ( -- )
  cache-table  0 cache-entries a*
  0 fill
  0 to cache-used ;
: save-buffers ( -- )
  cache-used for
    0 i 1- a*  cache-table +w  cache-write
    if
      ." failed to write block "
      0 i 1- a* 1+ 1+  cache-table +w
      8 dump cr
    then
  next ;
: block ( -- [w:a] err? )
  cache-lookup if
    cache-new-entry ?dup skip
    cache-allocate
    dupw block-read ?dup if
      dropw true exit
    then
  else
    1+w @ 0
  then
  false ;
: update ( -- )
  cache-lookup skip
  cache-state-dirty -rot ! ;
: flush ( -- )
  save-buffers empty-buffers ;

: uncompress-text ( w:a -- )
  text-buffer dupw 2 ff +w uncompress
  text-buffer -w  tow text-size ;
: compress-text ( page -- err? )
  >r  text-buffer dupw text-size +w
  r> compress ;

: save ( -- )
  scr-valid not if
    ." no screen open" cr exit
  then
  text-modified so
  scr blk !block
  block if
    ." error loading block" cr exit
  then
  update
  drop compress-text ?dup if
    ." compress error: " . cr exit
  then ;
: save! ( -- )
  save  save-buffers ;
: edit ( -- )
  false to text-modified
  scr blk !block
  block if
    ." read error" exit
  then
  uncompress-text
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
    edit run
    1 +scr
  next ;
: hex# ( w:a n -- )
  for
    char c>hex 4 <<  char c>hex  +  !+
  next dropw ;
: block# ( w:a -- ) 8 hex# ;
: bs-key# ( -- ) bs-key 10 hex# ;
: go ( -- ) edit p ;
: go# ( -- ) save  scr block#  go ;
: go> ( -- ) save  1 +scr  go ;
: <go ( -- ) save  1 -scr  go ;
: go* ( n -- ) save  scr7 !  go ;
