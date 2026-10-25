# code
( Block variables )
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
# doc
( Block variables

Block addresses are 8-byte big-endian.
First 4 bytes give the IPv4 address of
block storage host, bs, except if the
first byte is 0, then the second byte
is the storage device number according
to the block-devices table below.
Last 4 bytes are the block offset.

Block addresses variables
blk: temporary parameter (see next)
scr: block in text-buffer
)
# doc
( Block device inteface

Block numbers are passed in the blk
variable.
*-read and *-write thus take only an
address to the data buffer, assumed to
be page aligned. The block to read
or write is in blk. The both return
bool error flags, true on error.

block-read and block-write below
use the same interface and route the
request to the appropriate device
based on the block number.
)
# code
( RAM drive )
: rd-block-page ( -- w:a )
  blk7 @  rd-page swap ;
: rd-read ( w:a -- err? )
  rd-block-page @
  ?dup 0= if
    1 0  ff  fill
    false exit
  then
  0  swapw
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
# doc
( RAM drive

The simple RAM drive structure used
here is the same one used by the boot
loader to compile the system. This
allows viewing and editing the system
sources.
)
# code
( Network block load request )
: bs-load-req ( -- )
  blk4  0 4  0 1  udp-copy-payload
  packet-write-buf 0 1d +w swapd
  a0 packet-write-buf 0 1c +w !
  my-ip  blk  ab cd  bs-port  0 5
  udp-create-header
  slip-write-packet ;
# code
( Network block loading )
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
# code
( Network block store utils )
: bs-pack-size ( -- w:size )
  0 0   1 0 forw
    packet-write-buf 0 31 +w
    1 0 iw -w  +w @
    ff <> if
      dropw 1 1 iw -w
    then
  nextw ;
# code
( Network block store request )
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
# code
( Network block read/write )
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
# doc
( Network block read/write

bs-read and bs-write conform to the
common interface for block device
reading and writing.
)
# code
( Block device structures )
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
# code
( Block device routing )
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
# code
( Block cache )
0 var cache-state-free
1 var cache-state-used
2 var cache-state-dirty
8 var cache-entries  # max $19
0 var cache-used
: a*  dup 2* 2* + 2* ;
create cache-table
  cache-entries a* allot

: cache-lookup ( -- [w:header] err? )
  cache-used for
    0 i 1- a* cache-table +w
    dupw @ 0<> if
      dupw 1+w 1+w  blk  8 mem= if
        r>drop false exit
      then
    then
    dropw next
    true ;
# doc
( Block cache

The cache consists of a table with
10-byte structs:
 0 - 1 byte, state (free/used/dirty)
 1 - 1 byte, RAM page
 2 - 8 bytes, block address

Structs are stored with the most recent
entry last. If full, the first entry is
flushed and all entries moved back.
)
# code
( Block cache )
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
  dupw 1+w @ free-page drop
  cache-state-free -rot !
  false ;
# code
( Block cache )
: cache-new-entry
  ( -- [w:header] err? )
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
# code
( Block cache )
: empty-buffers ( -- )
  cache-table  0 cache-entries a*
  0 fill
  0 to cache-used ;
: save-buffers ( -- )
  cache-used for
    0 i 1- a*  cache-table +w
    cache-write if
      ." failed to write block "
      0 i 1- a* 1+ 1+  cache-table +w
      8 dump cr
    then
  next ;
# code
( High-level block words )
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
# code
( Block compression management )
: uncompress-text ( w:a -- )
  text-buffer dupw 2 ff +w uncompress
  text-buffer -w  tow text-size ;
: compress-text ( page -- err? )
  >r  text-buffer dupw text-size +w
  r> compress ;
# code
( Block cache flushing )
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
# code
( Block editor integration )
: find-page ( w:a c -- n ) [
  2 s>a  a>t  3 s>a  1 a>t+
  0 ldyimm
  tos>a
  target>
  t cmp (y)
  0 beq ref> swapw
  iny
  <ref bne
  <target
  inx inx  tya  a>tos
  ] ;
: scr-info ( -- )
  text-size .w ." text bytes, "
  block skip  ff find-page .
  ." compressed" cr ;
# code
( Text block loading )
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
# code
( Text block management/execution )
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
# code
( Block utils )
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
