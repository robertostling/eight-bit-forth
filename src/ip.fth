# code
( RS-232/SLIP variables )
7 var 600baud ( largest that works here )
2 var serial-file
0 var default-file

02 40 varw packet-max-size
0 0 varw packet-read-size
0 0 varw packet-write-size

create packet-read-buf
packet-max-size allotw

create packet-write-buf
packet-max-size allotw

create my-ip c0 , a8 , 0 , 1 ,
create sunet-ip 25 , 9c , c0 , 32 ,
create gw-ip c0 , a8 , 0 , 2 ,

# code
( RS-232 initialization )
create fn-serial  2 , 600baud , 0 ,

: serial-init ( -- )
  fn-serial kernal-setnam
  serial-file 2 0 kernal-setlfs
  kernal-open ;
: serial-in ( -- c valid? )
  kernal-getin  02 97 @ 8 and 0= ;

: use-terminal  default-file kernal-chkin ;
: use-serial  serial-file kernal-chkin ;

# code
( Writing packets with SLIP )
: slip-emit ( c -- )
  ( default-file kernal-chkout
  ." [" dup .h ." ]" space )
  serial-file kernal-chkout
  dup c0 = if
    drop db emit dc emit exit
  then
  dup db = if
    drop db emit dd emit exit
  then
  emit ;
: slip-write-packet ( -- )
  serial-file kernal-chkout
  c0 emit
  packet-write-buf packet-write-size forw
    dupw @ slip-emit  1+w
  nextw dropw
  c0 emit
  default-file kernal-chkout ;

# code
( Reading packets with SLIP  )
0 var slip-state-start
1 var slip-state-wait
2 var slip-state-esc
3 var slip-state-time
4 var slip-state-done
5 var slip-state-err
6 var slip-state-over

slip-state-start var slip-rx-state

0 0 varw slip-timeout
: slip-timeout?
  slip-timeout jiffy@w <w ;
: slip-set-timeout
  jiffy@w 1 2c +w tow slip-timeout ;
# code
( SLIP receiving primitives )
: slip-rx-got
  packet-read-size packet-max-size =w if
    drop
    slip-state-over to slip-rx-state
    exit
  then
  packet-read-size dupw 1+w tow packet-read-size
  packet-read-buf +w !
  slip-set-timeout ;
: slip-start
  dup c0 <> if
    slip-rx-got
  else
    drop
  then
  slip-state-wait to slip-rx-state ;
# code
( SLIP escape code handling )
: slip-wait-esc
  slip-state-esc to slip-rx-state ;
: slip-wait-end  
  slip-state-done to slip-rx-state ;
: slip-wait
  db case slip-wait-esc
  c0 case slip-wait-end
  slip-rx-got ;
: slip-esc
  slip-state-wait to slip-rx-state
  dup dc = if  drop c0 slip-rx-got exit  then
  dup dd = if  drop db slip-rx-got exit  then
  slip-state-err to slip-rx-state ;
# code
( SLIP packet reception )
: slip-rx-next-state
  slip-rx-state
  slip-state-start case slip-start
  slip-state-wait case slip-wait
  slip-state-esc case slip-esc
  drop drop ;
: slip-rx
  0 0 tow packet-read-size
  slip-set-timeout
  use-serial
  slip-state-start to slip-rx-state
  begin
    serial-in if
      slip-rx-next-state
    else
      drop
      slip-timeout? if
        slip-state-time to slip-rx-state
      then
    then
  slip-rx-state slip-state-time >= until
  use-terminal ;
# code
( Process ICMP and generic UDP packets )
: process-ping
  ." ping from "
  packet-read-buf 0 c +w 4 dump cr ;
: process-icmp
  packet-read-buf 0 14 +w @
  dup 8 = if
    drop process-ping exit
  then
  0= if
    ." pong from "
    packet-read-buf 0 c +w 4 dump cr
  else
    ." unknown icmp" cr
  then ;
: process-udp
  ." udp message: "
  packet-read-buf 0 1c +w
  packet-read-size overw -w nip type cr ;
# code
( Process IP packet )
: process-ip
  packet-read-size 0 14 <w if
    exit
  then
  packet-read-buf 0 9 +w @
  dup 1 = if
    drop process-icmp exit
  then
  dup 11 = if
    drop process-udp exit
  else
    ." unknown ip protocol " . cr
  then ;
# code
( Compute IP header checksums )
: ip-checksum ( w:a w:n -- w:x )
  ( todo: avoid writing to buffer )
  dup 1 and 0<> if
    overw overw +w 0 -rot !
  then
  0 0 swapw 1+w 2/w forw
    overw @w swap 1c+w
    swapw 1+w 1+w swapw
  nextw
  swapw dropw
  ff ff xorw ;
# code
( Construct IP header )
: ,@ ( w:a -- w:a+1 )
  dupw @ , 1+w ;
: 4,@ ,@ ,@ ,@ ,@ ;
: ip-build-header ( w:src w:dest proto -- )
  here@w >rw
  packet-write-buf here!w
  45 , 0 , packet-write-size swap ,2
  12 , 34 , 40 , 0 ,
  40 , ( proto ) , 0 , 0 ,
  swapw 4,@ dropw 4,@ dropw
  r>w here!w
  packet-write-buf  0 14  ip-checksum swap
  packet-write-buf  0 a +w  !w
  ;
# code
( ICMP ping )
0 1 varw ping-seq
ab ba varw ping-id
: icmp-ping ( w:dest -- )
  0 24 tow packet-write-size
  my-ip swapw 1 ip-build-header
  packet-write-buf 0 14 +w
  8 !+ 0 !+ 0 !+ 0 !+
  ping-id swap !+w  ping-seq swap !+w
  8 for  i !+  next
  dropw
  packet-write-buf 0 14 +w  0 10  ip-checksum swap
  packet-write-buf 0 16 +w !w
  slip-write-packet
  ping-seq 1+w tow ping-seq
  ping-id 1+w tow ping-id
  ;
# code
( ICMP ping, packet dumping )
: ping ( ip -- )
  ." ping.." icmp-ping
  ." ."
  slip-rx
  ." ." slip-rx-state . cr
  process-ip ;
: packet-info
  ." read packet with " packet-read-size .w
  ." bytes" cr
  packet-read-buf packet-read-size nip dump cr
  ." header checksum: "
  packet-read-buf 0 14 ip-checksum .w cr ;
# code
( UDP packet creation utils )
: udp-create-header ( w:src w:dest
  w:src-port w:dest-port w:size -- )
  dupw 0 8 +w swap
  packet-write-buf 0 18 +w  !w
  0 1c +w tow packet-write-size
  swap  packet-write-buf 0 16 +w  !w
  swap  packet-write-buf 0 14 +w  !w
  0 0  packet-write-buf 0 1a +w  !w
  11 ip-build-header ;
# code
( UDP packet creation utils )
: udp-copy-payload ( w:data w:size w:offset -- )
  packet-write-buf +w 0 1c +w  swapw
  <move ;
