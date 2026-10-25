# doc
( Debugging utils )
: mix8 ( x -- y )
  [ tos>a  tay
  sbox> 0 lda adr,y  a>tos ] ;
: page-checksum ( page -- x )
  0  0 begin
    >r @+ r> xor mix8
  over 0= until
  nip nip ;
: pages-checksums ( from to -- )
  swap do
    i page-checksum .
  1 +loop cr ;
: debug-info
  cd page-checksum . here@w .w cr
  10 here@w drop pages-checksums ;
# debug-info
# code
# adding this block makes things work again... why?
: .name ( w:a -- )
  >name type ;
: .name-next ( w:a -- )
  dupw 0=w if  dropw exit  then
  dupw .name space  @w .name-next ;
: hash-head ( n -- w:a )
  dup  cb swap @ >r  cc swap @  r> ;
: words ( -- )
  0  begin
    dup hash-head .name-next
  1+ ?dup 0= until ;
# code
: drop-all fc sp! ;
: check-stack
  0 sp@ 0 3 <w if
    cr ." stack overflow" cr  drop-all
  then
  0 sp@ 0 fc >=w if
    cr ." stack underflow" cr  drop-all
  then ;
( : self cr ." <" r@w 0 5 -w >name type
  ."  -- " .s ." > " check-stack
  key drop ;
' :
: : [ >code jsr ] postpone self
  ; immediate )
