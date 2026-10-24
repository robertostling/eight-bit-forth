# code
( KERNAL call wrapper )
variablew kernal-routine
: kernal-call [ kernal-routine jmpi ] ;
: kernal ( a x y c w:adr -- a x y c )
  kernal-routine !w [
  >a lsr impl
  txa pha
  3 s>a pha  tos>a tay  nos>a tax  pla
  ] kernal-call [
  a>t  1 t+ stxzp
  pla tax
  tya a>tos  1 t+>a a>nos  t>a 3 a>s
  0 lda imm  rol impl  a>
  ] ;
# code
( KERNAL calls 1 )
: kernal-open ( -- )
  0 0 0 0  ff c0 kernal  dropw dropw ;
: kernal-close ( log -- )
  0 0 0  ff c3 kernal  dropw dropw ;
: kernal-setlfs ( fileno dev sec -- )
  0  ff ba kernal dropw dropw ;
: kernal-setnam ( w:str -- )
  count -rot swap 0  ff bd kernal
  dropw dropw ;
: kernal-chkout ( fileno -- )
  0 swap 0 0  ff c9 kernal
  dropw dropw ;
: kernal-chkin ( fileno -- )
  0 swap 0 0  ff c6 kernal
  dropw dropw ;
# code
( KERNAL calls 2 )
: kernal-load
  ( w:start -- w:last/errno errf )
  0 -rot swap 0  ff d5 kernal 
  if dropw 1
  else rot drop swap 0 then ;
: kernal-getin ( -- byte )
  0 0 0 0  ff e4 kernal  dropw drop ;
: kernal-chrin ( -- byte )
  0 0 0 0  ff cf kernal  dropw drop ;
: kernal-save
  ( w:start w:last -- errno errf )
  swap  swapw [
  >a zsave sta zp
  >a zsave 1+ sta zp
  ] zsave -rot  0
  ff d8 kernal  nip nip ;
# code
( KERNAL calls, file scratching )
20 array filename-buffer
: prepend-scratch ( w:name -- )
  dupw @ dup 2 +  0 filename-buffer !
  for
    dupw 0 i +w @
    i 2 + filename-buffer !
  next dropw
  [char] s 1 filename-buffer !
  [char] : 2 filename-buffer ! ;
: scratch-file ( dev w:name -- )
  prepend-scratch
  0 filename-buffer kernal-setnam
  f swap f kernal-setlfs
  kernal-open
  f kernal-close ;
# code
( Time and cursor management )
: jiffy@w 0 a1 @w swap ;
: jiffys 0 swap jiffy@w +w
  begin jiffy@w overw >=w until
  dropw ;
: seconds for 3c jiffys next ;
: cursor@ ( -- row col )
  0 0 0 1  ff f0 kernal
  drop  rot drop ;
: cursor! ( row col -- )
  0 -rot 0  ff f0 kernal
  dropw dropw ;
# code
( KERNAL line buffer )
2 0 varw kernal-line-buf
: kernal-read-line ( -- )
  kernal-line-buf 1+w begin
    kernal-chrin dup >r
    !+
    r> 0d = if
      nip  1-  kernal-line-buf !  exit
    then
  again ;
: kernal-key
  kernal-getin ?dup skip kernal-key ;
: key maker kernal-key ;
