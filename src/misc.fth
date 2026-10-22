: vice-key kernal-key
  dup 23 = so drop 3b ;
make key vice-key ;
: skip-interpret
  0 zend @w  0 zinput !w ;
: confirm? ." sure? [y/n]"
  key dup emit [char] y = ;

4 var checksum-size
create checksum checksum-size allot
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
