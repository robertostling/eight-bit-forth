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

# Macros (larger but faster):

: if pre>a 0 beq ref> ; immediate

# : add-shadow-ram ( -- )
#   ff d0 do  i free-page drop  1 +loop ;
# add-shadow-ram

# Non state smart to/tow

: to  postpone (to) ' 0 6 +w ,2 ; immediate
: tow  postpone (tow) ' 0 6 +w ,2 ; immediate

# More compact if statement, but slower:

: ?0branch [
  pla a>t  pla 1 a>t+
  t 1+zpw
  pre>a 0 bne ref>
  0 ldyimm  t lda (y)
  2 bpl  t 1+ dec zp
  clc  t adc zp  t sta zp
  2 bcc  t 1+ inc zp
  <target
  1 t+>a pha  t>a pha ] ;
: if litw [ ' ?0branch >code ,2 ]
  jsr  0 ,  ref> ; immediate
