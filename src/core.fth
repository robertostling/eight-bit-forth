: ,2 , , ;
: ,3 , , , ;
: 0 00 ;
: 1 01 ;
( use with imm )
: ora 0 ;
: anda 20 ;
: eora 40 ;
: adc 60 ;
: sta 80 ;
: lda a0 ;
: cmp c0 ;
: sbc e0 ;
( use with impl )
: asl 1 ;
: rol 21 ;
: lsr 41 ;
: ror 61 ;
( use with neither )
: dec c1 ;
: inc e1 ;
( addressing modes )
: impl 9 + , ;
: imm 9 + ,2 ;
: adr d + ,3 ;
: zp 5 + ,2 ;
: (x) 1 + ,2 ;
: (y) 11 + ,2 ;
: zp,x 15 + ,2 ;
: adr,y 19 + ,3 ;
: adr,x 1d + ,3 ;
( branching )
: bpl 10 ,2 ;
: bmi 30 ,2 ;
: bvc 50 ,2 ;
: bvs 70 ,2 ;
: bcc 90 ,2 ;
: bcs b0 ,2 ;
: bne d0 ,2 ;
: beq f0 ,2 ;
: brk 00 , ;
: jsr 20 ,3 ; 
: rti 40 , ;
( rts has built-in optimizing word )
: jmp 4c ,3 ;
: jmpi 6c ,3 ;
: nop ea , ;
: jam 02 , ;
( zero-operand instructions )
: php 08 , ;
: clc 18 , ;
: plp 28 , ;
: sec 38 , ;
: pha 48 , ;
: cli 58 , ;
: pla 68 , ;
: sei 78 , ;
: dey 88 , ;
: tya 98 , ;
: tay a8 , ;
: clv b8 , ;
: iny c8 , ;
: cld d8 , ;
: inx e8 , ;
: sed f8 , ;
: txa 8a , ;
: txs 9a , ;
: tax aa , ;
: tsx ba , ;
: dex ca , ;
( various instructions with non-uniform
  addressing modes )
: ldximm a2 ,2 ;
: ldxzp a6 ,2 ;
: ldyimm a0 ,2 ;
: styzp 84 ,2 ;
: styzp,x 94 ,2 ;
: ldyzp a4 ,2 ;
: ldyzp,x b4 ,2 ;
: styadr 8c ,3 ;
: ldyadr ac ,3 ;
: ldyadr,x bc ,3 ;
: stxzp 86 ,2 ;
: stxzp,y 96 ,2 ;
: stxadr 8e ,3 ;
: cpyimm c0 ,2 ;
: cpximm e0 ,2 ;
: cpyzp c4 ,2 ;
: cpxzp e4 ,2 ;
: cpyadr cc ,3 ;
: cpxadr ec ,3 ;
: bitzp 24 ,2 ;
: bitadr 2c ,3 ;
( core compiler )
: stack> ce ;
: stack-1 cd ff ;
: stack stack> 0 ;
: sbox> cd ;
: zhere 2 ;
: zlast 4 ;
: zinput 6 ;
: zend 8 ;
: zstate c ;
: zoptimize d ;
: ztask e ;
: ztemp 13 ;
: zsave 1e ;
: zmreg 20 ;
: znreg 22 ;
( temp access macros )
: t ztemp ;
: t+ ztemp + ;
: a>t+ t+ sta zp ;
: t+>a t+ lda zp ;
: a>t t sta zp ;
: t>a t lda zp ;
( stack operations and macros )
: swap [
  stack lda adr,x
  pha
  stack> 1 lda adr,x
  stack sta adr,x
  pla
  stack> 1 sta adr,x ] ;
( note: 1-based addressing )
: s>a stack> swap ff +  lda adr,x ;
: a>s stack> swap ff +  sta adr,x ;
: >a stack lda adr,x  inx ;
: a> dex  stack sta adr,x ;
( same as >a but sets flags and costs
  1 more cycle )
: pre>a inx  stack-1 lda adr,x ;
: tos>a 1 s>a ;
: a>tos 1 a>s ;
: nos>a 2 s>a ; 
: a>nos 2 a>s ;
: #a>tos lda imm  a>tos ;
( compiler words )
: immediate [
  2 ldyimm
  80 lda imm
  zlast ora (y)
  zlast sta (y) ] ;
: on ff ;
: off 0 ;
: compiling [ zstate lda zp  a> ] ;
: compiler [ >a  zstate sta zp ] ;
: so pre>a 1 bne rts ; immediate
: skip pre>a 1 beq rts ; immediate
( stack operations )
: dup [ tos>a a> ] ;
: drop [ inx ] ;
: over [ nos>a a> ] ;
: oover [ 3 s>a a> ] ;
: nip [ tos>a a>nos inx ] ;
: sp@ [ txa a> ] ;
: sp! [ >a tax ] ;
: rp@ [
  txa pha tsx txa tay pla tax tya a>
  ] ;
: rp! [
  txa tay  tos>a tax txs
  tya tax inx ] ;
: >r >a pha ; immediate
: r> pla a> ; immediate
: r>drop pla ; immediate
: r@ pla pha a> ; immediate
: -rot [
  tos>a pha
  nos>a a>tos
  3 s>a a>nos
  pla 3 a>s ] ;
: rot -rot -rot ;
: ?dup [ tos>a  1 bne  rts  a> ] ;
: 1+ [ stack inc adr,x ] ;
: 1- [ stack dec adr,x ] ;
: 2* [ stack asl adr,x ] ;
: 2/ [ stack lsr adr,x ] ;
: 2*w [
  stack asl adr,x
  stack> 1 rol adr,x ] ;
: 2/w [
  stack> 1 lsr adr,x
  stack ror adr,x ] ;
: dupw over over ;
: overw [ 4 s>a a>  4 s>a a> ] ;
: swapw overw [
  3 s>a 5 a>s  4 s>a 6 a>s
  tos>a 3 a>s  nos>a 4 a>s  inx inx ] ;
( needed early, rest defined below )
: +w [
  clc
  3 s>a  stack> 0 adc adr,x  3 a>s
  4 s>a  stack> 1 adc adr,x  4 a>s
  inx inx ] ;
( zero-page macros )
: >zpw ( zp -- )
  dup  >a sta zp  >a 1+ sta zp ;
: zp>w ( zp -- )
  dup  1+ lda zp a>  lda zp a> ;
: +zpw ( zp n -- )
  lda imm  clc  dup adc zp  dup sta zp
  2 bcc  1+ inc zp ;
: -zpw ( zp n -- )
  over lda zp  sec  sbc imm  dup sta zp
  2 bcs  1+ dec zp ;
: 1+zpw ( zp -- )
  dup  inc zp  2 bne  1+ inc zp ;
( compiler words )
: here 0 zhere ;
: here@ [ zhere lda zp  a> ] ;
: here@w
  [ zhere 1 + lda zp  a> ] here@ ;
: here!w [
  >a  zhere sta zp
  >a  zhere 1 + sta zp ] ;
: >code 0 3 +w ;
: >flags 0 2 +w ;
: >last ;
( used for hot-swapping, see make ) 
: maker here@w 0 3 +w jmp ; immediate
( handle backward references with
  conditional branching ops )
: kernal-emit [ >a  ff d2 jsr ] ;
: emit maker kernal-emit ;
: space 20 emit ;
: cr 0d emit ;
: hexdigit [
  tos>a
  0f anda imm
  0a cmp imm
  2 bcc
  6 adc imm
  30 adc imm
  a>tos ] ;
: .h
  dup 2/ 2/ 2/ 2/ hexdigit emit
  hexdigit emit ;
: . .h space ;
: .w swap .h .h space ;
: = [
  >a  stack cmp adr,x  6 beq
  00 #a>tos  rts
  ff #a>tos ] ;
( memory manipulation )
: s>tw nos>a 1 a>t+  tos>a 0 a>t+ ;
: @ [ s>tw  0 ldyimm
  t lda (y)
  inx a>tos ] ;
: @w [ s>tw  0 ldyimm
  t lda (y)  a>tos  iny
  t lda (y)  a>nos ] ;
: ! [ s>tw  inx inx  0 ldyimm
  >a  t sta (y) ] ;
: !w ! [ iny  >a  t sta (y) ] ;
: +! [ s>tw  inx inx  0 ldyimm
  >a  clc t adc (y)  t sta (y) ] ;
: +!w +! [
  iny >a  t adc (y)  t sta (y) ] ;
( arithmetic )
: aluop ( op - )
  nos>a  stack> swap 0 swap adr,x
  inx a>tos ;
: - [ sec sbc aluop ] ;
( : + [ clc adc aluop ] ; )
: or [ ora aluop ] ;
: xor [ eora aluop ] ;
: and [ anda aluop ] ;
: aluopw ( op - )
  dup
  3 s>a
  stack> swap 0 swap adr,x  3 a>s
  4 s>a
  stack> swap 1 swap adr,x  4 a>s
  inx inx ;
: orw [ ora aluopw ] ;
: xorw [ eora aluopw ] ;
: andw [ anda aluopw ] ;
: -w [ sec sbc aluopw ] ;
( : +w [ clc adc aluopw ] ; )
: 1+w [
  stack inc adr,x
  3 bne
  stack> 1 inc adr,x ] ;
: 1c+w +w [ 1 bcs  rts ] 1+w ;
( comparison operators )
: true ff ;
: false 0 ;
: compop ( opcode -- )
  tos>a  , 6 ,
  false #a>tos rts  true #a>tos ;
: 0= [ f0 compop ] ;
: not 0= ;
: 0< [ 30 compop ] ;
: 0>= [ 10 compop ] ;
: 0<> [ d0 compop ] ;
: 0> dup 0>= swap 0<> and ;
: 0<= dup 0< swap 0= or ;
: < - 0< ;
: > - 0> ;
: <= - 0<= ;
: >= - 0>= ;
( : = - 0= ; )
: <> - 0<> ;
: <w -w drop 0< ;
: >=w -w drop 0>= ;
: 0=w or 0= ;
: 0<>w or 0<> ;
: =w -w 0=w ;
: <>w -w 0<>w ;
: 0<w drop 0< ;
( word header operations )
: >name
  dupw 0 2 +w @ 1f and  dup >r
  0 swap -w  r> ;
: tail;
  ' >code jmp  off compiler ; immediate
: jump ' >code jmp ;
: call ' >code jsr ;
( 16-bit addresses for both forward and
  backward references, but only 8-bit
  jumps, with error checking )
: range-error  0 zinput @w 40 type cr  3 error ;
: <ref here@w 1+w 1+w -w
  swap 1+ [ pre>a  3 beq  jump range-error ]
  [ tos>a  3 bmi  jump range-error ] ;
: ref> here@w ;
: <target
  dupw  here@w swapw -w
  swap [ pre>a  3 beq  jump range-error ]
  [ tos>a  3 bpl  jump range-error ]
  -rot 1-w ! ;
: target> here@w ;
( conditionals )
: if pre>a 0 beq ref> ; immediate
: disable-tail
  0 zoptimize @ 7f and 0 zoptimize ! ;
: then <target disable-tail
  ; immediate
: else
  clc 0 bcc ref>  swapw
  tail; then immediate
: case
  >a
  stack cmp adr,x
  4 bne
  inx  ' >code jmp ; immediate
( inlined words )
: exit rts ; immediate
: drop
  compiling if inx
  else [ inx ] then ; immediate
: dropw
  compiling if inx inx
  else [ inx inx ] then ; immediate
( return stack 16-bit ops )
: dup>rw
  nos>a pha tos>a pha ; immediate
: r>dropw pla pla ; immediate
: >rw 
  nos>a pha tos>a pha
  inx inx ; immediate
: r>w
  dex dex
  pla a>tos pla a>nos
  ; immediate
: r@w
  dex dex
  pla a>tos tay
  pla pha a>nos
  tya pha ; immediate
( literals/execution )
: execute [
  >a  clc 3 adc imm  0 a>t+
  >a  0 adc imm  1 a>t+
  0 ztemp jmpi ] ;
: lit r>w 1+w dup>rw @ ;
: litw r>w 1+w dupw 1+w >rw @w ;
: (to) r>w 1+w dupw 1+w >rw @w ! ;
: (tow) r>w 1+w dupw 1+w >rw @w !w ;
: letw
  dupw dup>rw @w >rw
  !w execute
  r>w r>w !w ;
( postpone )
: docompile
  r>w 1+w dupw 1+w >rw @w jsr ;
: postpone
  '
  dupw >flags @ 0< if
    >code jsr
  else
    litw [ ' docompile >code ,2 ] jsr
    >code ,2
  then ; immediate
( make and dynamic relinking )
: patch
  ' >code  dupw 4c -rot ! ( jmp )
  1+w here@w swapw !w  on compiler ;
: domake
  r>w 1+w dupw @w swapw
  1+w 1+w swapw !w ;
( use only with maker words )
: >make >code 1+w ;
: make
  compiling if
    postpone domake  ' >make ,2
  else patch then ; immediate
: unmake ' >make
  dupw 1+w 1+w swapw !w ;
: making ' >make ' >code swapw letw ;
( loops )
: begin target> ; immediate
: again jmp ; immediate
: until pre>a <ref beq ; immediate
: for pre>a  0 beq ref>  pha target>
  ; immediate
: forw
  nos>a  stack ora adr,x  0 beq ref>
  nos>a pha tos>a pha inx inx
  target> ; immediate
: next
  pla  sec 1 sbc imm  pha
  <ref bne
  pla
  <target ; immediate
: nextw
  pla  sec 1 sbc imm a>t
  pla  0 sbc imm  pha  t ora zp  tay
  t>a pha
  tya  <ref bne
  pla pla
  <target ; immediate
: i pla pha a> ; immediate
: iw pla tay pla a> tya a>
  nos>a pha tos>a pha ; immediate
: do
  nos>a pha tos>a pha inx inx
  target>
  pla a>t ( t = count )
  pla  t cmp zp
  0 beq  target>  pha
  t>a pha ; immediate
: +loop
  pla  clc stack adc adr,x inx  pha
  swapw clc <ref bcc
  tail; then immediate
( input stream )
: end@w [ zend zp>w ] ;
: input@w [ zinput zp>w ] ;
: eob?  input@w end@w =w ;
: input@@ [
  0 ldyimm  zinput lda (y)  a>  ] ;
: input>  [ zinput 1+zpw ] ;
: # eob? skip  input@@ 0d = skip
  input> # ; immediate
: char
  eob? if 0 else input@@ input> then ;
: [char]
  char postpone literal ; immediate
: .(
  char dup [char] ) = if
    drop cr exit
  then emit tail; .( immediate
: # char dup 0= swap 0d = or
  skip tail; # immediate
( strings )
: s" here@w 0 ,
  begin
    char dup [char] " =
    if drop exit then
    ,  dupw 1 -rot +!
  again ;
: count dupw 1+w swapw  @ ;
: countw count 0 swap ;
: lits
  r>w 1+w dupw dupw @ 0 swap +w >rw ;
: dump
  dup 0= if drop dropw exit then
  -rot dupw @ . 1+w rot  1- dump ;
: .s
  sp@ stack> swap
  fc over - 3f and dump ;
: " postpone lits s" dropw ; immediate
: ."
  postpone lits s" dropw
  postpone count postpone type
  ; immediate
( high-level compiler )
: last 0 zlast ;
: literalw postpone litw ,2 ; immediate
: to postpone (to) ' 0 6 +w ,2 ; immediate
: tow postpone (tow) ' 0 6 +w ,2 ; immediate
: ['] ' postpone literalw ; immediate
( does> )
: here>code last @w >code here!w ;
: dodoes
  r>w 1+w  last @w >code 1+w !w ;
: does>
  postpone dodoes
  postpone r>w postpone 1+w ; immediate
( additional looping constructs )
: call-parent [
  ztemp stxzp  tsx
  01 04 lda adr,x  pha
  01 03 lda adr,x  pha
  ztemp ldxzp ] ;
: for-each
  ?dup 0= if r>dropw dropw exit then
  -rot dupw 1+w swapw @
  call-parent
  rot 1- for-each ;
: times
  ?dup 0= if r>dropw exit then
  call-parent
  1- times ;
( data structures )
: variable create 0 , ;
: variablew variable  0 , ;
: var create , does> @ ;
: varw create ,2 does> @w ;
: allot for 0 , next ;
: allotw forw 0 , nextw ;
: array
  create allot does> rot 0 swap +w ;
: arrayw
  create 2* allot does>
  rot 0 swap 2*w +w ;
( misc utilities )
: << ( x n -- x<<n )
  dup 0= if drop exit then [
  target>
  stack> 1 asl adr,x
  stack dec adr,x
  <ref bne
  ] drop ;
: !op ( w:a c -- w:a )
  -rot  dupw >rw  !  r>w ;
: !+  !op 1+w ;
: !-  !op 1-w ;
: !+w ( w:a w:x -- w:a+2 )
  overw !w 1+w 1+w ;
: @+ ( w:a -- w:a+1 c )
  dupw 1+w swapw @ ;
: @+w ( w:a -- w:a+2 w:x )
  dupw 1+w 1+w swapw @w ;
: negate 0 swap - ;
: negatew 0 0 swapw -w ;
: -!w swapw negatew swapw +!w ;
: dumpw ( w:a w:n -- )
  forw @+ . nextw dropw ;
: typew ( w:a w:n -- )
  forw @+ emit nextw dropw ;
: dup!+ ( w:a c -- w:a+1 c )
  >r  dupw r@ -rot !  1+w r> ;
: fillw ( w:a c w:n -- )
  forw  dup!+  nextw  drop dropw ;
: !nb ( x y z ... w:a n -- )
  dup >r  1- 0 swap +w  r>
  for  dup>rw ! r>w 1-w  next
  dropw ;
: !nl ( x y z ... w:a n -- )
  for  dup>rw ! r>w 1+w  next
  dropw ;
