( morse code )
variable dit-length
4 dit-length !
: beep ( len -- )
  triangle gate or voice1 control
  jiffys
  triangle voice1 control ;
: 3* dup 2* + ;
: dit dit-length @ beep
  dit-length @ jiffys ;
: dah dit-length @ 3* beep
  dit-length @ jiffys ;
: letter-spacing
  dit-length @ 2* jiffys ;
: word-spacing
  dit-length @ 2* 2* jiffys ;
: cw-char ( x n -- )
  for
    dup 0< if dah else dit then
  2* next drop ;
create morse-table
6 cc ,2 ( , )
0 0  ,2
6 54 ,2 ( . )
0 0  ,2
5 f8 ,2 ( 0 )
5 78 ,2
5 38 ,2
5 18 ,2
5 8  ,2
5 0  ,2
5 80 ,2
5 c0 ,2
5 e0 ,2
5 f0 ,2 ( 9 )
0 0  ,2
0 0  ,2
0 0  ,2
5 88 ,2 ( = )
0 0  ,2
6 30 ,2 ( ? )
0 0  ,2
2 40 ,2 ( a )
4 80 ,2
4 a0 ,2
3 80 ,2
1 0  ,2
4 20 ,2 ( f )
3 c0 ,2
4 0  ,2
2 0  ,2
4 70 ,2
3 a0 ,2 ( k )
4 40 ,2
2 c0 ,2
2 80 ,2
3 e0 ,2
4 60 ,2 ( p )
4 d0 ,2
3 40 ,2
3 0  ,2
1 80 ,2
3 20 ,2 ( u )
4 10 ,2
3 60 ,2
4 90 ,2
4 b0 ,2
4 c0 ,2 ( z )
: cw-emit ( c -- )
  dup 20 = if
    drop word-spacing exit
  then
  dup [char] , <  over [char] z >  or
  if drop exit then
  [char] , - 2*  0 swap morse-table +w
  @w swap
  dup 0= if dropw exit then
  cw-char letter-spacing ;
: bpf
  band-pass filters
  7 filter-control
  8 00 filter-frequency ;
: enable-noise
  20 00 voice3 frequency
  1 1 voice3 decay-attack
  0 f voice3 release-sustain
  noise gate or voice3 control ;
: turn-up ( speed -- )
  f 0 do i volume dup jiffys 1 +loop
  drop ;
: turn-down ( speed -- )
  0 10 do
    i 1- volume dup jiffys
  ff +loop drop ;
: enable-morse
  0 0 voice1 decay-attack
  0 f voice1 release-sustain ;
: morse-type ( w:adr n -- )
  for dupw 1+w swapw @ cw-emit next
  dropw ;
: morse-demo
  enable-morse
  enable-noise
  20 00 voice1 frequency
  8 turn-up ( 0f volume )
  " cq cq de sm0ysr k"
  count morse-type ;

