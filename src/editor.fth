( general screen handling )
: cls 5 emit 0e emit 93 emit ;
: check-stack
  0 sp@ 0 3 <w if
    cr ." stack overflow" cr  fc sp!
  then
  0 sp@ 0 fc >=w if
    cr ." stack underflow" cr  fc sp!
  then ;
: drop-all fc sp! ;

false var text-modified
0 0 varw text-size
0 0 varw editor-offset

: yes-no ( -- t/f )
  key
  [char] y case true
  [char] n case false
  drop yes-no ;
: list-line ( w:a -- w:a' )
  dupw text-buffer -w
  text-size =w skip
  @+ dup emit
  0d = skip list-line ;
: l  0 text-buffer
  begin
    dupw text-buffer -w text-size =w if
      dropw drop cr exit
    then
    rot dup >r dup . 1+ -rot ." o "
    list-line
    r> 0f and 0f = if
      ." cont (y/n)?" yes-no cr not if
        dropw drop exit
      then
    then
  again ;
: p  text-buffer text-size typew ;

: touch-text  true to text-modified ;
: text-start?  editor-offset 0=w ;
: text-end?  editor-offset text-size =w ;
: text@ ( -- c )
  editor-offset text-buffer +w @ ;
: text-char>
  text-end? skip
  editor-offset 1+w tow editor-offset ;
: text-eol>
  text-end? skip
  text@ 0d = skip
  text-char> text-eol> ;
: text-line>
  text-eol> text-char> ;
: text-seek ( n -- )
  0 0 tow editor-offset
  for text-line> next ;
: editor-offsets ( w:o1 w:o2 -- w:a1 w:a2 )
  >rw
  text-buffer +w swapw
  text-buffer +w swapw
  r>w ;
: del ( n -- )
  text-seek editor-offset
  text-line> editor-offset
  swapw
  overw overw -w >rw
  overw text-size swapw -w
  editor-offsets <move
  text-size r>w -w tow text-size
  touch-text ;
: ins ( w:a w:count n -- )
  ( assume inserted line ends with cr )
  text-seek
  editor-offset  overw +w
  editor-offset  swapw
  overw text-size swapw -w
  editor-offsets move>
  dupw text-size +w tow text-size
  editor-offset text-buffer +w
  swapw <move
  touch-text ;
create cr-str 0d ,
: rest-of-line ( -- w:a w:count )
  input@w  begin
    eob? if
      input@w overw -w
      dupw 0=w if
        # temporary hack to ensure cr
        dropw dropw cr-str 0 1
      then
      exit
    then
    input@@ input> 0d = if
      input@w overw -w exit
    then
  again ;
: o ( n -- )
  dup >r del  rest-of-line r> ins ;
: w ( n -- )
  >r rest-of-line r> ins ;
: append-char ( c -- )
  text-buffer text-size +w !
  text-size 1+w tow text-size
  touch-text ;
: x ( -- )
  rest-of-line
  # dupw 0=w if  dropw dropw exit  then
  forw @+ append-char nextw
  dropw ;

: .jiffy
  0 a0 @ .h  0 a1 @ .h  0 a2 @ .h ;
: status
  here@w ['] ,2 -w
  .w ." bytes used, time " .jiffy cr ;
