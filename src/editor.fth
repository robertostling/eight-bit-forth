# code
# Line editor variables and utils
: cls 5 emit 0e emit 93 emit ;
: yes-no ( -- t/f )
  key
  [char] y case true
  [char] n case false
  drop yes-no ;

false var text-modified
0 0 varw text-size
0 0 varw editor-offset

: touch-text  true to text-modified ;
# doc
( Line editor and utilities

text-buffer: 768 byte text buffer
 used for editing and executing
 forth source
text-size: number of bytes used in
 text-buffer
text-modified: boolean indicator of
 whether text-buffer has been
 modified by screen editor
editor-offset: used internally in
 line editor )
# code
# Line editor, source listing
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
# doc
( Source listings

l: main listing word, prints line
 numbers and overwrite commands for
 easy editing )
# code
# Line editor text manipulation
: text-start?  editor-offset 0=w ;
: text-end?
  editor-offset text-size =w ;
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
# code
# Line editor delete
: editor-offsets
  ( w:o1 w:o2 -- w:a1 w:a2 )
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
# code
# Line editor insert
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
# code
# Line editor commands
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
# code
# Line editor commands
: append-char ( c -- )
  text-buffer text-size +w !
  text-size 1+w tow text-size
  touch-text ;
: x ( -- )
  rest-of-line
  # dupw 0=w if  dropw dropw exit  then
  forw @+ append-char nextw
  dropw ;
: p  text-buffer text-size typew ;
# code
# Line editor/UI utilities
: .jiffy
  0 a0 @ .h  0 a1 @ .h  0 a2 @ .h ;
: status
  here@w ['] ,2 -w
  .w ." bytes used, time " .jiffy cr ;
: drop-all fc sp! ;
# code
# Line editor/UI utilities
: check-stack
  0 sp@ 0 3 <w if
    cr ." stack overflow" cr  fc sp!
  then
  0 sp@ 0 fc >=w if
    cr ." stack underflow" cr  fc sp!
  then ;
