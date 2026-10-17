.( testing utilities )
: .context
  0 zinput @w 0 40 -w 40 type
  ." ****"
  0 zinput @w 40 type ;
: assert ( f -- ) skip .context
  cr ." assertion failed!"
  status ;
( ------------------------------------ )
.( testing memory manipulation )
73 stack> 10 !
88 stack> 11 !
stack> 10 @  73 = assert
stack> 11 @  88 = assert
stack> 10 @w  73 = assert 88 = assert
ab ba stack> 10 !w
stack> 10 @w  ba = assert ab = assert
11 22 stack> 10 +!w
stack> 10 @w  dc = assert bc = assert
11 stack> 10 +!
stack> 10 @  ed = assert
status
( ------------------------------------ )
.( testing arithmetic )
( ' -w 10 dump cr )
84 11 -  73 = assert
62 11 +  73 = assert
33 61 or  73 = assert
77 f3 and  73 = assert
72 1+  73 = assert
74 1-  73 = assert
9a 98  11 22 +w ba = assert ab = assert
bc dc  11 22 -w ba = assert ab = assert
status
( ------------------------------------ )
.( testing comparison )
3 7 < assert
7 3 < 0= assert
7 3 > assert
3 7 > 0= assert
73 37 <> assert
7 7 >= assert
7 7 <= assert
8 7 <= 0= assert
7 8 >= 0= assert
6 7 <= assert
7 6 >= assert
ab ba ab ba =w assert
ab ba ba ab =w 0= assert
ab ba ba ab <>w assert
status
( ------------------------------------ )
.( testing compiler )
: test-immediate1 73 ; immediate
: test-immediate2 37 test-immediate1 ;
test-immediate2 37 = assert 73 = assert
status
( ------------------------------------ )
.( testing return stack/literals )
: test-r>w  ab ba >rw 73 r>w ;
test-r>w  ab ba =w assert 73 = assert
: test-lit  lit [ cd , ] ;
dc test-lit cd = assert dc = assert
: test-litw  litw [ 65 02 ,2 ] ;
13 37 test-litw
65 02 =w assert 13 37 =w assert
status
( ------------------------------------ )
.( testing strings )
char 0  30 = assert
char  1  1 = assert 20 = assert
73
s" hello there?" count type cr
73 = assert
status
( ------------------------------------ )
.( testing conditionals )
: test3 if 73 else 37 then ;
true test3  73 = assert
false test3  37 = assert
: test4 if 73 exit then 37 ;
true test4  73 = assert
false test4  37 = assert
: test5 ab ba drop ;
test5  ab = assert
' test5 execute  ab = assert
status
( ------------------------------------ )
.( testing loops )
: log4 0
  begin
    swap 2/ 2/ swap 1+
    over 1 <= if nip exit then
  again ;
13 log4  2 = assert
: log2 0
  begin
    swap 2/ swap 1+
  over 1 <= until
  nip ;
13 log2  4 = assert
: test-for 0 swap for i + next ;
73 5 test-for  0f = assert  73 = assert
: test-do 0  6 2 do  i +  1 +loop ;
37 test-do 0e = assert 37 = assert
status
( ------------------------------------ )
.( testing postpone )
: test-postpone1 postpone + ; immediate
: test-postpone2 64 0f test-postpone1 ;
test-postpone2  73 = assert
( todo: postpone with immediate word )
status
( ------------------------------------ )
.( testing variables )
variable test-variable-x
variable test-variable-y
test-variable-x @ 0= assert
test-variable-y @ 0= assert
17 test-variable-x !
20 test-variable-y !
test-variable-x @ test-variable-y @ +
37 = assert
status
( ------------------------------------ )
.( testing literals/constants )
: test-literalw [ 12 34 ] literalw ;
56 test-literalw
12 34 =w assert  56 = assert
73 constant test-constant
37 test-constant
73 = assert 37 = assert
status
( ------------------------------------ )
: test-type for-each emit ;
: test-times 8 times dup . ;

