( pi game demo )
: data"
  char dup [char] " = skip
  dup 21 >= if , else drop then
  data" ;
create pi-digits data"
314159265358979323846264338327950288
419716939937510582097494459230781640
628620899862803482534211706798214808
651328230664709384460955058223172535"
here@w pi-digits -w varw n-digits
variablew pi-pos
pi-digits pi-pos !w
: pi-digit pi-pos @w @ ;
: jiffy@w 0 a1 @w swap ;
: jiffys 0 swap jiffy@w +w
  begin jiffy@w overw >=w until
  dropw ;
: advance 0 1 pi-pos +!w ;
variable score
: score+!
  score +!
  score @ 0< if 0 score ! then
  cursor@  0 0 cursor!
  ." score: " score @ .
  cursor! ;
: pi-game
  key dup 20 = if
    5 emit pi-digit emit 9a emit
    advance
    5 negate score+!
  else pi-digit = if
    pi-digit emit  advance
    1 score+!
  else
    3 negate score+!
    1 d0 20 +!
    10 jiffys
    ff d0 20 +!
  then
  then
  pi-game ;
