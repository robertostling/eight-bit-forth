( simplified version of "taco cat goat
  cheese pizza" )
: joystick ( player -- x )
  dc swap 1- @ ;
variablew time0
: current-time 0 a1 @w swap ;
: start-timer current-time time0 !w ;
: timeout? current-time time0 @w -w 0 c0 >=w ;
2 array player-cards
: pressed? and 0= ;
10 var fire
8 var right
4 var left
2 var down
1 var up
variable slapped
: react
  0 slapped !
  start-timer
  begin
    timeout? skip
    1 joystick fire pressed?
    if  1 slapped !  exit  then
    2 joystick fire pressed?
    if  2 slapped !  exit  then
  again ;
5 var #card-names
#card-names arrayw card-names
s" taco"   0 card-names !w
s" cat"    1 card-names !w
s" goat"   2 card-names !w
s" cheese" 3 card-names !w
s" pizza"  4 card-names !w
variable cards-in-pile
variable next-call
variable shown-card
variable next-player
: should-slap? next-call @ shown-card @ = ;
( fix below, mod 5 not mod 4 )
: random-card 0 a2 @  3 and ;
: other-player 3 xor ;
: no-slap
  next-call @ 1+
  dup #card-names = if
    drop 0
  then next-call ! ;
: bar 12 emit for e0 emit next 12 emit ;
: .card card-names @w count type ;
: emits ( char n -- )
  for dup emit next drop ;
: display
  93 emit
  ." player 1: " 0 player-cards @ bar cr
  ." player 2: " 1 player-cards @ bar cr
  ." pile:     " cards-in-pile @ bar cr
  ." call:     " next-call @ .card 21 emit
  b 10 cursor!
  d5 emit  63 8 emits  c9 emit
  c 10 cursor! 62 emit space
  shown-card @ .card
  c 19 cursor! 62 emit
  d 10 cursor!
  ca emit  63 8 emits  cb emit ;
: start-over
  1 cards-in-pile !
  0 next-call !
  1 next-player ! ;
: winner
  0 player-cards @ 0= if  1 exit  then
  1 player-cards @ 0= if  2 exit  then
  0 ;
: round
  ff next-player @ 1- player-cards +!
  random-card shown-card !
  display
  react
  slapped @ ?dup if
    should-slap? if other-player then
    dup next-player !
    10 a cursor!
    ." player " dup . ." gets "
    cards-in-pile @ . ." cards"
    1- player-cards cards-in-pile @ -rot +!
    start-over
    3 seconds
  else
    no-slap
    1 cards-in-pile +!
    next-player @
    other-player next-player !
  then ;
: initialize-game
  8 0 player-cards !  8 1 player-cards !
  start-over ;
: have-a-winner ( player -- )
  93 emit  c f cursor!
  ." player " . ." won!"
  16 4 cursor!
  ." press stop to leave,
    any other key to restart"
  key 3 = ;
: play
  initialize-game
  begin
    round
    winner ?dup if
      have-a-winner so
      initialize-game
    then
  again ;
play
