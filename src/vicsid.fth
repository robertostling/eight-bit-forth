( c64 vic and sid )
: foreground 02 86 ;
: background d0 21 ;
: border d0 20 ;
: black 0 ; : white 1 ;  : red 2 ;
: cyan 3 ;  : purple 4 ; : green 5 ;
: blue 6 ;  : yellow 7 ; : orange 8 ;
: brown 9 ; : pink a ;
: dark-grey b ;   : grey c ;
: light-green d ; : light-blue e ;
: light-grey f ;
( sid )
: voice1 0 ;
: voice2 7 ;
: voice3 0e ;
: noise 80 ;
: pulse 40 ;
: sawtooth 20 ;
: triangle 10 ;
: gate 1 ;
: low-pass 10 ;
: band-pass 20 ;
: high-pass 40 ;
: frequency d4 swap !w ;
: control d4 swap 4 + ! ;
: duty d4 swap 2 + !w ;
: filter-frequency d4 15 !w ;
: decay-attack ( dec att voice -- )
  -rot 4 << or swap d4 swap 5 + ! ;
: release-sustain ( rel sus voice -- )
  -rot 4 << or swap d4 swap 6 + ! ;
: filter-control d4 17 ! ;
variable sid-volume
variable sid-filter
: filter-volume
  sid-volume @ sid-filter @ or
  d4 18 ! ;
: filters ( bitmask -- )
  sid-filter ! filter-volume ;
: volume sid-volume ! filter-volume ;

