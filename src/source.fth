( source code management )
variablew source-adr
: active ( w:source -- )
  source-adr !w ;
variable device
: tape 1 device ! ;
: disk 8 device ! ;
disk
: source ( w:adr -- )
  create dupw dupw ,2 ,2 ,2 ;
: setup-load/save
  1 device @ 0 kernal-setlfs
  word kernal-setnam ;
: scratch ( -- )
  device @  word  scratch-file ;
: load-to ( w:start -- w:end err )
  setup-load/save kernal-load ;
: save-from ( w:start w:end -- err f )
  setup-load/save kernal-save ;
: source-end source-adr @w 0 2 +w ;
: source-offset source-adr @w 0 4 +w ;
: source-start source-adr @w ;
: source-range
  source-start @w source-end @w ;
: load ( -- )
  source-start @w load-to if
    ." load error " . cr exit
  then source-end !w ;
: save ( -- )
  source-range save-from if
    ." save error " . cr exit
  then drop ;
: run source-range interpret ;