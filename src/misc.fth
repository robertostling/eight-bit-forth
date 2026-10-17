: vice-key kernal-key
  dup 23 = so drop 3b ;
make key vice-key ;
: skip-interpret
  0 zend @w  0 zinput !w ;
: confirm? ." sure? [y/n]"
  key dup emit [char] y = ;
