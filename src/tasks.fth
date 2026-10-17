( multi-tasking )
4 var max-tasks
( 2 bytes per task, storing x and s
  x = 0 indicates slot is not used )
create tasks max-tasks 2* allot
max-tasks 1- 2* 0 ztask !
: active-task [
  ztask ldyzp
  iny iny
  max-tasks 2* cpyimm
  2 bne
  0 ldyimm
  ztask styzp
  tasks lda adr,y
  1 beq
  rts ]
  active-task ;
: free-task [
  ztask ldyzp
  iny iny
  max-tasks 2* cpyimm
  2 bne
  0 ldyimm
  ztask styzp
  tasks lda adr,y
  1 bne
  rts ]
  free-task ;
: next-task [
  call active-task
  iny
  tasks lda adr,y  tax txs
  dey
  tasks lda adr,y  tax ] ;
: yield [
  ztask ldyzp
  txa  tasks sta adr,y
  iny
  tsx txa  tasks sta adr,y
  jump next-task ] ;
: terminate [
  ztask ldyzp
  0 lda imm
  tasks sta adr,y
  iny
  tasks sta adr,y
  jump next-task ] ;
: launch ( w:word -- n ) >code [
  s>tw inx
  ztask ldyzp
  ztemp 2 + stxzp
  txa  tasks sta adr,y
  iny
  tsx txa  tasks sta adr,y
  call free-task
  ztemp 2 + ldxzp  tya lsr impl a>tos
  iny iny tya
  asl impl asl impl asl impl
  asl impl asl impl sec 4 sbc imm
  tax
  txs
  ' terminate >code 1-w
  swap  lda imm pha  lda imm pha
  0 ztemp jmpi ] ;
: kill ( n -- ) [
  >a  asl impl  tay  0 lda imm
  tasks sta adr,y
  iny
  tasks sta adr,y ] ;
make jiffys 0 swap jiffy@w +w
  begin yield  jiffy@w overw >=w until
  dropw ;
make key
  begin
    kernal-getin ?dup
    skip yield
  again ;
make emit kernal-emit yield ;
