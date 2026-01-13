# eight-bit-forth

This is (yet another) Forth environment for the Commodore 64 computer,
with a few features that together makes it one-of-a-kind system:

 * 8-bit data stack for minimal overhead
 * subroutine threading with some optimizations, including tail call
   elimination, and inlining for some words
 * minimalistic assembly language kernel with only 24 built-in words
 * nearly the whole system compiles itself at start, from a compressed source
   block attached to the kernel binary
 * very fast compiler using hashed dictionaries
 * simple editor capable of saving and loading from disk and tape
 * you can change the code of the core system and recompile in a couple of
   seconds -- while running!

When you run the executable, which also includes the compressed Forth
source code of the system, the following takes place within the span
of about four seconds:

 0. The kernel generates the AES S-box in memory, used for Forth
    dictionary hashing.
 1. The kernel uncompresses the source code into memory, and starts
    compiling it.
 2. A simple assembler is compiled.
 3. A more complete vocabulary is defined using the assembler, which
    allows definitions in high-level Forth.
 4. Libraries and applications are compiled using mostly high-level
    Forth, including the editor.

Steps 2, 3 and 4 are technically the same thing: creating new Forth
definitions from the code in `kernel.fth`. This type of flexibility
and mixing of low- and high-level code is one of the strengths of
Forth.

Also included (as separate source files) is a library for cooperative
multitasking, a sound and graphics library, and a morse code
generator.


## Running some demos

You need to install the Debian packages `vice` and `acme`, or
corresponding.

If you want to try this in the VICE emulator type `make run`. If you
want to run on a real C64, type `make forth.d64` and copy the disk
image to a floppy or suitable cartridge. I have also included a turbo
tape loader, so that I can use my Kung Fu Flash cartrige as a disk
emulator, then saving everything to tape so that the whole system can
run from a C64 with only a tape unit.

You will see messages while the system is compiling itself for a few
second, then enter the editor.

The editor divides the screen into 16 edit buffer rows, and 8 command
rows where `ENTER` runs the code on the current line.  First, press
`SHIFT+ENTER` to go down to the command section. Then type:

    DISK LOAD VICSID RUN

This loads the source code for the graphics and sound library, and
runs it (thus defining a bunch of useful words).

    LOAD MORSE RUN

This loads the source code for the morse code generation, and compiles
it. Now, you can type:

    MORSE-DEMO

You should hear noise, and a call from me (SM0YSR) in morse code. Once
you get tired of the noise, you can silence it:

    0 VOLUME

To test the multitasking, you can import the `tasks` library and then
define a word that turns out the volume briefly every third second,
and run that in the background:

    LOAD TASKS RUN

    : ANNOY
      F VOLUME 10 JIFFYS
      0 VOLUME 3 SECONDS
      ANNOY ;

    ' ANNOY LAUNCH
    VAR ANNOY-PID

While this is running, you can work with the editor and do other
things in the foreground. For instance, you can entertain yourself by
looking at the most recently loaded source file in the editor. Use F5
and F7 for page up and page down.

To kill the process, pass the process ID that was saved above to the
word `kill`:

    ANNOY-PID KILL

We use `VAR` rather than `CONSTANT` because it is shorter and such
values can be updated with `TO`. What would normally be expressed like
this, if both X and Y are defined using `VARIABLE`:

    X @ TRANSFORM Y !

could be expressed using (inside colon definitions) in the following
way, assuming X and Y are both defined with `VAR`:

    X TRANSFORM TO Y


## Technical highlights

The assembly kernel consists of only 24 words:

    ( .( CHAR , [ MOVE> <MOVE ] : ; + 1-W ' DOVAR
    CREATE LIT LITERAL TYPE WORD
    RTS ERROR KERNEL-SOURCE UNCOMPRESS INTERPRET

Any one- or two-letter word that can be interpreted as a hexademical
number is created as such.

Note that the RTS instruction actually belongs to the assembler (see
below), but is included in the kernel in order to allow tail call
elimination to be performed on the assembler definitions
themselves. This saves quite a lot of space and time, since the
sequence

    JSR xxx
    RTS

is replaced by a simple jump:

    JMP xxx

The W suffix in `1-W` is used to indicate (16-bit) word operations,
its stack diagram is

    1-W ( XL XH -- YL YH )

where XH:XL - 1 == YH:YL.


### Assembler

The assembler is very simplistic, and at this point does not have any
means of computing branches. Below follows parts of `kernel.fth` with
commentary.

    : ,2 , , ;
    : ,3 , , , ;
    ( USE WITH IMM )
    : ORA 0 ;
    : ANDA 20 ;
    : EORA 40 ;
    : ADC 60 ;
    : STA 80 ;
    : LDA A0 ;
    : CMP C0 ;
    : SBC E0 ;
    ( USE WITH IMPL )
    : ASL 1 ;
    : ROL 21 ;
    : LSR 41 ;
    : ROR 61 ;
    ( USE WITH NEITHER )
    : DEC C1 ;
    : INC E1 ;
    ( ADDRESSING MODES )
    : IMPL 9 + , ;
    : IMM 9 + ,2 ;
    : ADR D + ,3 ;
    : ZP 5 + ,2 ;
    : (X) 1 + ,2 ;
    : (Y) 11 + ,2 ;
    : ZP,X 15 + ,2 ;
    : ADR,Y 19 + ,3 ;
    : ADR,X 1D + ,3 ;
    ( BRANCHING )
    : BPL 10 ,2 ;
    : BMI 30 ,2 ;
    : BVC 50 ,2 ;
    : BVS 70 ,2 ;
    : BCC 90 ,2 ;
    : BCS B0 ,2 ;
    : BNE D0 ,2 ;
    : BEQ F0 ,2 ;
    : BRK 00 , ;
    : JSR 20 ,3 ; 
    : RTI 40 , ;
    ( RTS HAS BUILT-IN OPTIMIZING WORD )
    : JMP 4C ,3 ;
    : JMPI 6C ,3 ;
    : NOP EA , ;
    : JAM 02 , ;
    ( ZERO-OPERAND INSTRUCTIONS )
    : PHP 08 , ;
    : CLC 18 , ;
    : PLP 28 , ;
    : SEC 38 , ;
    : PHA 48 , ;
    : CLI 58 , ;
    : PLA 68 , ;
    : SEI 78 , ;
    : DEY 88 , ;
    : TYA 98 , ;
    : TAY A8 , ;
    : CLV B8 , ;
    : INY C8 , ;
    : CLD D8 , ;
    : INX E8 , ;
    : SED F8 , ;
    : TXA 8A , ;
    : TXS 9A , ;
    : TAX AA , ;
    : TSX BA , ;
    : DEX CA , ;
    ( VARIOUS INSTRUCTIONS WITH NON-UNIFORM
      ADDRESSING MODES )
    : LDXIMM A2 ,2 ;
    : LDXZP A6 ,2 ;
    : LDYIMM A0 ,2 ;
    : STYZP 84 ,2 ;
    : STYZP,X 94 ,2 ;
    : LDYZP A4 ,2 ;
    : LDYZP,X B4 ,2 ;
    : STYADR 8C ,3 ;
    : LDYADR AC ,3 ;
    : LDYADR,X BC ,3 ;
    : STXZP 86 ,2 ;
    : STXZP,Y 96 ,2 ;
    : STXADR 8E ,3 ;
    : CPYIMM C0 ,2 ;
    : CPXIMM E0 ,2 ;
    : CPYZP C4 ,2 ;
    : CPXZP E4 ,2 ;
    : CPYADR CC ,3 ;
    : CPXADR EC ,3 ;
    : BITZP 24 ,2 ;
    : BITADR 2C ,3 ;


### Core system

The assembler words can be used as macros in Forth definitions, and at
this point most of the words defined are meant to be used as
macros. We define some zero-page locations as constants, and some
operations to transfer data to and from the top stack elements.

Note that the word `VAR` (typically named `CONSTANT`) is not defined
yet, so we define constants using full colon definitions.

    : STACK> CE ;
    : STACK-1 CD FF ;
    : STACK STACK> 0 ;
    : SBOX> CD ;
    : ZHERE 2 ;
    : ZLAST 4 ;
    : ZINPUT 6 ;
    : ZEND 8 ;
    : ZSTATE C ;
    : ZOPTIMIZE D ;
    : ZTASK E ;
    : ZTEMP 13 ;
    : ZSAVE 1E ;
    : ZMREG 20 ;
    : ZNREG 22 ;
    ( TEMP ACCESS MACROS )
    : T ZTEMP ;
    : T+ ZTEMP + ;
    : A>T+ T+ STA ZP ;
    : T+>A T+ LDA ZP ;
    : A>T T STA ZP ;
    : T>A T LDA ZP ;
    ( STACK OPERATIONS AND MACROS )
    : SWAP [
      STACK LDA ADR,X
      PHA
      STACK> 1 LDA ADR,X
      STACK STA ADR,X
      PLA
      STACK> 1 STA ADR,X ] ;
    ( NOTE: 1-BASED ADDRESSING )
    : S>A STACK> SWAP FF +  LDA ADR,X ;
    : A>S STACK> SWAP FF +  STA ADR,X ;
    : >A STACK LDA ADR,X  INX ;
    : A> DEX  STACK STA ADR,X ;
    ( SAME AS >A BUT SETS FLAGS AND COSTS
      1 MORE CYCLE )
    : PRE>A INX  STACK-1 LDA ADR,X ;
    : TOS>A 1 S>A ;
    : A>TOS 1 A>S ;
    : NOS>A 2 S>A ; 
    : A>NOS 2 A>S ;
    : #A>TOS LDA IMM  A>TOS ;


### Defining the core of Forth

This whole project is a huge chicken-and-egg problem, so we need to define some core compiler words in assembler.

    ( COMPILER WORDS )
    : IMMEDIATE [
      2 LDYIMM
      80 LDA IMM
      ZLAST ORA (Y)
      ZLAST STA (Y) ] ;
    : ON FF ;
    : OFF 0 ;
    : COMPILING [ ZSTATE LDA ZP  A> ] ;
    : COMPILER [ >A  ZSTATE STA ZP ] ;

The words `SO` and its inverse `SKIP` are very simple flow control
words, they are simply compiling conditional return instructions. This
means that they can be used both as partial replacements for
`IF/ELSE/THEN` and together with tail recursion to implement simple
loops.

    : SO PRE>A 1 BNE RTS ; IMMEDIATE
    : SKIP PRE>A 1 BEQ RTS ; IMMEDIATE

Below are definitions of core stack manipulation words. Later we will
re-implement some of them with the ability of doing inlining, to avoid
things like compiling a JSR instruction to `DROP`, a
single-instruction word.

    ( STACK OPERATIONS )
    : DUP [ TOS>A A> ] ;
    : DROP [ INX ] ;
    : OVER [ NOS>A A> ] ;
    : OOVER [ 3 S>A A> ] ;
    : NIP [ TOS>A A>NOS INX ] ;
    : SP@ [ TXA A> ] ;
    : SP! [ >A TAX ] ;
    : RP@ [
      TXA PHA TSX TXA TAY PLA TAX TYA A>
      ] ;
    : RP! [
      TXA TAY  TOS>A TAX TXS
      TYA TAX INX ] ;
    : >R >A PHA ; IMMEDIATE
    : R> PLA A> ; IMMEDIATE
    : R>DROP PLA ; IMMEDIATE
    : R@ PLA PHA A> ; IMMEDIATE
    : -ROT [
      TOS>A PHA
      NOS>A A>TOS
      3 S>A A>NOS
      PLA 3 A>S ] ;
    : ROT -ROT -ROT ;
    : ?DUP [ TOS>A  1 BNE  RTS  A> ] ;
    : 1+ [ STACK INC ADR,X ] ;
    : 1- [ STACK DEC ADR,X ] ;
    : 2* [ STACK ASL ADR,X ] ;
    : 2/ [ STACK LSR ADR,X ] ;
    : 2*W [
      STACK ASL ADR,X
      STACK> 1 ROL ADR,X ] ;
    : DUPW OVER OVER ;
    : OVERW [ 4 S>A A>  4 S>A A> ] ;
    : SWAPW OVERW [
      3 S>A 5 A>S  4 S>A 6 A>S
      TOS>A 3 A>S  NOS>A 4 A>S  INX INX ] ;
    ( NEEDED EARLY, REST DEFINED BELOW )
    : +W [
      CLC
      3 S>A  STACK> 0 ADC ADR,X  3 A>S
      4 S>A  STACK> 1 ADC ADR,X  4 A>S
      INX INX ] ;
    ( ZERO-PAGE MACROS )
    : >ZPW ( ZP -- )
      DUP  >A STA ZP  >A 1+ STA ZP ;
    : ZP>W ( ZP -- )
      DUP  1+ LDA ZP A>  LDA ZP A> ;
    : +ZPW ( ZP N -- )
      LDA IMM  CLC  DUP ADC ZP  DUP STA ZP
      2 BCC  1+ INC ZP ;
    : -ZPW ( ZP N -- )
      OVER LDA ZP  SEC  SBC IMM  DUP STA ZP
      2 BCS  1+ DEC ZP ;
    : 1+ZPW ( ZP -- )
      DUP  INC ZP  2 BNE  1+ INC ZP ;
    ( COMPILER WORDS )
    : HERE 0 ZHERE ;
    : HERE@ [ ZHERE LDA ZP  A> ] ;
    : HERE@W
      [ ZHERE 1 + LDA ZP  A> ] HERE@ ;
    : HERE!W [
      >A  ZHERE STA ZP
      >A  ZHERE 1 + STA ZP ] ;
    : >CODE 0 3 +W ;
    : >FLAGS 0 2 +W ;
    : >LAST ;

We use the `MAKER` and `MAKE` words to enable changing word
definitions during execution, without recompiling. See below for the
definition of `MAKE` and further explanation.

    ( USED FOR HOT-SWAPPING, SEE MAKE ) 
    : MAKER HERE@W 0 3 +W JMP ; IMMEDIATE
    : KERNAL-EMIT [ >A  FF D2 JSR ] ;
    : EMIT MAKER KERNAL-EMIT ;

We implement `CASE` as a simple comparison and conditional jump. See
the implementation of the editor below for an example of how it is
used.

    : CASE
      >A
      STACK CMP ADR,X
      4 BNE
      INX  ' >CODE JMP ; IMMEDIATE

Here we re-define a few very short words to make them state-smart, and
either inline the code (if compiling) or running the corresponding
operation (if interpreting). Some compile-only words are also always inlined.

    : DROP
      COMPILING IF INX
      ELSE [ INX ] THEN ; IMMEDIATE
    : DROPW
      COMPILING IF INX INX
      ELSE [ INX INX ] THEN ; IMMEDIATE
    ( RETURN STACK 16-BIT OPS )
    : DUP>RW
      NOS>A PHA TOS>A PHA ; IMMEDIATE
    : R>DROPW PLA PLA ; IMMEDIATE
    : >RW 
      NOS>A PHA TOS>A PHA
      INX INX ; IMMEDIATE
    : R>W
      DEX DEX
      PLA A>TOS PLA A>NOS
      ; IMMEDIATE
    : R@W
      DEX DEX
      PLA A>TOS TAY
      PLA PHA A>NOS
      TYA PHA ; IMMEDIATE

`LETW ( 'WORD X A -- )` temporarily lets the value of the variable at
address A be X while executing the word at a given address. The old
value at A is temporarily stored on the return stack while WORD is
executed.

    : LETW
      DUPW DUP>RW @W >RW
      !W EXECUTE
      R>W R>W !W ;

Words defined with `MAKER` can have their implementation changed
later, by overwriting a jump address. For instance, one could use
`MAKE EMIT ...` to change the behavior of `EMIT` to the code
represented by `...`. Note that `MAKE` is state-smart and works both
when compiling and interpreting.

This can be restored to the original definition with `UNMAKE`.

The word `PATCH` can be used to change the definition of any word, not
just those defined with `MAKER`, by overwriting its first three bytes
with a jump instruction to the new address. This can not be undone,
but is handy for testing, especially when the corresponding source
code is still in RAM and can be recompiled.

A more advanced word is MAKING, which can be used like:
`' TYPE MAKING EMIT SLOW-EMIT`
which would call `TYPE` with `EMIT` redefined as `SLOW-EMIT`.

    ( MAKE AND DYNAMIC RELINKING )
    : PATCH
      ' >CODE  DUPW 4C -ROT ! ( JMP )
      1+W HERE@W SWAPW !W  ON COMPILER ;
    : DOMAKE
      R>W 1+W DUPW @W SWAPW
      1+W 1+W SWAPW !W ;
    ( USE ONLY WITH MAKER WORDS )
    : >MAKE >CODE 1+W ;
    : MAKE
      COMPILING IF
        POSTPONE DOMAKE  ' >MAKE ,2
      ELSE PATCH THEN ; IMMEDIATE
    : UNMAKE ' >MAKE
      DUPW 1+W 1+W SWAPW !W ;
    : MAKING ' >MAKE ' >CODE SWAPW LETW ;

The `CALL-PARENT` word performs a call to the return address of the
word calling it, and can be used multiple times to execute the
remainder of the calling word repeatedly. This makes it convenient to
define loop constructions like `FOR-EACH` and `TIMES` below. As an
example, you can use `TIMES` like this for a succinct loop:
`: CHEER 4 TIMES ." HOORAY!" CR ;`

    ( ADDITIONAL LOOPING CONSTRUCTS )
    : CALL-PARENT [
      ZTEMP STXZP  TSX
      01 04 LDA ADR,X  PHA
      01 03 LDA ADR,X  PHA
      ZTEMP LDXZP ] ;
    : FOR-EACH
      ?DUP 0= IF R>DROPW DROPW EXIT THEN
      -ROT DUPW 1+W SWAPW @
      CALL-PARENT
      ROT 1- FOR-EACH ;
    : TIMES
      ?DUP 0= IF R>DROPW EXIT THEN
      CALL-PARENT
      1- TIMES ;
