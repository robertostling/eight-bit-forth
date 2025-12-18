# eight-bit-forth

This is (yet another) Forth environment for the Commodore 64 computer,
with a few features that makes it one-of-a-kind:

 * 8-bit data stack, with double-size operators for 16-bit arithmetic
 * minimalistic assembly language kernel with only 24 built-in words
 * native code compilation, including tail call elimination
 * nearly the whole system compiles itself at start
 * kernel supports source code compression
 * very fast compiler using hashed dictionaries

When you run the executable, which also includes the compressed Forth
source code of the system, the following takes place within the span
of about four seconds:

 1. The kernel uncompresses the source code into memory, and starts
    compiling it.
 2. A simple assembler is compiled.
 3. A more complete vocabulary is defined using the assembler, which
    allows definitions in high-level Forth.
 4. Libraries and applications are compiled using mostly high-level
    Forth, including an editor, cooperative multitasking, and a morse
    code generator.

Steps 2, 3 and 4 are technically the same thing: creating new Forth
definitions from the code in `kernel.fth`. This type of flexibility
and mixing of low- and high-level code is one of the strengths of
Forth.


## Running some demos

You need to install `vice` and `acme`.

If you want to try this in an emulator, ensure that you have `vice`
installed and type `make run`. If you want to run on a real C64, type
`make forth.d64` and copy the disk image to a floppy or suitable
cartridge.

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

To test the multitasking, you can define a word that turns out the
volume briefly every third second, and run that in the background:

    LOAD TASKS RUN

    : ANNOY
      F VOLUME 10 JIFFYS
      0 VOLUME 3 SECONDS
      ANNOY ;

    ' ANNOY LAUNCH

While this is running, you can work with the editor and do other
things in the foreground. For instance, you can entertain yourself by
looking at the most recently loaded source file in the editor. Use F5
and F7 for page up and page down.
