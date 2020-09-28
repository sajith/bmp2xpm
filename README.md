# bmp2xpm

bmp2xpm is an embarrassingly simple image processing program which
converts BMP (v3) files to XPM (v3) files.  I'm writing this to get a
better idea of parallel programming in Haskell.

To build and run this program, do this:

    cabal build
    cabal exec bmp2xpm input.bmp output.xpm

Use a sandbox if you need to.

For a profiling build, do this:

    ghc -O2 -rtsopts -threaded -prof -auto-all Main.hs -o Main.exe

We will need profiling libraries.  We might also need `ghc-prof`.

To review eventlog under ThreadScope, compile the program like below,
run the program, and then open the resulting Main.exe.eventlog file in
ThreadScope:

    ghc -O2 -rtsopts -threaded -eventlog Main.hs -o Main.exe

Note that `-eventlog` and `-prof` are mutually incompatible.
