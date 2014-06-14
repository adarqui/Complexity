Original code by:

    author:        Roel van Dijk
    maintainer:    vandijk.roel@gmail.com
    copyright:     (c) 2009 Roel van Dijk
    license:       BSD3

See examples/simple.hs

Removed the gtk dependencies/code for two reasons:
1. GTK packages kept breaking
2. I don't have a gui

hstats wouldn't compile either: Hence that repo being on my github as well

To build/install:
1. cabal sandbox init
2. cabal-meta install

Tiny modifications:
1. exaamples/simple.hs
2. Test.Complexity.{Base,Utils,Pretty}.hs

Will probably add back in some charts at some point.
