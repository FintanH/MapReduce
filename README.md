MapReduce
=========

Map reduce implemented in Haskell, trying to exploit parallelism

To compile:

ghc -O2 -threaded -rtsopts mais.hs

To run:

./main +RTS -Nx

where x is the amount of cores you want to run on.

If you want to see the parallelism exposed you can use ThreadScope (https://www.haskell.org/haskellwiki/ThreadScope), compiling with -eventlog and running with -ls

Consulted sources include:
Parallel and Concurrent Programming in Haskell by Simon Marlow -- http://chimera.labs.oreilly.com/books/1230000000929
Google’s MapReduce Programming Model — Revisited by Ralf Lämmel -- http://userpages.uni-koblenz.de/~laemmel/MapReduce/paper.pdf
MapReduce as a Monad by Julian Porter -- http://jpembeddedsolutions.files.wordpress.com/2011/04/mapreduce.pdf

