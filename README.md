MapReduce
=========

Map reduce implemented in Haskell, trying to exploit parallelism

To compile:

ghc -O2 -threaded -rtsopts mais.hs

To run:

./main +RTS -Nx

where x is the amount of cores you want to run on.

If you want to see the parallelism exposed you can use ThreadScope (https://www.haskell.org/haskellwiki/ThreadScope), compiling with -eventlog and running with -ls
