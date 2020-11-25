#!/bin/sh
set -xe

rm ./dist-newstyle/reflex-monad-auth-*-docs.tar.gz

# assumes cabal 2.4 or later
cabal v2-haddock --haddock-for-hackage --enable-doc

cabal upload -d --publish ./dist-newstyle/reflex-monad-auth-*.tar.gz
