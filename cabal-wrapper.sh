#!/bin/sh

set -e

if [ $# -lt 2 ]; then
    # Pass the (erroneous) command straight through to the real cabal
    # which will moan.
    exec cabal "$@"
fi

SUBCMD="$1"
shift

# The build subcommand in Cabal version 2.x supports a -j parameter
# for the number of cores to use. The default is to use the number of
# cores on the machine. There's also a sanity check where if you say
# "-j100" it moans at you, pointing out that would be a crazy thing to
# do. Unfortunately, running without -j on an enormous machine hits
# this error. Aargh!
CABAL_DASH_J=""
if [ x"$SUBCMD" = xbuild ]; then
    CABAL_MAJOR="$(cabal --version | grep 'of the Cabal library' | \
                    sed 's/compiled using version \([^ .]*\).*/\1/')"
    if [ x"$CABAL_MAJOR" = x2 ]; then
        if [ ! "$(nproc)" -le 40 ]; then
            CABAL_DASH_J="-j40"
        fi
    fi
fi

exec cabal "$SUBCMD" $CABAL_DASH_J "$@"
