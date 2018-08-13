#!/bin/sh

set -e

if [ $# -lt 2 ]; then
    # Pass the (erroneous) command straight through to the real cabal
    # which will moan.
    exec cabal "$@"
fi

SUBCMD="$1"
shift

# The build subcommand in Cabal from version 1.20.0.0 supports a -j
# parameter for the number of cores to use. The default is to use the
# number of cores on the machine. There's also a sanity check where if
# you say "-j100" it moans at you, pointing out that would be a crazy
# thing to do. Unfortunately, running without -j on an enormous
# machine hits this error. Aargh!
#
# We decide that Cabal is too old to need "-j" if its version string
# starts with "1.1".
CABAL_DASH_J=""
if [ x"$SUBCMD" = xbuild ]; then
    if cabal --version | \
            grep 'version 1\.1.*of the Cabal library' >/dev/null; then
        # Old Cabal. No -j needed.
        CABAL_DASH_J=""
    else
        if [ ! "$(nproc)" -le 4 ]; then
            CABAL_DASH_J="-j4"
        fi
    fi
fi

exec cabal "$SUBCMD" $CABAL_DASH_J "$@"
