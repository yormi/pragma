#! /bin/bash

# exit on first error
set -o errexit


case "$1" in
  install )
    cat << EOF
You need to install:
  stack
  ghcid
EOF
    ;;

  start )
    ghcid --command 'stack ghci' --test ':main' --warnings --no-height-limit --reverse-errors
    ;;

  *)
    cat << EOF
Usage: $(pwd)
  god install
  god start
EOF
    ;;
esac
