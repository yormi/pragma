#! /bin/bash

# exit on first error
set -o errexit


case "$1" in
  install )
    cat << EOF
You need to install:
  stack
  ghcid
  haskell-language-server (lsp - linter, auto-completion, etc.)
EOF
    ;;

  start )
    ghcid --command 'stack ghci' --test ':main' --warnings --no-height-limit --reverse-errors
    ;;

  test )
    #ENV=test stack test --file-watch
    #ghcid --command 'stack ghci' --test ':main' --warnings --no-height-limit --reverse-errors

    ghcid \
          --command "stack ghci pragma:lib pragma:test:pragma-test --ghci-options=-fobject-code" \
          --warnings \
          --no-height-limit \
          --reverse-errors \
          --test "main"
    ;;

  coverage )
    stack test --coverage
    ;;

  clean )
    stack clean --full
    ;;

  loc )
    cloc src
    ;;

  format )
    mapfile -t _files < <(find . -name "*.hs")
    hindent-elm "${_files[@]}"
    ;;

  *)
    cat << EOF
Usage: $(pwd)
  god install

  god start
  god test
  god coverage

  god clean
  god loc
  god format
EOF
    ;;
esac
