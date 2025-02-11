set -eo pipefail

cd "${0%/*}"; # Go to the directory of the script

### Updating Math.elm
which g++ &>/dev/null || { echo "g++ missing" >&2; exit 1; }
mkdir -p bin
if ! [ -f bin/parser ] || [ $(date -r bin/parser '+%s') -lt $(date -r grammar-parser/main.cpp '+%s') ]; then
    g++ -std=c++20 -o bin/parser grammar-parser/main.cpp
fi
( # start new process, so we can run trap as a defer
    temp_file="$(mktemp)"
    trap 'rm -f "${temp_file}"' EXIT ERR
    sed -n 'p; /^-- BEGIN AUTO GENERATED/q;' src/Math.elm > "${temp_file}"
    bin/parser grammar-parser/grammar.txt >> "${temp_file}"
    mv -f "${temp_file}" src/Math.elm
)