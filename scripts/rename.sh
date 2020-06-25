#!/bin/bash

set -o errexit -o nounset -o pipefail

# Rename a file and fix the links to it.


mv "$1" "$2"

perl -pi'*.bak' -E 'BEGIN {
    $old = shift;
    $new = shift;
}

s/\Qfile:$old\E/file:$new/g;
' "$1" "$2" *.org

for f in *.org; do
    touch --no-create --reference "$f.bak" "$f"
done
