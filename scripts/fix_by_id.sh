#!/bin/bash

set -o errexit -o nounset -o pipefail

# Fix the links after renaming a file while keeping the old ID.


for f in "$@"; do
    perl -pi'*.bak' -E 'BEGIN {
        ($id, $new) = shift =~ /^(\d+)_(.*)\.org$/;
    }

    s/\Qfile:${id}_\E.*?\.org/file:${id}_$new.org/g;
    ' "$f" *.org
done

for f in *.org; do
    touch --no-create --reference "$f.bak" "$f"
done
