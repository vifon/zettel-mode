#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

# Fix the links after adding an ID prefix to a given file.  The rest
# of the filename must not have changed.


for f in "$@"; do
    perl -pi'*.bak' -E 'BEGIN {
        $new = shift;
        $old = $new =~ s/^\d+_//r;
    }

    s/\Qfile:$old\E/file:$new/g;
    ' "$f" *.org
done

for f in *.org; do
    touch --no-create --reference "$f.bak" "$f"
done
