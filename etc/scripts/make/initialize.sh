#!/bin/bash

source ${DOTDIR_PATH}/etc/vital.sh 2>/dev/null
if ! is_vitalize; then
    echo 'vital.sh not found.'
    exit 1
fi

set -ue

ignores=('.gitkeep')

read -p "Run initialize scripts? (y/n): "
if [ "$REPLY" = 'y' ]; then
    echo
    echo '--- Run initialize scripts ---'
    for file_abs in $(find ${DOTDIR_PATH}/etc/init -type f); do
        filename=$(basename ${file_abs})
        echo ${ignores[@]} | grep -q $filename && continue
        echo
        echo "--- script: $(basename ${file_abs}) ---"
        bash $file_abs
    done
else
    e_error "terminated"
fi
