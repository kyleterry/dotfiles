#!/bin/bash

BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROAMER_BACKUP_DIR=${HOME}/.roamer_backups
DATE=$(date +%s)
MANIFEST=${1}
if [ ! ${MANIFEST} ]; then
    echo "usage: ${0} manifests/manifest.in"
    echo "\t manifest.in files are required"
    exit 1
fi

mkdir -p ${ROAMER_BACKUP_DIR}

echo "Reading manifest and linking files..."
for item in $(cat ${MANIFEST}); do
    set -- $(echo ${item} | tr '=' ' ')
    file_source=${BASE_DIR}/${1}
    file_destination=${2}
    file_destination_full=${HOME}/${file_destination}
    echo "Backing up ${file_destination_full}..."
    if [ -f ${file_destination_full} ]; then
        if [ ! -L ${file_destination_full} ]; then
            mv ${file_destination_full} ${ROAMER_BACKUP_DIR}/${file_destination}.bak-${DATE}
        else
            echo "Not a real file!"
            echo "Removing old link..."
            rm ${file_destination_full}
        fi
    fi
    echo "Linking ${file_source} -> ${file_destination_full}"
    ln -s ${file_source} ${file_destination_full}
done

echo "Updating submodules..."
${BASE_DIR}/submodules.sh | grep 'Entering' | awk '{print $2}'
