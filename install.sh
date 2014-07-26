#!/bin/bash

BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROAMER_BACKUP_DIR=${HOME}/.roamer_backups
DATE=$(date +%s)
MANIFEST=${1}
if [[ ! ${MANIFEST} ]]; then
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
    if [[ -f ${file_destination_full} ]] || [[ -d ${file_destination_full} ]]; then
        if [[ ! -L ${file_destination_full} ]]; then
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

# Make sure ${HOME}/.conf.d exists
if [[ ! -f ${HOME}/.conf.d ]]; then
    mkdir -p ${HOME}/.conf.d
fi

# Make sure ${HOME}/.vimtmp exists
if [[ ! -f ${HOME}/.vimtmp ]]; then
    mkdir -p ${HOME}/.vimtmp
fi

# Make sure ${HOME}/bin exists
if [[ ! -f ${HOME}/bin ]]; then
    cp -r ${BASE_DIR}/bin ${HOME}
fi

if [[ -d ${BASE_DIR}/install_hooks.d ]]; then
    for item in ${BASE_DIR}/install_hooks.d/*; do
        if [[ -x ${item} ]]; then
            . ${item}
        fi
    done
fi

echo "Updating submodules..."
${BASE_DIR}/submodules.sh | grep 'Entering' | awk '{print $2}'

echo "Installing packages"
if [ -f /etc/debian_version ]; then
    sudo apt-get update
    sudo apt-get install git mercurial golang python-pip build-essential htop dzen2 tmux feh xautolock
    sudo pip install virtualenvwrapper
fi
