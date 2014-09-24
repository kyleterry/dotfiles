#!/bin/bash

BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROAMER_BACKUP_DIR=${HOME}/.roamer_backups
DATE=$(date +%s)
INITIAL_WP=RCkQyvn.jpg
APT_PACKAGES=(git mercurial golang python-pip build-essential htop dzen2 tmux
    feh xautolock xmonad xmobar xorg feh)
PIP_PACKAGES=(virtualenvwrapper udiskie beets)
MANIFEST=${1}

if [[ ! ${MANIFEST} ]]; then
    echo "usage: ${0} manifests/manifest.in"
    exit 1
fi

mkdir -p "${ROAMER_BACKUP_DIR}"

echo "==> Reading manifest and linking files..."
while IFS="=" read -r file_source file_destination
do
    file_source_full=${BASE_DIR}/${file_source}
    file_destination_full=${HOME}/${file_destination}
    echo "==> Backing up ${file_destination_full}..."
    if [[ -f ${file_destination_full} ]] || [[ -d ${file_destination_full} ]]; then
        if [[ ! -L ${file_destination_full} ]]; then
            mv "${file_destination_full}" "${ROAMER_BACKUP_DIR}/${file_destination}.bak-${DATE}"
        else
            echo "==> Not a real file!"
            echo "====> Removing old link..."
            rm "${file_destination_full}"
        fi
    fi
    echo "==> Linking ${file_source} -> ${file_destination_full}"
    ln -s "${file_source}" "${file_destination_full}"
done < "${MANIFEST}"

# Make sure ${HOME}/.conf.d exists
if [[ ! -f ${HOME}/.conf.d ]]; then
    echo "==> Creating ${HOME}/.conf.d"
    echo "====> You can symlink scripts to here from ${BASE_DIR}/bash_conf.d"
    mkdir -p "${HOME}/.conf.d"
fi

# Make sure ${HOME}/.vimtmp exists
if [[ ! -f ${HOME}/.vimtmp ]]; then
    echo "==> Creating ${HOME}/.vimtmp"
    mkdir -p "${HOME}/.vimtmp"
fi

# Make sure ${HOME}/bin exists
if [[ ! -f ${HOME}/bin ]]; then
    echo "==> Creating ${HOME}/bin"
    cp -r "${BASE_DIR}/bin ${HOME}"
fi

if [[ -d ${BASE_DIR}/install_hooks.d ]]; then
    echo "==> Sourcing install hooks"
    for item in ${BASE_DIR}/install_hooks.d/*; do
        if [[ -x ${item} ]]; then
            source "${item}"
        fi
    done
fi

echo "==> Updating submodules..."
"${BASE_DIR}"/submodules.sh | grep 'Entering' | awk '{print $2}'

if [ -f /etc/debian_version ]; then
    echo "==> Sniffed Debian; Installing packages"
    sudo apt-get update
    sudo apt-get install -y "${APT_PACKAGES[@]}"
    sudo pip install "${PIP_PACKAGES[@]}"
    echo "==> Setting wallpaper"
    feh --bg-scale "${HOME}/.wallpaper/${INITIAL_WP}"
fi
