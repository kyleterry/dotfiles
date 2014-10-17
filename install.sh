#!/bin/bash

base_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
roamer_backup_dir=${HOME}/.roamer_backups
_date=$(date +%s)
initial_wp=RCkQyvn.jpg
declare -a apt_packages=(git mercurial golang python-pip build-essential htop dzen2 tmux
    feh xautolock xmonad xmobar xorg feh)
declare -a pip_packages=(virtualenvwrapper udiskie beets)
manifest=${1}

if [[ ! ${manifest} ]]; then
    echo "usage: ${0} manifests/manifest.in"
    exit 1
fi

mkdir -p "${roamer_backup_dir}"

echo "==> Reading manifest and linking files..."
while IFS="=" read -r file_source file_destination
do
    file_source_full=${base_dir}/${file_source}
    file_destination_full=${HOME}/${file_destination}
    echo "==> Backing up ${file_destination_full}..."
    if [[ -f ${file_destination_full} ]] || [[ -d ${file_destination_full} ]]; then
        if [[ ! -L ${file_destination_full} ]]; then
            mv "${file_destination_full}" "${roamer_backup_dir}/${file_destination}.bak-${_date}"
        else
            echo "==> Not a real file!"
            echo "====> Removing old link..."
            rm "${file_destination_full}"
        fi
    fi
    echo "==> Linking ${file_source} -> ${file_destination_full}"
    ln -s "${file_source}" "${file_destination_full}"
done < "${manifest}"

# Make sure ${HOME}/.conf.d exists
if [[ ! -f ${HOME}/.conf.d ]]; then
    echo "==> Creating ${HOME}/.conf.d"
    echo "====> You can symlink scripts to here from ${base_dir}/bash_conf.d"
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
    cp -r "${base_dir}/bin ${HOME}"
fi

if [[ -d ${base_dir}/install_hooks.d ]]; then
    echo "==> Sourcing install hooks"
    for item in ${base_dir}/install_hooks.d/*; do
        if [[ -x ${item} ]]; then
            source "${item}"
        fi
    done
fi

echo "==> Updating submodules..."
"${base_dir}"/submodules.sh | grep 'Entering' | awk '{print $2}'

if [[ -f /etc/debian_version ]]; then
    echo "==> Sniffed Debian; Installing packages"
    sudo apt-get update
    sudo apt-get install -y "${apt_packages[@]}"
    sudo pip install "${pip_packages[@]}"
    echo "==> Setting wallpaper"
    feh --bg-scale "${HOME}/.wallpaper/${initial_wp}"
fi
