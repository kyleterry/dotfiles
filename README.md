# Dotfiles or Something

Be careful

## Installation

`install.sh` will attempt to backup your dotfiles and then link all my dotfiles.

Create a manifest.in file structured like so:

```manifest
_bashrc=.bashrc
_vim=.vim
_vimrc=.vimrc
```

Anything in manifest.in will be symlinked into your home directory.

```bash
$ git clone https://github.com/kyleterry/new-dotfiles
$ cd new-dotfiles
$ ./install.sh manifest.in
```

Cool.
