#!/usr/bin/env bash

echo "Linking Bash configs.."
ln -sv ~/.dotfiles/bash/profile ~/.profile
ln -sv ~/.dotfiles/bash/bash_profile ~/.bash_profile
ln -sv ~/.dotfiles/bash/bashrc ~/.bashrc
ln -sv ~/.dotfiles/bash/bash_aliases ~/.bash_aliases
ln -sv ~/.dotfiles/bash/bash_helpers ~/.bash_helpers
ln -sv ~/.dotfiles/bash/bash_logout ~/.bash_logout

echo "Linking Git configs.."
ln -sv ~/.dotfiles/git/gitconfig ~/.gitconfig

echo "Linking Vim configs.."
ln -sv ~/.dotfiles/vimfiles/vim ~/.vim
ln -sv ~/.dotfiles/vimfiles/vimrc ~/.vimrc
ln -sv ~/.dotfiles/vimfiles/gvimrc ~/.gvimrc

echo "Linking misc files.."
#ln -sv ~/.dotfiles/gdbinit ~/.gdbinit
ln -sv ~/.dotfiles/fonts ~/.fonts
