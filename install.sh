#!/usr/bin/env bash

echo "Linking Bash configs.."
ln -sv ~/.dotfiles/bash/profile ~/.profile
ln -sv ~/.dotfiles/bash/bash_profile ~/.bash_profile
ln -sv ~/.dotfiles/bash/bashrc ~/.bashrc
ln -sv ~/.dotfiles/bash/bash_aliases ~/.bash_aliases
ln -sv ~/.dotfiles/bash/bash_helpers ~/.bash_helpers
ln -sv ~/.dotfiles/bash/bash_logout ~/.bash_logout
ln -sv ~/.dotfiles/bash/bash_prompt ~/.bash_prompt
ln -sv ~/.dotfiles/bash/git_prompt.sh ~/.git_prompt.sh

echo "Linking Git configs.."
ln -sv ~/.dotfiles/git/gitconfig ~/.gitconfig

echo "Linking Vim configs.."
ln -nsv ~/.dotfiles/vimfiles/vim ~/.vim
ln -sv ~/.dotfiles/vimfiles/vimrc ~/.vimrc
ln -sv ~/.dotfiles/vimfiles/gvimrc ~/.gvimrc

echo "Linking misc files.."
#ln -sv ~/.dotfiles/gdbinit ~/.gdbinit
ln -nsv ~/.dotfiles/fonts ~/.fonts
ln -sv ~/.dotfiles/TBox.conkyrc ~/.config/conky/TBox.conkyrc
ln -sv ~/.dotfiles/cpu_tbox.lua ~/.config/conky/LUA/cpu_tbox.lua
