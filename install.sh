#!/usr/bin/env bash

echo -e "$(tput setaf 2)-----------------"
echo -e "Bootstraping Dotfiles."
echo -e "-----------------$(tput sgr0)\n"


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

echo "Linking xmonad configs"
ln -nsv ~/.dotfiles/xmonad ~/.xmonad

echo "Linking misc files.."
#ln -sv ~/.dotfiles/gdbinit ~/.gdbinit
ln -nsv ~/.dotfiles/fonts ~/.fonts
ln -sv ~/.dotfiles/TBox.conkyrc ~/.config/conky/TBox.conkyrc
ln -sv ~/.dotfiles/cpu_tbox.lua ~/.config/conky/LUA/cpu_tbox.lua

# source the new setup
source ~/.bashrc

echo -e "\n$(tput setaf 2)-----------------"
echo -e "Installing packages."
echo -e "-----------------$(tput sgr0)\n"

sudo -v # ask admin password upfront

# Keep-alive: update existing `sudo` time stamp until the script has finished.
# while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

if [[ $(which apt-get) == "" ]]; then
    echo -e "$(tput setaf 1)[Error]:$(tput sgr0) Aptitude doesn't seem present."
    echo -e "         Only Debian systems supported."
    exit 1
fi

sudo apt-get update && sudo apt-get upgrade

sudo apt-get install -y coreutils
sudo apt-get install -y findutils
sudo apt-get install -y binutils
sudo apt-get install -y build-essential

sudo apt-get install -y bash-completion

sudo apt-get install -y python
sudo apt-get install -y python3


sudo apt-get install -y vim
sudo apt-get install -y vim-gtk

sudo apt-get install -y silversearcher-ag
sudo apt-get install -y imagemagick


# RVM with ruby
curl -sSL https://get.rvm.io | bash -s stable --ruby --ignore-dotfiles --with-gems="pry, rake"

source ~/.rvm/scripts/rvm
