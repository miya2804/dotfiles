#!/bin/bash

DOT_FILES=(.bashrc .emacs.d .gitignore .gitconfig)

for file in ${DOT_FILES[@]}
do
    ln -s $HOME/dotfiles/$file $HOME/$file
done
