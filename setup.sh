#!/bin/bash

for file in .??*
do
    [[ "$file" == ".git" ]] && continue
    ln -s $HOME/dotfiles/$file $HOME/$file
done
