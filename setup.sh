#!/bin/bash

for file in .??*
do
    # ignorefile
    [[ "$file" == ".git" ]] && continue

    # linking
    ln -s $HOME/dotfiles/$file $HOME/$file
done
