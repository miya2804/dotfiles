#!/bin/bash

SCRIPT_DIR=$(cd $(dirname $0); pwd)

echo dotfiles setting.
echo
echo -------------- linking --------------
for file_path in `\find $SCRIPT_DIR/ln -mindepth 1 -maxdepth 1`;
do
    read file_name < <(echo $file_path | sed -e "s:^$SCRIPT_DIR/ln/::")
    if [ -e $HOME/$file_name ]; then
	    if [ -d $file_path ]; then
	        echo "[FAILED] Directory exists: $HOME/$file_name"
	    else
	        echo "[FAILED] File exists: $HOME/$file_name"
	    fi             
    else
	    if [ -d $file_path ]; then
	        ln -si $file_path $HOME/$file_name && echo "[  OK  ] '$file_path/' -> '$HOME/$file_name/'"
	    else
	        ln -si $file_path $HOME/$file_name && echo "[  OK  ] '$file_path' -> '$HOME/$file_name'"
	    fi
    fi				
done
echo
echo
echo -------------- copying --------------
for file_path in `\find $SCRIPT_DIR/cp -mindepth 1 -maxdepth 1`;
do
    read file_name < <(echo $file_path | sed -e "s:^$SCRIPT_DIR/cp/::")
    if [ -e $HOME/$file_name ]; then
	    if [ -d $file_path ]; then
	        echo "[FAILED] Directory exists: $HOME/$file_name"
	    else
	        echo "[FAILED] File exists: $HOME/$file_name"
	    fi
    else
	    if [ -d $file_path ]; then
	        echo "[  OK  ] $file_name/ "
	        cp -ibvr $file_path $HOME/$file_name | sed -e 's:^:\t:'
	    else
	        cp -ibvr $file_path $HOME/$file_name | sed -e 's:^:[  OK  ] :'
	    fi
    fi				
done
