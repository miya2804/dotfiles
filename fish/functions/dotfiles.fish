function dotfiles --description "Change directory to the dotfiles."
    test -n "$DOTDIR_PATH"
    and cd $DOTDIR_PATH
end
