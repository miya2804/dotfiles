function fish_user_key_bindings
    functions -q fzf_key_bindings
    fzf_key_bindings
    bind --preset \cd delete-char
end
