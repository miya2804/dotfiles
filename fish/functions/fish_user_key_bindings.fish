function fish_user_key_bindings
    functions -q fzf_key_bindings
    and fzf_key_bindings
    bind --preset \cd delete-char
end
