function em --description 'emacs --no-window-system'
    if not command -sq emacs
        __echo_error 'em: emacs command not found.'
        return 1
    end

    command emacs -nw $argv
end
