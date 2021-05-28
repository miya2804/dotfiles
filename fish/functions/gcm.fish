function gcm --description 'git commit' --wraps='git commit'
    if not command -sq git
        __echo_error 'gcm: git command not found.'
        return 1
    end

    if test -z "$argv"
        command git commit -v
    else
        command git commit $argv
    end
end
