function gc --description 'git commit' --wraps='git commit'
    if not command -sq git
        __echo_error 'gc: git command not found.'
        return 1
    end
    command git commit -v $argv
end
