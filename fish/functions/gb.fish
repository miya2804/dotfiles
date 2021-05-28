function gb --description 'git branch' --wraps='git branch'
    if not command -sq git
        __echo_error 'gb: git command not found.'
        return 1
    end
    command git branch $argv
end
