function gb --description 'git branch' --wraps='git branch'
    if not command -sq git
        __echo_error 'gb: git command not found.'
        return 1
    end

    if functions -q __fzf_git_checkout; and test -z "$argv"
        __fzf_git_checkout
        return
    else
        command git branch $argv
    end
end
