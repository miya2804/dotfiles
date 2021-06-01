function gco --description 'git checkout' --wraps='git checkout'
    if not command -sq git
        __echo_error 'gco: git command not found.'
        return 1
    end

    if functions -q __fzf_git_checkout; and test -z "$argv"
        __fzf_git_checkout
    else
        command git checkout $argv
    end
end
