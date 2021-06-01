function ga --description 'git add' --wraps='git add'
    if not command -sq git
        __echo_error 'ga: git command not found.'
        return 1
    end

    if test -z "$argv"; and command -sq fzf
        __fzf_git_add
    else
        command git add $argv
    end
end
