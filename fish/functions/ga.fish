function ga --description 'git add' --wraps='git add'
    if not command -sq git
        __echo_error 'ga: git command not found.'
        return 1
    end

    if functions -q __fzf_git_add; and test -z "$argv"
        __fzf_git_add
    else
        command git add $argv
    end
end
