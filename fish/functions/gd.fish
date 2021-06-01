function gd --description 'git diff' --wraps='git diff'
    if not command -sq git
        __echo_error 'gd: git command not found.'
        return 1
    end

    if test -z "$argv"; and command -sq fzf
        __fzf_git_diff_including_staged
    else
        command git diff $argv
    end
end
