function gd --description 'git diff' --wraps='git diff'
    if not command -sq git
        __echo_error 'gd: git command not found.'
        return 1
    end

    if functions -q __fzf_git_diff_including_staged; and test -z "$argv"
        __fzf_git_diff_including_staged
    else
        command git diff $argv
    end
end
