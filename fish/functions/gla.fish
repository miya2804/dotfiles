function gla --description 'git log --all' --wraps='git log --all'
    if not command -sq git
        __echo_error 'gla: git command not found.'
        return 1
    end

    if test -z "$argv"
        if functions -q __fzf_git_log_all; and command -sq fzf
            __fzf_git_log_all
        else
            command git log --oneline --graph --all --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"
        end
    else
        command git log --all $argv
    end
end
