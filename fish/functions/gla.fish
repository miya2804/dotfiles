function gla --description 'git log --all' --wraps='git log --all'
    if not command -sq git
        __echo_error 'gla: git command not found.'
        return 1
    end

    if test -z "$argv"
        if command -sq fzf
            __fzf_git_log_all
        else
            command git log --oneline --graph --all --color=always \
              --date=format-local:'%Y-%m-%d %H:%M:%S' \
              --format="%C(auto)%h%d %s %C(black)%C(bold)%cd"
        end
    else
        command git log --all $argv
    end
end
