function gl --description 'git log' --wraps='git log'
    if not command -sq git
        __echo_error 'gl: git command not found.'
        return 1
    end

    if test -z "$argv"
        if functions -q __fzf_git_log; and command -sq fzf
            __fzf_git_log
        else
            command git log --oneline --graph --color=always \
              --date=format-local:'%Y-%m-%d %H:%M:%S' \
              --format="%C(auto)%h%d %s %C(black)%C(bold)%cd"
        end
    else
        command git log $argv
    end
end
