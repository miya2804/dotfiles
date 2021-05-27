function __fzf_preview_git_diff
    if not command -sq git
        __echo_error '__fzf_preview_git_diff: git command not found.'
        return 1
    end

    set -l git_status (echo $argv | awk '{print $1}')
    set -l file (echo $argv | awk '{print $2}')

    if test "$git_status" = "??"
        echo (set_color red)'Untracked file.'(set_color normal)
        echo '----'
        cat $file
    else
        git diff --color=always $file
    end
end
