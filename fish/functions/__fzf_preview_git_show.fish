function __fzf_preview_git_show
    if not command -sq git
        __echo_error '__fzf_preview_git_show: git command not found.'
        return 1
    end

    set -l commit_id (echo -- $argv | grep -o "[a-f0-9]\{7\}")
    and command git show --color=always $commit_id
end
