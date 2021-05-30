function __fzf_preview_git_diff --description 'Show changes between commits for fzf preview using sentence of git status --short.'
    if not command -sq git
        __echo_error '__fzf_preview_git_diff: git command not found.'
        return 1
    end

    set -l git_status_short_format (echo $argv | awk '{print substr($0,1,2)}')
    set -l file (echo $argv | awk '{print $2}')

    if test "$git_status_short_format" = "??"
        echo (set_color red)'Untracked: '(set_color -o)$file(set_color normal)
        echo '----'
        echo
        command -sq bat
        and bat --plain --color=always $file
        or cat $file
    else
        command git diff --color=always $file
    end
end
