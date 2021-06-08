function __fzf_preview_git_diff --description 'Show changes between commits for fzf preview using sentence of git status --short.'
    if not command -sq git
        __echo_error '__fzf_preview_git_diff: git command not found.'
        return 1
    end

    set -l git_status_short_format (echo $argv | awk '{print substr($0,1,2)}')
    set -l git_index_status (echo $git_status_short_format | awk '{print substr($0,1,1)}')
    set -l git_workingtree_status (echo $git_status_short_format | awk '{print substr($0,2,1)}')
    set -l file (echo $argv | awk '{print $2}')
    set -l rename_file (echo $argv | awk '{print $4}')

    if test "$git_status_short_format" = "??"
        echo (set_color red)'Untracked: '(set_color -o)$file(set_color normal)
        echo '----'
        echo
        if test ! -d "$file"
            command -sq bat
            and bat --plain --color=always $file
            or cat $file
        else
            ls -al --color=always "$file"
        end
    else
        echo "$git_status_short_format" | grep R >/dev/null
        and set file $rename_file

        if test "$git_workingtree_status" = 'D'
            set_color red
            echo "deleted: $file"
            set_color normal
        else
            command git diff --color=always $file
        end
    end
end
