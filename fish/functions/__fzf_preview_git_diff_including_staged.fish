function __fzf_preview_git_diff_including_staged --description 'Input: line of git status --short'
    if not command -sq git
        __echo_error '__fzf_preview_git_diff_including_staged: git command not found.'
        return 1
    end

    set -l git_status_short_format (echo $argv | awk '{print substr($0,1,2)}')
    set -l git_index_status (echo $git_status_short_format | awk '{print substr($0,1,1)}')
    set -l git_workingtree_status (echo $git_status_short_format | awk '{print substr($0,2,1)}')
    set -l file (echo $argv | awk '{print $2}')
    set -l staged_background_color green
    set -l not_staged_background_color red

    echo (set_color -o)Changes of $file
    echo

    if test "$git_index_status" != ' '
        set_color --background $staged_background_color
        echo '<<<<<<<<<< STAGED <<<<<<<<<<'
        set_color normal
        command git diff --staged --color=always $file
        echo
    end

    if test "$git_workingtree_status" != ' '
        set_color --background $not_staged_background_color
        echo '>>>>>>>>>> NOT STAGED >>>>>>>>>>'
        set_color normal
        command git diff --color=always $file
        echo
    end
end
