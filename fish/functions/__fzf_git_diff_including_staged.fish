function __fzf_git_diff_including_staged --description 'Fizzy-find and show changes between commits including staged'
    if not command -sq git
        __echo_error '__fzf_git_diff_including_staged: git command not found.'
        return 1
    end

    if functions -q __fzf_preview_git_diff_including_staged
        command git status --short | awk '{if(substr($0,1,2) != "??") print $0}' | \
          fzf --height 90% --exit-0 --prompt 'GIT DIFFS > ' \
          --preview "__fzf_preview_git_diff_including_staged {}" \
          --preview-window=right:70%:wrap \
          --bind "$fzf_preview_bind" >/dev/null
    else
        __echo_error '__fzf_git_diff_including_staged: __fzf_preview_git_diff_including_staged function not found.'
        return 1
    end
end
