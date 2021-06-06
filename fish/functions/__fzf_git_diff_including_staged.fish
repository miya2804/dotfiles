function __fzf_git_diff_including_staged --description 'Fizzy-find and show changes between commits including staged'
    if not command -sq git
        __echo_error '__fzf_git_diff_including_staged: git command not found.'
        return 1
    end

    if functions -q __fzf_preview_git_diff_including_staged
        command git status --short | awk '{if(substr($0,1,2) != "??") print $0}' | \
        fzf --height 100% --exit-0 --no-multi --prompt 'GIT DIFFS > ' \
          --preview "__fzf_preview_git_diff_including_staged {}" \
          --preview-window=down:85%:wrap \
          --bind "$fzf_preview_bind"",tab:toggle-preview" | \
        awk '{
               if (substr($0,1,2) !~ /R/) {
                 print $2
               } else {
                 print $4
               }
             }' | \
        xargs git diff --color=always | less -XFR
    else
        __echo_error '__fzf_git_diff_including_staged: __fzf_preview_git_diff_including_staged function not found.'
        return 1
    end
end
