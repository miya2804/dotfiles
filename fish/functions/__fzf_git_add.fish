function __fzf_git_add --description 'Use fzf to interactively add modified contents in the working tree.'
    if not command -sq git
        __echo_error '__fzf_git_add: git command not found.'
        return 1
    end

    if functions -q __fzf_preview_git_diff
        command git status --short | awk '{if (substr($0,2,1) !~ / /) print $0}' | \
          fzf --height 100% --prompt 'GIT ADD > ' --exit-0 \
          --preview "__fzf_preview_git_diff {}" \
          --preview-window=down:85%:wrap \
          --bind "$fzf_preview_bind" | awk '\
            {
              if (substr($0,1,2) !~ /R/) {\
                print $2 \
              } else {
                print $4 \
              }
            }' | \
          while read staged_file
              echo 'Staged: '$staged_file
              command git add $staged_file
          end
    else
        __echo_error '__fzf_git_add: __fzf_preview_git_diff command not found.'
        return 1
    end
end
