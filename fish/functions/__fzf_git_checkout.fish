function __fzf_git_checkout --description 'Fuzzy-find and checkout a branch'
    if not command -sq git
        __echo_error '__fzf_git_checkout: git command not found.'
        return 1
    end
    command git branch --all --color=always | grep -v HEAD | cut -c 3- | \
    fzf --no-multi --height 100% --prompt 'GIT CHECKOUT > ' \
      --preview "git log --color=always {}" \
      --preview-window=down:70%:wrap \
      --bind "$fzf_preview_bind"",tab:toggle-preview" | \
    read -l result; and git checkout "$result"
end
