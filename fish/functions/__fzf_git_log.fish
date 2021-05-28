function __fzf_git_log
    if not command -sq git
        __echo_error '__fzf_git_log: git command not found.'
        return 1
    end

    if functions -q __fzf_preview_git_show
        command git log --oneline --graph --color=always \
          --date=format-local:'%Y-%m-%d %H:%M:%S' \
          --format="%C(auto)%h%d %s %C(black)%C(bold)%cd" | \
          fzf --color=dark --no-sort --no-multi --no-cycle --reverse --tiebreak=index \
          --height=100% --prompt 'GIT LOG GRAPH > ' \
          --preview-window=right:wrap:hidden \
          --preview '__fzf_preview_git_show {}' \
          --bind "$fzf_preview_bind"
    else
        __echo_error '__fzf_git_log: __fzf_preview_git_show command not found.'
        return 1
    end
end
