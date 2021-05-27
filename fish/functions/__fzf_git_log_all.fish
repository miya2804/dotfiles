function __fzf_git_log_all
    if not command -sq git
        __echo_error '__fzf_git_log_all: git command not found.'
        return 1
    end

    if functions -q __fzf_preview_git_show
        command git log --oneline --graph --all --color=always \
          --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" | \
          fzf --color=dark --no-sort --no-multi --no-cycle --reverse --tiebreak=index \
          --height=100% --prompt 'GIT LOG GRAPH ALLREFS > ' \
          --preview-window=right:wrap:hidden \
          --preview '__fzf_preview_git_show {}' \
          --bind "$fzf_preview_bind"
    else
        __echo_error '__fzf_git_log_all: __fzf_preview_git_show command not found.'
        return 1
    end
end
