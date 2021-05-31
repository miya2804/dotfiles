function my_fish_git_prompt --description 'Write out the git prompt'
    # If git isn't installed, there's nothing we can do
    # Return 1 so the calling prompt can deal with it
    if not command -sq git
        return 1
    end

    # Get the directory for later use.
    # Return if not inside a Git repository work tree.
    if not set -l repo_info (command git rev-parse \
      --git-dir --is-inside-git-dir --is-bare-repository 2>/dev/null)
        return
    end
    set -l git_dir $repo_info[1]
    set -l inside_gitdir $repo_info[2] # true of false
    set -l bare_repo $repo_info[3] # true of false

    # Get the current action ("merge", "rebase", etc.)
    # and if there's one get the current commit hash too.
    set -l commit ''
    if set -l action (fish_print_git_action "$git_dir")
        set commit (command git rev-parse HEAD 2>/dev/null | string sub -l 7)
    end

    # Get either the branch name of a branch descriptor.
    set -l branch_detached 0
    if not set -l branch (command git symbolic-ref --short HEAD 2>/dev/null)
       set branch_detached 1
       set branch (command git describe --contains --all HEAD 2>/dev/null)
    end

    # Get the commit difference counts between local and remote.
    set -l status_upstream 1
    command git rev-list --count --left-right 'HEAD...@{upstream}' 2>/dev/null \
        | read -d \t -l status_ahead status_behind
    if test $status -ne 0
        set status_upstream 0
        set status_ahead 0
        set status_behind 0
    end

    # Get the stash status.
    # (git stash list) is very slow. => Avoid using it.
    set -l status_stashed 0
    if test -f "$git_dir/refs/stash"
        set status_stashed 1
    else if test -r "$git_dir/commondir"
        read -l commondir <"$git_dir/commondir"
        if test -f "$commondir/refs/stash"
            set status_stashed 1
        end
    end

    set -l porcelain_status (command git status --porcelain 2>/dev/null | string sub -l2)

    set -l status_clean 0
    if test -z "$porcelain_status"
        set status_clean 1
    end
    set -l staged 0
    if string match -qr '^[ACDMT]' $porcelain_status
        set staged 1
    end
    set -l status_added 0
    if string match -qr '[ACDMT][ MT]|[ACMT]D' $porcelain_status
        set status_added 1
        set status_dirty 1
    end
    set -l status_deleted 0
    if string match -qr '[ ACMRT]D' $porcelain_status
        set status_deleted 1
        set status_dirty 1
    end
    set -l status_modified 0
    if string match -qr '[MT]$' $porcelain_status
        set status_modified 1
        set status_dirty 1
    end
    set -l status_renamed 0
    if string match -qe R $porcelain_status
        set status_renamed 1
        set status_dirty 1
    end
    set -l status_unmerged 0
    if string match -qr 'AA|DD|U' $porcelain_status
        set status_unmerged 1
    end
    set -l status_untracked 0
    if string match -qe '\?\?' $porcelain_status
        set status_untracked 1
    end

    # Print branch name
    set -l bare ''
    set -l gitdir_char 'GIT_DIR!'
    if test -n "$branch"
        if test true = "$inside_gitdir"
            if test true = "$bare_repo"
                set bare (set_color normal)'BARE:'
            else
                set branch $gitdir_char
            end
        end

        if test "$branch" = "$gitdir_char"
            set branch (set_color normal)$branch
        else if test $branch_detached -ne 0
            set branch (set_color $fish_color_git_branch_detached)$branch
        else if test $status_clean -ne 0
            set branch (set_color $fish_color_git_clean)$branch
        else if test $staged -ne 0
            set branch (set_color $fish_color_git_staged)$branch
        else
            set branch (set_color $fish_color_git_dirty)$branch
        end
    end

    set -l space ' '
    test false = "$space_branch_prefix"
    and set space ''
    echo -n "$space$bare$branch"
    set_color normal

    # Print current action and current commit hash
    set -l git_action_status ''
    if test -n "$commit"
        set -a git_action_status (set_color $fish_color_git_commit_hash)"$commit"
    end
    if test -n "$action"
        set -a git_action_status (set_color normal)':'(set_color -o $fish_color_git_action)"$action"
    end
    if test -n "$git_action_status"
        set_color normal
        echo -n '('
        for i in $git_action_status
            echo -n $i
        end
        set_color normal
        echo -n ')'
    end

    # Print git informations
    set -l git_info ''
    if test $status_stashed -ne 0
        set -a git_info (set_color $fish_color_git_stashed)$fish_prompt_git_status_stashed
    end
    if test $status_modified -ne 0
        set -a git_info (set_color $fish_color_git_modified)$fish_prompt_git_status_modified
    end
    if test $status_added -ne 0
        set -a git_info (set_color $fish_color_git_added)$fish_prompt_git_status_added
    end
    if test $status_deleted -ne 0
        set -a git_info (set_color $fish_color_git_deleted)$fish_prompt_git_status_deleted
    end
    if test $status_renamed -ne 0
        set -a git_info (set_color $fish_color_git_renamed)$fish_prompt_git_status_renamed
    end
    if test $status_unmerged -ne 0
        set -a git_info (set_color $fish_color_git_unmerged)$fish_prompt_git_status_unmerged
    end
    if test $status_untracked -ne 0
        set -a git_info (set_color $fish_color_git_untracked)$fish_prompt_git_status_untracked
    end
    test $status_behind -ne 0
    or test $status_ahead -ne 0
    or if test $status_upstream -ne 0; and test false = "$inside_gitdir"
        set -a git_info (set_color $fish_color_git_upstream)$fish_prompt_git_status_upstream
    end
    if test $status_behind -ne 0
        set -a git_info (set_color $fish_color_git_behind)$fish_prompt_git_status_behind
    end
    if test $status_ahead -ne 0
        set -a git_info (set_color $fish_color_git_ahead)$fish_prompt_git_status_ahead
    end

    set_color -o
    if test -n "$git_info"
        echo -n ' '
        for i in $git_info
            echo -n $i
        end
    end

    set_color normal
end
