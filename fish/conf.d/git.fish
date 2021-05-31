### variables
# prefix of the git branch name is show or not
set space_branch_prefix true

# git prompt color
# branch name
set fish_color_git_branch_detached brmagenta
set fish_color_git_clean green
set fish_color_git_staged yellow
set fish_color_git_dirty red
# action
set fish_color_git_commit_hash yellow
set fish_color_git_action brred
# info
set fish_color_git_upstream normal
set fish_color_git_ahead brmagenta
set fish_color_git_behind brmagenta
set fish_color_git_stashed yellow
set fish_color_git_added green
set fish_color_git_deleted red
set fish_color_git_modified blue
set fish_color_git_renamed cyan
set fish_color_git_unmerged yellow
set fish_color_git_untracked white

# git prompt character
set fish_prompt_git_status_stashed '$'
set fish_prompt_git_status_modified '*'
set fish_prompt_git_status_added '+'
set fish_prompt_git_status_deleted 'x'
set fish_prompt_git_status_renamed '→'
set fish_prompt_git_status_unmerged '!'
set fish_prompt_git_status_untracked '?'
set fish_prompt_git_status_upstream '='
set fish_prompt_git_status_behind '<'
set fish_prompt_git_status_ahead '>'

### plugin settings
# tide
# color setting
set tide_prompt_char_success_color normal

# set git information of tide prompt
if functions -q tide _tide_item_git
    function _tide_item_git
        set -l git_status (my_fish_git_prompt)
        test -n "$git_status"
        and echo (set_color brblack)\["$git_status"(set_color brblack)\](set_color normal)
    end
    set space_branch_prefix false
end
