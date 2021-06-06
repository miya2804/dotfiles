### PROMPT ###

# tide
set tide_prompt_char_success_color normal
set tide_prompt_char_icon '>'
set tide_time_format '%H:%M'

### GIT PROMPT ###

# prefix of the git branch name is show or not
set space_branch_prefix true

# branch name color
set fish_color_git_branch_detached brmagenta
set fish_color_git_clean green
set fish_color_git_staged yellow
set fish_color_git_dirty red

# action color
set fish_color_git_commit_hash yellow
set fish_color_git_action brred

# info color
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

# status character
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

# set git informations of tide
if functions -q tide
    set space_branch_prefix false
    function _tide_item_git
        set -l git_prompt (my_fish_git_prompt)
        test -n "$git_prompt"
        and echo (set_color brblack)\["$git_prompt"(set_color brblack)\](set_color normal)
    end
end
