function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus

    echo

    set -g fish_prompt_pwd_dir_length 0
    set -l cwd (prompt_pwd)

    test $cwd = '~'
    and set cwd 'HOME'
    #and set cwd 'HOME🏠'

    set_color brblack
    echo -n '[ '
    set_color normal

    set_color blue
    echo -n $cwd
    set_color normal

    my_fish_git_prompt

    set_color brblack
    echo -n ' ]'
    set_color normal

    set -l pipestatus_string (__fish_print_pipestatus " err-" " " "|" (set_color --bold $fish_color_status) (set_color --bold $fish_color_status) $last_pipestatus)
    echo $pipestatus_string

    echo -n "$USER> "
end
