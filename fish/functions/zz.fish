function zz --description "Overrapping function of z.fish use fzf" --wraps='z'
    if not functions -q z
        __echo_error 'zz: z function not found.'
        return 1
    end
    if not command -sq fzf
        __echo_error 'zz: fzf command not found.'
        return 1
    end

    if test -z "$argv"
        set -l dir (z --list | sort -rn | cut -c 12- | fzf)
        and test -n "$dir"
        and echo 'Move to '(set_color -o)$dir; cd $dir
    else
        z $argv
    end
end
