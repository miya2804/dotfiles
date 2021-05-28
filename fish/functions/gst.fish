function gst --description 'git status'
    if not command -sq git
        __echo_error 'gst: git command not found.'
        return 1
    end

    command git status $argv
end
