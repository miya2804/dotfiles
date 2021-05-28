function fssh --description "Fuzzy-find ssh host via ag and ssh into it"
    if not command -sq ag
        __echo_error 'fssh: ag command not found.'
        return 1
    end

    ag --ignore-case '^host [^*]' ~/.ssh/ | cut -d ' ' -f 2 | fzf | read -l result
    and ssh "$result"
end
