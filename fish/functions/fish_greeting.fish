function fish_greeting

    set -l system (uname -smo)
    set -l shell (fish --version | awk '{ sub(",",""); printf "%s %s", $1, $3 }')
    set -l pid "pid $fish_pid"

    # Greeting message
    echo "Hello:)"
    echo
    echo "SYSTEM - $hostname / $system"
    echo "SHELL - $shell / $pid"
    echo "DISPLAY - $DISPLAY"
end
