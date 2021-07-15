# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case "$-" in
    *i*) ;;
      *) return;;
esac

#[ -x /usr/bin/clear ] && /usr/bin/clear

if [ -z "$DOTDIR_PATH" ]; then
    echo '$DOTDIR_PATH not set.' 1>&2
    return 1
fi

# load vital utilities.
source "${DOTDIR_PATH}/etc/vital.sh" 2>/dev/null
if ! is_vitalize 2>/dev/null; then
    echo 'Cannot vitalize.' 1>&2
    return 1
fi

source "${DOTDIR_PATH}/etc/lib/fzf-functions.bash" 2>/dev/null



# functions
# ---------

# --- prompt ---

function new_line_prompt {
    if [ -z "$FIRST_PROMPT" ] || [ "$FIRST_PROMPT" = 1 ]; then
        FIRST_PROMPT=0
    else
        printf '\n';
    fi
}

function eval_prompt_commands() {

    # execute commands according to PROMPT_COMMAND_*

    export EXIT_STATUS="$?"
    local func
    for func in ${!PROMPT_COMMAND_*}
    do
        eval "${!func}"
    done
    unset func
}

# --- tmux ---

function _tmux_autostart_message() {
    local header='tmux_autostart:'
    local nl
    local param=()

    for OPT in "$@"
    do
        case "$OPT" in
            -n)
                nl=1
                shift
                ;;
            -*)
                echo "${header} illegal option -- '$(echo $1 | sed 's/^-*//')'" 1>&2
                return 1
                ;;
            *)
                param=("${param[@]}" "$1")
                shift
                ;;
        esac
    done

    if [ "$nl" = 1 ]; then
        echo -n "${header} ${param[@]}"
    else
        echo "${header} ${param[@]}"
    fi
}

function tmux_autostart() {

    # if not inside a tmux session, and if no session is started,
    # start a new session.
    #
    # Environment variables
    #   $TMUX_AUTOSTART=1
    #
    # set below variable if you don't want to create a new session
    # disable automatically create new session automatically
    #   $TMUX_DISABLE_AUTO_NEW_SESSION=1

    [ -n "$BASH_EXECUTION_STRING" ] && return 0

    if ! is_exists 'tmux'; then
        e_error $(_tmux_autostart_message 'tmux is not exists')
        return 1
    fi

    if [ ! "$TMUX_AUTOSTART" = 1 ]; then
        e_bashrc_message 'TERM MUX' "tmux $(tmux -V | awk '{print $2}') / autostart is disabeled"
        return 0
    fi

    if ! is_tmux_running; then
        if is_interactive_shell && ! is_ssh_running; then
            if tmux has-session >/dev/null 2>&1 && tmux list-sessions | grep -qE '.*]$'; then
                e_newline
                _tmux_autostart_message 'detached session exists'
                tmux list-sessions
                e_newline
                echo -n 'Attach? (Y/n/num): '; read
                if [[ "$REPLY" =~ ^[Yy][Ee]*[Ss]*$ ]] || [[ "$REPLY" == '' ]]; then
                    _tmux_autostart_message 'attach session'
                    tmux attach-session
                    if [ $? -eq 0 ]; then
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    _tmux_autostart_message "attach to session ${REPLY}"
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0 ]; then
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[Nn][Oo]*$ ]]; then
                    return 0
                fi
            elif [ ! "$TMUX_DISABLE_AUTO_NEW_SESSION" = 1 ]; then
                e_newline
                _tmux_autostart_message 'create a new session automatically'
                tmux new-session
            fi
        fi
    else
        # Shell on tmux
        e_bashrc_message 'TERM MUX' "tmux $(tmux -V | awk '{print $2}') / session >> $(tmux display-message -p '#S')"
        if [ -e "${HOME}/.dotfiles/etc/ascii-art/tmux.txt" ]; then
            : cat "${HOME}/.dotfiles/etc/ascii-art/tmux.txt"
        fi
    fi
}

# --- setup functions ---

function _prompt_setup() {
    local color_prompt
    local force_color_prompt
    local debian_chroot

    # set variable identifying the chroot you work in (used in the prompt below)
    if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        debian_chroot=$(cat /etc/debian_chroot)
    fi

    # set a fancy prompt (non-color, unless we know we "want" color)
    case "$TERM" in
        xterm-color|*-256color) color_prompt=yes;;
    esac

    # uncomment for a colored prompt, if the terminal has the capability; turned
    # off by default to not distract the user: the focus in a terminal window
    # should be on the output of commands, not on the prompt
    #force_color_prompt=yes

    if [ -n "$force_color_prompt" ]; then
        if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
            # We have color support; assume it's compliant with Ecma-48
            # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
            # a case would tend to support setf rather than setaf.)
            color_prompt=yes
        else
            color_prompt=
        fi
    fi

    # --- prompt command ----

    local prompt_command_name='eval_prompt_commands'

    if [[ ! "$PROMPT_COMMAND" =~ .*${prompt_command_name}.* ]]; then
        PROMPT_COMMAND_DEFAULT="$PROMPT_COMMAND"
    fi
    export FIRST_PROMPT=1
    export PROMPT_COMMAND_DEFAULT
    PROMPT_COMMAND="$prompt_command_name"

    # --- colors ---

    # specified by RGB
    # [01;38;2;<R>;<G>;<B>m\] (Foreground)
    # [01;48;2;<R>;<G>;<B>m\] (Background)

    black=$'\e[30m'
    red=$'\e[31m'
    green=$'\e[32m'
    yellow=$'\e[33m'
    blue=$'\e[34m'
    purple=$'\e[35m'
    cyan=$'\e[36m'
    white=$'\e[37m'
    gray=$'\e[90m'
    reset=$'\e[m'

    # --- set PS1 ---

    local symbol_prompt
    local git_prompt
    local host_prompt
    local decoration_prompt
    local prefix_prompt='${debian_chroot:+($debian_chroot)}'
    local new_line=$'\n'

    if [ ${EUID:-${UID}} = 0 ]; then
        symbol_prompt='# '
    else
        symbol_prompt='> '
    fi

    if [ -f ~/bin/git-prompt.sh ]; then
        source ~/bin/git-prompt.sh
        GIT_PS1_SHOWUPSTREAM=1
        GIT_PS1_SHOWUNTRACKEDFILES=1
        GIT_PS1_SHOWSTASHSTATE=1
        GIT_PS1_SHOWDIRTYSTATE=1
        git_prompt='$(__git_ps1 "(%s)")'
    fi

    if ! is_tmux_running; then
        host_prompt='@\h'
    fi

    #color_prompt=no
    if [ "$color_prompt" = yes ]; then
        PS1="${prefix_prompt}${decoration_prompt}\u${host_prompt} \[${blue}\]\w \[${cyan}\]${git_prompt}\[${reset}\]${new_line}${symbol_prompt}\[${reset}\]"
    else
        PS1="${prefix_prompt}[\u${host_prompt} \w ${git_prompt}${new_line}${symbol_prompt}"
    fi

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
        xterm*|rxvt*)
            PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
            ;;
        *)
            ;;
    esac
}

function _alias_setup() {
    # enable color support of ls and also add handy aliases
    if [ -x /usr/bin/dircolors ]; then
        if [ -r ~/.dircolors ]; then
            eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        fi

        alias ls='ls -F --color=auto'
        #alias dir='dir --color=auto'
        #alias vdir='vdir --color=auto'
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    else
        alias ls='ls -F'
    fi

    alias ll='ls -al'
    alias la='ls -A'
    alias l='ls -C'
    alias rm='rm -i'
    alias mv='mv -i'
    alias cp='cp -i'
    alias df='df -h'
    alias du='du -h'
    alias less='less -XF'

    # git (require DOTDIR/etc/lib/fzf-functions.bash)
    function gl() {
        if ! is_exists 'git'; then
            e_error 'gl: git command not found'
            return 1
        fi

        if [ "$#" -eq 0 ]; then
            if is_exists 'fzf'; then
                _fzf_git_log
            else
                git log --oneline --graph --color=always \
                    --date=format-local:'%Y-%m-%d %H:%M:%S' \
                    --format="%C(auto)%h%d %s %C(black)%C(bold)%cd"
            fi
        else
            git log "$@"
        fi
    }
    function gla() {
        if ! is_exists 'git'; then
            e_error 'gla: git command not found'
            return 1
        fi

        if [ "$#" -eq 0 ]; then
            if is_exists 'fzf'; then
                _fzf_git_log_all
            else
                git log --all --oneline --graph --color=always \
                    --date=format-local:'%Y-%m-%d %H:%M:%S' \
                    --format="%C(auto)%h%d %s %C(black)%C(bold)%cd"
            fi
        else
            git log --all "$@"
        fi
    }
    function ga() {
        if ! is_exists 'git'; then
            e_error 'ga: git command not found'
            return 1
        fi

        if [ "$#" -eq 0 ] && is_exists 'fzf'; then
            _fzf_git_add
        else
            git add "$@"
        fi
    }
    function gd() {
        if ! is_exists 'git'; then
            e_error 'gd: git command not found'
            return 1
        fi

        if [ "$#" -eq 0 ] && is_exists 'fzf'; then
            if [ "$PLATFORM" = 'msys' ]; then
                git diff "$@"
            else
                _fzf_git_diff_including_staged
            fi
        else
            git diff "$@"
        fi
    }
    function gco() {
        if ! is_exists 'git'; then
            e_error 'gco: git command not found'
            return 1
        fi

        if [ "$#" -eq 0 ] && is_exists 'fzf'; then
            _fzf_git_checkout
        else
            git checkout "$@"
        fi
    }
    alias gb='git branch'
    alias gst='git status'
    alias gc='git commit -v'

    if [ "$PLATFORM" = 'msys' ]; then
        alias open='start'
    else
        alias open='xdg-open'
    fi

    # edit
    if is_exists 'emacs'; then
        if [ "$PLATFORM" = 'msys' ]; then
            alias e='emacsclientw.exe -n'
            alias et='emacs -nw'
            alias ec='emacsclientw.exe -c -n'
            alias ekill='emacsclientw.exe -e "(kill-emacs)"'
        elif [ "$PLATFORM" = 'wsl' ]; then
            function is_esrv() {
                if emacsclient -e "t" > /dev/null 2>&1; then
                    echo 'Emacs server is already running.'
                    return 0
                else
                    echo 'Emacs server is not running.' 1>&2
                    return 1
                fi
            }
            function e() {
                if [ -z "$*" ]; then
                    emacsclient -c -n
                elif is_esrv 1> /dev/null; then
                    for f in "$@"; do
                        emacsclient -n "$f"
                    done
                fi
            }
            alias et='emacsclient -t'
            alias ec='emacsclient -c -n'
            if is_exists 'wslemacs-start.exe'; then
                function estart() {
                    if ! is_esrv 2> /dev/null; then
                        wslemacs-start.exe &
                    fi
                }
                alias ekill='wslemacs-stop.exe'
            else
                function estart() {
                    if ! is_esrv 2> /dev/null; then
                        emacs --daemon
                    fi
                }
                alias ekill='emacsclient -e "(kill-emacs)"'
            fi
        fi
    fi

    function fssh() {
        if ! is_exists 'fzf'; then
            e_error 'fssh: fzf command not found'
            return 1
        fi

        if ! is_exists 'ag'; then
            e_error 'fssh: ag command not found'
            return 1
        fi

        local ssh_target=$(ag --ignore-case '^host [^*]' ~/.ssh/ | \
                               cut -d ' ' -f 2 | fzf)
        [ -n "$ssh_target" ] && ssh "$ssh_target"
    }

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    #alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    # You may want to put all your additions into a separate file like
    # ~/.bash_aliases, instead of adding them here directly.
    # See /usr/share/doc/bash-doc/examples in the bash-doc package.
    if [ -f ~/.bash_aliases ]; then
        source ~/.bash_aliases
    fi

    function zz() {
        if ! is_exists 'fzf'; then
            e_error 'fssh: fzf command not found'
            return 1
        fi

        if [ -z "$@" ]; then
            local dir=$(z --list | sort -rn | cut -c 12- | fzf) && \
            [ -n "$dir" ] && echo "Move to ${dir}"; cd "$dir"
        else
            z "$@"
        fi
    }
}

function _shopt_setup() {
    # append to the history file, don't overwrite it
    shopt -s histappend

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    # If set, the pattern "**" used in a pathname expansion context will
    # match all files and zero or more directories and subdirectories.
    #shopt -s globstar

    # disable overwrite (redirect >)
    # if want to overwrite then use >|.
    # set -o noclobber
    shopt -so noclobber

    # avoid logout by "C-d"
    # set -o ignoreeof
    shopt -so ignoreeof
}

function bashrc_startup() {
    _prompt_setup
    _alias_setup

    e_bashrc_message 'Hello:)'
    e_newline
    #e_bashrc_message 'DATETIME' "$(date '+%Y-%m-%d %H:%M:%S')"
    e_bashrc_message 'SYSTEM' "${HOSTNAME} / $(uname -smo)"
    e_bashrc_message 'SHELL' "bash ${BASH_VERSION%.*} / pid $$"
    e_bashrc_message 'DISPLAY' "${DISPLAY:-not set}"

    is_exists 'tmux' && tmux_autostart
    e_newline
}



# settings
# --------

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=100000

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        source /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    fi
fi

# additional prompt commands
export PROMPT_COMMAND_ADDITIONAL='new_line_prompt;'

# fzf settings
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
if is_exists 'fzf'; then
    export FZF_DEFAULT_OPTS="--multi --cycle --height=60% --layout=reverse \
                             --border=rounded --info=inline --ansi --exit-0 \
                             --bind ctrl-v:half-page-down,alt-v:half-page-up,alt-p:toggle-preview,ctrl-k:kill-line,ctrl-d:delete-char,ctrl-x:delete-char"
fi

# symlink setting of msys
if [ "$PLATFORM" = 'msys' ]; then
    export MSYS=winsymlinks:nativestrict
fi

# manpager setting
if is_exists 'bat'; then
    export MANPAGER='sh -c "col -bx | bat -l man -p"'
elif is_exists 'vim'; then
    export MANPAGER='/bin/sh -c "col -bx | vim -MRn -c \"set ft=man ts=8 nolist nomod nonu noma\" -"'
fi

# fzf_ghq keybind (Ctrl-g)
if is_exists 'fzf' && is_exists 'ghq'; then
    bind -x '"\C-g": _fzf_ghq'
fi

# startup
if bashrc_startup; then
    _shopt_setup
fi
