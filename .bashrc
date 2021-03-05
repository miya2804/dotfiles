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

# *** functions ***

function new_line_prompt {
    if [ -z "$FIRST_PROMPT" ] || [ "$FIRST_PROMPT" = 1 ]; then
        FIRST_PROMPT=0
    else
        printf '\n';
    fi
}

function eval_prompt_commands() {
    export EXIT_STATUS="$?"
    local func
    for func in ${!PROMPT_COMMAND_*}
    do
        eval "${!func}"
    done
    unset func
}

# tmux
function tmux_autostart_info() {
    local header='tmux_autostart:'
    printf "%s %s\n" "$header" "$*"
}

function tmux_autostart_error() {
    local header='tmux_autostart:'
    printf "%s %s\n" "$header" "$*" 1>&2
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

    if ! is_exists 'tmux'; then
        tmux_autostart_error 'tmux is not exists'
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
                tmux_autostart_info 'detached session exists'
                tmux list-sessions
                echo -n 'Attach? (Y/n/num): '; read
                if [[ "$REPLY" =~ ^[Yy][Ee]*[Ss]*$ ]] || [[ "$REPLY" == '' ]]; then
                    tmux_autostart_info 'tmux attaching session...'
                    tmux attach-session
                    if [ $? -eq 0 ]; then
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    tmux_autostart_info 'tmux attaching session...'
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0 ]; then
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[Nn][Oo]*$ ]]; then
                    return 0
                fi
            elif [ ! "$TMUX_DISABLE_AUTO_NEW_SESSION" = 1 ]; then
                e_newline
                tmux_autostart_info 'create a new session automatically'
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
    export PROMPT_COMMAND_DEFAULT
    export PROMPT_COMMAND_ADDITIONAL='new_line_prompt;'
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

    if [ "$PLATFORM" = 'msys' ]; then
        alias open='start'
    else
        alias open='xdg-open'
    fi

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    #alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    # You may want to put all your additions into a separate file like
    # ~/.bash_aliases, instead of adding them here directly.
    # See /usr/share/doc/bash-doc/examples in the bash-doc package.
    if [ -f ~/.bash_aliases ]; then
        source ~/.bash_aliases
    fi
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
}

function bashrc_startup() {
    _prompt_setup
    _alias_setup
    _shopt_setup

    e_bashrc_message 'Hello:)'
    e_newline
    #e_bashrc_message 'DATETIME' "$(date '+%Y-%m-%d %H:%M:%S')"
    e_bashrc_message 'SYSTEM' "${HOSTNAME} / $(uname -smo)"
    e_bashrc_message 'SHELL' "bash ${BASH_VERSION%.*} / pid $$"
    e_bashrc_message 'DISPLAY' "${DISPLAY:-not set}"

    if is_exists 'tmux'; then tmux_autostart; fi
    e_newline
}

# *** settings ***

platform_detect

export FIRST_PROMPT=1

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

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

# disable overwrite (redirect >)
# if want to overwrite then use >|.
set -o noclobber

# avoid logout by "C-d"
set -o ignoreeof

bashrc_startup
