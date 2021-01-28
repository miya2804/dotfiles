# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# functions
function is_exists() { type "$1" >/dev/null 2>&1; return $?; }
function echo_unicode() { echo -e "\U$1"; }
# tmux
function precmd() {
  if [ ! -z $TMUX ]; then
    tmux refresh-client -S
  else
    dir="%F{cyan}%K{black} %~ %k%f"
    if git_status=$(git status 2>/dev/null ); then
      git_branch="$(echo $git_status| awk 'NR==1 {print $3}')"
       case $git_status in
        *Changes\ not\ staged* ) state=$'%{\e[30;48;5;013m%}%F{black} ± %f%k' ;;
        *Changes\ to\ be\ committed* ) state="%K{blue}%F{black} + %k%f" ;;
        * ) state="%K{green}%F{black} ✔ %f%k" ;;
      esac
      if [[ $git_branch = "master" ]]; then
        git_info="%K{black}%F{blue}⭠ ${git_branch}%f%k ${state}"
      else
        git_info="%K{black}⭠ ${git_branch}%f ${state}"
      fi
    else
      git_info=""
    fi
  fi
}
function is_shell_on_tmux() { [ ! -z "$TMUX" ]; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }
# if not inside a tmux session, and if no session is started,
# start a new session
function tmux_autostart()
{
    if [ ! "$TMUX_AUTOSTART" = yes ]; then
        echo 'TMUX: Automatically attach session disabled.'
        echo 'TMUX: Please set the environment variable TMUX_AUTOSTART to "yes".'
        return 0
    elif ! is_exists 'tmux'; then
        echo 'Error: Tmux command not found.' 2>&1
        return 1
    fi

    if ! is_shell_on_tmux; then
        if shell_has_started_interactively && ! is_ssh_running; then
            if tmux has-session >/dev/null 2>&1 && tmux list-sessions | grep -qE '.*]$'; then
                echo 'TMUX: Detached session exists.'
                tmux list-sessions
                echo -n 'TMUX: Attach? (Y/n/num): '
                read
                if [[ "$REPLY" =~ ^[Yy][Ee]*[Ss]*$ ]] || [[ "$REPLY" == '' ]]; then
                    tmux attach-session
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session."
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session."
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[Nn][Oo]*$ ]]; then
                    return 0
                fi
            elif [ "$TMUX_AUTO_NEW_SESSION" == yes ]; then
                echo 'TMUX: Created a new session automatically.'
                tmux new-session
            else
                echo 'TMUX: Automatically new session create is disabled.'
            fi
        fi
    else
        # Shell on tmux
        if [ -e "$HOME/.dotfiles/etc/ascii-art/tmux.txt" ]; then
            echo -n 'Welcome to the tmux! Session: '
            tmux display-message -p '#S'
            #cat "$HOME/.dotfiles/etc/ascii-art/tmux.txt"
        fi
    fi
}

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

# [01;38;2;<R>;<G>;<B>m\] (Foreground)
# [01;48;2;<R>;<G>;<B>m\] (Background)
if [ "$color_prompt" = yes ]; then
    if [ ! -z "$TMUX" ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;34m\]$(echo_unicode '1F340')\u> \[\033[00m\]'
    else
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;38;2;135;255;197m\]$(echo_unicode '1F340')\u@\h\[\033[00m\]:\[\033[01;34m\]\w$(__git_ps1 " (%s)")> \[\033[00m\]'
    fi
else
    if [ ! -z "$TMUX" ]; then
        PS1='${debian_chroot:+($debian_chroot)}\u> '
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w$(__git_ps1 " (%s)")> '
    fi
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
#alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Default alias definitions.
alias ll='ls -alF'
alias la='ls -AF'
alias l='ls -CF'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias open='xdg-open'

# Local Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# git-prompt.sh setting.
source ~/bin/git-prompt.sh
GIT_PS1_SHOWUPSTREAM=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWDIRTYSTATE=1

# Settings.
# disable overwrite (redirect >)
# if want to overwrite then use >|.
set -o noclobber
# avoid logout by "C-d"
set -o ignoreeof

tmux_autostart
