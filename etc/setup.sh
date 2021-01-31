#!/bin/bash

# *** general utilities ***

# is_exists returns true if executable $1 exists in $PATH
is_exists() {
    which "$1" >/dev/null 2>&1
    return $?
}

# is_debug returns true if $DEBUG is set
is_debug() {
    if [ "$DEBUG" = 1 ]; then
        return 0
    else
        return 1
    fi
}

e_newline() { printf "\n"; }
e_warning() { printf " \033[31m%s\033[m\n" "$*"; }
e_arrow()   { printf " \033[37;1m%s\033[m\n" "➜ $*"; }
e_header()  { printf " \r\033[37;1m%s\033[m\n" "$*"; }
e_error()   { printf " \033[31m%s\033[m\n" "✖ $*" 1>&2; }
e_done()    { printf " \033[37;1m%s\033[m...\033[32mOK\033[m\n" "✔ $*"; }
e_fail()    { printf " \033[37;1m%s\033[m...\033[31mFAILD\033[m\n" "✔ $*"; }

ink() {
    if [ "$#" -eq 0 -o "$#" -gt 2 ]; then
        echo "Usage: ink <color> <text>"
        echo "Colors:"
        echo "  black, white, red, green, yellow, blue, purple, cyan, gray"
        return 1
    fi

    local open="\033["
    local close="${open}0m"
    local black="0;30m"
    local red="1;31m"
    local green="1;32m"
    local yellow="1;33m"
    local blue="1;34m"
    local purple="1;35m"
    local cyan="1;36m"
    local gray="0;37m"
    local white="$close"

    local text="$1"
    local color="$close"

    if [ "$#" -eq 2 ]; then
        text="$2"
        case "$1" in
            black | red | green | yellow | blue | purple | cyan | gray | white)
            eval color="\$$1"
            ;;
        esac
    fi

    printf "${open}${color}${text}${close}"
}

logging() {
    if [ "$#" -eq 0 -o "$#" -gt 2 ]; then
        echo "Usage: ink <fmt> <msg>"
        echo "Formatting Options:"
        echo "  TITLE, ERROR, WARN, INFO, SUCCESS"
        return 1
    fi

    local color=
    local text="$2"

    case "$1" in
        TITLE)
            color=yellow
            ;;
        ERROR | WARN)
            color=red
            ;;
        INFO)
            color=blue
            ;;
        SUCCESS)
            color=green
            ;;
        *)
            text="$1"
    esac

    timestamp() {
        ink gray "["
        ink purple "$(date +%H:%M:%S)"
        ink gray "] "
    }

    timestamp; ink "$color" "$text"; echo
}

log_fail() { logging WARN "$1"; }
log_fail() { logging ERROR "$1" 1>&2; }
log_pass() { logging SUCCESS "$1"; }
log_info() { logging INFO "$1"; }
log_echo() { logging TITLE "$1"; }

# *** preparation ***

# SCRIPT_DIR="$(cd "$(dirname -- "${BASH_SOURCE:-$0}")"; pwd -P)"

if [ -z "${DOTDIR_PATH:-}" ]; then
    DOTDIR_PATH=~/.dotfiles; export DOTDIR_PATH
fi

GITHUB_USER="mmugi"
DOTFILES_GITHUB="https://github.com/${GITHUB_USER}/dotfiles.git"; export DOTFILES_GITHUB
DOTFILES_GITHUB_BRANCH="master"

dotfiles_logo='
*********************************************
 ___   ___  _____  ____  _   _     ____  __
| | \ / / \  | |  | |_  | | | |   | |_  ( (`
|_|_/ \_\_/  |_|  |_|   |_| |_|__ |_|__ _)_)

  *** WHAT IS INSIDE? ***
  1. Download https://github.com/'"$GITHUB_USER"'/dotfiles.git
  2. Symlinking dot files to your home directory
  3. Execute all sh files within `etc/init/` (optional)

  See the README for documentation.
    - https://github.com/'"$GITHUB_USER"'/dotfiles

  I used dotfiles(@b4b4r07) as reference.
  I would like to thank you for your activity!!!
    - https://github.com/b4b4r07/dotfiles
    - https://qiita.com/b4b4r07/items/b70178e021bef12cd4a2

*********************************************
'

dotfiles_download() {
    if [ -d "$DOTDIR_PATH" ]; then
        log_fail "${DOTDIR_PATH}: already exists"
        exit 1
    fi

    e_newline
    e_header "Downloading dotfiles..."

    if is_debug; then
        :
    else
        if is_exists "git"; then
            # --recursive equals to ...
            # git submodule init
            # git submodule update
            git clone --recursive -b "$DOTFILES_GITHUB_BRANCH" "$DOTFILES_GITHUB" "$DOTDIR_PATH"
        elif is_exists "curl" || is_exists "wget"; then
            # curl or wget
            local tarball="https://github.com/${GITHUB_USER}/dotfiles/archive/${DOTFILES_GITHUB_BRANCH}.tar.gz"
            local extract_dir="dotfiles-${DOTFILES_GITHUB_BRANCH}"
            if is_exists "curl"; then
                curl -L "$tarball"
            elif is_exists "wget"; then
                wget -O - "$tarball"
            fi | tar xvz

            if [ ! -d "$extract_dir" ]; then
                log_fail "${extract_dir}: not found"
                exit 1
            fi
            command mv -f "$extract_dir" "$DOTDIR_PATH"
        else
            log_fail "curl or wget required"
            exit 1
        fi
    fi

    e_newline &&

	if [ ! -d $DOTDIR_PATH ]; then
            log_fail "Dotfiles download failed"
            exit 1
	else
            e_done "Download"
	fi
}

dotfiles_deploy() {
    e_newline
    e_header "Deploying dotfiles..."

    if [ ! -d $DOTDIR_PATH ]; then
        log_fail "${DOTDIR_PATH}: not found"
        exit 1
    fi

    cd "$DOTDIR_PATH"

    if is_debug; then
        :
    else
        if ! is_exists "make"; then
            log_fail "make required"
            exit 1
        elif [ ! -f Makefile ]; then
            log_fail "Makefile: not found"
            exit 1
        else
            make deploy
        fi
    fi &&

        e_newline && e_done "Deploy"
}

dotfiles_initialize() {
    if [ "$1" = "init" ]; then
        e_newline
        e_header "Initializing dotfiles..."

        if is_debug; then
            :
        else
            if ! is_exists "make"; then
                log_fail "make required"
                exit 1
            elif [ ! -f Makefile ]; then
                log_fail "Makefile: not found"
                exit 1
            else
                make init
            fi
        fi &&

            e_newline && e_done "Initialize"
    fi
}

dotfiles_install() {
    printf " \r\033[37;1m*** dotfiles install ***\033[m\n"
    # 1. Download the repository
    # ==> downloading
    #
    # Priority: git > curl > wget
    dotfiles_download &&

    # 2. Deploy dotfiles to your home directory
    # ==> deploying
    dotfiles_deploy &&

    # 3. Execute all sh files within etc/init/
    # ==> initializing
    dotfiles_initialize "$@"
}

# *** body ***

if echo "$-" | grep -q "i"; then
    # -> source a.sh
    VITALIZED=1
    export VITALIZED

    : return
else
    # three patterns
    # -> cat a.sh | bash
    # -> bash -c "$(cat a.sh)"
    # -> bash a.sh

    # -> bash a.sh
    if [ "$0" = "${BASH_SOURCE:-}" ]; then
        exit
    fi

    # -> cat a.sh | bash
    # -> bash -c "$(cat a.sh)"
    if [ -n "${BASH_EXECUTION_STRING:-}" ] || [ -p /dev/stdin ]; then
        # if already vitalized, skip to run dotfiles_install
        if [ "${VITALIZED:=0}" = 1 ]; then
            exit
        fi
    fi

    trap "e_error 'terminated'; exit 1" INT ERR
    echo "$dotfiles_logo"
    dotfiles_install "$0"

    # Restart shell if specified "bash -c $(curl -L {URL})"
    # not restart:
    #   curl -L {URL} | bash
    if [ -p /dev/stdin ]; then
        e_warning "Now continue with Rebooting your shell"
    else
        e_newline
        e_arrow "Restarting your shell..."
        exec "${SHELL:-/bin/bash}"
    fi
fi

# setup script ends here
