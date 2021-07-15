source "${DOTDIR_PATH}/etc/vital.sh" 2>/dev/null \
    && platform_detect

# path settings
if [ -d "${HOME}/bin" ]; then
    PATH="${HOME}/bin:${PATH}"
fi

if [ -d "${HOME}/.local/bin" ]; then
    PATH="${HOME}/.local/bin:${PATH}"
fi
export PATH

# x server settings of wsl
if [ "$PLATFORM" = 'wsl' ]; then
    if is_exists 'tasklist.exe' && ! tasklist.exe | grep -F -q 'vcxsrv.exe'; then
        (
            cd $(dirname $(which vcxsrv.exe))
            ./vcxsrv.exe :0 -multiwindow -clipboard -noprimary -wgl > /dev/null 2>&1 &

            # authentication using xauth
            display=$(ip route | awk '/^default/ {print $3; exit}'):0.0
            ./xauth.exe generate $display . trusted timeout 0
            ./xauth.exe extract - $display | xauth merge -
        )
    fi
    export DISPLAY=$(ip route | awk '/^default/ {print $3; exit}'):0.0
fi

# local settings
if [ -f "${HOME}/.local_profile" ]; then
    source "${HOME}/.local_profile"
fi

# load bashrc
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
        source "${HOME}/.bashrc"
    fi
fi
