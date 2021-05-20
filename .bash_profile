# local settings
if [ -f "${HOME}/.local_profile" ]; then
   source "${HOME}/.local_profile"
fi

# path settings
if [ -d "${HOME}/bin" ]; then
	PATH="${HOME}/bin:${PATH}"
fi

if [ -d "${HOME}/.local/bin" ]; then
	PATH="${HOME}/.local/bin:${PATH}"
fi
export PATH

# load bashrc
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
	    source "${HOME}/.bashrc"
    fi
fi
