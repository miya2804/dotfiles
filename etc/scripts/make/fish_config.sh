#!/bin/bash

source "${DOTDIR_PATH}/etc/vital.sh" 2>/dev/null
if ! is_vitalize; then
    echo 'vital.sh not found.'
    exit 1
fi

set -ue

fish_config_root="${HOME}/.config/fish"
fish_config_root_dotfiles="${DOTDIR_PATH}/fish"
fish_prompt_plugin='IlanCosman/tide'
nolink_fish_prompt=false

function help() {
    cat <<EOF
usage: $0 [OPTION]

    --help       show this message
    --deploy     Deploy fish config files as symlink
    --unlink     Remove symlinks of fish config files
EOF
}

function _deploy() {
    echo '--- Deploy fish config files ---'
    if ! is_exists 'fish'; then
        e_newline
        e_warning 'Fish shell is not available'
        e_skip 'Deploy fish config files'
        return
    fi
    mkdir -pv ${fish_config_root}/conf.d ${fish_config_root}/functions
    cat ${fish_config_root_dotfiles}/fish_plugins | grep $fish_prompt_plugin >/dev/null \
      && nolink_fish_prompt=true
    for file_abs in $(find ${fish_config_root_dotfiles} -type f); do
        file_rel=$(realpath --relative-to ${fish_config_root_dotfiles} ${file_abs})
        [ "$file_rel" = 'functions/fish_prompt.fish' ] && [ $nolink_fish_prompt = 'true' ] \
          && echo "skip '$file_rel', because use plugin ${fish_prompt_plugin} for fish prompt." \
          && continue
        target_path="${fish_config_root}/${file_rel}"
        ln -ns --backup=numbered $file_abs ${target_path} \
          && echo "symlink created '${target_path}'"
    done && e_newline && e_done 'Deploy fish config files'
}

function _unlink() {
    echo '--- Remove symlinks of fish config files ---'
    for file_abs in $(find ${fish_config_root_dotfiles} -type f); do
        file_rel=$(realpath --relative-to ${fish_config_root_dotfiles} ${file_abs})
        if [ -L ${fish_config_root}/${file_rel} ]; then
            rm -vrf ${fish_config_root}/${file_rel}
        fi
    done && e_newline && e_done 'Remove symlinks of fish config files'
}

if [ "$#" != 1 ]; then
    e_error 'Invalid number of option'
    help
    exit 1
fi

for opt in "$@"; do
    case $opt in
        --help)
            help
            exit 0
            ;;
        --deploy)
            _deploy
            exit 0
            ;;
        --unlink)
            _unlink
            exit 0
            ;;
        *)
            e_error "unknown option: $opt"
            help
            exit 1
            ;;
    esac
done
