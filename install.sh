#!/bin/sh
#
# Install dotfiles.
# Run this in the project root.

here=`pwd`
backup_directory=.dotfiles.backup

dotfiles=(.zshrc\
              .zsh.d\
              .emacs\
              .emacs.d\
              .iterm2_shell_integration.zsh\
              .shell_aliases\
              .shell_env\
         )

install() {
    cd ~
    for file in ${dotfiles[@]}; do
        if [ -x $file ]; then
            mkdir -p "$backup_directory"
            mv $file "$backup_directory"
        fi

        ln -sf "${here}/${file}" $file
    done
}

uninstall() {
    cd ~
    for file in ${dotfiles[@]}; do
        rm -f $file
        if [ -x "${backup_directory}/${file}" ]; then
            cp -f "${backup_directory}/${file}" $file
        fi
    done
}

help() {
    cat <<EOF
USAGE:

        $0 [install] - Install dotfiles
        $0 uninstall - Uninstall dotfiles and restore previous files from backup
        $0 help      - Show this message
EOF
}

case "$1" in
    help)
        help
        ;;
    uninstall)
        uninstall
        ;;
    *)
        install
esac
