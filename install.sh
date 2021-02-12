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

cd ~
for file in ${dotfiles[@]}; do
    if [ -x $file ]; then
        mkdir -p "$backup_directory"
        mv $file "$backup_directory"
    fi

    ln -sf "${here}/${file}" $file
done
