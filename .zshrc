test -r ~/.zsh.d/spectrum.zsh && source ~/.zsh.d/spectrum.zsh

test -r ~/.shell_aliases && source ~/.shell_aliases

test -r ~/.shell_env && source ~/.shell_env

function short_pwd() {
    pwd | sed -E 's/\/.*(\/.*\/.*)/\.\.\.\1/'
}

function show_gitbranch() {
    branch=$(git branch 2>/dev/null | grep -F --color=never '* ' | sed 's/\* *//g')
    if [ ! -z "$branch" ]; then
        str="$branch"
        if [ `git diff 2>/dev/null | wc -l 2>/dev/null` -gt 0 ]; then
            str="${str}*"
        fi
        echo "($str)"
    fi
}

function last_status_symbol() {
    exit_code=$?
    if [ $exit_code -eq 0 ]; then
        echo ►
    else
        echo ▻
    fi
}

function left_prompt() {
    status_symbol="$(last_status_symbol)"
    date_string="$(date +%T)"
    directory_string="$(short_pwd)"
    echo "%{$FG[099]%}# [${date_string}] %{$FG[203]%}${directory_string}%{$FG[009]%} ${status_symbol} %{$FX[reset]%}"
}

setopt PROMPT_SUBST
export PROMPT="\$(left_prompt)"
export RPROMPT="%{$FG[172]%}\$(show_gitbranch)%{$FX[reset]%}"

setopt MENU_COMPLETE

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Disable start/stop control flow so that we can use C-s to i-search forward.
stty -ixon

# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' max-errors 2 not-numeric
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/Users/luke/.zshrc'

# 3rd party completions
fpath=(~/.zsh.d/completions $fpath)

# Load completions
autoload -Uz compinit && compinit

# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=5000
SAVEHIST=5000
setopt appendhistory extendedglob
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
