test -r ~/.zsh.d/spectrum.zsh && source ~/.zsh.d/spectrum.zsh

test -r ~/.shell_aliases && source ~/.shell_aliases

test -r ~/.shell_env && source ~/.shell_env

test -r ~/.shell_secrets && source ~/.shell_secrets

alias reload="source ~/.zshrc"

function rgb() {
    printf "\e[38;2;${1};${2};${3}m"
}

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

function spotify_status() {
    if [ "$SPOTIFY_PROMPT" = "" ]; then
        return
    fi
    output=$(spotify status 2>&1)
    song=$(echo $output | head -n 1 | tr -d '🎤🎵\r' | sed -e 's/^ *//' -e 's/ *$//')
    if [ "$(echo $song | wc -m)" -gt 20 ]; then
        song="$(echo $song | cut -c1-20)…"
    fi
    artist=$(echo $output | head -n 2 | tail -1 | tr -d '🎤🎵\r' | sed -e 's/^ *//' -e 's/ *$//')
    if [ "$(echo $artist | wc -m)" -gt 15 ]; then
        artist="$(echo $artist | cut -c1-15)…"
    fi
    echo "🎵 ${song} by 🎤 ${artist}"
}

function last_status_symbol() {
    exit_code=$?
    if [ $exit_code -eq 0 ]; then
        echo ►
    else
        echo ▻
    fi
}

# 136 100 200; 236 40 40; 255 156 34;78 175 252
function left_prompt() {
    status_symbol="$(last_status_symbol)"
    date_string="$(date +%T)"
    directory_string="$(short_pwd)"
    echo "%{$(rgb 236 40 40)%}# [${date_string}] %{$(rgb 255 156 34)%}${directory_string} ${status_symbol} %{$FX[reset]%}"
}

setopt PROMPT_SUBST
export PROMPT="\$(left_prompt)"
export RPROMPT="\$(spotify_status)%{$(rgb 78 175 252)%}\$(show_gitbranch)%{$FX[reset]%}"

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

eval "$(direnv hook zsh)"
