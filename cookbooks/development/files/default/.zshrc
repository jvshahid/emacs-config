# Set up the prompt

autoload -Uz promptinit
promptinit
prompt off

setopt histignorealldups sharehistory

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# my changes

setopt KSH_ZERO_SUBSCRIPT

WORDCHARS="*?[]~&;!#$%^(){}<>"
source $HOME/alias.sh
source $HOME/functions.sh
source $HOME/exports.sh
source $HOME/prompt.sh
source $HOME/.fzf.zsh

# install direnv
eval "$(direnv hook zsh)"

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e
bindkey \^U backward-kill-line
bindkey \^W kill-region
bindkey '^[[Z' reverse-menu-complete

function copy-command-to-clipboard {
    echo "$BUFFER" | xclipc
}

zle -N copy-command-to-clipboard
bindkey '^[C' copy-command-to-clipboard
PATH=$PATH:$HOME/bin

# turn on command line editing
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line
