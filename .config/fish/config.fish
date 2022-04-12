# EXPORT
#set -gx PATH $PATH $HOME/bin
set -gx PATH $PATH $HOME/.local/bin
#set TERM "xterm-256color"
set -gx EDITOR nvim

# PLUGINS
starship init fish | source


# ALIASES

# clear
alias clear='printf \'\033[2J\033[3J\033[1;1H\''

# cd nav
alias ..='cd ..'

# vim
alias vim='nvim'

# exa
alias ls='exa -la --group-directories-first'
alias la='exa -la --group-directories-first'
alias ll='exa -la --group-directories-first'

# ssh
alias ssh='kitty +kitten ssh'

# su
#alias su='su -c fish -m'

