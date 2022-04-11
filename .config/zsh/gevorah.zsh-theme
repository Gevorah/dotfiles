# red:     '#cc241d'
# green:   '#98971a'
# yellow:  '#d79921'
# blue:    '#458588'
# magenta: '#b16286'
# cyan:    '#689d6a'
# white:   '#a89984'

# black:   '#928374'
# red:     '#fb4934'
# green:   '#b8bb26'
# yellow:  '#fabd2f'
# blue:    '#83a598'
# magenta: '#d3869b'
# cyan:    '#8ec07c'
# white:   '#ebdbb2'
# #d65d0e

setup() {
  autoload -Uz vcs_info
  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' check-for-changes false
  zstyle ':vcs_info:git*' formats '%b'
  zstyle ':vcs_info:git*' actionformats '%b (%a)'
}

precmd() {
  vcs_info
}

username_prompt() {
  echo "%F{#b16286}%n%f@%F{#689d6a}%m%f"
}

directory_prompt() {
  echo "%F{#458588}%~%f"
}

git_prompt() {
  changes() {
    echo $(git status --porcelain | grep -o "$1" | wc -l)
  }
  ref="$vcs_info_msg_0_"
  if [[ -n "$ref" ]]; then
    add=""
    if [[ "$(changes "??")" != "0" ]]; then
      add="%F{#cc241d}+$(changes "??")%f"
    fi
    mod="%F{#83a598}..$(changes "M")%f"
    bod="%F{#d65d0e}"
    if [[ "${ref/.../}" == "$ref" ]]; then
      bod+="*%f"
      ref="${ref}"
    else
      bod=">%f"
      ref="${ref/.../}"
    fi
    echo "[$bod%F{#d79921}$ref%f $add$mod]"
  fi
}

setup "$@"
PROMPT='$(username_prompt) $(directory_prompt) $(git_prompt)
%F{#d65d0e}$%f '

