# Theme configurations
ZSH_THEME="bullet-train"
BULLETTRAIN_PROMPT_ORDER=(
  context
  git
  dir
)

BULLETTRAIN_PROMPT_CHAR=\λ

if [ -z "$VIMRUNTIME" -a "$NONINTERACTIVE" = false ]; then
  IFLAG="-i"
fi

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"
plugins=(git zsh-autosuggestions zsh-z colored-man-pages zsh-syntax-highlighting ansible aws brew docker docker-compose lein osx pip python)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

alias -g sleep="~/scripts/./sleep.sh"
alias lg='lazygit'

[ -s "$HOME/.jabba/jabba.sh" ] && source "$HOME/.jabba/jabba.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ /usr/local/bin/kubectl ]] && source <(kubectl completion zsh)

. /usr/local/opt/asdf/asdf.sh
if command -v pyenv >/dev/null; then eval "$(pyenv init -)"; fi
