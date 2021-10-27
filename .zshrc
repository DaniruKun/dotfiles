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
alias -g sleep="~/scripts/./sleep.sh"
alias lg='lazygit'
alias ec='emacsclient'

[ -s "$HOME/.jabba/jabba.sh" ] && source "$HOME/.jabba/jabba.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ /usr/local/bin/kubectl ]] && source <(kubectl completion zsh)

. /usr/local/opt/asdf/asdf.sh
if command -v pyenv >/dev/null; then eval "$(pyenv init -)"; fi
