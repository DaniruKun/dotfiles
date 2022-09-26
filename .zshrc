# Theme configurations
ZSH_THEME="bullet-train"
BULLETTRAIN_PROMPT_ORDER=(
  context
  git
  dir
)
BULLETTRAIN_PROMPT_SEPARATE_LINE=false
BULLETTRAIN_PROMPT_CHAR=\λ

if [ -z "$VIMRUNTIME" -a "$NONINTERACTIVE" = false ]; then
  IFLAG="-i"
fi

_fix_cursor() {
   echo -ne '\e[5 q'
}

# precmd_functions+=(_fix_cursor)

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"
plugins=(git zsh-autosuggestions zsh-z colored-man-pages zsh-syntax-highlighting ansible aws brew docker docker-compose lein macos pip python)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"
alias lg='lazygit'
alias ec='emacsclient'
alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias my-wip-tickets='zube card ls --status in_progress --project-id 5953 --assignee-id 324110'
alias my-done-tickets='zube card ls --status done --project-id 5953 --assignee-id 324110'
# Aliases for Platogo Office VPN
alias vpnc='networksetup -connectpppoeservice "Platogo VPN"'
alias vpndc='networksetup -disconnectpppoeservice "Platogo VPN"'
# ZSH autocorrect ignorelist
alias rspec='nocorrect rspec'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ /usr/local/bin/kubectl ]] && source <(kubectl completion zsh)

. /usr/local/opt/asdf/asdf.sh
export PATH="/usr/local/opt/gnupg@2.2/bin:$PATH"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
