# Theme configurations
ZSH_THEME="apple"
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

_prompt_aws_vault() {
  local vault_segment
  vault_segment="`prompt_aws_vault_segment`"
  [[ $vault_segment != '' ]] && prompt_segment cyan black "$vault_segment"
}

AWS_VAULT_PL_DEFAULT_PROFILE=platogo

# precmd_functions+=(_fix_cursor)
precmd_functions+=(_prompt_aws_vault)

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"
plugins=(git mix-fast zsh-autosuggestions zsh-z zsh-aws-vault colored-man-pages zsh-syntax-highlighting aws brew lein macos pip python direnv)

source $ZSH/oh-my-zsh.sh

# User configuration

# Aliases

# export MANPATH="/usr/local/man:$MANPATH"
alias lg='lazygit'
alias ec='emacsclient'
alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias my-wip-tickets='zube card ls --status in_progress --project-id 5953 --assignee-id 324110'
alias my-done-tickets='zube card ls --status done --project-id 5953 --assignee-id 324110'
alias atoss='atoss-cli'
alias zube='zube-cli'

# ZSH autocorrect ignorelist
alias rspec='nocorrect rspec'

ulimit -n 10240

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ /usr/local/bin/kubectl ]] && source <(kubectl completion zsh)

export PATH="/usr/local/opt/gnupg@2.2/bin:$PATH"

# GPG stuff
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Nix
export NIXPKGS_ALLOW_UNFREE=1
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  unset __ETC_PROFILE_NIX_SOURCED
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

# pnpm
export PNPM_HOME="/Users/dpetrovs/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
