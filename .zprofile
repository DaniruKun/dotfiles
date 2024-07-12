export ZSH="$HOME/.oh-my-zsh"

eval "$(/opt/homebrew/bin/brew shellenv)"

# Setup GPG agent
gpgconf --launch gpg-agent
gpg-connect-agent updatestartuptty /bye > /dev/null

export EGREP=egrep

# Completions
FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

export ERL_AFLAGS="-kernel shell_history enabled"

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :
