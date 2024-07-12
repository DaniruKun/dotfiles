export JAVA_HOME
export PATH=$PATH:$JAVA_HOME/bin
export PATH="/usr/local/sbin:$PATH"
export PATH=$HOME/.cache/rebar3/bin:$PATH
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"

export PATH=$PATH:~/.roswell/bin
export PATH="$HOME/.poetry/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.mix/escripts:$PATH"
export PATH="/Library/Application Support/iODBC/bin:$PATH"

export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR="$(which code) --wait"
export DISABLE_SPRING=true

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# AWS tools
## AWS Vault
export AWS_SESSION_TOKEN_TTL=36h
export AWS_VAULT_PROMPT=ykman

. "$HOME/.cargo/env"
