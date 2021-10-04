export JAVA_HOME
export PATH=$PATH:$JAVA_HOME/bin
export PATH="/usr/local/sbin:$PATH"
export PATH=$HOME/.cache/rebar3/bin:$PATH
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"

export PATH=~/Library/Python/3.7/bin:$PATH

export PATH=$PATH:~/.roswell/bin
export PATH="$HOME/.poetry/bin:$PATH"

export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR="emacs"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# GPG stuff
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# AWS tools
export AWS_DEFAULT_REGION="eu-central-1"
