
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

export ZSH="$HOME/.oh-my-zsh"

# NVM

export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# Setup GPG agent
gpgconf --launch gpg-agent
gpg-connect-agent updatestartuptty /bye > /dev/null

# KERL

export CFLAGS="-O2 -g -fno-stack-check -I/opt/X11/include"
export LFLAGS="-L/opt/X11/lib -lGL -lX11"

# Required for building with OpenSSL
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CPPFLAGS="-I/usr/local/opt/openssl/include"

export KERL_BUILD_DOCS=yes
#export KERL_CONFIGURE_OPTIONS="--disable-hipe --without-javac --without-observer --without-reltool --without-wx --with-ssl=$(brew --prefix openssl)"
