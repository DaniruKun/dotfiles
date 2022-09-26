export ZSH="$HOME/.oh-my-zsh"

# Setup GPG agent
gpgconf --launch gpg-agent
gpg-connect-agent updatestartuptty /bye > /dev/null

# Ruby
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=`brew --prefix openssl@1.1` --with-readline-dir=`brew --prefix readline` --with-libyaml-dir=`brew --prefix libyaml`"

# KERL

export CFLAGS="-O2 -g -fno-stack-check -I/opt/X11/include"
export LFLAGS="-L/opt/X11/lib -lGL -lX11"

# Required for building with OpenSSL
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"

export KERL_BUILD_DOCS=yes
export KERL_CONFIGURE_OPTIONS="--without-javac --with-ssl=$(brew --prefix openssl@1.1)"

# Completions
FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
