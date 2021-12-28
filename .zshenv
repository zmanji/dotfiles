if command -v brew >/dev/null 2>&1; then
  export PATH="$(/usr/local/bin/brew --prefix)/bin:${PATH}"
  export PATH="$(/usr/local/bin/brew --prefix)/sbin:${PATH}"
fi

export VISUAL='emacsclient -a ""'
export EDITOR=$VISUAL
export ALTERNATE_EDITOR=""


export CLICOLOR=1


export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
