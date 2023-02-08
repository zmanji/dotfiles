if command -v /usr/local/bin/brew >/dev/null 2>&1; then
  export PATH="$(/usr/local/bin/brew --prefix)/bin:${PATH}"
  export PATH="$(/usr/local/bin/brew --prefix)/sbin:${PATH}"
fi

# for brew on m1 macs
if command -v /opt/homebrew/bin/brew >/dev/null 2>&1; then
  export PATH="$(/opt/homebrew/bin/brew --prefix)/bin:${PATH}"
  export PATH="$(/opt/homebrew/bin/brew --prefix)/sbin:${PATH}"
fi

export VISUAL='emacsclient -a ""'
export EDITOR=$VISUAL
export ALTERNATE_EDITOR=""


export CLICOLOR=1


export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
