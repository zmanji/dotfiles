# This is the first file sourced by zsh
# From the zsh introduction:
# `.zshenv` is sourced on all invocations of the shell, unless the -f option is set.
# It should contain commands to set the command search path, plus other important environment variables.
# `.zshenv` should not contain commands that produce output or assume the shell is attached to a tty.

# Cabal is the Haskell package manager. Need this for markdown2pdf mainly
export PATH=~/.cabal/bin:$PATH

# This came from the mysql install
export DYLD_LIBRARY_PATH="/usr/local/mysql/lib:$DYLD_LIBRARY_PATH"
export PATH="/usr/local/mysql/bin:$PATH"

# For binaries installed by homebrew
export PATH="/usr/local/bin:$PATH"

# Personal binaries should override everything
export PATH="~/bin:$PATH"

# Rbenv should be managing rubies
eval "$(rbenv init -)"

export EDITOR=vim
export PAGER=less

# Enable color BSD coreuilts
export CLICOLOR=1

export HISTSIZE=100000
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=$HISTSIZE
