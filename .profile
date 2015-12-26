export PATH="$(brew --prefix)/bin:${PATH}"
export PATH="${HOME}/bin:${PATH}"

export VISUAL=vim
export EDITOR=$VISUAL

export CLICOLOR=1

# Jumping Time
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

if command -v rbenv >/dev/null 2>&1; then eval "$(rbenv init -)"; fi
if command -v pyenv >/dev/null 2>&1; then eval "$(pyenv init -)"; fi
