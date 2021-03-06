# Jumping Time
# TODO: consider replacing this with fasd
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

export PATH="${HOME}/bin:${PATH}"
if command -v rbenv >/dev/null 2>&1; then eval "$(rbenv init -)"; fi
if command -v pyenv >/dev/null 2>&1; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"; 

fi
if [[ -f ~/.cargo/env ]]; then source ~/.cargo/env; fi
if command -v direnv >/dev/null 2>&1; then eval "$(direnv hook zsh)"; fi
if command -v nodenv >/dev/null 2>&1; then eval "$(nodenv init -)"; fi


if command -v broot >/dev/null 2>&1; then 
  eval "$(broot --print-shell-function zsh)";
  broot --set-install-state installed;
fi

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=$HISTSIZE

setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_VERIFY

setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt PUSHD_TO_HOME
DIRSTACKSIZE=20

setopt EXTENDED_GLOB
setopt NO_CASE_GLOB

setopt LONG_LIST_JOBS
setopt NOTIFY

setopt HASH_LIST_ALL

setopt COMPLETE_IN_WORD
setopt ALWAYS_TO_END
setopt AUTO_LIST
setopt AUTO_MENU
setopt AUTO_PARAM_SLASH
unsetopt FLOW_CONTROL

setopt NO_BEEP

setopt INTERACTIVECOMMENTS

setopt NO_CLOBBER

setopt PROMPT_SUBST

# Report CPU Usuage for commands running longer than 10 seconds
REPORTTIME=10

fpath=(~/.zsh $(brew --prefix)/share/zsh-completions $fpath)

autoload -Uz compinit
compinit

if command -v kitty >/dev/null 2>&1; then eval "$(kitty + complete setup zsh)"; fi

zstyle ':completion:*' menu select
# Case Insensitive Auto Complete
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# History Completion
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

autoload zmv

autoload -Uz promptinit
promptinit
prompt zmanji

# Dumb terminals (aka emacs shell-mode) can't handle the control codes.
if [[ "$TERM" != 'dumb' ]]; then
  function zle-line-init zle-keymap-select () {
    case $KEYMAP in
      vicmd) echo -n "\e[2 q" ;; # block cursor
      viins|main) echo -n "\e[6 q";; # line cursor
    esac
  }
  zle -N zle-line-init
  zle -N zle-keymap-select
fi

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Order of these two matters
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $(brew --prefix)/share/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey -v

if [[ -d $(brew --prefix)/opt/fzf/shell ]]; then
  # This binds the following:
  # * CTRL-T => find file
  # * ALT-C cd
  # * CTRL-R history search
  export FZF_CTRL_T_COMMAND='fd --type f --hidden --follow --exclude .git'
  export FZF_CTRL_T_OPTS="--select-1 --exit-0"
  source "/usr/local/opt/fzf/shell/key-bindings.zsh"
fi


autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd "v" edit-command-line
bindkey -M vicmd "u" undo

zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete # Shift-Tab

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Get bacspace to work
bindkey -M viins "^?" backward-delete-char

# add path to tab title
if [[ "$TERM" != 'dumb' ]]; then
  function zmanji_set_title() {
    print -Pn "\033]zsh ◆ %1~\007"
  }
  add-zsh-hook precmd zmanji_set_title
  add-zsh-hook preexec zmanji_set_title
fi

source ~/.zsh/3rdparty/base16-tomorrow-night.sh
source ~/.zsh/3rdparty/zsh-system-clipboard/zsh-system-clipboard.zsh
