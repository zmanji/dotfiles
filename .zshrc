# Jumping Time
# TODO: consider replacing this with fasd
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

export PATH="${HOME}/bin:${PATH}"
if command -v rbenv >/dev/null 2>&1; then eval "$(rbenv init -)"; fi
if command -v pyenv >/dev/null 2>&1; then eval "$(pyenv init -)"; fi

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

setopt NO_CLOBBER

setopt PROMPT_SUBST

# Report CPU Usuage for commands running longer than 10 seconds
REPORTTIME=10

fpath=(~/.zsh $(brew --prefix)/share/zsh-completions $fpath)

autoload -Uz compinit
compinit

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


function zle-line-init zle-keymap-select () {
  case $KEYMAP in
    vicmd) echo -n "\e[2 q" ;; # block cursor
    viins|main) echo -n "\e[6 q";; # line cursor
  esac
}
zle -N zle-line-init
zle -N zle-keymap-select

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Order of these two matters
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $(brew --prefix)/share/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey -v

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

source ~/.zsh/3rdparty/base16-tomorrow-night.sh
