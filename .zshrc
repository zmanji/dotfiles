# This is the third file sourced by zsh after `.zshenv` and `.zprofile`
# This is sourced before `.zlogin` and `.zlogout`
# From the zsh guide:
# `.zshrc' is sourced in interactive shells.
# It should contain commands to set up aliases, functions, options, key bindings, etc.


# Add autojump into the shell

if [ -f `brew --prefix`/etc/autojump ]; then
  . `brew --prefix`/etc/autojump
fi


# Super useful aliases

alias g='git'

# opens file in new tab in existing vim
alias vt='mvim --remote-tab-silent'

# Directory related aliases
alias ..='cd ..'
alias ...='cd ../..'
alias cd..='cd ..'
alias -- -='cd -'

# Super user
alias _='sudo'

alias l='ls -lah'


# Shell options

# This allows for multiple zsh sessions to not clobber history
setopt appendhistory
# If a command can't be executed and it is the name of a directory, cd into it
setopt autocd

# If a command is a duplicate of the previous command, don't append it to history
setopt histignoredups

# Do not autoselect the first completion entry
unsetopt menu_complete

# Show the completion menu on succesive tab presses
setopt auto_menu

# Automatically list choices on ambiguous completion.
setopt auto_list


# Loadup Autocompletion
autoload -Uz compinit
# Enable autocompletion
compinit -i


# PS1='\h:\W \u\$ '
