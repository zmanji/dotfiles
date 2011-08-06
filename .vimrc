set nocompatible "Disable vi shit

let mapleader = "," "Leader key

"So I don't have to press shift
nnoremap ; :

set number "set line numbers
set ruler "Show the current position at the bottom
set laststatus=2
syntax on "Turn on syntax highlighting

set encoding=utf-8 "Anything else is plan dumb

set nowrap "Don't wrap lines

"Defaults for whitespace
set tabstop=2
set softtabstop=2 "When backspacing, kill two spaces
set sw=2
set expandtab "Death to tabs

"Show trailing whitespace and tabs
set list listchars=tab:\ \ ,trail:·

"Searching
set hlsearch "highlight searched text
set incsearch "incremental search
set ignorecase "case InSeNsTiVE
set smartcase "If I do use a captial letter in the search, be case-sensitive
"Clear highlights by doing ,/
nmap <silent> <leader>/ :nohlsearch<CR>


set autoread "Automatically read files that are modified outside of vim
set autowrite "Auto-save a modified buffer before switching to another buffer

set backspace=2 "Backspace does what it should do

set numberwidth=5 "For those really long files
set novisualbell "Don't blink please
set noerrorbells "Don't make noise

filetype plugin indent on "Automatically detect file types
set history=1000 "Save a lot of history

set backup "For swp files
"Don't let swp files pollute my current directory
set backupdir=~/.vim/backup

