set nocompatible "Disable vi shit

let mapleader = "," "Leader key
let maplocalleader = "," "Without this, things might break

" Plugin installation (Vundle)

filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" let Vundle manage Vundle
" required!
Bundle "gmarik/vundle"

" Solarized Colorscheme
Bundle "altercation/vim-colors-solarized"

" In vim ack
Bundle "mileszs/ack.vim"

" Pandoc (including Markdown) syntax highlighting
Bundle "vim-pandoc/vim-pandoc"

" Disable the folding by the pandoc plugin
let g:pandoc_no_folding = 1

" CoffeeScript (and bonus eco) highlighting
Bundle "kchmck/vim-coffee-script"

" HAML/SASS/SCSS syntax highlighting
Bundle "tpope/vim-haml"

" Detects incorrect syntax
Bundle "scrooloose/syntastic"

" Enable Syntastic
let g:syntastic_enable_signs=1

" Autocomplete trigged by tab
Bundle "ervandew/supertab"

" File browsing
Bundle "scrooloose/nerdtree"
map <leader>n :NERDTreeToggle<CR>
",n will open or close the nerd tree buffer

" Project File Open
Bundle "wincent/Command-T"

" Command-T configuration
set wildignore+=*.o,.git,*.jpg,*.png,*.swp,*.d,*.gif " Ignore these filetypes

" Note that Command-T takes <Leader>t to open up the file browser and
" <Leader>b for the buffer browser

" A GUI for Vims undo
Bundle "sjl/gundo.vim"

" Use ,u to open Gundo only in normal mode
nmap <Leader>u :GundoToggle<CR>

" Rails integration
Bundle "tpope/vim-rails"

" Tim Pope may actually be the best programmer alive
" Best Git Integration Ever
Bundle "tpope/vim-fugitive"

" End Plugin installation

set notitle " Disable 'thanks for flying vim' thing

set mouse=a " Enable mouse in gui in terminal (just in case)

set hidden " Allows me to have unsaved changes in a buffer when switching to another one

"So I don't have to press shift
nnoremap ; :

set number "set line numbers
set ruler "Show the current position at the bottom
set laststatus=2
syntax on "Turn on syntax highlighting

set encoding=utf-8 "Anything else is plain dumb

set nowrap "Don't wrap lines

"Defaults for whitespace
set tabstop=2
set softtabstop=2 "When backspacing, kill two spaces
set sw=2
set expandtab "Death to tabs
set shiftround "use a mutliple of shiftwidth (sw) when using < and >

"Show trailing whitespace and tabs
set list listchars=tab:\ \ ,trail:·

"Searching
set hlsearch "highlight searched text
set incsearch "incremental search
set ignorecase "case InSeNsTiVE
set smartcase "If I do use a captial letter in the search, be case-sensitive
"Clear highlights by doing ,/
nmap <silent> <leader>/ :nohlsearch<CR>

set wildmenu "Tab autocompletion

set autoread "Automatically read files that are modified outside of vim
set autowrite "Auto-save a modified buffer before switching to another buffer

set backspace=2 "Backspace does what it should do

set numberwidth=5 "For those really long files
set novisualbell "Don't blink please
set noerrorbells "Don't make noise
set vb t_vb= "Disable any time of beeping or flashing

filetype plugin indent on "Automatically detect file types
set history=1000 "Save a lot of history

set nobackup "No need for ~ files, I use git most of the time

" To help me master vim
" Disable arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
vnoremap <up> <nop>
vnoremap <down> <nop>
vnoremap <left> <nop>
vnoremap <right> <nop>

" It's 2011.
noremap j gj
noremap k gk


" So splits work like in other programs I se
set splitbelow "Split to the bottom
set splitright "Split to the right

"Color Options
set background=dark
colorscheme solarized
