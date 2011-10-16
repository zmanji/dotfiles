" This is my `.vimrc`. These are my settings for Vim
"
" First disable Vi compatibility. This must be done first because it will
" affect the other options
set nocompatible
" ###Leader Mappings###
" These two mappings set up the leader key. The <leader> mapping defaults to
" `\` but that key is hard to reach. I prefer to set it to `,`
" `mapleader` is used for global mappings and `maplocalleader` is used for
" buffer-local mappings. For example the pandoc plugin has a few buffer local
" mappings that make editing a pandoc document nicer.
"
" There can be some conflict with buffer local mappings and global mappings
" from plugins and what I define if I set `mapleader` and `maplocalleader` to
" be the same, but I prefer to deal with that when the problem arises.
"
" Note that these need to be defined here because all the plugins make use of
" the leader key

let mapleader = ","
let maplocalleader = ","

" ### Plugin Installation ###
" Vim plugin management is difficult at best so we use [Vundle][vu] to help us
" install and manage plugins.
"
" Vundle lets a very cool `Bundle` function become available. You can feed it
" a string in the form from `user/repo` and will get the appropriate github
" repo. Other strings could be a vim-script repo or a git url for a non github
" repo. The Vundle documentation has more information.
"
" I use Vundle over Pathogen because pathogen requires you to manage the
" plugins externally and it would fill my `.vim/` will all sorts of stuff that
" I did not write.
"
" [vu]: https://github.com/gmarik/vundle
"
" These lines are required to set up vundle/
" This assumes we have vundle installed in `/.vim/bundle/vundle`
"

filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" This line lets Vundle manage itself. This is required. This will also ensure
" that when you run `BundleInstall!` to update your plugins it will also
" update vundle, which is very nice.
Bundle "gmarik/vundle"

" ####Solarized Colorscheme####
" [Solarized][sol] is the only good colorscheme ever. This also ensures I
" always have the latest version.
"
" [sol]: http://ethanschoonover.com/solarized
Bundle "altercation/vim-colors-solarized"
" Enables solarized
colorscheme solarized
" Use dark solarized
set background=dark

" ####Ack.vim####
" `ack` is better than `grep` and [Ack.vim][ack] brings integration to Vim. It
" acts as a replacement for Vim's `:grep` commands.
"
" [ack]: https://github.com/mileszs/ack.vim
Bundle "mileszs/ack.vim"

" ####Vim-Pandoc####
" This [plugin][pdc-vim] is a bundle of tools for [Pandoc][pdc]'s extended markdown. It
" provides Pandoc and regular markdown syntax highlighting, folding and a few
" useful local-leader mappings. Weird behaviour tends to happen when installed
" with vim-markdown but this has most of that functionality.
"
" [pdc]: http://johnmacfarlane.net/pandoc/
" [pdc-vim]: https://github.com/vim-pandoc/vim-pandoc
Bundle "vim-pandoc/vim-pandoc"

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
noremap <leader>n :NERDTreeToggle<CR>
",n will open or close the nerd tree buffer

" Project File Open
Bundle "wincent/Command-T"

" Command-T configuration
set wildignore+=*.o,.git,*.jpg,*.png,*.swp,*.d,*.gif " Ignore these filetypes
nnoremap <silent> <leader>t :CommandT<CR>
nnoremap <silent> <leader>b :CommandTBuffer<CR>

" A GUI for Vims undo
Bundle "sjl/gundo.vim"

" Use ,u to open Gundo only in normal mode
nnoremap <Leader>u :GundoToggle<CR>

" Rails integration
Bundle "tpope/vim-rails"

" Tim Pope may actually be the best programmer alive
" Best Git Integration Ever
Bundle "tpope/vim-fugitive"

" Commenting
Bundle "tpope/vim-commentary"

" Surrounding things
Bundle "tpope/vim-surround"

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
nnoremap <silent> <leader>/ :nohlsearch<CR>

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
