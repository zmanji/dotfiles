set nocompatible



let mapleader = ","
let maplocalleader = ","

filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" This line lets Vundle manage itself. This is required. This will also ensure
" that when you run `BundleInstall!` to update your plugins it will also
" update vundle, which is very nice.
Bundle "gmarik/vundle"

Bundle 'chriskempson/base16-vim'
colorscheme base16-tomorrow
" This ensures that the dark version is used.
set background=dark

Bundle "vim-pandoc/vim-pandoc"

let g:pandoc_no_folding = 1
Bundle "vim-ruby/vim-ruby"

Bundle "klen/python-mode"
let g:pymode_folding = 0

Bundle "derekwyatt/vim-scala"

" A GUI for Vims undo
Bundle "sjl/gundo.vim"

" Use ,u to open Gundo only in normal mode
nnoremap <Leader>u :GundoToggle<CR>

" Tim Pope may actually be the best programmer alive
" Best Git Integration Ever
Bundle "tpope/vim-fugitive"

" Commenting
Bundle "tpope/vim-commentary"

" Surrounding things
Bundle "tpope/vim-surround"

" Inserts matching brackets in insert mode
Bundle "kana/vim-smartinput"

" A way better Statusline
Bundle "Lokaltog/vim-powerline"
let g:Powerline_symbols = 'unicode'

" Shows at match N out of M matches while searching
Bundle "IndexedSearch"

" Supercharge the repeat "." command
Bundle "tpope/vim-repeat"

" Easymotion is faster vim motions
Bundle "Lokaltog/vim-easymotion"
let g:EasyMotion_leader_key = '<space>'

" Good C++ Autocomplete using clang
Bundle "Rip-Rip/clang_complete"
" Use libclang instead of calling `clang`
let g:clang_use_library=1
" Disable auto completion, trigger it with <Tab> only.
let g:clang_complete_auto = 0
" Show clang errors in the quickfix window
let g:clang_complete_copen = 1
" Close preview window after a completion
let g:clang_close_preview = 1


" This disables Vim's ability to change the terminal title to "Thanks for
" flying vim"
set notitle
" This enables the use of a mouse in all modes. It is occasionally useful.
set mouse=a
" This allows me to have buffers that have unsaved changes. It's possible to
" lose those changes if I am careless with `:q!` but this behaviour improves
" my productivity because I frequently swap buffers around.
set hidden

" I press the `:` key a lot in Vim and I often get typos such as `:W` because
" I have to hold shift. I thus map it to `;`.
nnoremap ; :

" I don't want to lose the functionality of the `;` key so I map that
" behaviour to the `:` key. This also disables the original function of the
" `:` key which forces me to use the `;` key. This is really helpful in fixing
" muscle memory.
nnoremap : ;

" I like line numbers.
set number
" This shows the position of the cursor at the bottom and the statusline.
set ruler
" This forces the statusline to always be visible
set laststatus=2
"Turn on syntax highlighting
syntax on
" UTF-8 is the only good encoding ever.
set encoding=utf-8
" This turns off line wrapping. I can't stand linewrapping and this forces me
" to keep my code skinny.
set nowrap

" These are the defaults for whitespae for all of my documents if there are no
" file specific ones set. Normally they are overridden either by a plugin or
" by something in my /.vim/ftplugins directory.
"
" This sets the number of spaces a tab counts for.
set tabstop=2
" This sets the number of spaces a tab counts for during editing. This means
" backspacing indentation will move this value.
set softtabstop=2
" This is the spaces to insert when using indenting functionality. I don't
" know why this is and `softtabstop` exits but they should be equal.
set shiftwidth=2
" No real tab characters only spaces
set expandtab
" This ensures indents are a multiple of `shiftwidth`
set shiftround

" Show trailing whitespace
set list listchars=tab:▸\ ,eol:¬,trail:·

" Highlight the currentline.
set cursorline

" Highlight the column after `textwidth`
set colorcolumn=+1

set hlsearch "highlight searched text
set incsearch "incremental search
set ignorecase "case InSeNsTiVE
set smartcase "If I do use a captial letter in the search, be case-sensitive
"Clear highlights by doing ,/
nnoremap <silent> <leader>/ :nohlsearch<CR>
" Tab autocompletion in all menus
set wildmenu
" Automatically read files that are modified outside of Vim. This is needed
" when working with git.
set autoread
set autowrite "Auto-save a modified buffer before switching to another buffer

set backspace=indent,eol,start "Backspace does what it should do

set numberwidth=5 "For those really long files
set novisualbell "Don't blink please
set noerrorbells "Don't make noise
set vb t_vb= "Disable any time of beeping or flashing

filetype plugin indent on "Automatically detect file types
set history=1000 "Save a lot of history
" No need for ~ files, I use git most of the time
set nobackup
set nowritebackup

" By default `j` and `k` both move in a file by lines delimited by `\n` which
" is not helpful when linewrapping is enabled. These mappings ensure that I
" move up and down by display lines and not just lines delimited by `\n`.
noremap j gj
noremap k gk
" In order to not lose the original functionality of the `j` and `k` keys I
" map their functionality to `gj` and `gk` respectively.
noremap gj j
noremap gk k

" Make Y consistent with C and D.  See :help Y.
nnoremap Y y$

" Sets the default splitting to be to the bottom and to the right.
set splitbelow
set splitright

" This allows for faster navigation of windows/splits.
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Ignore these files for file selection menus
set wildignore+=*.o,.git,*.jpg,*.png,*.swp,*.d,*.gif
set wildignore+=*.zip,*.tar,*.obj,*.class,*.pyc

" Ignore the .sass-cache directory
set wildignore+=.sass-cache/*

" Automatically resize splits when the window is resized
au VimResized * exe "normal! \<c-w>="

" Use the system clipboard by default. So I don't need to specify
" * + registers for every copy and paste action.
set clipboard=unnamed

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Local .vimrc for machine/environment specific configuration.
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
