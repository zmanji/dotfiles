" This is my `.vimrc`.
"
" First disable Vi compatibility. This must be done first because it affects
" every other option.
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
" the leader key. Also note that I lose the original function of the `,` key.

let mapleader = ","
let maplocalleader = ","

" ### Plugin Installation ###
" Vim plugin management is difficult at best so we use [Vundle][vu] to help us
" install and manage plugins.
"
" Vundle lets a very cool `Bundle` function become available. You can feed it
" a string in the form from `user/repo` and will get the appropriate github
" repo. Other strings could be a vim-script repo or a git url for a non github
" repo. Once this is done I have a `:BundleInstall` command available which
" will download and install those packages for me.
"
" I use Vundle over Pathogen because pathogen requires you to manage the
" plugins externally and it would fill my `.vim/` will all sorts of stuff that
" I did not write. I don't want this to occur because I would like my dotfiles
" repo to be for code that I wrote or need to bootstrap (like Vundle).
"
" [vu]: https://github.com/gmarik/vundle
"
" These lines are required to set up Vundle and they come from the Vundle
" documentation.
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
" A solarized configuration option. This disables the solarized menu at the
" top of the gui.
let g:solarized_menu=0
" This sets solarized to be the colorscheme.
colorscheme solarized
" This ensures that the dark solarized version is used.
set background=dark

" ####Ack.vim####
" `ack` is better than `grep` and [Ack.vim][ack] brings integration to Vim. It
" acts as a replacement for Vim's `:grep` commands.
"
" [ack]: https://github.com/mileszs/ack.vim
Bundle "mileszs/ack.vim"

" ####Vim-Pandoc####
" This [plugin][pdc-vim] is a bundle of tools for [Pandoc][pdc]'s extended markdown.
" It provides Pandoc and regular markdown syntax highlighting, folding and a few
" useful local-leader mappings. Weird behaviour tends to happen when installed
" with vim-markdown but this has most of that functionality.
"
" [pdc]: http://johnmacfarlane.net/pandoc/
" [pdc-vim]: https://github.com/vim-pandoc/vim-pandoc
Bundle "vim-pandoc/vim-pandoc"

let g:pandoc_no_folding = 1

" ####Vim-CoffeeScript####
" This [plugin][vim-cs] provides coffee-script support to Vim. It brings
" syntax highlighting for coffee-script, eco, a `:CoffeMake` command and a lot
" of configuration options.
"
" [vim-cs]: https://github.com/kchmck/vim-coffee-script
Bundle "kchmck/vim-coffee-script"

" ####Vim-Haml####
" This [plugin][vim-haml] adds Haml, Sass and SCSS support to Vim.
"
" [vim-haml]: https://github.com/tpope/vim-haml
Bundle "tpope/vim-haml"

" Detects incorrect syntax
" Bundle "scrooloose/syntastic"

" " Enable Syntastic
" let g:syntastic_enable_signs=1

" Autocomplete trigged by tab
Bundle "ervandew/supertab"
let g:SuperTabDefaultCompletionType = "context"

" File browsing
Bundle "scrooloose/nerdtree"

",n will open or close the nerd tree buffer
noremap <leader>n :NERDTreeToggle<CR>

" Use arrows instead of the `+` and `~` characters
let g:NERDTreeDirArrows=1

" CtrlP File, Buffer and Most Recently Used finder
Bundle "kien/ctrlp.vim"

" CtrlP settings
" This will jump to a file that is already in an opened buffer if it is in
" another tab.
let g:ctrlp_jump_to_buffer = 2
" This tells CtrlP to not manage the working directory as the other modes
" become a little irksome over time. This is the same behaviour as Command-T.
let g:ctrlp_working_path_mode = 0

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

" The latest Vim Ruby runtime files
Bundle "vim-ruby/vim-ruby"

" The inserts 'end' magically when editing ruby
Bundle "tpope/vim-endwise"

" Inserts matching brackets in insert mode
Bundle "kana/vim-smartinput"

" Lets me maximize a split when needed
" Note that <C-w>o is mapped to toggling the zoomed window
Bundle "ZoomWin"

" Resizes splits according to the golden-ratio
" Bundle "roman/golden-ratio"

" Scratch Buffer plugin `scratch.vim`
" Opens a scratch buffer with `:Scratch`
Bundle "duff/vim-scratch"

" A way better Statusline
Bundle "Lokaltog/vim-powerline"
let g:Powerline_symbols = 'unicode'

" Python Mode
Bundle "klen/python-mode"

" Disable Python Folding
let g:pymode_folding = 0


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

" ####Defaults for Whitespace####
" These are the defaults for whitespae for all of my documents if there are no
" file specific ones set. Normally they are overridden either by a plugin or by
" something in my /.vim/ftplugins directory.
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
set list listchars=tab:\ \ ,trail:·

" Highlight the currentline.
set cursorline

" Highlight the column after `textwidth`
set colorcolumn=+1

" ####Searching####
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

set backspace=2 "Backspace does what it should do

set numberwidth=5 "For those really long files
set novisualbell "Don't blink please
set noerrorbells "Don't make noise
set vb t_vb= "Disable any time of beeping or flashing

filetype plugin indent on "Automatically detect file types
set history=1000 "Save a lot of history
" No need for ~ files, I use git most of the time
set nobackup
set nowritebackup

" In the ongoing journey to master Vim, it's useful to disable keys that
" should not be used. These lines disable the arrow keys in normal, insert and
" visual modes.
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

" By default `j` and `k` both move in a file by lines delimited by `\n` which
" is not helpful when linewrapping is enabled. These mappings ensure that I
" move up and down by display lines and not just lines delimited by `\n`.
noremap j gj
noremap k gk
" In order to not lose the original functionality of the `j` and `k` keys I
" map their functionality to `gj` and `gk` respectively.
noremap gj j
noremap gk k


" Sets the default splitting to be to the bottom and to the right.
set splitbelow
set splitright

" This allows for faster navigation of windows/splits.
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Ctrl-tab to switch next/prev tab, like in FireFox, Chrome, etc.
nnoremap <silent><C-S-Tab> :tabp<CR>
nnoremap <silent><C-Tab> :tabn<CR>

" Ignore these files for file selection menus
set wildignore+=*.o,.git,*.jpg,*.png,*.swp,*.d,*.gif
set wildignore+=*.zip,*.tar,*.obj,*.class

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
