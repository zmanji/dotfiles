set nocompatible

let mapleader = ","
let maplocalleader = ","

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))
" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Core Improvements to Vim
NeoBundle 'matchit.zip'
NeoBundle "tpope/vim-surround"
NeoBundle "tpope/vim-repeat"
NeoBundle 'Raimondi/delimitMate'
NeoBundle "tpope/vim-commentary"
NeoBundle "IndexedSearch"

NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'nmake -f make_msvc.mak nodebug=1',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

" Better Language Support
NeoBundleLazy 'tpope/vim-haml', {
      \ 'autoload':{'filetypes':['haml','scss','sass']}
      \ }
NeoBundleLazy 'othree/html5.vim', {'autoload':{'filetypes':['html']}}
NeoBundleLazy 'leshill/vim-json', {'autoload':{'filetypes':['javascript','json']}}
NeoBundleLazy 'vim-ruby/vim-ruby', {'autoload':{'filetypes':['ruby']}}

NeoBundle "vim-pandoc/vim-pandoc"
let g:pandoc_no_folding = 1
NeoBundleLazy 'klen/python-mode', {'autoload':{'filetypes':['python']}}
let g:pymode_rope=0
let g:pymode_folding = 0
let g:pymode_lint_ignore = "E501"
NeoBundleLazy 'davidhalter/jedi-vim', {'autoload':{'filetypes':['python']}}
let g:jedi#popup_on_dot=0
let g:jedi#auto_vim_configuration = 0

NeoBundle "derekwyatt/vim-scala"
NeoBundleLazy 'Rip-Rip/clang_complete', {
      \ 'autoload' : {
      \     'filetypes' : ['c', 'cpp'],
      \    },
      \ }
" Use libclang instead of calling `clang`
let g:clang_use_library=1
" Disable auto completion, trigger it with <Tab> only.
let g:clang_complete_auto = 0
let g:clang_auto_select = 0
" Show clang errors in the quickfix window
let g:clang_complete_copen = 1
" Close preview window after a completion
let g:clang_close_preview = 1

NeoBundle "sjl/gundo.vim"
" Use ,u to open Gundo only in normal mode
nnoremap <Leader>u :GundoToggle<CR>

NeoBundle "Lokaltog/vim-powerline"
let g:Powerline_symbols = 'unicode'

NeoBundle "Lokaltog/vim-easymotion"
let g:EasyMotion_leader_key = '<space>'

NeoBundle "tpope/vim-fugitive"

NeoBundleLazy 'godlygeek/tabular', {'autoload':{'commands':'Tabularize'}}

NeoBundleLazy 'Shougo/neocomplete.vim', {'autoload':{'insert':1}}
let g:neocomplete#enable_at_startup=1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#data_directory='~/.vim/.cache/neocomplete'
let g:neocomplete#enable_auto_delimiter=1
let g:neocomplete#enable_refresh_always=1
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_overwrite_completefunc = 1
let g:neocomplete#force_omni_input_patterns.c =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.cpp =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.objc =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.objcpp =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.ruby =
      \ '[^. *\t]\.\w*\|\h\w*::'

NeoBundle 'ervandew/supertab'
let g:SuperTabDefaultCompletionType = "context"


NeoBundle 'honza/vim-snippets'
NeoBundle 'Shougo/neosnippet'
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
let g:neosnippet#enable_snipmate_compatibility=1
" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

NeoBundle 'Shougo/unite.vim'
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
let g:unite_enable_start_insert = 1
let g:unite_enable_short_source_names = 1
let g:unite_data_directory='~/.vim/.cache/unite'
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable=1
let g:unite_source_rec_max_cache_files=5000
let g:unite_prompt='»'

function! s:unite_settings()
  " Play nice with supertab
  let b:SuperTabDisabled=1
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction
autocmd FileType unite call s:unite_settings()
nnoremap <leader>f :Unite -start-insert file_rec/async<cr>
nnoremap <leader>y :Unite -buffer-name=yanks history/yank<cr>

NeoBundleLazy 'Shougo/unite-outline', {'autoload':{'unite_sources':'outline'}}
nnoremap <leader>o :Unite -auto-resize -buffer-name=outline outline<cr>

NeoBundle 'Shougo/vimfiler.vim'
let g:vimfiler_as_default_explorer=1
let g:vimfiler_data_directory='~/.vim/.cache/vimfiler'
let g:vimfiler_tree_leaf_icon = ' '
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_marked_file_icon = '✓'

NeoBundle 'chriskempson/base16-vim'
colorscheme base16-tomorrow
" This ensures that the dark version is used.
set background=dark

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

" Hide the cursor line when the split is not in focus
autocmd WinLeave * setlocal nocursorline
autocmd WinEnter * setlocal cursorline

" Highlight the column after `textwidth`
set colorcolumn=+1

" Hide the column line when the split is not in focus
autocmd WinLeave * setlocal nocursorcolumn
autocmd WinEnter * setlocal cursorcolumn

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
