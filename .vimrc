" vim:fdm=marker
" TODO: Regorganize code
" Plugins to Consider:
" * vim-tabpagecd - https://github.com/kana/vim-tabpagecd
" * vim-signify - https://github.com/mhinz/vim-signify
"
" TODO: Fork bufkill.vim to not add keyboard mappings.
" TODO: Add the function to remove trailing whitespace on save.

" Basics {{{
  set nocompatible
""}}}

" Map Leaders {{{
  let mapleader = ","
  let maplocalleader = ","
" }}}

" NeoBundle {{{
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))
" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'
" }}}

" Plugins {{{

" Core Improvements {{{
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
" }}}

" Language Support {{{
NeoBundleLazy 'tpope/vim-haml', {
      \ 'autoload':{'filetypes':['haml','scss','sass']}
      \ }
NeoBundleLazy 'othree/html5.vim', {'autoload':{'filetypes':['html']}}
NeoBundleLazy 'leshill/vim-json', {'autoload':{'filetypes':['javascript','json']}}
NeoBundleLazy 'vim-ruby/vim-ruby', {'autoload':{'filetypes':['ruby']}}
NeoBundleLazy 'tpope/vim-markdown', {'autoload: {filetypes':['markdown']}

NeoBundleLazy 'klen/python-mode', {'autoload':{'filetypes':['python']}}
let g:pymode_rope=0
let g:pymode_folding = 0
let g:pymode_lint_ignore = "E501"
NeoBundleLazy 'davidhalter/jedi-vim', {'autoload':{'filetypes':['python']}}
let g:jedi#auto_initialization=0
let g:jedi#auto_vim_configuration=0
let g:jedi#popup_on_dot=0

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
" compile all sources as c++11
let g:clang_user_options = '-std=c++11'
" }}}

" UI Improvements {{{
NeoBundle "sjl/gundo.vim"
" Use ,u to open Gundo only in normal mode
nnoremap <Leader>u :GundoToggle<CR>


" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_alt_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_alt_sep = '◀'
let g:airline_linecolumn_prefix = '␊ '
let g:airline_linecolumn_prefix = '␤ '
let g:airline_linecolumn_prefix = '¶ '
let g:airline_branch_prefix = '⎇ '
let g:airline_paste_symbol = 'ρ'
let g:airline_paste_symbol = 'Þ'
let g:airline_paste_symbol = '∥'
NeoBundle "bling/vim-airline"
" }}}

" Misc {{{
NeoBundle "Lokaltog/vim-easymotion"
let g:EasyMotion_leader_key = '<leader><leader>'

NeoBundle "tpope/vim-fugitive"

NeoBundleLazy 'godlygeek/tabular', {'autoload':{'commands':'Tabularize'}}

" Quick tmp files, saved in ~/.vim_junk
NeoBundle "Shougo/junkfile.vim"

NeoBundle "tpope/vim-characterize"

NeoBundle "bufkill.vim"

NeoBundle "cmdalias.vim"

augroup init_aliases
  autocmd!
  " Alias internal :bd to use buffkill's :BD
  autocmd VimEnter * :call CmdAlias('bd', 'BD')
augroup END

NeoBundle "tpope/vim-eunuch"

" }}}

" AutoComplete {{{
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

if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.ruby =
      \ '[^. *\t]\.\w*\|\h\w*::'

" For smart TAB completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ neocomplete#start_manual_complete()

inoremap <expr><s-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"
  function! s:check_back_space() "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction "}}}

NeoBundle 'ujihisa/neco-look'

" }}}

" Snippets {{{
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
" }}}

" Unite {{{
NeoBundle 'Shougo/unite.vim'
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

  call unite#custom#source('file_rec,file_rec/async',
        \ 'matchers', 'matcher_fuzzy')

  call unite#custom#source('file_rec,file_rec/async',
        \ 'sorters', 'sorter_rank')
endfunction

augroup ft_unite
  autocmd!
  autocmd FileType unite call s:unite_settings()
augroup END

nnoremap <leader>f :Unite -start-insert file_rec/async:!<cr>
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

NeoBundle 'Shougo/vimshell'
let g:vimshell_prompt = "$ "
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
function! s:vimshell_settings()
  call vimshell#altercmd#define('g', 'git')
endfunction
augroup ft_vimshell
  autocmd!
  autocmd FileType vimshell call s:vimshell_settings()
  autocmd FileType vimshell setlocal colorcolumn=""
augroup END
" }}}

" Colors {{{
NeoBundle 'chriskempson/base16-vim'
" }}}

" Install Plugins and Clean {{{
NeoBundleCheck
silent NeoBundleClean!
" }}}

" }}}

" Colorscheme {{{
colorscheme base16-tomorrow
set background=dark
syntax on
" }}}

" Basic Options {{{
filetype plugin indent on
set encoding=utf-8
set number
set ruler
set list listchars=tab:▸\ ,eol:¬,trail:·
set showbreak=↪
set autoread
set autowrite
set hidden
set backspace=indent,eol,start
set notitle
set mouse=a
set laststatus=2
set history=1000
" Force myself to keep text skinny.
set nowrap
" Sets the default splitting to be to the bottom and to the right.
set splitbelow
set splitright
set novisualbell "Don't blink please
set noerrorbells "Don't make noise
set vb t_vb= "Disable any time of beeping or flashing
" Use the system clipboard by default. So I don't need to specify * +
" registers for every copy and paste action.
set clipboard=unnamed
" Automatically resize splits when the window is resized
augroup basic_options
  autocmd!
  autocmd VimResized * :wincmd =
augroup END
" Custom dictionary
set spellfile=~/.vim/custom-dictionary.en.utf8.add
" Less delay between escape and normal mode.
set ttimeoutlen=50
" }}}

" Backups and Undo {{{
set nobackup
set writebackup
set undofile

set undodir=~/.vim/tmp/undo//
set backupdir=~/.vim/tmp/backup//
set directory=~/.vim/tmp/swap//

if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif
" }}}

" Whitespace {{{
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

set textwidth=80
" }}}

" Highlights {{{
" Highlight the currentline.
set cursorline
" Highlight the column after `textwidth`
set colorcolumn=+1

augroup highlights
  autocmd!
  " Hide the cursor line when the split is not in focus
  autocmd WinLeave * setlocal nocursorline
  autocmd WinEnter * setlocal cursorline
  " Hide the column line when the split is not in focus
  autocmd WinLeave * setlocal colorcolumn=""
  autocmd WinEnter * setlocal colorcolumn=+1
augroup END
" }}}

" Searching {{{
set hlsearch "highlight searched text
set incsearch "incremental search
set ignorecase "case InSeNsTiVE
set smartcase "If I do use a captial letter in the search, be case-sensitive
"Clear highlights by doing ,/
nnoremap <silent> <leader>/ :nohlsearch<CR>
" }}}

" Mappings {{{

" I press the `:` key a lot in Vim and I often get typos such as `:W` because
" I have to hold shift. I thus map it to `;`.
nnoremap ; :

" I don't want to lose the functionality of the `;` key so I map that
" behaviour to the `:` key. This also disables the original function of the
" `:` key which forces me to use the `;` key. This is really helpful in fixing
" muscle memory.
nnoremap : ;

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

" This allows for faster navigation of windows/splits.
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv
" }}}

" Wildmenu {{{
set wildmenu
set wildignore+=*.o,.git,*.jpg,*.png,*.swp,*.d,*.gif
set wildignore+=*.zip,*.tar,*.obj,*.class,*.pyc
set wildignore+=.sass-cache/*
" }}}

" File Type Configurations {{{

" C {{{
augroup ft_c
  autocmd!
  autocmd FileType c setlocal tabstop=4
  autocmd FileType c setlocal softtabstop=4
  autocmd FileType c setlocal shiftwidth=4
  autocmd FileType c setlocal textwidth=80
  autocmd FileType c setlocal smarttab
  autocmd FileType c setlocal foldmethod=syntax
  autocmd FileType c setlocal omnifunc=ClangComplete
  " Disable 'preview' option, so there is no popup window with clang_complete.
  autocmd FileType c setlocal completeopt-=preview
augroup END
" }}}

" CPP {{{
" Should be the same as C
augroup ft_cpp
  autocmd!
  autocmd FileType cpp setlocal tabstop=4
  autocmd FileType cpp setlocal softtabstop=4
  autocmd FileType cpp setlocal shiftwidth=4
  autocmd FileType cpp setlocal textwidth=80
  autocmd FileType cpp setlocal smarttab
  autocmd FileType cpp setlocal foldmethod=syntax
  autocmd FileType cpp setlocal omnifunc=ClangComplete
  " Disable 'preview' option, so there is no popup window with clang_complete.
  autocmd FileType cpp setlocal completeopt-=preview
augroup END
" }}}

" TeX {{{
augroup ft_tex
  autocmd!
  autocmd FileType tex setlocal textwidth=80
  autocmd FileType tex setlocal smarttab
  autocmd FileType tex setlocal cole=2
  autocmd FileType tex setlocal spell
  autocmd FileType tex setlocal autoindent
augroup END
" }}}

" Markdown {{{
augroup ft_markdown
  autocmd!
  autocmd FileType markdown setlocal spell
  autocmd FileType markdown setlocal foldlevel=1
augroup END
" }}}

" Ruby {{{
augroup ft_ruby
  autocmd!
  autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
  let g:rubycomplete_buffer_loading=1
  let g:rubycomplete_classes_in_global=1
  let g:rubycomplete_rails=1
augroup END
" }}}

" Python {{{
augroup ft_python
  autocmd!
  autocmd FileType python setlocal omnifunc=jedi#complete
  autocmd FileType python setlocal smarttab
  " Disable 'preview' option, so there is no popup window with vim-jedi
  autocmd FileType python setlocal completeopt-=preview
augroup END
" }}}

" Java {{{
augroup ft_java
  autocmd!
  autocmd FileType java setlocal tabstop=4
  autocmd FileType java setlocal softtabstop=4
  autocmd FileType java setlocal shiftwidth=4
  autocmd FileType java setlocal smarttab
  autocmd FileType java setlocal expandtab
augroup END
" }}}

" }}}

" Local .vimrc {{{
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
"}}}
