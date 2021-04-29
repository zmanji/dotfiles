" vim:fdm=marker

" Basics {{{
  set nocompatible
""}}}

" Map Leaders {{{
  let mapleader = ","
  let maplocalleader = ","
" }}}

" Pathogen {{{
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()
execute pathogen#helptags()
" }}}

" Plugins {{{

" UI Improvements {{{
" Use ,u to open Gundo only in normal mode
nnoremap <Leader>u :GundoToggle<CR>

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'
" }}}

" Misc {{{
augroup init_aliases
  autocmd!
  " Alias internal :bd to use bufkill's :BD
  autocmd VimEnter * :call CmdAlias('bd', 'Bdelete')
augroup END

" }}}

" Vimfiler {{{

let g:vimfiler_as_default_explorer=1
let g:vimfiler_data_directory='~/.vim/.cache/vimfiler'
let g:vimfiler_tree_leaf_icon = ' '
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_marked_file_icon = '✓'

" }}}

" Startify {{{
let g:startify_custom_header = []
" }}}

" Goyo {{{
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
" }}}


" Gundo {{{
let g:gundo_prefer_python3 = 1
" }}}


" FZF {{{
if filereadable("/usr/local/opt/fzf/README.md")
	set rtp+=/usr/local/opt/fzf
endif


nnoremap <leader>f :Files<cr>
nnoremap <leader>b :Buffers<cr>
" }}}

" }}}

" Colorscheme {{{
let g:base16_shell_path='~/.zsh/3rdparty'
let base16colorspace=256
set background=dark
colorscheme base16-tomorrow-night
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
set showcmd " Show the current command in the status line.
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
silent mkspell! ~/.vim/custom-dictionary.en.utf8.add
" Less delay between escape and normal mode.
set ttimeoutlen=50

" Use DECSCUSR escape codes on iTerm2/xterm to change cursor shape in terminal
" vim. See http://git.io/zvDeWQ for example code.
if !has("gui_running") && !exists("$TMUX")
  if &term =~ "xterm"
    " Enter Insert Mode (Cursor Shape: vertical bar)
    let &t_SI = "\<Esc>[6 q"
    " Leave Insert Mode (Cursor Shape: block)
    let &t_EI = "\<Esc>[2 q"
  endif
endif
" Enable better matching with '%'
runtime macros/matchit.vim
" Fixes a problem with pathogen and sessions
set sessionoptions-=options
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

" Function to remove trailing whitespace from a file, used in autocmds for
" different files.
" Taken from http://vimcasts.org/episodes/tidying-whitespace/
function! <SID>strip_trailing_whitespace()
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  %s/\s\+$//e
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction
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

" Netrw {{{
" Porting https://github.com/vim/vim/issues/4738#issuecomment-798790444
if has('macunix')
  function! OpenURLUnderCursor()
    let s:uri = expand('<cWORD>')
    let s:uri = matchstr(s:uri, "[a-z]*:\/\/[^ >,;)'\"]*")
    let s:uri = substitute(s:uri, '#', '\\#', '')
    if s:uri != ''
      silent exec "!open '".s:uri."'"
      :redraw!
    endif
  endfunction
  nnoremap gx :call OpenURLUnderCursor()<CR>
endif
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
  autocmd BufWritePre * if &ft == "c" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif
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
  autocmd BufWritePre * if &ft == "cpp" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif
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
  autocmd BufWritePre * if &ft == "tex" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif
augroup END
" }}}

" Markdown {{{
augroup ft_markdown
  autocmd!
  autocmd FileType markdown setlocal spell
  autocmd FileType markdown setlocal foldlevel=99
  autocmd FileType markdown setlocal autoindent
  autocmd BufWritePre * if &ft == "markdown" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif

  let g:pandoc_use_hard_wraps = 1
augroup END
" }}}

" Ruby {{{
augroup ft_ruby
  autocmd!
  autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
  autocmd BufWritePre * if &ft == "ruby" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif
augroup END
" }}}

" Python {{{
augroup ft_python
  autocmd!
  autocmd FileType python setlocal smarttab
  autocmd BufWritePre * if &ft == "python" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif
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
  autocmd BufWritePre * if &ft == "java" |
        \ :call <SID>strip_trailing_whitespace() |
        \ endif
augroup END
" }}}

" Man {{{
augroup ft_man
  autocmd!
  autocmd FileType man setlocal textwidth=0
augroup END
" }}}

let g:vimwiki_folding='syntax'
" VimWiki {{{
augroup ft_vimwiki
  autocmd!
  autocmd FileType vimwiki setlocal spell
  autocmd FileType vimwiki setlocal foldlevel=2
  autocmd FileType vimwiki setlocal autoindent
  autocmd FileType vimwiki setlocal foldmethod=syntax
augroup END
" }}}

" }}}

" Local .vimrc {{{
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
"}}}
