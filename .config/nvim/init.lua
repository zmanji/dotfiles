-- vim:fdm=marker
-- This is a work in progress

-- Map Leaders {{{
vim.g.mapleader = ","
vim.g.maplocalleader = ","
-- }}}

-- Colorscheme {{{
vim.opt.termguicolors = true
vim.g.base16_shell_path = '~/.zsh/3rdparty'
vim.g.base16colorspace = '256'
vim.opt.background='dark'
vim.cmd('colorscheme base16-tomorrow-night')
-- }}}


-- Plugins {{{
vim.api.nvim_set_keymap('n', '<Leader>u', ':UndotreeToggle<CR>', {noremap = true})

vim.api.nvim_set_keymap('n', '<Leader>bd', ':Bdelete<CR>', {noremap = true})


-- Startify {{{
vim.g.startify_custom_header = {}
-- }}}


-- Tree Sitter {{{
require('nvim-treesitter.configs').setup {
        ensure_installed = "maintained",
        highlight = {
                enable = true,
        },
        indent = {
                enable = true,
        },
}

vim.cmd([[
set foldmethod=expr
set nofoldenable
set foldexpr=nvim_treesitter#foldexpr()
]])
-- }}}

require("todo-comments").setup {}

require("which-key").setup {}

local trouble = require('trouble')

local telescope = require('telescope')
local actions = require("telescope.actions")

telescope.setup {
  defaults = {
    mappings = {
      i = { ["<c-j>"] = actions.move_selection_next,
            ["<c-k>"] = actions.move_selection_previous,
            ["<C-h>"] = "which_key",
                        },
    },
  },
}

vim.api.nvim_set_keymap('n', '<leader>f', ':Telescope find_files find_command=fd,--type,f,--hidden,--no-ignore,--follow,--exclude,.git<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<leader>b', ':Telescope buffers<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<leader>s', ':Telescope live_grep<cr>', {noremap = true})

-- }}}

-- Basic Options {{{
vim.opt.number = true
vim.opt.ruler = true
vim.opt.listchars = "tab:▸ ,eol:¬,trail:·"

vim.opt.showbreak = '↪'
vim.opt.autoread = true
vim.opt.autowrite = true
vim.opt.title = false
vim.opt.mouse = 'a'
vim.opt.wrap = false

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.clipboard = 'unnamedplus'

vim.cmd([[
augroup basic_options 
  autocmd!
  autocmd VimResized * :wincmd =
augroup END
]])

-- Highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]

-- Custom dictionary
vim.opt.spellfile = "~/.vim/custom-dictionary.en.utf8.add"
-- }}}

-- Backups and Undo {{{
vim.opt.undofile = true
-- }}}

-- {{{ Whitespace
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true
vim.opt.shiftround = true
vim.opt.textwidth = 80
vim.opt.breakindent = true
-- }}}

-- Highlights {{{
vim.opt.cursorline = true
vim.opt.colorcolumn = '+1'

vim.cmd([[
augroup highlights
  autocmd!
  " Hide the cursor line when the split is not in focus
  autocmd WinLeave * setlocal nocursorline
  autocmd WinEnter * setlocal cursorline
  " Hide the column line when the split is not in focus
  autocmd WinLeave * setlocal colorcolumn=""
  autocmd WinEnter * setlocal colorcolumn=+1
augroup END
]])
-- }}}g

-- Searching {{{
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.api.nvim_set_keymap('n', '<Leader>/', ':set hlsearch!<CR>', { noremap = true, silent = true })
-- }}}

-- Mappings {{{
vim.api.nvim_set_keymap('n', ';', ':', {noremap = true})
vim.api.nvim_set_keymap('n', ':', ';', {noremap = true})

vim.api.nvim_set_keymap('n', 'j', 'gj', {noremap = true})
vim.api.nvim_set_keymap('n', 'k', 'gk', {noremap = true})

vim.api.nvim_set_keymap('v', '<', '<gv', {noremap = true})
vim.api.nvim_set_keymap('v', '>', '>gv', {noremap = true})
-- }}}
