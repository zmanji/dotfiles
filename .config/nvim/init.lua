-- vim:fdm=marker
-- This is a work in progress

-- Map Leaders {{{
vim.g.mapleader = ","
vim.g.maplocalleader = ","
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


-- }}}

-- Colorscheme {{{
vim.opt.termguicolors = true
vim.g.base16_shell_path = '~/.zsh/3rdparty'
vim.g.base16colorspace = '256'
vim.opt.background='dark'
vim.cmd('colorscheme base16-tomorrow-night')
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

vim.opt.clipboard = 'unnamed'

local function zmanji_yank(lines, register)
	-- Try very very hard to get the TTY of neovim and pass it as
	-- YANK_TTY. Will work even if in a dtach session
	local pty = vim.loop.fs_realpath('/proc/self/fd/0')
	local data = table.concat(lines, "\n")

	local datafilename = os.tmpname()
	local datafile = assert(io.open(datafilename, "w"))
	datafile:write(data)
	datafile:flush()

	local po = io.popen('YANK_TTY=' .. pty .. ' term-yank <' .. datafilename, 'r')
        po:flush()
	po:close()

	datafile:close()
	os.remove(datafilename)
end

local function zmanji_paste()
	-- Try very very hard to get the TTY of neovim and pass it as
	-- PASTE_TTY. Will work even if in a dtach session
	local pty = vim.loop.fs_realpath('/proc/self/fd/0')

	local po = io.popen('PASTE_TTY=' .. pty .. ' term-paste', 'r')
  po:flush()

	local output = po:read('*all')
	po:close()

  return vim.split(output, "\n")
end

local ssh_tty = os.getenv("SSH_TTY")
if (ssh_tty ~= nil and ssh_tty ~= '' and vim.fn.has('ttyin') == 1) then
	vim.g.clipboard = {
		name = "zmanji-osc-52-lua",
		copy={
                        ["+"] = zmanji_yank,
                        ["*"] = zmanji_yank,
		},
		paste={
                        ["+"] = zmanji_paste,
                        ["*"] = zmanji_paste,
		},
	}
end


vim.cmd([[
augroup basic_options 
  autocmd!
  autocmd VimResized * :wincmd =
augroup END
]])

--Custom dictionary
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
