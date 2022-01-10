-- vim:fdm=marker
-- This is a work in progress

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

	return {output}
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


-- Backups and Undo {{{
vim.opt.undofile = true
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
