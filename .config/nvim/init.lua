-- this is a work in progress 

vim.opt.clipboard = 'unnamedplus'

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
