local grep_or_qfgrep = require("dotfiles.functions").grep_or_qfgrep
local list_toggle = require("dotfiles.functions").list_toggle
-- Navigation in insert mode:
vim.keymap.set("i", "<C-a>", "<C-o>^", { silent = true })
vim.keymap.set("i", "<C-e>", "<C-o>$", { silent = true })
local function move_char(backwards)
	return function()
		local row, col = unpack(vim.api.nvim_win_get_cursor(0))
		if (backwards and col == 0) or (not backwards and col == #vim.api.nvim_get_current_line()) then
			return
		end
		return vim.api.nvim_win_set_cursor(0, { row, backwards and col - 1 or col + 1 })
	end
end
vim.keymap.set("i", "<C-b>", move_char(true))
vim.keymap.set("i", "<C-f>", move_char())
local function move_word(backwards)
	return function()
		local _, new_position =
			unpack(vim.fn.searchpos(backwards and [[\<]] or [[\>]], backwards and "bn" or "n", vim.fn.line(".")))
		local row, col = unpack(vim.api.nvim_win_get_cursor(0))
		if new_position == 0 then
			col = backwards and 0 or #vim.api.nvim_get_current_line()
		else
			col = new_position - 1
		end
		vim.api.nvim_win_set_cursor(0, { row, col })
	end
end
vim.keymap.set("i", "<A-b>", move_word(true))
vim.keymap.set("i", "<A-f>", move_word())

-- Clear Currently Highlighted Regexp:
vim.keymap.set("n", "<leader>cr", ':let<C-u>let @/=""<CR>', { silent = true, noremap = true })

-- Jump to last buffer:
vim.keymap.set("n", "``", "<cmd>e #<CR>", { silent = true, noremap = true })

-- Navigate Location List:
vim.keymap.set("n", "]d", "<cmd>lnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[d", "<cmd>lprev<CR>", { silent = true, noremap = true })

-- Tab navigation:
vim.keymap.set("n", "]t", "<cmd>tabnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[t", "<cmd>tabprev<CR>", { silent = true, noremap = true })

-- Toggle Quickfix:
vim.keymap.set("n", "<leader>q", function()
	list_toggle("c")
end, { silent = true, noremap = true })
vim.keymap.set("n", "<leader>d", function()
	list_toggle("l")
end, { silent = true, noremap = true })

-- Project Grep:
vim.keymap.set("n", "<leader>/", function()
	grep_or_qfgrep()
end, { silent = true, noremap = true })

-- Highlight a block and type "@" to run a macro on the block:
vim.keymap.set("x", "@", function()
	vim.cmd([[echo '@'.getcmdline()
	execute ":'<,'>normal @".nr2char(getchar())]])
end, { silent = true, noremap = true })

-- Calculator:
vim.keymap.set("i", "<C-X><C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>", { silent = true, noremap = true })

-- Vertical split like in my Tmux config:
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>")

-- Jump to last buffer:
vim.keymap.set("n", "<leader>b", "<cmd>b#<cr>")

-- Sourced from jessarcher/dotfiles {{{
--  \ https://github.com/jessarcher/dotfiles/blob/master/nvim/init.vim
-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
-- vim.keymap.set("n", "j", "(v:count == 0 ? 'gj' : 'j')", { silent = true, noremap = true, expr = true })
-- vim.keymap.set("n", "k", "(v:count == 0 ? 'gk' : 'k')", { silent = true, noremap = true, expr = true })
-- }}}
-- Sourced from romainl/minivimrc {{{
--  \ https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
vim.keymap.set("n", "<localleader>f", ":find *", { noremap = true })
vim.keymap.set("n", "<localleader>s", ":sfind *", { noremap = true })
vim.keymap.set("n", "<localleader>v", ":vert sfind *", { noremap = true })
-- Minimal Buffer Jumping:
vim.keymap.set("n", "<leader>a", ":buffers<CR>:buffer<Space> ", { noremap = true })
vim.keymap.set("n", "<localleader>a", ":buffer *", { noremap = true })
vim.keymap.set("n", "<localleader>A", ":sbuffer *", { noremap = true })
-- }}}


