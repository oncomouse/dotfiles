local t = require("dotfiles.utils.termcode")
local map = require("dotfiles.utils.map")
-- Basic Maps: {{{

-- Enable TODO Manager:
vim.g.enable_todo = 1

-- Highlight a block and type "@" to run a macro on the block:
map.xnoremap("<silent>", "@", vim.fn["visualat#execute_macro_over_visual_range"])

-- Grep project:
local function grep_or_qfgrep()
	if vim.opt.buftype:get() == 'quickfix' then
		local input = vim.fn.input('QFGrep/')
		if #input > 0 then
			local prefix = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist and 'L' or 'C'
			vim.cmd("execute '" .. prefix .. "filter /'.l:input.'/'")
		end
	else
		local input = vim.fn.input('Grep/')
		if #input > 0 then
			vim.cmd("Grep " .. input)
		end
	end
end
map.nnoremap("<silent>", "<leader>/", grep_or_qfgrep)

-- Calculator (not sure how this works):
map.inoremap("<C-A>", t"<C-O>yiW<End>=<C-R>=<C-R>0<CR>")
-- }}}
-- List Bindings: {{{
map.nnoremap("<silent>", "<leader>d", function() vim.fn["dotfiles#lists#toggle"]("Location List", "l") end)
map.nnoremap("<silent>", "<leader>q", function() vim.fn["dotfiles#lists#toggle"]("Quickfix List", "c") end)
-- }}}
-- FZF Bindings: {{{
map.nmap("<silent>", "<c-p>", "<cmd>Files<CR>")
map.nmap("<silent>", "<leader>F", "<cmd>Files ~<CR>")
map.nmap("<silent>", "<leader>a", "<cmd>Buffers<CR>")
map.nmap("<silent>", "<leader>A", "<cmd>Windows<CR>")
map.nmap("<silent>", "<leader>l", "<cmd>Blines<CR>")
map.nmap("<silent>", "<leader>?", "<cmd>Commands<CR>")
-- }}}
-- Uniform Visual Motion Toggle: {{{
map.map("<leader>w", vim.fn["edit_mode#toggle"])
-- }}}
-- # vim:foldmethod=marker:foldlevel=0
