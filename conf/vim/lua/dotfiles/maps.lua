local t = require("dotfiles.utils.termcode")
local map = require("dotfiles.utils.map")
-- Basic Maps: {{{

-- Enable TODO Manager:
vim.g.enable_todo = 1

-- Highlight a block and type "@" to run a macro on the block:
map.xnoremap("<silent>", "@", ":<C-u>call visualat#execute_macro_over_visual_range()<CR>")

-- Grep project:
function _G.grep_or_qfgrep()
	if vim.opt.buftype:get() == 'quickfix' then
		local input = vim.fn.input('QFGrep/')
		if #input > 0 then
			vim.cmd("execute 'Cfilter /'.l:input.'/'")
		end
	else
		local input = vim.fn.input('Grep/')
		if #input > 0 then
			vim.cmd("execute 'Grep ' . l:input")
		end
	end
end
map.nnoremap("<silent>", "<leader>/", t"<cmd>lua grep_or_qfgrep()<CR>")

-- Calculator (not sure how this works):
map.inoremap("<C-A>", t"<C-O>yiW<End>=<C-R>=<C-R>0<CR>")

-- Jump out of a lexima pair with <C-l>:
-- map.inoremap("<C-l>", t"<C-r>=lexima#insmode#leave(1, \"\")<CR>")
--- }}}
-- List Bindings: {{{
map.nnoremap("<silent>", "<leader>d", t":call dotfiles#lists#toggle('Location List', 'l')<CR>")
map.nnoremap("<silent>", "<leader>q", t":call dotfiles#lists#toggle('Quickfix List', 'c')<CR>")
-- }}}
-- Default Diagnostic Bindings: {{{
map.nmap("<silent>", "<Plug>(dotfiles-diagnostic-next)", t"<cmd>cnext<CR>")
map.nmap("<silent>", "<Plug>(dotfiles-diagnostic-previous)", t"<cmd>cprev<CR>")
-- }}}
-- Standard Fuzzy Bindings: {{{
map.nmap("<silent>", "<c-p>", t"<Plug>(dotfiles-files)")
map.nmap("<silent>", "<leader>F", t"<Plug>(dotfiles-home-files)")
map.nmap("<silent>", "<leader>a", t"<Plug>(dotfiles-buffers)")
map.nmap("<silent>", "<leader>A", t"<Plug>(dotfiles-windows)")
map.nmap("<silent>", "<leader>l", t"<Plug>(dotfiles-lines)")
map.nmap("<silent>", "<leader>?", t"<Plug>(dotfiles-commands)")
-- }}}
-- Standard LSP Bindings: {{{
-- As with Fuzzy bindings (above), we set all the LSP commands to Plug
-- bindings and then rebind them here to the keys we actually want to use:
map.nmap("<silent>", "<F2>", "<Plug>(dotfiles-rename)")
map.nmap("<silent>", "<F5>", "<Plug>(dotfiles-commands)")
map.vmap("<silent>", "ga", "<Plug>(dotfiles-codeaction-selected)")
map.nmap("<silent>", "ga", "<Plug>(dotfiles-codeaction)")
map.nmap("<silent>", "gl", "<Plug>(dotfiles-codelens)")
map.nmap("<silent>", "gd", "<Plug>(dotfiles-definition)")
map.nmap("<silent>", "gy", "<Plug>(dotfiles-type-definition)")
map.nmap("<silent>", "gi", "<Plug>(dotfiles-implementation)")
map.nmap("<silent>", "gr", "<Plug>(dotfiles-references)")
map.nmap("<silent>", "<leader>s", "<Plug>(dotfiles-document-symbols)")
map.nmap("<silent>", "K", "<Plug>(dotfiles-documentation)")
map.nmap("<silent>", "[d", "<Plug>(dotfiles-diagnostic-previous)")
map.nmap("<silent>", "]d", "<Plug>(dotfiles-diagnostic-next)")
-- }}}
-- # vim:foldmethod=marker
