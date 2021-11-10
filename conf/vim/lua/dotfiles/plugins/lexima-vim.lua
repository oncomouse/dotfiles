--luacheck: globals vim
return {
	"cohama/lexima.vim",
	config = function()
		vim.cmd([[autocmd! dotfiles-settings FileType lua call dotfiles#lexima#extend_endwise()]])
		local map = require("dotfiles.utils.map")
		map.inoremap("<silent>", "<Plug>(dotfiles-lexima)", '<C-r>=lexima#insmode#leave_till_eol("")<CR>')
		map.imap("<silent>", "<C-l>", "<Plug>(dotfiles-lexima)")
	end,
}
