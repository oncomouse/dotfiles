-- luacheck: globals vim
return {
	"hrsh7th/vim-vsnip",
	event = "VimEnter",
	config = function()
		local map = require("dotfiles.utils.map")
		vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/dotfiles/conf/vim/snippets"
		map.imap("<expr>", "<Tab>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'")
		map.smap("<expr>", "<Tab>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'")
		map.imap("<expr>", "<S-Tab>", "vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
		map.smap("<expr>", "<S-Tab>", "vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
	end,
	requires = {
		{ "rafamadriz/friendly-snippets", after = { "vim-vsnip" } }, -- Base Snippets
		{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
	},
}
