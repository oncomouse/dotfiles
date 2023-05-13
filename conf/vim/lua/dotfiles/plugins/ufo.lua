return {
	"kevinhwang91/nvim-ufo",
	opts = {
		-- selene: allow(unused_variable)
		provider_selector = function(bufnr, filetype, buftype)
			local lspWithOutFolding = { "markdown", "bash", "sh", "bash", "zsh", "css" }
			if vim.tbl_contains(lspWithOutFolding, filetype) then
				return { "treesitter", "indent" }
			else
				return { "lsp", "indent" }
			end
		end,
	},
	dependencies = {
		"kevinhwang91/promise-async",
	},
	config = function(_, opts)
		require("ufo").setup(opts)
		vim.keymap.set("n", "zR", require("ufo").openAllFolds)
		vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
		vim.keymap.set("n", "zr", require("ufo").openFoldsExceptKinds)
		vim.keymap.set("n", "zm", require("ufo").closeFoldsWith)
	end,
}
