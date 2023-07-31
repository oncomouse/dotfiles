return {
	{
		"dimfeld/section-wordcount.nvim",
		lazy = true,
		opts = {
			highlight = "String",
			virt_text_pos = "eol",
		},
		init = function()
			local ag = vim.api.nvim_create_augroup("dotfiles-wordcount-autocmd", { clear = true })

			vim.api.nvim_create_autocmd("FileType", {
				pattern = "markdown",
				group = ag,
				callback = function()
					require("section-wordcount").wordcounter()
				end,
			})
		end,
	},
}
