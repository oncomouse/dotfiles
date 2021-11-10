-- luacheck: globals vim
return {
	"nvim-treesitter/nvim-treesitter",
	run = function()
		vim.cmd([[TSUpdate]])
	end,
	config = function()
		vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
		vim.opt_local.foldmethod = "expr"
		require("nvim-treesitter.configs").setup({
			ensure_installed = "maintained",
			highlight = { enable = true },
			indent = { enable = true },
			autotag = { enable = true },
			textobjects = {
				select = {
					enable = true,
					lookahead = true,
					keymaps = {
						["af"] = "@function.outer",
						["if"] = "@function.inner",
						["ac"] = "@class.outer",
						["ic"] = "@class.inner",
					},
				},
			},
			context_commentstring = {
				enable = true,
			},
		})
	end,
	requires = {
		{ "nvim-treesitter/nvim-treesitter-textobjects" },
		{ "windwp/nvim-ts-autotag", ft = { "html", "javascript", "javascriptreact" } },
		{
			"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
			ft = {
				"javascript",
				"javascriptreact",
				"typescript",
				"typescriptreact",
				"css",
				"scss",
				"html",
				"lua",
			},
		},
	},
}
