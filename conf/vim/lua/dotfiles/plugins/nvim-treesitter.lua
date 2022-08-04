local function config_treesitter()
	local parsers = {
		"bash",
		"bibtex",
		"c",
		"cmake",
		"comment",
		"cpp",
		"css",
		"dockerfile",
		"fennel",
		"fish",
		"go",
		"graphql",
		"html",
		"http",
		"java",
		"javascript",
		"jsdoc",
		"json",
		"jsonc",
		"latex",
		"lua",
		"make",
		"ninja",
		"nix",
		"norg",
		"org",
		"perl",
		"php",
		"python",
		"r",
		"rasi",
		"regex",
		"ruby",
		"rust",
		"scss",
		"tsx",
		"typescript",
		"vim",
		"vue",
		"xml",
		"yaml",
		"zig",
	}
	vim.api.nvim_create_autocmd("FileType", {
		pattern = vim.fn.join(parsers, ","),
		group = "dotfiles-settings",
		callback = function()
			vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
			vim.opt_local.foldmethod = "expr"
		end,
		desc = "Set fold method for treesitter",
	})

	require("nvim-treesitter.parsers").list.xml = {
		install_info = {
			url = "https://github.com/Trivernis/tree-sitter-xml",
			files = { "src/parser.c" },
			generate_requires_npm = true,
			branch = "main",
		},
		filetype = "xml",
	}

	require("nvim-treesitter.configs").setup({
		ensure_installed = parsers,
		highlight = { enable = true },
		context_commentstring = { enable = true },
		endwise = { enable = true },
		autotag = { enable = true },
		matchup = { enable = true },
	})
end

return config_treesitter
