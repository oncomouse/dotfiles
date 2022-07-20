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

	require("nvim-treesitter.configs").setup({
		ensure_installed = parsers,
		highlight = { enable = true },
		context_commentstring = { enable = true },
		endwise = { enable = true },
		autotag = { enable = true },
		playground = {
			enable = true,
			disable = {},
			updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
			persist_queries = false, -- Whether the query persists across vim sessions
			keybindings = {
				toggle_query_editor = "o",
				toggle_hl_groups = "i",
				toggle_injected_languages = "t",
				toggle_anonymous_nodes = "a",
				toggle_language_display = "I",
				focus_language = "f",
				unfocus_language = "F",
				update = "R",
				goto_node = "<cr>",
				show_help = "?",
			},
		},
	})
end

return config_treesitter
