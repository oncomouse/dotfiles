local nvim_treesitter = {
	installed_parser = {
		"bash",
		"bibtex",
		"c",
		"cmake",
		"comment",
		"cpp",
		"css",
		"diff",
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
		"luap",
		"make",
		"markdown",
		"markdown_inline",
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
		"svelte",
		"tsx",
		"typescript",
		"vim",
		"vue",
		"xml",
		"yaml",
		"zig",
	},
	parser_configs = {

		-- TODO: Use repo in https://github.com/serenadeai/tree-sitter-scss/pull/19
		scss = {
			install_info = {
				url = "https://github.com/goncharov/tree-sitter-scss",
				files = { "src/parser.c", "src/scanner.c" },
				branch = "placeholders",
				revision = "30c9dc19d3292fa8d1134373f0f0abd8547076e8",
			},
			maintainers = { "@goncharov" },
		},
	},
	line_threshold = {
		base = {
			cpp = 30000,
			javascript = 30000,
			perl = 10000,
		},
		extension = {
			cpp = 10000,
			javascript = 3000,
			perl = 3000,
		},
	}, -- Disable check for highlight, highlight usage, highlight context module
}

nvim_treesitter.should_highlight_disable = function(lang, bufnr)
	local line_count = vim.api.nvim_buf_line_count(bufnr or 0)

	return nvim_treesitter.line_threshold[lang] ~= nil and line_count > nvim_treesitter.line_threshold[lang].base
end

nvim_treesitter.should_buffer_higlight_disable = function()
	local ft = vim.bo.ft
	local bufnr = vim.fn.bufnr()
	return nvim_treesitter.should_highlight_disable(ft, bufnr)
end

return {
	{
		"nvim-treesitter/nvim-treesitter",
		cmd = "TSUpdate",
		event = { "BufReadPost", "BufNewFile" },
		build = function()
			vim.cmd([[TSUpdate]])
			-- Disable in large C++ buffers & JavaScript buffers
		end,
		opts = {
			ensure_installed = nvim_treesitter.installed_parser,
			highlight = {
				enable = true,
				-- additional_vim_regex_highlighting = false,
				additional_vim_regex_highlighting = { "org" },
				disable = nvim_treesitter.should_highlight_disable,
			},
			autotag = { enable = true },
			context_commentstring = {
				enable = true,
			},
			matchup = {
				enable = nvim_treesitter.should_buffer_higlight_disable,
			},
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
		},
		config = function(_, opts)
			local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
			for f, c in pairs(nvim_treesitter.parser_configs) do
				parser_configs[f] = c
			end
			local ts_foldexpr_augroup_id = vim.api.nvim_create_augroup("nvim_treesitter_foldexpr", {})

			vim.api.nvim_create_autocmd("FileType", {
				pattern = vim.fn.join(opts.ensure_installed, ","),
				group = ts_foldexpr_augroup_id,
				callback = function()
					vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
					vim.opt_local.foldmethod = "expr"
				end,
				desc = "Set fold method for treesitter",
			})

			require("nvim-treesitter.configs").setup(opts)
		end,
		dependencies = {
			{ "JoosepAlviste/nvim-ts-context-commentstring" },
			{
				"nvim-treesitter/nvim-treesitter-textobjects",
				init = function()
					local plugin = require("lazy.core.config").spec.plugins["nvim-treesitter"]
					local opts = require("lazy.core.plugin").values(plugin, "opts", false)
					local enabled = false
					if opts.textobjects then
						for _, mod in ipairs({ "move", "select", "swap", "lsp_interop" }) do
							if opts.textobjects[mod] and opts.textobjects[mod].enable then
								enabled = true
								break
							end
						end
					end
					if not enabled then
						require("lazy.core.loader").disable_rtp_plugin("nvim-treesitter-textobjects")
					end
				end,
			},
			"windwp/nvim-ts-autotag",
			{
				"andymass/vim-matchup",
				init = function()
					vim.g.matchup_matchparen_offscreen = {
						method = "popup",
					}
				end,
			},
		},
	},
}
