local parsers = {
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

return {
	"nvim-treesitter/nvim-treesitter",
	build = function()
		vim.cmd([[TSUpdate]])
	end,
	config = function()
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
			autotag = { enable = true },
			matchup = { enable = true },
			textobjects = {
				select = {
					enable = true,

					-- Automatically jump forward to textobj, similar to targets.vim
					lookahead = true,
					keymaps = {
						-- You can use the capture groups defined in textobjects.scm
						["af"] = "@function.outer",
						["if"] = "@function.inner",
						["ac"] = "@class.outer",
						-- you can optionally set descriptions to the mappings (used in the desc parameter of nvim_buf_set_keymap
						["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
					},
				},
				move = {
					enable = true,
					set_jumps = true, -- whether to set jumps in the jumplist
					goto_next_start = {
						["]m"] = "@function.outer",
					},
					goto_next_end = {
						["]M"] = "@function.outer",
					},
					goto_previous_start = {
						["[m"] = "@function.outer",
					},
					goto_previous_end = {
						["[M"] = "@function.outer",
					},
				},
			},
		})
	end,
	dependencies = {
		"nvim-treesitter/nvim-treesitter-textobjects",
		"windwp/nvim-ts-autotag",
		{
			"andymass/vim-matchup",
			lazy = false,
			init = function()
				vim.g.matchup_matchparen_offscreen = {
					method = "popup",
				}
			end,
		},
		{
			"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
			config = function()
				require("mini.comment").setup({
					hooks = {
						pre = function()
							require("ts_context_commentstring.internal").update_commentstring()
						end,
					},
				})
			end,
		},
	},
}
