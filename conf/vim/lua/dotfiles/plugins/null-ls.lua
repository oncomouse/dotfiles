return {
	{
		"nvimtools/none-ls.nvim",
		dependencies = {
			"nvimtools/none-ls-extras.nvim",
		},
		ft = {
			"bash",
			"css",
			"fish",
			"graphql",
			"handlebars",
			"html",
			"javascript",
			"javascript.react",
			"json",
			"jsonc",
			"less",
			"lua",
			"markdown",
			"markdown.mdx",
			"python",
			"scss",
			"sh",
			"svelte",
			"typescript",
			"typescript.react",
			"vim",
			"vue",
			"yaml",
		},
		opts = function()
			local sources = {
				-- GENERAL PURPOSE
				require("null-ls").builtins.formatting.prettier.with({
					update_on_insert = false,
					filetypes = {
						"css",
						"graphql",
						"handlebars",
						"html",
						"json",
						"jsonc",
						"less",
						"markdown",
						"markdown.mdx",
						"scss",
						"svelte",
						"typescript",
						"typescript.react",
						"vue",
					},
					extra_args = { "--use-tabs" },
					prefer_local = "node_modules/.bin",
				}),

				-- YAML
				require("null-ls").builtins.formatting.prettier.with({
					filetypes = {
						"yaml",
					},
					prefer_local = "node_modules/.bin",
				}),

				-- LUA
				require("null-ls").builtins.formatting.stylua,
				require("null-ls").builtins.diagnostics.selene.with({
					cwd = function(_)
						return vim.fs.dirname(
							vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
						) or vim.fn.expand("~/.config/selene/") -- fallback value
					end,
				}),

				-- PYTHON
				require("none-ls.formatting.ruff"),
				require("none-ls.diagnostics.ruff"),

				-- FISH
				require("null-ls").builtins.formatting.fish_indent,

				-- SHELL
				require("null-ls").builtins.formatting.shfmt,

				-- VIML
				require("null-ls").builtins.diagnostics.vint,

				-- HTML
				require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

				-- JAVASCRIPT DEFAULTS
				require("null-ls").builtins.diagnostics.standardjs,
				require("null-ls").builtins.formatting.standardjs,
			}

			return {
				sources = sources,
			}
		end,
	},
}

