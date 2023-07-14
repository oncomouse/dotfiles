return {
	{
		"jose-elias-alvarez/null-ls.nvim",
		ft = {
			"bash",
			"css",
			"fish",
			"html",
			"javascript",
			"javascript.react",
			"json",
			"jsonc",
			"less",
			"lua",
			"markdown",
			"py",
			"ruby",
			"scss",
			"sh",
			"svelte",
			"tex",
			"typescript",
			"typescript.react",
			"vim",
			"vue",
			"yaml",
		},
		opts = function()
			local eslint_project = require("dotfiles.null-ls.helpers.eslint_project")
			local b = require("null-ls").builtins

			local sources = {
				-- LUA
				b.diagnostics.selene.with({
					cwd = function(_params)
						return vim.fs.dirname(
							vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
						) or vim.fn.expand("~/.config/selene/") -- fallback value
					end,
				}),

				-- PYTHON
				b.diagnostics.flake8,

				-- SHELL
				b.diagnostics.shellcheck.with({
					diagnostics_format = "#{m} [#{c}]",
				}),
				b.code_actions.shellcheck,

				-- VIML
				b.diagnostics.vint,

				-- HTML
				require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

				-- JAVASCRIPT
				-- Use standard and prettier for non-eslint projects:
				b.diagnostics.standardjs.with({
					condition = function()
						return not eslint_project()
					end,
				}),
				b.diagnostics.eslint_d.with({
					condition = function()
						return eslint_project()
					end,
				}),
				b.code_actions.eslint_d.with({
					condition = function()
						return eslint_project()
					end,
				}),
			}

			return {
				on_attach = require("dotfiles.plugins.lsp.on_attach"),
				sources = sources,
			}
		end,
	},
}
