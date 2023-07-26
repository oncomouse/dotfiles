return {
	{
		"jose-elias-alvarez/null-ls.nvim",
		opts = function()
			local eslint_project = require("dotfiles.null-ls.helpers.eslint_project")
			local b = require("null-ls").builtins

			local sources = {
				-- HTML
				require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

				-- JAVASCRIPT
				-- Use standard for non-eslint projects:
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

				-- LUA
				b.diagnostics.selene.with({
					cwd = function()
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
			}

			return {
				sources = sources,
			}
		end,
	},
}
