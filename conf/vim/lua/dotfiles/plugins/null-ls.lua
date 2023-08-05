return {
	{
		"jose-elias-alvarez/null-ls.nvim",
		opts = function()
			local b = require("null-ls").builtins

			local sources = {
				-- HTML
				require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

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