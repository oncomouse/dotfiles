return {
	"jose-elias-alvarez/null-ls.nvim",
	event = "BufReadPre",
	opts = function()
		local eslint_project = require("dotfiles.null-ls.helpers.eslint_project")
		local b = require("null-ls").builtins
		local has_cmp = pcall(require, "cmp")

		local sources = {
			-- GENERAL PURPOSE
			b.formatting.prettier.with({
				update_on_insert = false,
				filetypes = {
					"css",
					"scss",
					"less",
					"html",
					"json",
					"jsonc",
					"markdown",
					"markdown.mdx",
					"graphql",
					"handlebars",
				},
				extra_args = { "--use-tabs" },
				prefer_local = "node_modules/.bin",
			}),
			has_cmp and {} or b.completion.luasnip,

			-- YAML
			b.formatting.prettier.with({
				filetypes = {
					"yaml",
				},
				prefer_local = "node_modules/.bin",
			}),

			-- LUA
			b.formatting.stylua,
			b.diagnostics.selene.with({
				cwd = function(_params)
					return vim.fs.dirname(
						vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
					) or vim.fn.expand("~/.config/selene/") -- fallback value
				end,
			}),

			-- PYTHON
			b.formatting.black.with({
				extra_args = { "-l", "79" }, -- PEP8 line lengths
			}),
			b.formatting.reorder_python_imports,
			b.diagnostics.flake8,

			-- FISH
			b.formatting.fish_indent,

			-- RUBY
			b.formatting.standardrb,
			b.diagnostics.standardrb,

			-- SHELL
			b.formatting.shfmt,
			b.formatting.shellharden,
			b.diagnostics.shellcheck.with({
				diagnostics_format = "#{m} [#{c}]",
			}),
			b.code_actions.shellcheck,

			-- VIML
			b.diagnostics.vint,

			-- HTML
			require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

			-- MARKDOWN
			require("nvim-ref.null-ls.hover"),
			has_cmp and {} or require("nvim-ref.null-ls.completion"),

			-- JAVASCRIPT
			-- Use standard and prettier for non-eslint projects:
			b.diagnostics.standardjs.with({
				condition = function()
					return not eslint_project()
				end,
			}),
			b.formatting.prettier_standard.with({
				filetypes = {
					"vue",
					"javascript",
					"javascriptreact",
					"typescript",
					"typescriptreact",
				},
				condition = function()
					return not eslint_project()
				end,
			}),
			-- Use eslint_d for eslint projects:
			b.formatting.eslint_d.with({
				condition = function()
					return eslint_project()
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
			on_attach = require("dotfiles.nvim-lspconfig").on_attach,
			sources = sources,
		}
	end,
}
