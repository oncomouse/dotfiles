if pcall(require, "null-ls") then
	local b = require("null-ls").builtins

	local sources = {
		b.formatting.prettier.with({
			update_on_insert = false,
			extra_args = { "--use-tabs" },
			prefer_local = "node_modules/.bin",
		}),
		b.formatting.prettier.with({
			filetypes = {
				"yaml",
			},
			prefer_local = "node_modules/.bin",
		}),
		b.formatting.stylua,
		b.formatting.black.with({
			extra_args = { "-l", "79" }, -- PEP8 line lengths
		}),
		b.formatting.reorder_python_imports,
		b.formatting.fish_indent,
		b.formatting.shfmt,
		b.formatting.shellharden,
		b.formatting.rubocop,
		b.formatting.standardrb,
		b.diagnostics.shellcheck.with({
			diagnostics_format = "#{m} [#{c}]",
		}),
		b.diagnostics.selene.with({
			cwd = function(_params)
				return vim.fs.dirname(
					vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
				) or vim.fn.expand("~/.config/selene/") -- fallback value
			end,
		}),
		b.diagnostics.flake8,
		b.diagnostics.vint,
		b.diagnostics.rubocop,
		b.diagnostics.standardrb,
		require("dotfiles.plugins.null-ls.builtins.diagnostics.htmlhint"),
		b.code_actions.shellcheck,
		require("nvim-ref.null-ls.hover"),
		require("nvim-ref.null-ls.completion"),
		-- b.completion.luasnip,
	}

	require("null-ls").setup({
		on_attach = require("dotfiles.plugins.nvim-lspconfig").on_attach,
		sources = sources,
	})
end
