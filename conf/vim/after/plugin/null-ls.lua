local ok = pcall(require, "null-ls")
if ok then
	local javascript_register = require("dotfiles.plugins.null-ls.helpers.javascript_register")
	local sources = {
		require("null-ls").builtins.formatting.prettier.with({
			update_on_insert = false,
			extra_args = { "--use-tabs" },
			prefer_local = "node_modules/.bin",
		}),
		require("null-ls").builtins.formatting.prettier.with({
			filetypes = {
				"yaml",
			},
			prefer_local = "node_modules/.bin",
		}),
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black.with({
			extra_args = { "-l", "79" }, -- PEP8 line lengths
		}),
		require("null-ls").builtins.formatting.reorder_python_imports,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.shellharden,
		require("null-ls").builtins.formatting.rubocop,
		require("null-ls").builtins.formatting.standardrb,
		require("null-ls").builtins.diagnostics.shellcheck.with({
			diagnostics_format = "#{m} [#{c}]",
		}),
		require("null-ls").builtins.diagnostics.selene,
		require("null-ls").builtins.diagnostics.flake8,
		require("null-ls").builtins.diagnostics.vint,
		require("null-ls").builtins.diagnostics.rubocop,
		require("null-ls").builtins.diagnostics.standardrb,
		require("null-ls").builtins.code_actions.shellcheck,
		require("dotfiles.plugins.null-ls.builtins.hover.bibtex"),
		require("dotfiles.plugins.null-ls.builtins.completion.bibtex"),
		javascript_register("formatting"),
		javascript_register("diagnostics"),
		javascript_register("code_actions"),
	}

	if pcall(require, "luasnip") then
		table.insert(sources, require("null-ls").builtins.completion.luasnip)
	end

	require("null-ls").setup({
		on_attach = require("dotfiles.nvim-lsp.on_attach"),
		sources = sources,
	})
end
