--luacheck: globals vim
require("null-ls").config({
	sources = {
		require("null-ls").builtins.formatting.prettier.with({
			extra_args = { "--use-tabs" },
			filetypes = {
				"vue",
				"svelte",
				"css",
				"scss",
				"less",
				"html",
				"json",
				"markdown",
				"graphql",
			},
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
			extra_args = { "-l", "79", }, -- PEP8 line lengths
		}),
		require("null-ls").builtins.formatting.reorder_python_imports,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.rubocop,
		require("null-ls").builtins.formatting.standardrb,
		require("dotfiles.null-ls.builtins.formatting.semistandard"),
		require("null-ls").builtins.diagnostics.shellcheck,
		require("null-ls").builtins.diagnostics.luacheck,
		require("null-ls").builtins.diagnostics.flake8,
		require("null-ls").builtins.diagnostics.vint,
		require("null-ls").builtins.diagnostics.rubocop,
		require("null-ls").builtins.diagnostics.standardrb,
		require("dotfiles.null-ls.builtins.diagnostics.semistandard"),
		require("dotfiles.null-ls.builtins.diagnostics.yamllint"),
		require("dotfiles.null-ls.builtins.completion.bibtex"),
		require("null-ls").builtins.completion.vsnip,
		require("dotfiles.null-ls.builtins.hover.bibtex"),
		require("dotfiles.null-ls.builtins.hover.dictionary"),
	},
})
