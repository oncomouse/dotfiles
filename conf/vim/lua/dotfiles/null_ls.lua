vim.cmd([[
	packadd plenary.nvim
	packadd null-ls.nvim
]])
require("null-ls").config({
	sources = {
		require("null-ls").builtins.formatting.prettier.with({
			extra_args = { "--use-tabs" },
		}),
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.rufo,
	},
})
