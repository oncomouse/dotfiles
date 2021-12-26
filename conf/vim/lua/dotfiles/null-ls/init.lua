-- luacheck: globals vim dotfiles
local function eslint_project(utils)
	return utils.root_has_file({
		".eslintrc",
		".eslintrc.js",
		".eslintrc.cjs",
		".eslintrc.yaml",
		".eslintrc.yml",
		".eslintrc.json",
	})
end

local semistandard_types = {
	"formatting",
	"diagnostics",
}

local eslint_types = {
	"formatting",
	"diagnostics",
	"code_actions",
}

local function javascript_register(type)
	return require("null-ls.helpers").conditional(function(utils)
		if not eslint_project(utils) then
			return vim.tbl_contains(semistandard_types, type)
					and require("dotfiles.null-ls.builtins." .. type .. ".semistandard")
				or nil
		end

		local program = vim.fn.executable("eslint_d") == 1 and "eslint_d" or "eslint"
		return vim.tbl_contains(eslint_types, type) and require("null-ls").builtins[type][program] or nil
	end)
end

require("null-ls").setup({
	on_attach = require("dotfiles.nvim_lsp.on_attach"),
	sources = {
		require("null-ls").builtins.formatting.prettier.with({
			update_on_insert = false,
			extra_args = { "--use-tabs" },
			filetypes = {
				"css",
				"graphql",
				"html",
				"json",
				"less",
				"markdown",
				"scss",
				"svelte",
				"vue",
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
			extra_args = { "-l", "79" }, -- PEP8 line lengths
		}),
		require("null-ls").builtins.formatting.reorder_python_imports,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.rubocop,
		require("null-ls").builtins.formatting.standardrb,
		require("null-ls").builtins.diagnostics.shellcheck,
		require("null-ls").builtins.diagnostics.luacheck,
		require("null-ls").builtins.diagnostics.flake8,
		require("null-ls").builtins.diagnostics.vint,
		require("null-ls").builtins.diagnostics.rubocop,
		require("null-ls").builtins.diagnostics.standardrb,
		require("null-ls").builtins.code_actions.shellcheck,
		require("dotfiles.null-ls.builtins.hover.bibtex"),
		javascript_register("formatting"),
		javascript_register("diagnostics"),
		javascript_register("code_actions"),
	},
})
