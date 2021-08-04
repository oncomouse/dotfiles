vim.cmd([[
	packadd plenary.nvim
	packadd null-ls.nvim
]])
local FORMATTING = require("null-ls.methods").internal.FORMATTING
local RANGE_FORMATTING = require("null-ls.methods").internal.RANGE_FORMATTING
local get_prettier_generator_args = function(common_args)
	return function(params)
		local args = vim.deepcopy(common_args)

		if params.method == FORMATTING then
			return args
		end

		local content, range = params.content, params.range

		local row, col = range.row, range.col
		local range_start = row == 1 and 0
			or vim.fn.strchars(table.concat({ unpack(content, 1, row - 1) }, "\n") .. "\n", true)
		range_start = range_start + vim.fn.strchars(vim.fn.strcharpart(unpack(content, row, row), 0, col), true)

		local end_row, end_col = range.end_row, range.end_col
		local range_end = end_row == 1 and 0
			or vim.fn.strchars(table.concat({ unpack(content, 1, end_row - 1) }, "\n") .. "\n", true)
		range_end = range_end + vim.fn.strchars(vim.fn.strcharpart(unpack(content, end_row, end_row), 0, end_col), true)

		table.insert(args, "--range-start")
		table.insert(args, range_start)
		table.insert(args, "--range-end")
		table.insert(args, range_end)

		return args
	end
end
require("null-ls").config({
	sources = {
		require("null-ls.helpers").make_builtin({
			method = { FORMATTING, RANGE_FORMATTING },
			filetypes = {
				"javascript",
				"javascriptreact",
				"typescript",
				"typescriptreact",
				"vue",
				"svelte",
				"css",
				"scss",
				"html",
				"json",
				"yaml",
				"markdown",
			},
			generator_opts = {
				command = "prettier",
				args = get_prettier_generator_args({ "--use-tabs", "--stdin-filepath", "$FILENAME" }),
				to_stdin = true,
			},
			factory = require("null-ls.helpers").formatter_factory,
		}),
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.rufo,
	},
})
