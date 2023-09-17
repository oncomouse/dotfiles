local function attach_filetype(opts, filetype, formatter)
	local new_opts = vim.tbl_extend("keep", opts, {})
	if type(new_opts.filetype[filetype]) == "nil" then
		new_opts.filetype[filetype] = { formatter }
	else
		table.insert(new_opts.filetype[filetype], formatter)
	end
	return new_opts
end
return {
	-- "mhartington/formatter.nvim",
	-- lazy = true,
	-- opts = function()
	-- 	local opts = {
	-- 		filetype = {},
	-- 	}
	-- 	local prettier = require("formatter.defaults").prettier
	-- 	for _, ft in pairs({
	-- 		"css",
	-- 		"graphql",
	-- 		"handlebars",
	-- 		"html",
	-- 		"json",
	-- 		"jsonc",
	-- 		"less",
	-- 		"markdown",
	-- 		"markdown.mdx",
	-- 		"scss",
	-- 		"svelte",
	-- 	}) do
	-- 		local parser = prettier(ft)
	-- 		table.insert(parser.args, "--use-tabs")
	-- 		opts = attach_filetype(opts, ft, parser)
	-- 	end
	-- 	opts = attach_filetype(opts, "yaml", prettier("yaml"))
	--
	-- 	opts = attach_filetype(opts, "lua", require("formatter.filetypes.lua").stylua)
	--
	-- 	local black = function()
	-- 		local b = require("formatter.filetypes.python").black()
	-- 		table.insert(b.args, "-l")
	-- 		table.insert(b.args, "79")
	-- 		return b
	-- 	end
	-- 	opts = attach_filetype(opts, "python", black)
	-- 	opts = attach_filetype(opts, "python", {
	-- 		exe = "reorder-python-imports",
	-- 		args = { "-", "--exit-zero-even-if-changed" },
	-- 		stdin = true,
	-- 	})
	--
	-- 	opts = attach_filetype(opts, "fish", require("formatter.filetypes.fish").fishindent)
	--
	-- 	opts = attach_filetype(opts, "sh", require("formatter.filetypes.sh").shfmt)
	-- 	opts = attach_filetype(opts, "sh", {
	-- 		exe = "shellharden",
	-- 		args = { "--transform", "" },
	-- 		stdin = true,
	-- 	})
	--
	-- 	for _, ft in pairs({
	-- 		"vue",
	-- 		"javascript",
	-- 		"javascriptreact",
	-- 		"typescript",
	-- 		"typescriptreact",
	-- 	}) do
	-- 		opts = attach_filetype(opts, ft, function()
	-- 			if require("dotfiles.utils.javascript").eslint_project() then
	-- 				return require("formatter.filetypes.javascript").eslint_d
	-- 			end
	-- 			return require("formatter.filetypes.javascript").standard
	-- 		end)
	-- 	end
	--
	-- 	return opts
	-- end,
}
