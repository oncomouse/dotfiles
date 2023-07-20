local Lsp = require("lspize")
local handlers = {
	[Lsp.methods.FORMATTING] = function(params, done)
		vim.print(params)
		local bufnr = vim.uri_to_bufnr(params.textDocument.uri)
		local temp_bufnr = vim.api.nvim_create_buf(false, true)
		vim.api.nvim_buf_set_option(temp_bufnr, "eol", vim.api.nvim_buf_get_option(bufnr, "eol"))
		vim.api.nvim_buf_set_option(temp_bufnr, "fixeol", vim.api.nvim_buf_get_option(bufnr, "fixeol"))
		vim.api.nvim_buf_set_option(temp_bufnr, "fileformat", vim.api.nvim_buf_get_option(bufnr, "fileformat"))
		vim.api.nvim_buf_set_lines(temp_bufnr, 0, -1, false, vim.api.nvim_buf_get_lines(bufnr, 0, -1, false))
		vim.schedule(function()
			vim.api.nvim_buf_delete(temp_bufnr, { force = true })
		end)
		local content
		vim.api.nvim_buf_call(temp_bufnr, function()
			require("formatter.format").format("", "", 1, vim.api.nvim_buf_line_count(bufnr))
			content = vim.api.nvim_buf_get_lines(temp_bufnr, 0, -1, false)
		end)
		done(nil, {
			range = {
				start = { line = 0, character = 0 },
				["end"] = { line = #content - 1, character = #content[#content] - 1 },
			},
			edits = {
				table.concat(content, "\n")
			}
		})
	end,
}

Lsp.create(handlers, {
	on_attach = require("dotfiles.plugins.lsp.on_attach"),
	name = "formatter.nvim",
})
