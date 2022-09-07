local hooks = require("nvim-ref.hooks")
local M = {}

hooks.add_hook("setup_done", function()
	hooks.run_hook("add_command", {
		id = "insert",
		name = "Insert a citation",
		subcommands = {
			{
				id = "ref",
				name = "Insert a reference",
				callback = function()
					require("nvim-ref.insert.select")("", { type = "ref" })
				end,
			},
			{
				id = "citation",
				name = "Insert a full citation",
				callback = function()
					require("nvim-ref.insert.select")("", { type = "citation" })
				end,
			},
		},
	})
end)

local function get_cursor_column()
	local _, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col
end

function M.insert(cite)
	local c = vim.fn.col(".")
	local line = vim.api.nvim_get_current_line()
	local before = line:sub(1, c - 1)
	local after = line:sub(c)
	local output
	if type(cite) == "table" then
		output = cite.before .. cite.after
	else
		output = cite
	end
	vim.api.nvim_set_current_line(before .. output .. after)
	vim.api.nvim_win_set_cursor(0, { vim.fn.line("."), c + (type(cite) == "table" and #cite.before or #output) - 1 })
	if vim.fn.mode() ~= "i" then
		if type(cite) == "string" and get_cursor_column() == (#vim.api.nvim_get_current_line() - 1) then
			vim.api.nvim_feedkeys("a", "", false)
		else
			vim.api.nvim_feedkeys("i", "", false)
		end
	end
end

return M
