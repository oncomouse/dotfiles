local format = require("nvim-ref.format")
local M = {}

local function get_cursor_column()
	local _, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col
end

local function insert(cite)
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

function M.key(citation)
	local cite = format.get_key(citation)
	return insert(cite)
end

function M.citation(citation)
	local cite = format.get_citation(citation)
	return insert(cite)
end

return M
