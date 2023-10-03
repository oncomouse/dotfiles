-- Patch to, when jumping to an item from the agenda, close the agenda window if they
-- item's file is already open.
local Agenda = require("orgmode.agenda")

local function get_all_buffers()
	local buffers = {}
	local len = 0
	local options_listed = false
	local vim_fn = vim.fn
	local buflisted = vim_fn.buflisted
	for buffer = 1, vim_fn.bufnr("$") do
		if not options_listed or buflisted(buffer) ~= 1 then
			len = len + 1
			buffers[len] = buffer
		end
	end
	return buffers
end

local function find_open_buffer(file_name)
	local target_buffer_name = vim.fn.substitute(file_name, vim.fn.getcwd() .. "/", "", "")
	local match = vim.tbl_filter(function(x)
		return vim.fn.bufname(x) == target_buffer_name
	end, get_all_buffers())
	if #match then
		return match[1]
	end
	return nil
end

function Agenda:switch_to_item()
	local item = self:_get_jumpable_item()
	if not item then
		return
	end
	-- Close the agenda window if the file in question is already open:
	local open_buffer = find_open_buffer(item.file)
	if open_buffer ~= nil then
		vim.api.nvim_win_close(vim.fn.bufwinid(open_buffer), true)
	end
	vim.cmd("edit " .. vim.fn.fnameescape(item.file))
	vim.fn.cursor({ item.file_position, 0 })
	vim.cmd([[normal! zv]])
end
