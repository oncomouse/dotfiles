local t = require("dotfiles.utils.termcode")
local M = {}
function toggle_line(lnum)
	lnum = lnum or "."
	local line = vim.fn.getline(lnum)
	if vim.regex([[ X$]]):match_str(line) then
		vim.fn.setline(lnum, vim.fn.substitute(line, " X$", "", ""))
	elseif vim.regex([==[\[ \]]==]):match_str(line) then
		vim.fn.setline(lnum, vim.fn.substitute(line, "[ ]", "[X]", ""))
	elseif vim.regex([==[\[X\]]==]):match_str(line) then
		vim.fn.setline(lnum, vim.fn.substitute(line, "[X]", "[ ]", ""))
	else
		vim.fn.setline(lnum, line .. " X")
	end
end
function M.toggle_lines(sl, el)
	for lnum = sl, el do
		toggle_line(lnum)
	end
end
function M.toggle_done_opfunc(mode)
	if mode == nil then
		vim.opt.opfunc = "v:lua.require('todo').toggle_done_opfunc"
		return "g@"
	end
	-- This code is from mini.nvim's comment module
	local mark_left, mark_right = "[", "]"
	if mode == "visual" then
		mark_left, mark_right = "<", ">"
	end

	local line_left, col_left = unpack(vim.api.nvim_buf_get_mark(0, mark_left))
	local line_right, col_right = unpack(vim.api.nvim_buf_get_mark(0, mark_right))

	-- Do nothing if "left" mark is not on the left (earlier in text) of "right"
	-- mark (indicating that there is nothing to do, like in comment textobject).
	if (line_left > line_right) or (line_left == line_right and col_left > col_right) then
		return
	end
	--- End code from mini.nvim

	M.toggle_lines(line_left, line_right)
	return ""
end

-- Find Project
local function find_project(direction)
	local flag = direction == "next" and "w" or "bw"
	for _ = 1, (vim.v.count == 0 and 1 or vim.v.count) do
		vim.fn.search([[^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$]], flag)
	end
	if vim.fn["repeat#set"] then
		vim.fn["repeat#set"](t("<Plug>todo-" .. direction .. "-project"), vim.v.count)
	end
end
-- Search
local function search_project(project, depth, begin, ed)
	vim.fn.cursor(begin, 1)
	return vim.fn.search([[\v^\t{]] .. depth .. [[}\V]] .. project .. ":", "c", ed)
end
local function search_end_of_item(...)
	local args = { ... }
	local lnum = args[1] > 0 and args[2] or vim.fn.line(".")
	local flags = args[1] > 1 and args[3] or ""

	local depth = vim.fn.len(vim.fn.matchstr(vim.fn.getline(lnum), "^\t*"))

	local ed = lnum
	lnum = lnum + 1
	while lnum <= vim.fn.line("$") do
		local line = vim.fn.getline(lnum)

		if not vim.regex([[^\s*$]]):match_str(line) then
			if depth < vim.fn.len(vim.fn.matchstr(line, "^\t*")) then
				ed = lnum
			else
				break
			end
		end

		lnum = lnum + 1
	end

	if not vim.regex([[n]]):match_str(flags) then
		vim.fn.cursor(ed, 0)
		vim.api.nvim_feedkeys("^", "x", true)
	end

	return ed
end
local function search_projects(projects)
	if vim.fn.empty(projects) == 1 then
		return 0
	end

	local save_pos = vim.fn.getpos(".")

	local begin = 1
	local ed = vim.fn.line("$")
	local depth = 0

	for _, project in pairs(projects) do
		if search_project(project, depth, begin, ed) == 0 then
			vim.fn.setpos(".", save_pos)
			return 0
		end

		begin = vim.fn.line(".")
		ed = search_end_of_item(begin)
		depth = depth + 1
	end

	vim.fn.cursor(begin, 1)
	vim.api.nvim_feedkeys("^", "x", true)

	return begin
end
function M.CompleteProject(lead)
	local lnum = 1
	local list = {}
	local stack = { "" }
	local depth = 1

	while lnum <= vim.fn.line("$") do
		local line = vim.fn.getline(lnum)
		local ml = vim.fn.matchlist(line, [[\v\C^\t*(.+):(\s+\@[^ \t(]+(\([^)]*\))?)*$]])

		if vim.fn.empty(ml) == 0 then
			local d = vim.fn.len(vim.fn.matchstr(line, "^\t*")) + 1

			while d < depth do
				table.remove(stack, #stack)
				depth = depth - 1
			end

			while d > depth do
				table.insert(stack, "")
				depth = depth + 1
			end

			stack[d] = ml[1]

			local candidate = vim.fn.join(stack, ":")
			if vim.regex([[^]] .. lead):match_str(candidate) then
				-- candidate = candidate:gsub(":$", "")
				table.insert(list, candidate)
			end
		end

		lnum = lnum + 1
	end

	return list
end

local function goto_project()
	local res = vim.fn.input("Project: ", "", "customlist,v:lua.require('todo').CompleteProject")

	if res ~= "" then
		search_projects(vim.fn.split(res, ":"))
	end
end

function M.setup()
	vim.keymap.set("", "<Plug>todo-next-project", function()
		find_project("next")
	end)
	vim.keymap.set("", "<Plug>todo-prev-project", function()
		find_project("prev")
	end)
	vim.keymap.set("", "<Plug>todo-search-done", "/ X$<CR>")
	vim.keymap.set("", "<Plug>todo-toggle-done", toggle_line)
	vim.keymap.set("", "<Plug>todo-toggle-visual", ":<C-u>lua require('todo').toggle_done_opfunc('visual')<CR>")
	vim.keymap.set("", "<Plug>todo-toggle-motion", function()
		return M.toggle_done_opfunc()
	end, { expr = true })
	vim.keymap.set("", "<Plug>todo-goto-project", goto_project)
end

return M
