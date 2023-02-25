local t = require("dotfiles.utils.termcode")
local M = {}

function toggle_lines(sl, el)
	for lnum = sl, el do
		local line = vim.fn.getline(lnum)
		if vim.regex([[ X$]]):match_str(line) then
			vim.fn.setline(lnum, vim.fn.substitute(line, " X$", "", ""))
		elseif vim.regex([==[\[ \]]==]):match_str(line) then
			vim.fn.setline(lnum, vim.fn.substitute(line, "\\[ \\]", "[X]", ""))
		elseif vim.regex([==[\[X\]]==]):match_str(line) then
			vim.fn.setline(lnum, vim.fn.substitute(line, "\\[X\\]", "[ ]", ""))
		else
			vim.fn.setline(lnum, line .. " X")
		end
	end
end
function M.operatorfunc(mode)
	if mode == nil then
		vim.opt.operatorfunc = "v:lua.require'todo'.operatorfunc" -- Can't have parentheses
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

	toggle_lines(line_left, line_right)
	return ""
end

-- Find Project
function M.find_project(direction)
	local flag = direction == "next" and "w" or "bw"
	for _ = 1, (vim.v.count == 0 and 1 or vim.v.count) do
		vim.fn.search([[^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$]], flag)
	end
	if vim.fn["repeat#set"] then
		vim.fn["repeat#set"](t("(<Plug>todo-" .. direction .. "-project)"), vim.v.count)
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
function M.complete_project(lead)
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

function M.goto_project()
	local res = vim.fn.input("Project: ", "", "customlist,v:lua.require'todo'.complete_project")

	if res ~= "" then
		search_projects(vim.fn.split(res, ":"))
	end
end

M.config = nil
local default_config = {
	maps = {
		done = {
			toggle = "gtd",
			motion = "gt",
			visual = "gt",
		},
		jump = {
			by_name = "gtg",
			next = "]t",
			prev = "[t",
			search = "gt/",
		},
	},
}

local function set_maps()
	if M.config.maps.done.toggle ~= "" then
		vim.keymap.set(
			"n",
			M.config.maps.done.toggle,
			"<Plug>(todo-toggle-done)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
	if M.config.maps.done.visual ~= "" then
		vim.keymap.set(
			"x",
			M.config.maps.done.visual,
			"<Plug>(todo-toggle-done-visual)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
	if M.config.maps.done.motion ~= "" then
		vim.keymap.set(
			"n",
			M.config.maps.done.motion,
			"<Plug>(todo-toggle-done-motion)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
	if M.config.maps.jump.next ~= "" then
		vim.keymap.set(
			"n",
			M.config.maps.jump.next,
			"<Plug>(todo-next-project)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
	if M.config.maps.jump.prev ~= "" then
		vim.keymap.set(
			"n",
			M.config.maps.jump.prev,
			"<Plug>(todo-prev-project)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
	if M.config.maps.jump.search ~= "" then
		vim.keymap.set(
			"n",
			M.config.maps.jump.search,
			"<Plug>(todo-search-done)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
	if M.config.maps.jump.by_name ~= "" then
		vim.keymap.set(
			"n",
			M.config.maps.jump.by_name,
			"<Plug>(todo-goto-project)",
			{ buffer = true, silent = true, noremap = true }
		)
	end
end

function M.setup(opts)
	if M.config == nil then
		M.config = vim.tbl_deep_extend("keep", opts or {}, default_config)
	end

	vim.keymap.set("", "<Plug>(todo-next-project)", ":<C-u>lua require('todo').find_project('next')<CR>")
	vim.keymap.set("", "<Plug>(todo-prev-project)", ":<C-u>lua require('todo').find_project('prev')<CR>")
	vim.keymap.set("", "<Plug>(todo-search-done)", [[/\(^\s*[-*] \[[xX]\].*$\|^.* X$\)<CR>]])
	vim.keymap.set("", "<Plug>(todo-toggle-done)", "v:lua.require'todo'.operatorfunc() . '$'", { expr = true })
	vim.keymap.set("", "<Plug>(todo-toggle-done-visual)", ":<C-u>lua require('todo').operatorfunc('visual')<CR>")
	vim.keymap.set("", "<Plug>(todo-toggle-done-motion)", "v:lua.require'todo'.operatorfunc()", { expr = true })
	vim.keymap.set("", "<Plug>(todo-goto-project)", ":<C-u>lua require('todo').goto_project()<CR>")

	local todo_augroup = vim.api.nvim_create_augroup("todo", { clear = true })
	vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
		group = todo_augroup,
		pattern = "todo.*",
		callback = set_maps,
	})
	vim.api.nvim_create_autocmd("FileType", {
		group = todo_augroup,
		pattern = "vimwiki,markdown",
		callback = set_maps,
	})
end
return M
