-- luacheck: globals dotfiles vim
dotfiles = _G.dotfiles or {}
dotfiles.todo = {}
dotfiles.todo.toggle_done = function()
	local line = vim.fn.getline(".")
	if vim.regex([[ X$]]):match_str(line) then
		vim.fn.setline(".", vim.fn.substitute(line, " X$", "", ""))
	elseif vim.regex([==[\[ \]]==]):match_str(line) then
		vim.fn.setline(".", vim.fn.substitute(line, "[ ]", "[X]", ""))
	elseif vim.regex([==[\[X\]]==]):match_str(line) then
		vim.fn.setline(".", vim.fn.substitute(line, "[X]", "[ ]", ""))
	else
		vim.fn.setline(".", line .. " X")
	end
end
dotfiles.todo.next_project = function()
	return vim.fn.search([[^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$]], "w")
end
dotfiles.todo.prev_project = function()
	return vim.fn.search([[^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$]], "bw")
end
-- Search
dotfiles.todo.search_project = function(project, depth, begin, ed)
	vim.fn.cursor(begin, 1)
	return vim.fn.search([[\v^\t{]] .. depth .. [[}\V]] .. project .. ":", "c", ed)
end
dotfiles.todo.search_end_of_item = function(...)
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
		vim.cmd([[normal! ^]])
	end

	return ed
end
dotfiles.todo.search_projects = function(projects)
	if vim.fn.empty(projects) == 1 then
		return 0
	end

	local save_pos = vim.fn.getpos(".")

	local begin = 1
	local ed = vim.fn.line("$")
	local depth = 0

	for _, project in pairs(projects) do
		if dotfiles.todo.search_project(project, depth, begin, ed) == 0 then
			vim.fn.setpos(".", save_pos)
			return 0
		end

		begin = vim.fn.line(".")
		ed = dotfiles.todo.search_end_of_item(begin)
		depth = depth + 1
	end

	vim.fn.cursor(begin, 1)
	vim.cmd([[normal! ^]])

	return begin
end
dotfiles.todo.CompleteProject = function(lead)
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

dotfiles.todo.goto_project = function()
	local res = vim.fn.input("Project: ", "", "customlist,v:lua.dotfiles.todo.CompleteProject")

	if res ~= "" then
		dotfiles.todo.search_projects(vim.fn.split(res, ":"))
	end
end

-- This is many of the commands from taskpaper.vim but set to load on my
-- todo.txt file and using my done notation:
if vim.fn.exists("g:enable_todo") == 0 then
	vim.cmd([[finish]])
end
dotfiles.todo.map = function()
	-- Mark A Task As Done:
	vim.keymap.set(
		"n",
		"<leader>td",
		"<cmd>lua dotfiles.todo.toggle_done()<CR>",
		{ buffer = true, silent = true, noremap = true }
	)
	vim.keymap.set(
		"v",
		"<leader>td",
		"<cmd>lua dotfiles.todo.toggle_done()<CR>",
		{ buffer = true, silent = true, noremap = true }
	)
	-- Go To Project:
	vim.keymap.set(
		"n",
		"<leader>tg",
		"<cmd>lua dotfiles.todo.goto_project()<CR>",
		{ buffer = true, silent = true, noremap = true }
	)
	-- Search For Done Tasks:
	vim.keymap.set("n", "<leader>t/", "/ X$<CR>", { buffer = true, silent = true, noremap = true })
	-- Go To Next Project:
	vim.keymap.set(
		"n",
		"]t",
		"<cmd>lua dotfiles.todo.next_project()<CR>",
		{ buffer = true, silent = true, noremap = true }
	)
	-- Go To Previous Project:
	vim.keymap.set(
		"n",
		"[t",
		"<cmd>lua dotfiles.todo.prev_project()<CR>",
		{ buffer = true, silent = true, noremap = true }
	)
end

vim.cmd([[augroup todo
	autocmd!
	autocmd BufRead,BufNewFile todo.* lua dotfiles.todo.map()
	autocmd FileType vimwiki lua dotfiles.todo.map()
augroup END]])