local M = {}

local join_patterns = {
	"^> ", -- Block quotes
	"^%s*[0-9]+%. ", -- Ordered lists
	"^%s*[*-] ", -- Bulleted lists
}

local function find_active_pattern(line, sub)
	local active_pattern = nil
	for _, pattern in pairs(join_patterns) do
		local match = line:match(pattern)
		if match then
			active_pattern = pattern
			if sub then
				line = line:sub(#match + 1)
			end
			break
		end
	end
	if active_pattern == nil and sub then
		line = line:gsub("^%s+", "")
	end
	return active_pattern, line
end

local function join_lines(linenr, end_linenr)
	local active_pattern = nil
	local lines = {}
	for _,ln in pairs(vim.fn.range(linenr, end_linenr)) do
		local line = vim.fn.getline(ln)
		local match = active_pattern and line:match(active_pattern) or nil
		if match then
			line = line:sub(#match + 1)
		else
			active_pattern, line = find_active_pattern(line, ln ~= linenr)
		end
		if #line > 0 then
			table.insert(lines, line)
		end
	end
	vim.api.nvim_buf_set_lines(0, linenr - 1, end_linenr, false, { vim.fn.join(lines, " ") })
end

function M.join()
	local linenr = vim.fn.line(".")
	local end_linenr = linenr + 1
	join_lines(linenr, end_linenr)
end

function M.join_opfunc(mode)
	if mode == nil then
		vim.opt.operatorfunc = "v:lua.require'dotfiles.markdown'.join_opfunc" -- Can't have parentheses
		return "g@"
	end
	-- This code is from mini.nvim's comment module
	local mark_left, mark_right = "[", "]"
	if mode == "visual" then
		mark_left, mark_right = "<", ">"
	end

	local linenr, col_left = unpack(vim.api.nvim_buf_get_mark(0, mark_left))
	local end_linenr, col_right = unpack(vim.api.nvim_buf_get_mark(0, mark_right))

	-- Do nothing if "left" mark is not on the left (earlier in text) of "right"
	-- mark (indicating that there is nothing to do, like in comment textobject).
	if (linenr > end_linenr) or (linenr == end_linenr and col_left > col_right) then
		return
	end
	--- End code from mini.nvim
	join_lines(linenr, end_linenr)
end

function M.set_buf_maps()
	local has_autolist = pcall(require, "autolist")
	if has_autolist then
		vim.keymap.set("i", "<C-z>", "<cmd>lua require('autolist').invert()<CR>", { buffer = true })
		vim.keymap.set("i", "<C-t>", "<C-t><cmd>lua require('autolist').tab()<CR>", { buffer = true })
		vim.keymap.set("i", "<CR>", "<CR><cmd>lua require('autolist').new()<CR>", { buffer = true })
		vim.keymap.set("n", ">>", ">><cmd>lua require('autolist').tab()<CR>", { buffer = true })
		vim.keymap.set("n", "<<", "<<<cmd>lua require('autolist').detab()<CR>", { buffer = true })
		vim.keymap.set("n", "o", "o<cmd>lua require('autolist').new()<CR>", { buffer = true })
		vim.keymap.set("n", "O", "O<cmd>lua require('autolist').new(true)<CR>", { buffer = true })
		vim.keymap.set("n", "<C-z>", "<cmd>lua require('autolist').recal()<CR>", { buffer = true })
		vim.keymap.set("n", "dd", "dd<cmd>lua require('autolist').recal()<CR>", { buffer = true })
		vim.keymap.set("n", "p", "p<cmd>lua require('autolist').recal()<CR>", { buffer = true })
		vim.keymap.set("n", "P", "P<cmd>lua require('autolist').recal()<CR>", { buffer = true })
	end
	vim.keymap.set("i", "<C-d>", function()
		local function get_match(regex, line)
			local match = nil
			local start, ed = vim.regex(regex):match_str(line)
			if start ~= nil then
				match = string.sub(line, start, ed)
			end
			return match
		end
		local line = vim.api.nvim_get_current_line()
		-- Check line for unordered list:
		local match = get_match([[^[*-] \(\[.\]\)\{0,1\}]], line)
		-- Check line for ordered list:
		if not match then
			match = get_match([[^\d\+\. \(\[.\]\)\{0,1\}]], line)
		end
		if match then
			local savepos = vim.fn.winsaveview().col
			local restore_input = (savepos == #line) and "$a" or savepos - #match .. "li"
			print(match, #match)
			return '<Esc>0"_' .. #match .. "dl" .. restore_input
		end
		return "<C-d>" .. (has_autolist and "<cmd>lua require('autolist').detab()<CR>" or "")
	end, {
		expr = true,
		buffer = true,
	})
	vim.keymap.set({ "n" }, "J", M.join, { buffer = true })
	vim.keymap.set({ "v" }, "J", M.join_opfunc, { expr = true, buffer = true })
end
return M
