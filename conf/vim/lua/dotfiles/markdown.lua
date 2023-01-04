local M = {}
local has_autolist = pcall(require, "autolist")

local join_patterns = {
	"^> ", -- Block quotes
	"^%s*([0-9]+%. )", -- Ordered lists
	"^%s*([*-] )", -- Bulleted lists
}

local function find_match(line, pattern)
	local start, stop, match = line:find(pattern)
	local results = { start, stop, match }
	return start ~= nil, results
end

local function do_substitution(line, results, no_indent)
	local start, stop, match = unpack(results)
	local ws = ""
	if no_indent then
		ws = line:sub(start, (match == nil and 0 or stop - #match))
	end
	line = ws .. line:sub(stop + 1)
	return line
end

local function find_active_pattern(line, sub, no_indent)
	local active_pattern = nil
	for _, pattern in pairs(join_patterns) do
		local found, results = find_match(line, pattern)
		if found then
			active_pattern = pattern
			if sub then
				line = do_substitution(line, results, no_indent)
			end
			break
		end
	end
	if not no_indent and active_pattern == nil and sub then
		line = line:gsub("^%s+", "")
	end
	return active_pattern, line
end

local function join_lines(linenr, end_linenr, no_indent)
	local active_pattern = nil
	local lines = {}
	for _, ln in pairs(vim.fn.range(linenr, end_linenr)) do
		local line = vim.fn.getline(ln)
		local found, results
		if active_pattern then
			found, results = find_match(line, active_pattern)
		end
		if found then
			line = do_substitution(line, results, no_indent)
		else
			active_pattern, line = find_active_pattern(line, ln ~= linenr, no_indent)
		end
		if #line > 0 then
			table.insert(lines, line)
		end
	end
	vim.api.nvim_buf_set_lines(0, linenr - 1, end_linenr, false, { vim.fn.join(lines, no_indent and "" or " ") })
end

function M.join(indent)
	local function call_join(no_indent)
		local linenr = vim.fn.line(".")
		local end_linenr = linenr + (vim.v.count == 0 and 1 or vim.v.count)
		join_lines(linenr, end_linenr, no_indent)
	end

	if indent then
		return function()
			return call_join(true)
		end
	end
	return call_join(false)
end

function M.join_opfunc(mode)
	-- Handle J vs gJ:
	local function do_opfunc(m)
		vim.b.dotfiles_markdown_join_no_indent = m
		vim.opt.operatorfunc = "v:lua.require'dotfiles.markdown'.join_opfunc" -- Can't have parentheses
		return "g@"
	end
	if type(mode) == "nil" then
		return do_opfunc(false)
	end
	if type(mode) == "boolean" then
		return function()
			return do_opfunc(mode)
		end
	end

	-- Read whether we are running J or gJ:
	local no_indent = vim.b.dotfiles_markdown_join_no_indent
	vim.b.dotfiles_markdown_join_no_indent = nil

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
	join_lines(line_left, line_right, no_indent)
end

local detab_regexes = {
	[[^[*-] \(\[.\]\)\{0,1\}]],
	[[^\d\+\. \(\[.\]\)\{0,1\}]],
	"^> ",
}

-- Works like string.match but for vim regexes:
local function match_vimregex(line, regex)
	local match = nil
	local start, ed = vim.regex(regex):match_str(line)
	if start ~= nil then
		match = string.sub(line, start, ed)
	end
	return match
end
function M.detab()
	local line = vim.api.nvim_get_current_line()
	for _, r in pairs(detab_regexes) do
		local match = match_vimregex(line, r)
		if match then
			local savepos = vim.fn.winsaveview().col
			local restore_input = (savepos == #line) and "$a" or savepos - #match .. "li"
			return '<Esc>0"_' .. #match .. "dl" .. restore_input
		end
	end
	if has_autolist then
		return require("autolist").indent(nil, "<c-d>")
	end
	return "<c-d>"
end

local function insert_newline(above)
	local action = above and "O" or "o"
	local line = vim.api.nvim_get_current_line()
	if line:match("^> ") then
		return action .. "> "
	end
	if has_autolist then
		if above then
			return require("autolist").new_before(nil, "O")
		else
			return require("autolist").new(nil, "o")
		end
	end
	return action
end

function M.newline(above)
	if above then
		return function()
			return insert_newline(true)
		end
	end
	return insert_newline()
end

function M.set_buf_maps()
	-- autolist.nvim mappings:
	if has_autolist then
		function create_mapping_hook(mode, mapping, hook, alias)
			vim.keymap.set(mode, mapping, function(motion)
				local keys = hook(motion, alias or mapping)
				if not keys then
					keys = ""
				end
				return keys
			end, { expr = true, buffer = true })
		end
		create_mapping_hook("n", "<leader>lx", require("autolist").invert_entry, "")
		create_mapping_hook("i", "<C-t>", require("autolist").indent)
		create_mapping_hook("n", ">>", require("autolist").indent)
		create_mapping_hook("i", "<CR>", require("autolist").new)
		create_mapping_hook("n", "<<", require("autolist").indent)
		create_mapping_hook("i", "<C-z>", require("autolist").force_recalculate)
		create_mapping_hook("n", "<leader>lr", require("autolist").force_recalculate)
		create_mapping_hook("n", "dd", require("autolist").force_recalculate)
		create_mapping_hook("n", "p", require("autolist").force_recalculate)
		create_mapping_hook("n", "P", require("autolist").force_recalculate)
	end
	vim.keymap.set("n", "o", M.newline, { expr = true, buffer = true })
	vim.keymap.set("n", "O", M.newline(true), { expr = true, buffer = true })
	vim.keymap.set("i", "<C-d>", M.detab, { expr = true, buffer = true })

	-- Join line mappings (works like J and v_J but removes markdown markup)
	vim.keymap.set({ "n" }, "J", M.join, { buffer = true })
	vim.keymap.set({ "n" }, "gJ", M.join(true), { buffer = true })
	vim.keymap.set({ "v" }, "J", M.join_opfunc, { expr = true, buffer = true })
	vim.keymap.set({ "v" }, "gJ", M.join_opfunc(true), { expr = true, buffer = true })

	vim.opt_local.comments = vim.opt_local.comments - "n:>"
end
return M
