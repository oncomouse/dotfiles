local M = {}
local join_patterns = {
	"^> ",
	"^[0-9]+%. ",
	"^[*-] ",
}

local function join_lines(linenr, end_linenr)
	local active_pattern = nil
	local lines = {}
	for _,ln in pairs(vim.fn.range(linenr, end_linenr)) do
		local line = vim.fn.getline(ln)
		if active_pattern then
			local match = line:match(active_pattern)
			if match then
				line = line:sub(#match + 1)
			else
				active_pattern = nil
				if ln ~= linenr then
					line = line:gsub("^%s+", "")
				end
			end
		else
			for _, pattern in pairs(join_patterns) do
				local match = line:match(pattern)
				if match then
					active_pattern = pattern
					if ln ~= linenr then
						line = line:sub(#match + 1)
					end
					break
				end
			end
			if active_pattern == nil and ln ~= linenr then
				line = line:gsub("^%s+", "")
			end
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
return M
