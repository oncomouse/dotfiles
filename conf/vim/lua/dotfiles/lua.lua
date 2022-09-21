local M = {}

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")
for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
	table.insert(runtime_path, path .. "/lua/?.lua")
	table.insert(runtime_path, path .. "/lua/?/init.lua")
end

function M.includeexpr(fname)
	if vim.fn.filereadable(vim.fn.fnamemodify(fname, ":p")) == 1 then
		return fname
	end

	local line = vim.api.nvim_get_current_line()
	local start, finish = vim.regex(vim.opt.include:get()):match_str(line)
	fname = line:sub(start + 1, finish)
	fname = fname:gsub("%.", "/")
	for _, path in pairs(runtime_path) do
		local expanded = vim.fn.substitute(path, "?", fname, "g")
		if vim.fn.filereadable(expanded) == 1 then
			return expanded
		end
	end
	return fname
end

return M
