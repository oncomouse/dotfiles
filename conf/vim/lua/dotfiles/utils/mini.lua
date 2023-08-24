local callbacks = {}
local augroup = vim.api.nvim_create_augroup("dotfiles-mini-config", {})
local M = {}
local function mini_autocmd(cmd, opts)
	if type(opts) == "number" then
		opts = { buffer = opts }
	end
	if opts.filetype then
		if not callbacks.filetype then
			callbacks.filetype = {}
		end
		opts.filetype = type(opts.filetype) == "string" and { opts.filetype } or opts.filetype

		for _, ft in pairs(opts.filetype) do
			if not callbacks.filetype[ft] then
				vim.api.nvim_create_autocmd("FileType", {
					group = augroup,
					pattern = ft,
					callback = function(ev)
						for _, func in pairs(callbacks.filetype[ev.match]) do
							if type(func) == "string" then
								vim.cmd(func)
							else
								func(ev)
							end
						end
					end,
				})
				callbacks.filetype[ft] = {}
			end
			table.insert(callbacks.filetype[ft], cmd)
		end
	end
	if opts.terminal then
		if not callbacks.terminal then
			vim.api.nvim_create_autocmd("TermOpen", {
				group = augroup,
				pattern = "*",
				callback = function(ev)
					for _, func in pairs(callbacks.terminal) do
						if type(func) == "string" then
							vim.cmd(func)
						else
							func(ev)
						end
					end
				end,
			})
			callbacks.terminal = {}
		end
		table.insert(callbacks.terminal, cmd)
	end
	if opts.buftype then
		if not callbacks.buftype then
			callbacks.buftype = {}
			vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPost" }, {
				group = augroup,
				callback = function(ev)
					if vim.tbl_contains(vim.tbl_keys(callbacks.buftype), vim.bo.buftype) then
						for _, func in pairs(callbacks.buftype[vim.bo.buftype]) do
							if type(func) == "string" then
								vim.cmd(func)
							else
								func(ev)
							end
						end
					end
				end,
			})
		end
		opts.buftype = type(opts.buftype) == "string" and { opts.buftype } or opts.buftype

		for _, bt in pairs(opts.buftype) do
			if not callbacks.buftype[bt] then
				callbacks.buftype[bt] = {}
			end
			table.insert(callbacks.buftype[bt], cmd)
		end
	end
end
function M.disable_mini_module(module, opts)
	if opts == nil then
		vim.g[string.format("mini%s_disable", module)] = true
		return
	end
	local var_name = string.format("b:mini%s_disable", module)
	local buf_disable = string.format("let %s = v:true", var_name)
	mini_autocmd(buf_disable, opts)
end
function M.configure_mini_module(module, config, opts)
	if type(config) == "function" then
		config = config()
	end
	if opts == nil then
		require("mini." .. module).setup(config or {})
	end
	local callback = function()
		vim.b["mini" .. module .. "_config"] = config
	end
	mini_autocmd(callback, opts)
end

return M
