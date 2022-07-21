local chad_loader = {}

local do_not_defer = {}

chad_loader.do_not_defer = function(plugin)
	table.insert(do_not_defer, plugin)
end

chad_loader.lazy_load = function(tb)
	local augroup = "ChadLazyLoader" .. tb.plugins
	vim.api.nvim_create_autocmd(tb.events, {
		pattern = "*",
		group = vim.api.nvim_create_augroup(augroup, {}),
		callback = function()
			if tb.condition() then
				vim.api.nvim_del_augroup_by_name(augroup)
				if vim.tbl_contains(do_not_defer, tb.plugins) then
					vim.cmd("PackerLoad " .. tb.plugins)
				else
					vim.defer_fn(function()
						vim.cmd("PackerLoad " .. tb.plugins)
					end, 0)
				end
			end
		end,
	})
end

-- load certain plugins only when there's a file opened in the buffer
-- if "nvim filename" is executed -> load the plugin after nvim gui loads
-- This gives an instant preview of nvim with the file opened
chad_loader.on_file_open = function(plugin_name)
	chad_loader.lazy_load({
		events = { "BufRead", "BufWinEnter", "BufNewFile" },
		plugins = plugin_name,
		condition = function()
			local file = vim.fn.expand("%")
			return file ~= "NvimTree_1" and file ~= "[packer]" and file ~= ""
		end,
	})
end

chad_loader.on_directory = function(plugin_name)
	function is_dir(path)
		local f = io.open(path)
		if f == nil then
			return false
		end
		local _, _, code = f:read(0)
		f:close()
		return code == 21
	end
	require("chad_loader").lazy_load({
		events = { "BufRead", "BufWinEnter", "BufNewFile" },
		plugins = plugin_name,
		condition = function()
			return is_dir(vim.fn.expand("%:p"))
		end,
	})
end

return chad_loader
