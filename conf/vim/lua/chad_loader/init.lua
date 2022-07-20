local chad_loader = {}

chad_loader.lazy_load = function(tb)
	vim.api.nvim_create_autocmd(tb.events, {
		pattern = "*",
		group = vim.api.nvim_create_augroup(tb.augroup_name, {}),
		callback = function()
			if tb.condition() then
				vim.api.nvim_del_augroup_by_name(tb.augroup_name)

				-- dont defer for treesitter as it will show slow highlighting
				-- This deferring only happens only when we do "nvim filename"
				if tb.plugins ~= "nvim-treesitter" then
					vim.defer_fn(function()
						vim.cmd("PackerLoad " .. tb.plugins)
					end, 0)
				else
					vim.cmd("PackerLoad " .. tb.plugins)
				end
			end
		end,
	})
end

chad_loader.colorizer = function()
	chad_loader.lazy_load({
		events = { "BufRead", "BufNewFile" },
		augroup_name = "ColorizerLazy",
		plugins = "nvim-colorizer.lua",

		condition = function()
			return true
		end,
	})
end

-- load certain plugins only when there's a file opened in the buffer
-- if "nvim filename" is executed -> load the plugin after nvim gui loads
-- This gives an instant preview of nvim with the file opened

chad_loader.on_file_open = function(plugin_name)
	chad_loader.lazy_load({
		events = { "BufRead", "BufWinEnter", "BufNewFile" },
		augroup_name = "BeLazyOnFileOpen" .. plugin_name,
		plugins = plugin_name,
		condition = function()
			local file = vim.fn.expand("%")
			return file ~= "NvimTree_1" and file ~= "[packer]" and file ~= ""
		end,
	})
end

return chad_loader
