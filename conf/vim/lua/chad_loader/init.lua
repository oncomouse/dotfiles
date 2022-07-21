local chad_loader = {}

local do_not_defer = {
	"nvim-treesitter",
	"vim-dirvish",
}

chad_loader.lazy_load = function(tb)
	vim.api.nvim_create_autocmd(tb.events, {
		pattern = "*",
		group = vim.api.nvim_create_augroup(tb.augroup_name, {}),
		callback = function()
			if tb.condition() then
				vim.api.nvim_del_augroup_by_name(tb.augroup_name)

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

function is_dir(path)
	f = io.open(path)
	return not f:read(0) and f:seek("end") ~= 0
end

chad_loader.dirvish = function()
	chad_loader.lazy_load({
		events = { "BufEnter" },
		augroup_name = "DirvishLazy",
		plugins = "vim-dirvish",

		condition = function()
			return is_dir(vim.fn.expand("%:p"))
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
