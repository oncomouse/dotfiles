-- Works like this:
-- local map = require("dotfiles.utils.map")
-- map.inoremap("<buffer><silent>", "<Up>", "<C-o>gk")
-- map.iunmap("<buffer>", "<Up>")
-- map.map("j", "gj")
local map = {}
-- Types of maps we will match against (does not need to contain :map and :map!):
map.__allowed_maps = {
	"c",
	"i",
	"l",
	"n",
	"o",
	"s",
	"t",
	"v",
	"x",
}
setmetatable(map, {
	__index = function(_, map_call)
		return function(...)
			local arg = {...}
			if type(map_call) ~= "string" or string.find(map_call, "map") == nil then
				error("Map method call is invalid, either not a map or not a string.")
				return
			end
			local type
			-- Match :map and :noremap
			if map_call == "map" or map_call == "noremap" or map_call == "unmap" then
				type = ""
			-- Match :map! and :noremap!
			elseif map_call:match'^.*()!' then
				type = "!"
			else
				type = string.sub(map_call, 1, 1)
				if not vim.tbl_contains(map.__allowed_maps, type) then
					error("Illegal map type, " .. type)
					return
				end
			end
			local noremap = string.find(map_call, "noremap") ~= nil
			local unmap = string.find(map_call, "unmap") ~= nil
			local silent = #arg == 3 and string.find(string.lower(arg[1]), "<silent>") ~= nil
			local buffer = #arg == 3 and string.find(string.lower(arg[1]), "<buffer>") ~= nil
			local keys = silent and arg[2] or arg[1]
			local to = silent and arg[3] or arg[2]
			if buffer then
				if unmap then
					vim.api.nvim_buf_del_keymap(vim.fn.bufnr("."), type, keys)
				else
					vim.api.nvim_buf_set_keymap(vim.fn.bufnr("."), type, keys, to, { silent = silent, noremap = noremap })
				end
			else
				if unmap then
					vim.api.nvim_del_keymap(type, keys)
				else
					vim.api.nvim_set_keymap(type, keys, to, { silent = silent, noremap = noremap })
				end
			end
		end
	end
})
return map
