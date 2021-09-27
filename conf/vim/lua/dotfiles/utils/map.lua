-- luacheck: globals vim
-- Works like this:
-- local map = require("dotfiles.utils.map")
-- map.inoremap("<buffer><silent>", "<Up>", "<C-o>gk")
-- map.iunmap("<buffer>", "<Up>")
-- map.map("j", "gj")
-- map.unmap("j")

local map = {}
map.__function_store = {}
map.__create = function(f)
	table.insert(map.__function_store, f)
	return #map.__function_store
end
_G._dotfiles_map_exec = function(id)
	map.__function_store[id]()
end
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
		assert(
			type(map_call) == "string" and string.find(map_call, "map") ~= nil,
			"Map method call is invalid, either not a map or not a string."
		)
		return function(...)
			local arg = { ... }
			local map_mode
			assert(#arg >= 2, "Not enough arguments provided to map function.")
			-- Match :map and :noremap
			if map_call == "map" or map_call == "noremap" or map_call == "unmap" then
				map_mode = ""
				-- Match :map! and :noremap!
			elseif map_call:match("^.*()!") then
				map_mode = "!"
				-- Otherwise, map against __allowed_maps (defined above):
			else
				map_mode = string.sub(map_call, 1, 1)
				assert(vim.tbl_contains(map.__allowed_maps, map_mode), "Illegal map type, " .. map_mode)
			end
			local noremap = string.find(map_call, "noremap") ~= nil
			local unmap = string.find(map_call, "unmap") ~= nil
			local silent = #arg == 3 and string.find(string.lower(arg[1]), "<silent>") ~= nil
			local buffer = #arg == 3 and string.find(string.lower(arg[1]), "<buffer>") ~= nil
			local expr = #arg == 3 and string.find(string.lower(arg[1]), "<expr>") ~= nil
			local nowait = #arg == 3 and string.find(string.lower(arg[1]), "<nowait>") ~= nil
			local unique = #arg == 3 and string.find(string.lower(arg[1]), "<unique>") ~= nil
			local script = #arg == 3 and string.find(string.lower(arg[1]), "<script>") ~= nil
			local lhs = #arg == 3 and arg[2] or arg[1]
			local rhs = #arg == 3 and arg[3] or arg[2]
			local mapping
			if type(rhs) == "function" then
				local func_id = map.__create(rhs)
				mapping = "<cmd>lua _dotfiles_map_exec(" .. func_id .. ")<CR>"
			else
				mapping = rhs
			end
			if buffer then
				if unmap then
					vim.api.nvim_buf_del_keymap(vim.fn.bufnr("."), map_mode, lhs)
				else
					vim.api.nvim_buf_set_keymap(
						vim.fn.bufnr("."),
						map_mode,
						lhs,
						mapping,
						{
							silent = silent,
							noremap = noremap,
							expr = expr,
							script = script,
							nowait = nowait,
							unique = unique,
						}
					)
				end
			else
				if unmap then
					vim.api.nvim_del_keymap(map_mode, lhs)
				else
					vim.api.nvim_set_keymap(
						map_mode,
						lhs,
						mapping,
						{
							silent = silent,
							noremap = noremap,
							expr = expr,
							script = script,
							nowait = nowait,
							unique = unique,
						}
					)
				end
			end
		end
	end,
})
return map
