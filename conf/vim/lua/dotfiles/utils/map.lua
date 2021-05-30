local map = {}
map.__allowed_maps = {
	"i",
	"n",
	"x",
	"v",
}
setmetatable(map, {
	__index = function(_, map_call)
		return function(...)
			local arg = {...}
			if type(map_call) ~= "string" then
				error("Map method is not a string")
				return
			end
			local type
			if map_call == "map" or map_call == "noremap" then
				type = "n"
			else
				type = string.sub(map_call, 1, 1)
				if not vim.tbl_contains(map.__allowed_maps, type) then
					error("Illegal map type, " .. type)
					return
				end
			end
			local noremap = string.find(map_call, "noremap") ~= nil
			local silent = #arg == 3 and string.find(string.lower(arg[1]), "<silent>") ~= nil
			local buffer = #arg == 3 and string.find(string.lower(arg[1]), "<buffer>") ~= nil
			local keys = silent and arg[2] or arg[1]
			local to = silent and arg[3] or arg[2]
			if buffer then
				vim.api.nvim_buf_set_keymap(vim.fn.bufnr("."), type, keys, to, { silent = silent, noremap = noremap })
			else
				vim.api.nvim_set_keymap(type, keys, to, { silent = silent, noremap = noremap })
			end
		end
	end
})
return map
