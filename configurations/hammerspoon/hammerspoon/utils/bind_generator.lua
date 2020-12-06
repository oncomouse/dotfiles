local function bind_generator(mod_table, processing_func)
	return function(key_table)
		for mod_key, keys in pairs(key_table) do
			local mod = (mod_key == "_" and "" or mod_table[mod_key])
			for key, func in pairs(keys) do
				processing_func(mod, key, func)
			end
		end
	end
end

return bind_generator
