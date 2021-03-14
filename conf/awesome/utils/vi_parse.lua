-- luacheck: globals terminal
local shell = require("awful.util").shell
-- Store a list of verbs characters in a hash
local verbs = {
	t = function(_, _, cmd) -- Spawn in a terminal --luacheck: no unused args
		return { terminal, "-e", cmd }
	end,
	s = function(_, _, cmd) -- Spawn with a shell --luacheck: no unused args
		return { shell, "-c", cmd }
	end,
}
-- Quite dumb, don't do something like <num>+<adj>+<num>+<verb>
local vi_parse = function (action, command)
	local req, ret = {
		count = {},
		adjectives = {},
	}
	for char in action:gmatch("(.)") do
		if tonumber(char) then
			table.insert(req.count, char)
		elseif verbs[char] then
			req.verb = char
		else
			table.insert(ret.adjectives, char)
		end
		if req.verb then
			req.count = tonumber(table.concat(req.count)) or 1
			ret = ret or verbs[req.verb](req.adjectives, req.count, command)
			req = {
				count = {},
				adjectives = {},
			}
		end
	end
	return ret
end

return vi_parse
