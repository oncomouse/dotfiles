local ruled = require("ruled")

local function should_float(c)
	for _, rule in pairs(ruled.client.matching_rules(c)) do
		if rule.id and rule.id == "floating" or rule.id == "floating-utilities" then
			return true
		end
	end
	return false
end

return should_float
