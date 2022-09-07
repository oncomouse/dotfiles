local function callback(citation)
	local cite = require("nvim-ref.format").get_ref(citation)
	return require("nvim-ref.insert").insert(cite)
end
local function ref(citation)
	if citation.key == nil then
		citation = require("nvim-ref.select").citation(callback)
	else
		callback(citation)
	end
end

return ref
