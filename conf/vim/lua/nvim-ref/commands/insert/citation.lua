local function callback(citation)
	local cite = require("nvim-ref.format").get_citation(citation)
	return require("nvim-ref.insert").insert(cite)
end
local function cite(citation)
	if citation == nil then
		citation = require("nvim-ref.select").citation(callback)
	else
		callback(citation)
	end
end

return cite
