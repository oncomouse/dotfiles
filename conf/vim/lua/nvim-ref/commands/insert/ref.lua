local function ref(citation)
	local function callback(c)
		local cite = require("nvim-ref.format").get_ref(c)
		return require("nvim-ref.insert").insert(cite)
	end
	if citation.key == nil then
		citation = require("nvim-ref.select").citation(callback)
	else
		callback(citation)
	end
end

return ref
