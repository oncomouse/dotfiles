local M = {}

function M.get_citation(citation_or_key)
	if citation_or_key.key then
		return citation_or_key
	elseif type(citation_or_key) == "string" then
		local citations = require("nvim-ref.utils.bibtex").query_bibtex(require("nvim-ref.config").bibfiles, citation_or_key)
		assert(#citations == 1, string.format("Inexact result returned by searching for key, %s. Got %d results instead of 1.", citation_or_key, #citations))
		return citations[1]
	end
	error("Unknown citation: " .. vim.inspect(citation_or_key))
	return nil
end

return M
