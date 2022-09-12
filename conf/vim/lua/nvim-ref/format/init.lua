local M = {}

function M.get_ref(citation)
	return require("nvim-ref.filetypes").ref(citation)
end

function M.get_citation(citation)
	return require("nvim-ref.filetypes").citation(citation)
end

return M
