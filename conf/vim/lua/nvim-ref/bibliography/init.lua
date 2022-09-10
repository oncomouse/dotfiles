local bibtex = require("nvim-ref.utils.bibtex")
local M = {}

-- Collect the various sources of sources:
local function gather_bibliographies()
	return vim.tbl_deep_extend("keep",
		require("nvim-ref").config.bibfiles or {},
		vim.b.nvim_ref_bibliographies or {}
	)
end

-- Wrapper for the bibliography parser:
function M.query(target)
	local bibliographies = gather_bibliographies()
	return #bibliographies == 0 and {} or bibtex.query_bibtex(bibliographies, target)
end

setmetatable(M, {
	__index = function(t, idx)
		if idx == "bibliographies" then
			return gather_bibliographies()
		end
		if t[idx] then
			return t[idx]
		end
		return nil
	end
})
return M
