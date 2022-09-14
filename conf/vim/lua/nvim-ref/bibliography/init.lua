local bibtex = require("nvim-ref.utils.bibtex")
local M = {}

-- Collect the various sources of sources:
local function gather_bibliographies()
	local bibfiles = {}
	bibfiles = require("nvim-ref.utils.table").append(
		bibfiles,
		require("nvim-ref").config.bibfiles or {},
		vim.b.nvim_ref_bibliographies or {}
	)
	return vim.tbl_filter(function(x)
		return vim.fn.filereadable(vim.fn.fnamemodify(x, ":p")) == 1
	end, bibfiles)
end

-- Wrapper for the bibliography parser:
function M.query(target)
	local bibliographies = gather_bibliographies()
	return #bibliographies == 0 and {} or bibtex.parser.query_bibtex(bibliographies, target)
end

function M.update(citation)
	bibtex.writer.update(citation)
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
	end,
})
return M
