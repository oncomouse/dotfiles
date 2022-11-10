local parser = require("nvim-ref.utils.bibtex.parser")
local M = {}

local function deprocess(citation)
	if citation.contents then
		return citation
	end
	local output = {
		key = citation.key,
		kind = citation.kind,
		type = "entry",
		contents = {},
	}
	for key, value in pairs(citation) do
		if not vim.tbl_contains({ "key", "file", "kind" }, key) then
			table.insert(output.contents, {
				key = key,
				value = value,
			})
		end
	end
	return output
end

local function write(file, citations)
	local output = vim.tbl_map(M.convert, citations)
	local fp = io.open(vim.fn.fnamemodify(file, ":p"), "w")
	fp:write(vim.fn.join(output, "\n\n"))
	fp:close()
end

-- input is raw parsed BibTeX
-- output is a string of BibTeX
function M.convert(citation)
	assert(
		type(citation) == "table" and citation.key ~= nil,
		string.format("Citation, %s, is not a valid citation object", vim.inspect(citation))
	)
	if citation.contents == nil then
		citation = deprocess(citation)
	end
	local output = ""
	if citation.type == "entry" then
		output = string.format("@%s{%s,\n", citation.kind, citation.key)
		local values = {}
		for _, entry in ipairs(citation.contents) do
			table.insert(values, string.format("    %s = %s", entry.key, entry.value))
		end
		output = output .. vim.fn.join(values, ",\n") .. "\n}"
	elseif citation.type == "comment" then
	elseif citation.type == "preamble" then
	elseif citation.type == "string" then
	end
	return output
end

-- @param citation ProcessedCitation
-- @param add boolean
-- return raw parsed BibTex
local function mutate(citation, add)
	local file = citation.file
	local citations = parser.read_bibfile(file)
	local inserted = false
	for i, c in ipairs(citations) do
		if add then
			if not inserted and c.key > citation.key then
				table.insert(citations, i, citation)
				inserted = true
			end
		else
			if c.key and c.key == citation.key then
				citations[i] = citation
			end
		end
	end
	if add and not inserted then
		table.insert(citations, citation)
	end
	write(file, citations)
end

function M.update(citation)
	mutate(citation, false)
	require("nvim-ref.utils.notifications").info(string.format("Citation, %s, has been updated in %s", citation.key, citation.file))
end

function M.add(citation)
	mutate(citation, true)
	require("nvim-ref.utils.notifications").info(string.format("Citation, %s, has been added to %s", citation.key, citation.file))
end

return M
