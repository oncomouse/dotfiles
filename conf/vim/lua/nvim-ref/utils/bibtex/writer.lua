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
			table.insert(values, string.format("    %s = {%s}", entry.key, entry.value))
		end
		output = output .. vim.fn.join(values, ",\n") .. "\n}"
	elseif citation.type == "comment" then
	elseif citation.type == "preamble" then
	elseif citation.type == "string" then
	end
	return output
end

local function binary_search_insert(citations, citation)
	local start = 1
	local stop = #citations
	while true do
		local pick = math.ceil((stop - start) / 2)
		local check = citations[pick]
		if check.key < citation.key then
			stop = pick - 1
			if stop == 0 then
				return 1
			end
			if citations[stop] > citation.key then
				return stop
			end
		elseif check.key > citation.key then
			start = pick + 1
			if start > #citations then
				return #citations
			end
			if citations[start] < citation.key then
				return start
			end
		end
	end
end

-- @param citation ProcessedCitation
-- @param add boolean
-- return raw parsed BibTex
local function mutate(citation, add)
	local file = citation.file
	local citations = parser.read_bibfile(file)
	if add then
		table.insert(citations, binary_search_insert(citations, citation), citation)
	else
		for i, c in ipairs(citations) do
			if c.key and c.key == citation.key then
				citations[i] = deprocess(citation)
			end
		end
	end
	write(file, citations)
end

function M.update(citation)
	mutate(citation, false)
end

function M.add(citation)
	mutate(citation, true)
end

return M
