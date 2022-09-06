local parser = require("lpeg-bibtex")

local M = {}
function M.parse_bibtex(data)
	local matches = {}
	for _,entry in pairs(data) do
		local entry_content = {
			key = entry.key,
		}
		for _,item in pairs(entry.contents or {}) do
			if item.key == "author" then
				entry_content.author = item.value:gsub("[,}{]", "")
			elseif item.key == "title" then
				entry_content.title = item.value:gsub("[,}{]", "")
			elseif item.key == "date" then
				entry_content.date = item.value
			end
		end
		table.insert(matches, entry_content)
	end
	return matches
end
local function escape_bibfile(file)
	return vim.fn.fnamemodify(file, ":p")
end
function M.parse_bibfiles(bibfiles)
	if type(bibfiles) == "table" then
		return vim.tbl_map(escape_bibfile, bibfiles)
	end
	return escape_bibfile(bibfiles)
end
function M.query_bibtex(bibfiles, key)
	bibfiles = M.parse_bibfiles(type(bibfiles) == "string" and { bibfiles } or bibfiles)
	local results = {}
	for _,bibfile in pairs(bibfiles) do
		local fp = assert(io.open(bibfile, "rb"))
		local contents = parser:match(fp:read("*a"))
		fp:close()
		for _,entry in pairs(contents) do
			if entry.type == "entry" and ("@" .. entry.key):match("^" .. key) then
				table.insert(results, entry)
			end
		end
	end
	return M.parse_bibtex(results)
end

return M
