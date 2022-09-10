local parser = require("lpeg-bibtex")

local M = {}
function M.parse_bibtex(data)
	local matches = {}
	for _,entry in pairs(data) do
		local entry_content = {
			key = entry.key,
			kind = entry.kind,
		}
		for _,item in pairs(entry.contents or {}) do
			entry_content[item.key] = item.value:gsub("[,}{]", "")
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
	if not string.match(key, "^@") then
		key = "@" .. key
	end
	bibfiles = M.parse_bibfiles(type(bibfiles) == "string" and { bibfiles } or bibfiles)
	local results = {}
	for _,bibfile in pairs(bibfiles) do
		local fp = io.open(bibfile, "rb")
		if fp ~= nil then
			local contents = parser:match(fp:read("*a"))
			fp:close()
			for _,entry in pairs(contents) do
				if entry.type == "entry" and ("@" .. entry.key):match("^" .. key) then
					table.insert(results, entry)
				end
			end
		else
			require("nvim-ref.utils.output").info("Unable to open bibliography file, " .. bibfile .. ".")
		end
	end
	return M.parse_bibtex(results)
end

return M
