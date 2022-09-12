local parser = require("lpeg-bibtex")

local M = {}
function parse_bibtex(data)
	local matches = {}
	for _,entry in pairs(data) do
		local entry_content = {
			key = entry.key,
			kind = entry.kind,
			file = entry.file,
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
local function parse_bibfiles(bibfiles)
	if type(bibfiles) == "table" then
		return vim.tbl_map(escape_bibfile, bibfiles)
	end
	return escape_bibfile(bibfiles)
end
function M.read_bibfile(bibfile)
	local fp = io.open(bibfile, "rb")
	if fp ~= nil then
		local contents = parser:match(fp:read("*a"))
		fp:close()
		return contents
	end
	return nil
end
function M.query_bibtex(bibfiles, key)
	if not string.match(key, "^@") then
		key = "@" .. key
	end
	bibfiles = parse_bibfiles(type(bibfiles) == "string" and { bibfiles } or bibfiles)
	local results = {}
	for _,bibfile in pairs(bibfiles) do
		local contents = M.read_bibfile(bibfile)
		if contents ~= nil then
			for _,entry in pairs(contents) do
				if entry.type == "entry" and ("@" .. entry.key):match("^" .. key) then
					entry.file = bibfile
					table.insert(results, entry)
				end
			end
		else
			require("nvim-ref.utils.output").info("Unable to open bibliography file, " .. bibfile .. ".")
		end
	end
	return parse_bibtex(results)
end

return M
