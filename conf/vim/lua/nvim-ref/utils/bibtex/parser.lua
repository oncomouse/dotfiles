local parser = require("lpeg-bibtex")

local M = {}
local entry_metatable = {
	__index = function(entry, idx)
		if entry.contents then
			for _,content in pairs(entry.contents) do
				if content.key == idx then
					return content.value
				end
			end
		end
		return nil
	end,
}
function parse_bibtex(data)
	local matches = {}
	for _, entry in pairs(data) do
		table.insert(matches, setmetatable(entry, entry_metatable))
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
function M.parse_bibtex_string(bibtex)
	return parse_bibtex(parser:match(bibtex))
end
function M.read_bibfile(bibfile)
	local fp = io.open(bibfile, "rb")
	if fp ~= nil then
		local contents = M.parse_bibtex_string(fp:read("*a"))
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
	for _, bibfile in pairs(bibfiles) do
		local contents = M.read_bibfile(bibfile)
		if contents ~= nil then
			for _, entry in pairs(contents) do
				if entry.type == "entry" and ("@" .. entry.key):match("^" .. key) then
					entry.file = bibfile
					table.insert(results, entry)
				end
			end
		else
			require("nvim-ref.utils.notifications").info("Unable to open bibliography file, " .. bibfile .. ".")
		end
	end
	return results
end

return M
