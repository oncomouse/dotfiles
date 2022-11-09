local parser = require("lpeg-bibtex")

local M = {}

-- Use a metatable to provide more object-y access to entry contents:
-- e.g. entry.author instead of finding the item in entry.contents that has
-- a "key" equal to "author" and returning that item's "value"
local entry_metatable = {
	__index = function(entry, idx)
		-- Generate a cache table:
		if idx == "__idx_cache" then
			return {}
		end

		-- If the entry is in cache, return it:
		if entry.__idx_cache[idx] == false then
			return nil
		end
		if entry.__idx_cache[idx] ~= nil then
			return entry.__idx_cache[idx]
		end

		-- Search for the item:
		if entry.contents then
			for _, content in pairs(entry.contents) do
				-- Item is found, cache it and return it:
				if content.key == idx then
					entry.__idx_cache[idx] = content.value
					return content.value
				end
			end
		end

		-- Nothing found, cache and return that:
		entry.__idx_cache[idx] = false
		return nil
	end,
}

local function escape_bibfile(file)
	return vim.fn.fnamemodify(file, ":p")
end

function M.parse_bibtex_string(bibtex)
	local data = parser:match(bibtex)
	local matches = {}
	for _, entry in pairs(data) do
		table.insert(matches, setmetatable(entry, entry_metatable))
	end
	return matches
end

function M.read_bibfile(bibfile)
	local fp = io.open(escape_bibfile(bibfile), "rb")
	if fp ~= nil then
		local contents = M.parse_bibtex_string(fp:read("*a"))
		fp:close()
		return contents
	else
		require("nvim-ref.utils.notifications").warn("Unable to open bibliography file, " .. bibfile .. ".")
	end
	return nil
end

function M.query_bibtex(bibfiles, key)
	if not string.match(key, "^@") then
		key = "@" .. key
	end
	local results = {}
	for _, bibfile in pairs(type(bibfiles) == "string" and { bibfiles } or bibfiles) do
		local contents = M.read_bibfile(bibfile)
		if contents ~= nil then
			for _, entry in pairs(contents) do
				if entry.type == "entry" and ("@" .. entry.key):match("^" .. key) then
					entry.file = bibfile
					table.insert(results, entry)
				end
			end
		end
	end
	return results
end

return M
