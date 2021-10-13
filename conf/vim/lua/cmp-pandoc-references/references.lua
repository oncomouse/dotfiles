--luacheck: globals vim
local cmp = require("cmp")

-- Portions of this function are based on code from: https://github.com/nvim-telescope/telescope-bibtex.nvim
local function parse_bibtex(data)
	local entries = {}
	local matches = {}
	local raw_entry
	while true do
		raw_entry = data:match("@%w*%s*%b{}\n")
		if raw_entry == nil then
			break
		end
		table.insert(entries, raw_entry)
		data = data:sub(#raw_entry + 2)
	end
	for _, entry in pairs(entries) do
		local label = entry:match("{%s*[^{},~#%\\]+,")
		label = vim.trim(label:gsub("\n", ""):sub(2, -2))
		local content = vim.split(entry, "\n")
		local entry_contents = {}
		local author, title, date
		for _, line in pairs(content) do
			author = line:match("%s*Author%s*=%s*{(.*)$")
			if author then
				entry_contents.author = author:gsub("[,}{]", "")
			end
			title = line:match("%s*Title%s*=%s*{(.*)$")
			if title then
				entry_contents.title = title:gsub("[,}{]", "")
			end
			date = line:match("%s*Date%s*=%s*{(%d*)")
			if date then
				entry_contents.date = date
			end
		end
		table.insert(matches, {
			label = "@" .. label,
			kind = cmp.lsp.CompletionItemKind.Reference,
			documentation = {
				kind = cmp.lsp.MarkupKind.Markdown,
				value = "*Author*: "
				.. (entry_contents.author or "")
				.. "\n"
				.. "*Title*: "
				.. (entry_contents.title or "")
				.. "\n"
				.. "*Year*: "
				.. (entry_contents.date or ""),

			}
		})
	end
	return matches
end
local function escape_bibfile(file)
	return string.gsub(file, " ", "\\ ")
end
local function parse_bibfiles(bibfiles)
	if type(bibfiles) == "table" then
		return vim.tbl_map(escape_bibfile, bibfiles)
	end
	return escape_bibfile(bibfiles)
end

-- From https://github.com/jc-doyle/cmp-pandoc-references
local function locate_bib(lines)
	if vim.g.bibfiles then
		return vim.g.bibfiles
	end
	for _, line in ipairs(lines) do
		local location = string.match(line, "bibliography: (%g+)")
		if location then
			return location
		end
	end
end

local function parse_bib(base, lines)
	local location = locate_bib(lines)
	local handle = io.popen(
		"bash -c 'bibtool -r biblatex -X \"^"
			.. string.gsub(base, "@", "")
			.. '" '
			.. parse_bibfiles(location)
			.. ' -- "keep.field {title}" -- "keep.field {author}" -- "keep.field {date}" -- "print.line.length {400}"\''
	)
	local results = handle:read("*a")
	handle:close()
	return parse_bibtex(results)
end

return parse_bib
