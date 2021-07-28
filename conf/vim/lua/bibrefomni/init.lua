-- Portions of this function are based on code from: https://github.com/nvim-telescope/telescope-bibtex.nvim
local function parse_bibtex(data)
	local entries = {}
	local matches = {}
	local raw_entry = ""
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
		local author, title
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
			word = label,
			abbr = "@" .. label,
			info = "*Author*: "
				.. (entry_contents.author or "")
				.. "\n"
				.. "*Title*: "
				.. (entry_contents.title or "")
				.. "\n"
				.. "*Year*: "
				.. (entry_contents.date or ""),
		})
	end
	return {
		words = matches,
	}
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
local function omnifunc(findstart, base)
    if findstart == 1 then
        local line_string = vim.fn.getline('.')
        local col = vim.fn.col('.')
        --- locate start of the key
        local start = col - 2
        while start >= 0 and line_string:sub(start, start) ~= '@' do
			-- Not typing a key:
			if string.match(line_string:sub(start, start), '[A-Za-z_0-9.-:_/]') == nil then
				return -3
			end
            start = start - 1
        end
		-- Did not find a key in this line:
		if start < 0 then
			start = -3
		end
        return start
    end
	base = base or _A
	local handle = io.popen(
		"bash -c 'bibtool -r biblatex -X \"^"
			.. string.gsub(base, "@", "")
			.. '" '
			.. parse_bibfiles(vim.g.bibfiles)
			.. ' -- "keep.field {title}" -- "keep.field {author}" -- "keep.field {date}" -- "print.line.length {400}"\''
	)
	local results = handle:read("*a")
	handle:close()
	return parse_bibtex(results)
end

return {
	omnifunc = omnifunc,
}
