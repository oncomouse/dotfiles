-- luacheck: globals vim
local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local COMPLETION = methods.internal.COMPLETION

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
			label = label,
			detail = (entry_contents.title or ""),
			documentation = {
				kind = vim.lsp.protocol.MarkupKind.Markdown,
				value = "*Author*: "
					.. (entry_contents.author or "")
					.. "\n"
					.. "*Title*: "
					.. (entry_contents.title or "")
					.. "\n"
					.. "*Year*: "
					.. (entry_contents.date or ""),
			},
			kind = vim.lsp.protocol.CompletionItemKind["Text"],
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

return h.make_builtin({
	method = COMPLETION,
	filetypes = { "markdown", "latex" },
	name = "bibtex",
	generator = {
		fn = function(params, done)
			local line = vim.api.nvim_get_current_line()
			local line_to_cursor = line:sub(1, params.col)
			local regex = vim.regex("@\\k\\+$")
			local match = regex:match_str(line_to_cursor)
			if match == nil then
				done({ { items = {}, isIncomplete = false } })
				return
			end
			local key = line:sub(match, params.col)
			local handle = io.popen(
				"bash -c 'bibtool -r biblatex -X \"^"
					.. string.gsub(key, "@", "")
					.. '" '
					.. parse_bibfiles(vim.g.bibfiles)
					.. ' -- "keep.field {title}" -- "keep.field {author}" -- "keep.field {date}" -- "print.line.length {400}"\''
			)
			local results = handle:read("*a")
			handle:close()
			local items = parse_bibtex(results)
			done({ { items = items, isIncomplete = #items } })
		end,
		async = true,
	},
})
