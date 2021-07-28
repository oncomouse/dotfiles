local parse_bibfiles = require("bibliography_omnifunc.bibtex").parse_bibfiles
local parse_bibtex = require("bibliography_omnifunc.bibtex").parse_bibtex
local function omnifunc(findstart, base)
    if findstart == 1 then
        local line_string = vim.fn.getline('.')
        local col = vim.fn.col('.')
        --- locate start of the key
        local start = col - 1
        while start > 0 and line_string:sub(start, start) ~= '@' do
			-- Not typing a key:
			if string.match(line_string:sub(start, start), '[A-Za-z_0-9.-:_/]') == nil then
				start = -3
				break
			end
            start = start - 1
        end
		-- Did not find a key in this line:
		if start <= 0 then
			start = -3
		end
        return start
    end
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
