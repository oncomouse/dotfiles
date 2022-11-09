local M = {}

local function get_last_word(words)
	local parts = vim.split(words, " ")
	return parts[#parts]
end

local function clean_key(word)
	return word:gsub("[^0-9A-Za-z]", "")
end

function M.make_key(entry)
	local author = entry.author
	if author == nil then
		author = "noauthor"
	elseif author:match(",") then
		author = string.sub(author, 1, string.find(author, ","))
	else
		author = get_last_word(author)
	end
	author = clean_key(author:lower())
	local title = entry.title
	if title == nil then
		title = "notitle"
	else
		title = string.sub(title, 1, string.find(title, " "))
	end
	title = clean_key(title:lower())
	local year = entry.year
	if year == nil then
		year = "nodate"
	end
	year = clean_key(year)
	return string.format("%s_%s_%s", author, title, year)
end

return M
