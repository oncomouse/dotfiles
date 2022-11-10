local hooks = require("nvim-ref.hooks")
local plenary_available, curl = pcall(require, "plenary.curl")
local parser = require("nvim-ref.utils.bibtex.parser")
local M = {}

M.helpers = {}
M.helpers.verification = {}

---@param regex string
---@return fun(check:string):boolean
function M.helpers.verification.regex(regex)
	regex = vim.regex(regex)
	return function(check)
		return regex:match_str(check)
	end
end

---@param str string
---@return fun(check:string):boolean
function M.helpers.verification.str_match(str)
	return function(check)
		return check:match(str)
	end
end

M.helpers.get = {}

---@param url string
---@param callback fun(results:string)
---@param opts table|nil
function M.helpers.get.url(url, callback, opts)
	local curl_opts = vim.tbl_extend("keep", {
		callback = vim.schedule_wrap(callback),
	}, opts or {})
	curl.get(url, curl_opts)
end

local import_formats = {}
local default_import_formats = {
	{
		name = "ISBN",
		verification = M.helpers.verification.regex(
			[[\(ISBN[-]*\(1[03]\)*\s*\(: \)\{0,1\}\)\{0,1\}\(978\|979\)\{0,1\}\([0-9Xx]\{10\}\)]]
		),
		get = function(isbn, cb)
			isbn = string.gsub(isbn, "[^0-9xX]", "")
			M.helpers.get.url(
				string.format("https://www.ebook.de/de/tools/isbn2bibtex?isbn=%s", isbn),
				function(results)
					vim.pretty_print(results)
					cb(parser.parse_bibtex_string(results.body)[1])
				end
			)
		end,
	},
	{
		name = "DOI",
		verification = M.helpers.verification.regex([[\(doi\(:\)\{0,1\}\)\{0,1\}10\.[0-9]\{2,\}\(?:\.[0-9]\+\)*\/\S\+]]),
		get = function(doi, cb)
			M.helpers.get.url(string.format("https://doi.org/%s", doi), function(results)
				cb(parser.parse_bibtex_string(results.body)[1])
			end, {
				accept = "application/x-bibtex",
			})
		end,
	},
}

local function add_bibtex_to_bib(bibtex)
	-- Lowercase all content entries:
	for i, item in ipairs(bibtex.contents) do
		local key = string.lower(item.key)
		if item.key ~= key then
			item.key = key
			bibtex.contents[i] = item
		end
	end

	-- Which bibliography to insert into:
	local bibfiles = require("nvim-ref").config.bibfiles
	local bibfile
	if #bibfiles == 1 then
		bibfile = bibfiles[1]
	else
		vim.ui.select(bibfiles, {
			prompt = "Which bibliography to add entry to? ",
		}, function(choice)
			bibfile = choice
		end)
	end
	bibtex.file = bibfile

	-- Generate key:
	bibtex.key = require("nvim-ref.utils.bibtex.helpers").make_key(bibtex)

	vim.ui.input({
		prompt = "Would you like to edit the BibTeX before it is inserted? [Y/n]: ",
	}, function(input)
		if input:match("^[Nn]") then
			require("nvim-ref.utils.bibtex.writer").add(bibtex)
		end
		require("nvim-ref.commands.edit").edit(bibtex, function(updated_bibtex)
			local citation = parser.parse_bibtex_string(updated_bibtex)[1]
			citation.file = bibfile
			require("nvim-ref.utils.bibtex.writer").add(citation)
		end)
	end)
end

function M.setup()
	hooks.define("add_import_format")
	hooks.listen("add_import_format", function(format)
		if format.name then
			for _, import in pairs(import_formats) do
				if import.name == format.name then
					require("nvim-ref.utils.notifications").warning(
						string.format("Attempting to add an import format, %s, which already exists!", format.name)
					)
					return
				end
			end
			table.insert(import_formats, format)
			local id = format.name:lower():gsub("%s+", "-"):gsub("[^0-9a-z-]", "")
			hooks.trigger("add_command", {
				id = string.format("import.%s", id),
				name = "Import Citation from " .. format.name,
				callback = function(args)
					if not plenary_available then
						require("nvim-ref.utils.notifications").warning(
							string.format("plenary.nvim is required to use nvim-ref.import.%s", id)
						)
						return
					end
					if #args > 0 then
						if not format.verification(args[1]) then
							require("nvim-ref.utils.notifications").error(
								string.format("Invalid %s entered", format.name)
							)
							return
						end
						format.get(args[1], add_bibtex_to_bib)
					else
						vim.ui.input({ prompt = string.format("Enter a valid %s: ", format.name) }, function(input)
							if not format.verification(input) then
								require("nvim-ref.utils.notifications").error(
									string.format("Invalid %s entered", format.name)
								)
								return
							end
							format.get(input, add_bibtex_to_bib)
						end)
					end
				end,
			})
		end
	end)
	hooks.trigger("add_command", {
		id = "import",
		name = "Import a citation",
	})
	for _, format in pairs(default_import_formats) do
		hooks.trigger("add_import_format", format)
	end
end

return M
