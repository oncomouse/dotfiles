local hooks = require("nvim-ref.hooks")
local plenary_available, curl = pcall(require, "plenary.curl")
local parser = require("nvim-ref.utils.bibtex.parser")
local M = {}

M.helpers = {}
M.helpers.verification = {}

function M.helpers.verification.regex(regex)
	regex = vim.regex(regex)
	return function(check)
		return regex:match_str(check)
	end
end

function M.helpers.verification.str_match(str)
	return function(check)
		return check:match(str)
	end
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
			curl.get(string.format("https://www.ebook.de/de/tools/isbn2bibtex?isbn=%s", isbn), {
				callback = vim.schedule_wrap(function(results)
					cb(parser.parse_bibtex_string(results.body)[1])
				end),
			})
		end,
	},
	{
		name = "DOI",
		verification = M.helpers.verification.regex([[\(doi\(:\)\{0,1\}\)\{0,1\}10\.[0-9]\{2,\}\(?:\.[0-9]\+\)*\/\S\+]]),
		get = function(doi, cb)
			curl.get(string.format("https://doi.org/%s", doi), {
				accept = "application/x-bibtex",
				callback = vim.schedule_wrap(function(results)
					cb(parser.parse_bibtex_string(results.body)[1])
				end),
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

	print(vim.inspect(bibtex))
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
			local id = string.lower(format.name)
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
