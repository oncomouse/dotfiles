local hooks = require("nvim-ref.hooks")
local M = {}

local function regex_verification(regex)
	regex = vim.regex(regex)
	return function(check)
		return regex:match_str(check)
	end
end

local import_formats = {
	{
		name = "ISBN",
		verification = [[\(ISBN[-]*\(1[03]\)*\s*\(: \)\{0,1\}\)\{0,1\}\(978\|979\)\{0,1\}\([0-9Xx]\{10\}\)]],
		import = nil,
	},
	{
		name = "DOI",
		verification = [[\(doi\(:\)\{0,1\}\)\{0,1\}10\.[0-9]\{2,\}\(?:\.[0-9]\+\)*\/\S\+]]
	}
}

function M.setup()
	hooks.trigger("add_command", {
		id = "import",
		name = "Import a citation",
		subcommands = vim.tbl_map(function(format)
			local verification = format.verification
			if type(format.verification) == "string" then
				verification = regex_verification(format.verification)
			end
			return {
				id = string.lower(format.name),
				name = "Import Citation from " .. format.name,
				callback = function(args)
					 vim.ui.input({ prompt = "Enter a valid " .. format.name .. ":" }, function(input)
						 if not verification(input) then
							 error("Invalid " .. format.name .. " entered")
							 return
						 end
						 -- Handle verification
					 end)
				end,
			}
		end, import_formats),
	})
end

return M
