local hooks = require("nvim-ref.hooks")
local M = {}
M.filetypes = {}

-- Filetype:
-- {
-- module: string to require
-- type: string for filetype pattern
-- }

function M.require(ft)
	if not M.filetypes[ft] then
		error(string.format("Unknown filetype, %s!", ft))
	else
		if M.filetypes[ft].module then
			if type(M.filetypes[ft].module) == "string" then
				return require(M.filetypes[ft].module)
			else
				return M.filetypes[ft].module
			end
		else
			return require("nvim-ref.filetypes." .. ft)
		end
	end
end

local function add_filetype(filetype)
	M.filetypes[filetype.type] = filetype
	-- TODO: Add filetype autocommands
	vim.api.nvim_create_autocmd("FileType", {
		pattern = filetype.type,
		group = require("nvim-ref.augroup"),
		callback = function(args)
			local module = M.require(args.match)
			if module.find_bibliography and type(module.find_bibliography) == "function" then
				print(vim.inspect(module.find_bibliography(args.buf)))
			end
		end,
	})
end
hooks.add_hook("add_filetype", function(args)
	if args.type then
		add_filetype(args)
	else
		for _, type in pairs(args) do
			add_filetype(type)
		end
	end
end)

return M
