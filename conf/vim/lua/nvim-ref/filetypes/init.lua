local hooks = require("nvim-ref.hooks")
local M = {}
M.filetypes = {}

-- Filetype:
-- {
-- module: string to require
-- type: string for filetype pattern
-- }

hooks.define("add_filetype")
hooks.define("filetype")

function M.require(ft)
	ft = ft or 0
	if type(ft) == "number"  then
		ft = vim.api.nvim_buf_get_option(ft, "filetype")
	end
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

function M.find_start(pattern)
	local curline = vim.fn.line(".")
	local line, col = unpack(vim.fn.searchpos(pattern, "bcn"))
	if line == curline then
		return col
	end
	return nil
end


local function scan_bibliography(buf)
	buf = buf or 0
	local module = M.require(vim.api.nvim_buf_get_option(buf, "filetype"))
	-- Attach file bibliographies:
	if module.find_bibliography and type(module.find_bibliography) == "function" then
		vim.b.nvim_ref_bibliographies = module.find_bibliography(buf)
	end
end

local function add_filetype(filetype)
	if type(filetype.type) == "string" then
		M.filetypes[filetype.type] = filetype
	else
		for _,type in pairs(filetype.type) do
			local ft = require("nvim-ref.utils.table").deep_copy(filetype)
			ft.type = type
			M.filetypes[type] = ft
		end
	end
	vim.api.nvim_create_autocmd("FileType", {
		pattern = type(filetype.type) == "table" and vim.fn.join(filetype.type, ",") or filetype.type,
		group = require("nvim-ref.augroup"),
		callback = function(args)
			hooks.trigger("filetype", args)
			-- Check for bibliographies:
			scan_bibliography(args.buf)
			vim.b.nvim_ref_loaded = true
			-- Check for changes to bibliographies after leaving insert:
			vim.api.nvim_create_autocmd("InsertLeave", {
				buffer = args.buf,
				group = require("nvim-ref.augroup"),
				callback = function()
					scan_bibliography(args.buf)
				end,
			})
		end,
	})
end
hooks.listen("add_filetype", function(args)
	if args.type then
		add_filetype(args)
	else
		for _, type in pairs(args) do
			add_filetype(type)
		end
	end
end)

return M
