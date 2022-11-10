local hooks = require("nvim-ref.hooks")
local M = {}
M.filetypes = {}

hooks.define("add_filetype")
hooks.define("filetype")

---@class FileTypeLibrary
---@field ref fun(citation:string):string Given a citation key, return a reference to that key
---@field citation fun(citation:string):string Given a citation key, return a full citation of that key
---@field bibliographies fun(bufnum:number?):<string>[] Gather all bibliographies defined in document metadata
---@field start_pattern string Vim regular expression to locate the beginning of a citation in the document
---@field setup fun():nil Run this at setup (usually used to trigger the `add_filetype` hook).

---@class FileTypeDefinition
---@field type string A vim filetype for which to load NvimRef
---@field module string|FileTypeLibrary|nil A file to include or a module to use or, if nothing, we guess based on type.

---@param ft string? A filetype (will be set to the current buffer's filetype if nil)
---@return FileTypeLibrary
function M.require(ft)
	ft = ft or M.buf_filetype(0)
	if not M.filetypes[ft] then
		require("nvim-ref.utils.notifications").error(string.format("Unknown filetype, %s!", ft))
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

---@param buf number? A buffer number
---@return string The filetype of the buffer
function M.buf_filetype(buf)
	buf = buf or 0
	return vim.api.nvim_buf_get_option(buf, "filetype")
end

---@param pattern string? A vim regular expression used to find the start of a citation (will be loaded from a FiletypeLibrary if nil)
---@return number? The current column position if pattern is present on the current line; nil otherwise
function M.find_start(pattern)
	pattern = pattern or M.require().start_pattern
	local curline = vim.fn.line(".")
	local line, col = unpack(vim.fn.searchpos(pattern, "bcn"))
	if line == curline then
		return col
	end
	return nil
end

---@param buf number? A buffer number to scan for metadata-defined bibliographies and attach to a buffer variable
---@return nil
local function scan_bibliography(buf)
	buf = buf or 0
	local module = M.require(vim.api.nvim_buf_get_option(buf, "filetype"))
	-- Attach file bibliographies:
	if module.find_bibliography and type(module.find_bibliography) == "function" then
		vim.b.nvim_ref_bibliographies = module.find_bibliography(buf)
	end
end

---@param filetype FileTypeDefinition A new filetype to define
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

---@param args FileTypeDefinition The definition to attach
---@return nil
function filetype_listner(args)
	if args.type then
		add_filetype(args)
	else
		for _, type in pairs(args) do
			add_filetype(type)
		end
	end
end
hooks.listen("add_filetype", filetype_listner)

local filetype_object_keys = {
	"ref",
	"citation",
	"find_bibliography",
}

setmetatable(M, {
	__index = function(t, idx)
		if vim.tbl_contains(filetype_object_keys, idx) then
			return t.require(t.buf_filetype())[idx]
		end
		return t[idx]
	end
})

return M
