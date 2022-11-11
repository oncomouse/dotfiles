local parser = require("nvim-ref.utils.bibtex").parser
local writer = require("nvim-ref.utils.bibtex").writer

local augroup = vim.api.nvim_create_augroup("__nvim-ref-editor-augroup", {})

---@class Editor
---@field tempfile string The temporary file used to save edits
---@field opts table
---@field is_open boolean Is the editor window open
---@field buf number?
---@field win number?
---@field open fun() Open the window if it is not opened
---@field set_contents fun(contents:string|table) Set the window contents
---@field set_citation fun(citation:Citation) Set the window contents to a citation
---@field set_file fun(file:string) Set the window contents to the content of file
---@field on_close fun(cb:fun(new_contents:string|Citation)) Set a callback for when the editor is closed
---@field close fun() Close the window if it is open (does not save or call cb)
local Editor = {}
function Editor:new(opts)
	local o = {}
	setmetatable(o, self)
	self.__index = self
	self.tempfile = vim.fn.tempname()
	self.opts = opts
	self.is_open = false
	return o
end
function Editor:open()
	if self.is_open == true then
		return
	end
	-- Create the scratch buffer displayed in the floating window
	self.buf = vim.api.nvim_create_buf(false, true)

	-- Get the current UI
	local ui = vim.api.nvim_list_uis()[1]

	-- Create the floating window
	local win_opts = {
		relative = "win",
		width = ui.width - 4,
		height = ui.height - 4,
		col = 2,
		row = 2,
		anchor = "NW",
		border = "single",
		title = string.format("[nvim-ref] %s", self.opts.title or ""),
	}
	self.win = vim.api.nvim_open_win(self.buf, true, win_opts)
	vim.api.nvim_buf_set_option(self.buf, "filetype", "bibtex")
	vim.api.nvim_buf_set_option(self.buf, "buftype", "")
	vim.api.nvim_buf_set_name(self.buf, self.tempfile)
	self.is_open = true
end
function Editor:set_contents(contents)
	if type(contents) == "string" then
		self.contents = vim.split(contents, "\n", {})
	else
		self.contents = contents
	end
	if self.__kind == nil then
		self.__kind = "other"
	end
	self:open()
	vim.api.nvim_buf_set_lines(self.buf, 0, 0, true, self.contents)
end
function Editor:set_citation(citation)
	self.__original = citation
	self.__kind = "citation"
	self:open()
	self:set_contents(writer.convert(citation))
end
function Editor:set_file(file_name)
	self.__kind = "file"
	local fp = io.open(file_name)
	if fp ~= nil then
		local contents = fp:read("*a")
		fp:close()
		self:open()
		self:set_contents(contents)
		return true
	end
	return false
end
function Editor:on_close(cb)
	vim.api.nvim_create_autocmd("WinClosed", {
		group = augroup,
		pattern = string.format("%d", self.win),
		once = true,
		callback = function()
			self.is_open = false
			local fp = io.open(self.tempfile)
			if fp ~= nil then
				local contents = fp:read("*a")
				fp:close()
				-- We don't run the callback if the user :q! from the editor buffer
				if contents ~= "" then
					if self.__kind == "citation" then
						contents = parser.parse_bibtex_string(contents)[1]
						contents.file = self.__original.file
					end
					cb(contents)
				else
					require("nvim-ref.utils.notifications").info("Content did not change, cancelling edit.")
				end
			else
				require("nvim-ref.utils.notifications").info("No changes were made in editor, cancelling edit.")
			end
		end,
	})
end
function Editor:close()
	if self.is_open then
		vim.api.nvim_win_close(self.win, true)
		vim.api.nvim_buf_delete(self.buf, { force = true })
		self.is_open = false
	end
end

return Editor
