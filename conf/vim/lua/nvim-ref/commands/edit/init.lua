local hooks = require("nvim-ref.hooks")
local writer = require("nvim-ref.utils.bibtex").writer
local M = {}

local augroup = vim.api.nvim_create_augroup("__nvim-ref-editor-augroup", {})

local Editor = {}
function Editor:new()
	local o = {}
	setmetatable(o, self)
	self.__index = self
	self.tempfile = vim.fn.tempname()
	return o
end
function Editor:open(opts)
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
		title = string.format("[nvim-ref] %s", opts.title or ""),
	}
	self.win = vim.api.nvim_open_win(self.buf, true, win_opts)
	vim.api.nvim_buf_set_option(self.buf, "filetype", "bibtex")
	vim.api.nvim_buf_set_option(self.buf, "buftype", "")
	vim.api.nvim_buf_set_name(self.buf, self.tempfile)
end
function Editor:set_contents(contents)
	if type(contents) == "string" then
		self.contents = vim.split(contents, "\n", {})
	else
		self.contents = contents
	end
	vim.api.nvim_buf_set_lines(self.buf, 0, 0, true, self.contents)
end
function Editor:on_close(cb)
	vim.api.nvim_create_autocmd("WinClosed", {
		group = augroup,
		pattern = string.format("%d", self.win),
		once = true,
		callback = function()
			local fp = io.open(self.tempfile)
			if fp ~= nil then
				local contents = fp:read("*a")
				fp:close()
				if contents ~= "" then
					cb(contents)
				end
			end
		end,
	})
end

function M.edit(citation, cb)
	local editor = Editor:new()
	editor:open({
		title = string.format("Edit %s", citation.key),
	})
	editor:set_contents(writer.convert(citation))
	editor:on_close(cb)
end

function M.edit_file(files, cb)
	local editor = Editor:new()
	editor:open({
		title = string.format("Edit %s", files),
	})
	local fp = io.open(files)
	if fp ~= nil then
		local contents = fp:read("*a")
		fp:close()
		editor:set_contents(contents)
		editor:on_close(cb)
	end
end

function M.setup()
	-- Add a LuaSnip using dynamic nodes to generate the populated citation:
	-- https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md#dynamicnode

	hooks.trigger("add_command", {
		id = "edit",
		name = "Edit a Bibliography",
		subcommands = {
			{
				id = "source",
				name = "Edit a BibTeX Source",
				callback = function(args)
					if #args == 0 then
						require("nvim-ref.select").citation(M.edit)
					else
						M.edit(require("nvim-ref.citations").get_citation(args))
					end
				end,
			},
			{
				id = "file",
				name = "Edit a BibTeX File",
				callback = function(args)
					if #args == 0 then
						require("nvim-ref.select").bibliography(M.edit_file)
					else
						M.edit_file(args)
					end
				end,
			},
		},
	})
end

return M
