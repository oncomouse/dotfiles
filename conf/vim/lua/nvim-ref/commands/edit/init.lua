local hooks = require("nvim-ref.hooks")
local writer = require("nvim-ref.utils.bibtex").writer
local M = {}

local function open_editor_window(title)
	-- Create the scratch buffer displayed in the floating window
	local buf = vim.api.nvim_create_buf(false, true)

	-- Get the current UI
	local ui = vim.api.nvim_list_uis()[1]

	-- Create the floating window
	local opts = {
		relative = "win",
		width = ui.width - 4,
		height = ui.height - 4,
		col = 2,
		row = 2,
		anchor = "NW",
		border = "single",
		title = string.format("[nvim-ref] %s", title)
	}
	local win = vim.api.nvim_open_win(buf, true, opts)
	vim.api.nvim_buf_set_option(buf, "filetype", "bibtex")
	return win, buf
end

function M.edit(citation)
	local win, buf = open_editor_window(string.format("Edit %s", citation.key))
	vim.api.nvim_buf_set_lines(buf, 0, 0, true, vim.split(writer.convert(citation), "\n", {}))
end

function M.edit_file(files)
	print(vim.inspect(files))
	open_editor_window("Edit File")
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
