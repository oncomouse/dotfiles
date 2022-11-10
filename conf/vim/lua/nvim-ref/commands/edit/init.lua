local hooks = require("nvim-ref.hooks")
local Editor = require("nvim-ref.commands.edit.editor")
local M = {}

function M.edit(citation, cb)
	local editor = Editor:new({
		title = string.format("Edit %s", citation.key),
	})
	editor:set_citation(citation)
	editor:on_close(cb)
end

function M.edit_file(files, cb)
	local editor = Editor:new({
		title = string.format("Edit %s", files),
	})
	local loaded = editor:set_file(files)
	if loaded then
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
						require("nvim-ref.select").citation(function(citation)
							M.edit(citation, function(updated_bibtex)
								require("nvim-ref.utils.bibtex.writer").update(updated_bibtex)
							end)
						end)
					else
						M.edit(require("nvim-ref.citations").get_citation(args), function(updated_bibtex)
							require("nvim-ref.utils.bibtex.writer").update(updated_bibtex)
						end)
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
