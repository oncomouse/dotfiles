local hooks = require("nvim-ref.hooks")
local Editor = require("nvim-ref.commands.edit.editor")
local M = {}

function M.edit(citation, cb)
	if cb == nil then
		cb = function(new_citation)
			require("nvim-ref.utils.bibtex.writer").update(new_citation)
		end
	end
	local editor = Editor:new({
		title = string.format("Edit %s", citation.key),
	})
	editor:set_citation(citation)
	editor:on_close(cb)
end

function M.edit_file(files, cb)
	if cb == nil then
		cb = function(new_file_contents)
			local fp = io.open(files, "a+")
			fp:write(new_file_contents)
			fp:close()
			require("nvim-ref.utils.notifications").info(string.format("File, %s, has been updated.", files))
		end
	end
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
