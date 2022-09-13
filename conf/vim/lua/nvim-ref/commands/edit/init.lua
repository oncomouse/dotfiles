local hooks = require("nvim-ref.hooks")
local writer = require("nvim-ref.utils.bibtex").writer
local M = {}

function M.edit(citation)
	print(vim.inspect(writer.convert(citation)))
end

function M.edit_file(files)
	print(vim.inspect(files))
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
			},{
				id = "file",
				name = "Edit a BibTeX File",
				callback = function(args)
					if #args == 0 then
						require("nvim-ref.select").bibliography(M.edit_file)
					else
						M.edit_file(args)
					end
				end,
			}
		},
	})
end

return M
