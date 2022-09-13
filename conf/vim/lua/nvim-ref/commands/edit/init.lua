local hooks = require("nvim-ref.hooks")
local writer = require("nvim-ref.utils.bibtex").writer
local M = {}

local function populate_luasnip(citation)
	-- https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md#on-the-fly-snippets
	return citation
end

function M.edit(citation)
	local ok = pcall(require, "luasnip")
	-- create a tmp file
	-- open it in a pop-up, tab, split, or new buffer
	local cite_text = writer.convert(citation)
	if ok then
		populate_luasnip(citation)
	else
		-- just insert the citation as text
	end
	-- on exit, if changed, write to disk
	print(vim.inspect(cite_text))
end

function M.edit_file(files)
	print(vim.inspect(files))
end

function M.setup()
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
