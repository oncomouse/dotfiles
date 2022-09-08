local hooks = require("nvim-ref.hooks")
local M = {}

hooks.add_hook("setup_done", function()
	hooks.run_hook("add_command", {
		id = "edit",
		name = "Edit a BibTeX Source",
		callback = function(args)
			if #args == 0 then
				require("nvim-ref.select").citation(M.edit)
			else
				M.edit(require("nvim-ref.citations").get_citation(args))
			end
		end,
	})
end)

M.edit = function(citation)
	print(vim.inspect(citation))
end

return M
