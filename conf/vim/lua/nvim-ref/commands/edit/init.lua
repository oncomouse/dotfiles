local hooks = require("nvim-ref.hooks")
local M = {}

function M.edit(citation)
	print(vim.inspect(citation))
end

function M.setup()
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
end

return M
