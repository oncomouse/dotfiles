local hooks = require("nvim-ref.hooks")
local M = {}

function M.setup()
	hooks.run_hook("add_command", {
		id = "capture",
		name = "Capture BibTeX from a source",
		subcommands = {
			{
				id = "doi",
				name = "Capture using a DOI",
				subcommands = {
					{
						id = "jstor",
						name = "Capture using DOI from JSTOR",
						callback = function() end,
					},
					{
						id = "orcid",
						name = "Capture using DOI from ORCID",
						callback = function() end,
					},
				}
			},
			{
				id = "isbn",
				name = "Capture using an ISBN",
				callback = function() end,
			},
		},
	})
end

return M
