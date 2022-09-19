local M = {}

function M.loader()
	local hydra = require("hydra")
	for _, definition in pairs(vim.g.dotfiles_hydras) do
		if type(definition) == "table" then
			if #definition > 0 then
				for _, d in pairs(definition) do
					hydra(d)
				end
			else
				hydra(definition)
			end
		end
	end
end

function M.setup()
	M.loader()
	vim.api.nvim_create_autocmd("BufWritePost", {
		pattern = "hydra.lua",
		command = "source <afile> | lua require('dotfiles.plugins.hydra').loader()"
	})
end

return M
