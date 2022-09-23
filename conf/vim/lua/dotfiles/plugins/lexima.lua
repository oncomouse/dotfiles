local M = {}

-- Load rules:
function M.loader()
	local add_rule = vim.fn["lexima#add_rule"]
	-- Clear existing rules:
	vim.fn["lexima#clear_rules"]()
	-- Reset default rules:
	vim.fn["lexima#set_default_rules"]()
	-- Add our defined rules:
	for _, rule in pairs(vim.g.dotfiles_lexima_rules) do
		if type(rule) == "table" then
			if #rule == 0 then
				add_rule(rule)
			else
				for _, r in pairs(rule) do
					add_rule(r)
				end
			end
		end
	end
end

function M.setup()
	-- Autoclose mapping:
	vim.keymap.set("i", "<Plug>(dotfiles-lexima-leave-til-eol)", '<C-r>=lexima#insmode#leave_till_eol("")<CR>', { noremap = true })
	-- Set rules:
	M.loader()
	-- Automatically reload rules when editing the rule file:
	vim.api.nvim_create_autocmd("BufWritePost", {
		pattern = "lexima.lua",
		command = "source <afile> | lua require('dotfiles.plugins.lexima').loader()"
	})
end

return M
