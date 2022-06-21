vim.cmd([[compiler eslint]])
vim.opt_local.formatprg="prettier --parser babel"
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│ "

local function start_flow_for_javascript()
	if require("null-ls.utils").make_conditional_utils().root_has_file({ ".flowconfig" }) then
		vim.cmd("LspStart flow")
	end
end

vim.api.nvim_create_autocmd("FileType", {
	group = "dotfiles-settings",
	pattern = "javascript,javascriptreact",
	callback = start_flow_for_javascript,
})
