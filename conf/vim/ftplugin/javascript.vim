compiler eslint
set formatprg=prettier\ --parser\ babel
setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

lua << EOF
local function start_flow_for_javascript()
	if require("null-ls.utils").make_conditional_utils().root_has_file({ ".flowconfig" }) then
		vim.cmd("LspStart flow")
	end
end

vim.api.nvim_create_autocmd("FileType", {
	group = "dotfiles-settings",
	pattern = "javascript,javascriptreact"
	callback = start_flow_for_javascript,
})
EOF
