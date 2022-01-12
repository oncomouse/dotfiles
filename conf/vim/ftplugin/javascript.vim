compiler eslint
set formatprg=prettier\ --parser\ babel
setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

lua << EOF
function _G.start_flow_for_javascript()
	if require("null-ls.utils").make_conditional_utils().root_has_file({ ".flowconfig" }) then
		vim.cmd("LspStart flow")
	end
end

vim.cmd([[autocmd! dotfiles-settings FileType javascript,javascriptreact lua start_flow_for_javascript()]])
EOF
