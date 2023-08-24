local function setup_javascript_environment()
	vim.cmd([[compiler eslint]])
	vim.opt_local.formatprg = "prettier --parser babel"
	vim.opt_local.tabstop = 2
	vim.opt_local.shiftwidth = 2
	vim.opt_local.softtabstop = 2
	vim.opt_local.expandtab = true
	vim.opt_local.listchars = vim.opt_local.listchars - "tab:| "
	vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│ "

	if vim.tbl_contains({ "javascript", "javascriptreact" }, vim.bo.filetype) then
		if require("null-ls.utils").make_conditional_utils().root_has_file({ ".flowconfig" }) then
			vim.cmd("LspStart flow")
		end
	end

	vim.api.nvim_create_autocmd("LspAttach", {
		group = vim.api.nvim_create_augroup("dotfiles-javascript-detector", {}),
		callback = function(args)
			local client = vim.lsp.get_client_by_id(args.data.client_id)
			if client and client.name == "eslint" then
				require("null-ls").deregister("standardjs")
			end
		end
	})
	require("null-ls").register({
		require("null-ls").builtins.diagnostics.standardjs,
	})
	require("dotfiles.lsp.start_server")("eslint")
	require("dotfiles.lsp.start_server")("tsserver")
end

return setup_javascript_environment
