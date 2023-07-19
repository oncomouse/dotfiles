return {
	"dense-analysis/ale",
	init = function()
		vim.g.ale_disable_lsp = 1
		vim.g.ale_use_neovim_diagnostics_api = 1
	end,
}
