local ok = pcall(require, "tpipeline.main")
if ok then
	local function recolor_statusline()
		vim.api.nvim_set_hl(0, "StatusLine", {})
	end
	vim.api.nvim_create_autocmd("ColorScheme", {
		group = "dotfiles-settings",
		pattern = "*",
		callback = recolor_statusline,
	})
	recolor_statusline()
end
