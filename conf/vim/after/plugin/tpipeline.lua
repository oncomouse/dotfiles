local function recolor_statusline()
	vim.api.nvim_set_hl(0, "StatusLine", { bg = vim.api.nvim_get_hl_by_name("AnsiColor0", true).foreground })
end
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "*", callback = recolor_statusline }
)
recolor_statusline()
