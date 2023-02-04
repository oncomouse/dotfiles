-- Line Number Colors in default:
local list_toggle = require("dotfiles.functions").list_toggle
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNr ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNrAbove ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNrBelow ctermfg=7" }
)
vim.api.nvim_create_autocmd("ColorScheme", {
	group = "dotfiles-settings",
	pattern = "default",
	command = "hi StatusLine ctermbg=8 ctermfg=7 cterm=NONE gui=NONE",
})
vim.api.nvim_create_autocmd("ColorScheme", {
	group = "dotfiles-settings",
	pattern = "default",
	command = "hi StatusLineNC ctermbg=8 ctermfg=240 cterm=NONE gui=NONE",
})

-- Turn Off Line Numbering:
vim.api.nvim_create_autocmd("TermOpen", { group = "dotfiles-settings", command = "setlocal nonumber norelativenumber" })

-- Start QuickFix:
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = "dotfiles-settings",
	pattern = "[^l]*",
	callback = function()
		list_toggle("c", 1)
	end,
})
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = "dotfiles-settings",
	pattern = "l*",
	callback = function()
		list_toggle("l", 1)
	end,
})

-- Highlighted Yank:
-- vim.api.nvim_create_autocmd("TextYankPost", {
-- 	group = "dotfiles-settings",
-- 	callback = function()
-- 		vim.highlight.on_yank({ higroup = "IncSearch", timeout = 500 })
-- 	end,
-- })

-- Close Preview Window:
vim.api.nvim_create_autocmd("CompleteDone", {
	group = "dotfiles-settings",
	callback = function()
		if vim.fn.pumvisible() == 0 then
			vim.cmd("pclose")
		end
	end,
})


