-- Why isn't this in nvim-orgmode?
vim.keymap.set("i", "<M-CR>", "<c-o><leader><cr>", { buffer = true, remap = true })

vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
