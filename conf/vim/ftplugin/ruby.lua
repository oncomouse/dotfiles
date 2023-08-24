vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
vim.opt_local.listchars = vim.opt_local.listchars - "tab:| "
vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│ "

vim.cmd([[compiler rubocop]])
vim.opt_local.formatprg = "rufo --filename=%"
require("dotfiles.lsp").start_server("standardrb")
