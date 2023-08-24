vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
vim.opt_local.listchars = vim.opt_local.listchars - "tab:| "
vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│ "

vim.cmd([[compiler yamllint]])
vim.opt_local.formatprg = "prettier --parser yaml"
require("dotfiles.lsp.").start_server("yamlls")
