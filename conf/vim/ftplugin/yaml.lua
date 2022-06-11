vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
vim.cmd([[compiler yamllint]])
vim.opt_local.formatprg = "prettier --parser yaml"
