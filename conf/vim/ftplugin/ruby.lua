vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
vim.cmd([[compiler rubocop]])
vim.opt_local.formatprg = "rufo --filename=%"
