vim.opt_local.iskeyword = vim.opt_local.iskeyword + "-"
vim.cmd([[compiler csslint]])
vim.opt_local.formatprg = "prettier --use-tabs --parser css"
require("dotfiles.lsp.").start_server("cssls")
