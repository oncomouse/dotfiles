vim.cmd([[compiler jsonlint]])
vim.opt_local.formatprg = "prettier --use-tabs --parser json"
require("dotfiles.lsp").start_server("jsonls")
