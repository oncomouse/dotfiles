vim.opt_local.matchpairs = vim.opt_local.matchpairs + "<:>"
vim.cmd([[compiler htmlhint]])
vim.opt_local.formatprg = "prettier --use-tabs --parser html"
require("dotfiles.lsp.").start_server("html")
