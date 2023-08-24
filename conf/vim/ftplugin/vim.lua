vim.cmd([[compiler vint]])
vim.opt_local.foldmethod = "marker"
vim.opt_local.foldlevel = 0
require("dotfiles.lsp.").start_server("vimls")
