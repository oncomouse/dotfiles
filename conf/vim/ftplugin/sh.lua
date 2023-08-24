vim.cmd([[compiler shellcheck]])
vim.opt_local.formatprg = "shfmt -ci -s -bn"
require("dotfiles.lsp").start_server("bashls")
