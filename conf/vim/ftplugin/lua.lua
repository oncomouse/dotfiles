vim.opt_local.formatprg="stylua -"
vim.opt_local.include=[==[^.*require\s*(\{0,1\}["']\zs[^"']\+\ze["']]==]
vim.opt_local.includeexpr="v:lua.require'dotfiles.lua'.includeexpr(v:fname)"
require("neodev").setup()
require("dotfiles.lsp.").start_server("lua_ls")
