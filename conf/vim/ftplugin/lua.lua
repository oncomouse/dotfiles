vim.opt_local.formatprg="stylua -"
vim.opt_local.include=[[^.*=\s*require(["']\zs[^"']\+\ze["'])]]
vim.opt_local.includeexpr="v:lua.require'dotfiles.lua'.includeexpr(v:fname)"
