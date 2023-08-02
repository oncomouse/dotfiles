-- Local settings for Markdown
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.list = false
vim.opt_local.spell = true
vim.opt_local.showbreak = "NONE"
-- vim.opt_local.omnifunc = "v:lua.require'nvim-ref.omnifunc'"

-- Remap gd to follow footnotes
vim.keymap.set("n", "gd", "<Plug>(markdown-nvim-footnote)", { buffer = true })

vim.cmd([[compiler markdown_combo]])

-- This messes up null-ls completion if uncommented:
-- vim.opt_local.iskeyword = vim.opt_local.iskeyword + "',-,@-@"
