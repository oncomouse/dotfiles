vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.list = false
vim.opt_local.spell = true
vim.opt_local.commentstring = "# %s"

-- Load markdown maps:
vim.b.markdown_nvim_unordered_default = "-"
require("markdown").setup()
