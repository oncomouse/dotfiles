-- Default mappings for select-digraphs
require("select-digraphs").setup()
vim.keymap.set("i", "<C-k><C-k>", "<Plug>(select-digraphs-i)")
vim.keymap.set("n", "r<C-k><C-k>", "<Plug>(select-digraphs-n)")
vim.keymap.set("v", "r<C-k><C-k>", "<Plug>(select-digraphs-v)")
