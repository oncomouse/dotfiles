-- Default mappings for select-digraphs
require("select-digraphs").setup()
vim.keymap.set("i", "<C-k><C-k>", "<Plug>(select-digraph-i)")
vim.keymap.set("n", "r<C-k><C-k>", "<Plug>(select-digraph-n)")
vim.keymap.set("v", "r<C-k><C-k>", "<Plug>(select-digraph-v)")
