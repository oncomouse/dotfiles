-- vim.api.nvim_create_autocmd({ "BufNewFile", "BufEnter"}, {
-- 	pattern = "*",
-- 	group = vim.api.nvim_create_augroup("luasnip-lsp-augroup", {}),
-- 	callback = function()
-- 		require("luasnip.lsp")
-- 	end
-- })
require("luasnip.lsp")
