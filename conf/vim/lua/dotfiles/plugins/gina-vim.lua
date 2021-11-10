-- luacheck: globals vim
return {
	"lambdalisue/gina.vim",
	cmd = "Gina",
	config = function()
		vim.fn["gina#custom#command#option"]("status", "--opener", vim.opt.previewheight:get() .. "split")
		vim.fn["gina#custom#command#option"]("commit", "--opener", vim.opt.previewheight:get() .. "split")
		vim.fn["gina#custom#command#option"]("diff", "--opener", vim.opt.previewheight:get() .. "split")
		vim.fn["gina#custom#command#option"]("status", "--group", "short")
		vim.fn["gina#custom#command#option"]("commit", "--group", "short")
		vim.fn["gina#custom#command#option"]("diff", "--group", "short")
		-- Implement vim-fugitive commands in Gina:
		vim.fn["gina#custom#mapping#nmap"]("status", "cc", ":<C-u>Gina commit<CR>", {
			noremap = 1,
			silent = 1,
		})
	end,
}
