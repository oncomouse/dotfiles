-- luacheck: globals vim
return {
	"junegunn/fzf.vim",
	event = "VimEnter",
	setup = function()
		vim.cmd(
			[[command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)]]
		)
		vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = "top" } }
		vim.g.fzf_action = {
			["ctrl-s"] = "split",
			["ctrl-v"] = "vsplit",
			["ctrl-t"] = "tabnew",
			["ctrl-e"] = "edit",
		}
		vim.g.fzf_nvim_statusline = 0 -- disable statusline overwriting
	end,
}
