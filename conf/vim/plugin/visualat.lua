-- luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
dotfiles.visualat = {}
function dotfiles.visualat.execute_macro_over_visual_range()
	vim.cmd([[echo '@'.getcmdline()
	execute ":'<,'>normal @".nr2char(getchar())]])
end
