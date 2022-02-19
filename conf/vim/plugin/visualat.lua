_dotfiles = _dotfiles or {}
_dotfiles.visualat = {}
function _dotfiles.visualat.execute_macro_over_visual_range()
	vim.cmd([[echo '@'.getcmdline()
	execute ":'<,'>normal @".nr2char(getchar())]])
end
