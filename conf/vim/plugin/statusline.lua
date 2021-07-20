dotfiles = _G.dotfiles or {}
function dotfiles.sl_wc()
	return vim.tbl_contains({
		"markdown",
		"txt",
		"vimwiki"
	}, vim.opt.filetype:get()) and " W:L" .. vim.fn.wordcount().words or ""
end
function dotfiles.sl_dg()
	local d = ''
	for kind,marker in pairs({ Error = " E:", Warning = " W:", Information = " I:", Hint = " H:" }) do
		local c = vim.lsp.diagnostic.get_count(0, kind)
		if c ~= 0 then
			d = d .. marker .. tostring(c)
		end
	end
	return d
end
local statusline = " %0.45f%m%h%w%r%= %y%{v:lua.dotfiles.sl_wc()} %l:%c%{v:lua.dotfiles.sl_dg()} "
local statusline_nc = " %0.45f%m%h%w%r%="
function dotfiles.sl_stl()
	return vim.g.statusline_winid == vim.fn.win_getid() and statusline or statusline_nc
end
vim.opt.statusline="%!v:lua.dotfiles.sl_stl()"
