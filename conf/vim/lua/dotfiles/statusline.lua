-- luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
function dotfiles.sl_wc()
	return vim.tbl_contains({
		"markdown",
		"txt",
		"vimwiki"
	}, vim.opt.filetype:get()) and " W:" .. vim.fn.wordcount().words or ""
end
function dotfiles.sl_dg()
	local d = ''
	if vim.diagnostic ~= nil then -- Neovim 0.6
		for marker,kind in pairs({
			[" E:"] = vim.diagnostic.severity.ERROR,
			[" W:"] = vim.diagnostic.severity.WARN,
			[" I:"] = vim.diagnostic.severity.INFO,
			[" H:"] = vim.diagnostic.severity.HINT
		}) do
			local c = #vim.diagnostic.get(0, { severity = kind })
			if c ~= 0 then
				d = d .. marker .. tostring(c)
			end
		end
	else -- Neovim 0.5
		for kind,marker in pairs({ Error = " E:", Warning = " W:", Information = " I:", Hint = " H:" }) do
			local c = vim.lsp.diagnostic.get_count(0, kind)
			if c ~= 0 then
				d = d .. marker .. tostring(c)
			end
		end
	end
	return d
end
local statusline = " %0.45f%m%h%w%r%= %y%{v:lua.dotfiles.sl_wc()} %l:%c %p%%%{v:lua.dotfiles.sl_dg()} "
local statusline_nc = " %0.45f%m%h%w%r%="
function dotfiles.sl_stl()
	return vim.g.statusline_winid == vim.fn.win_getid() and statusline or statusline_nc
end
vim.opt.statusline="%!v:lua.dotfiles.sl_stl()"

