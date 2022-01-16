-- luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
function dotfiles.sl_wc()
	return vim.tbl_contains({
		"markdown",
		"txt",
		"vimwiki",
	}, vim.opt.filetype:get()) and " W:" .. vim.fn.wordcount().words or ""
end
function dotfiles.sl_dg()
	local d = ""
	for sign_key, kind in pairs({
		DiagnosticSignError = vim.diagnostic.severity.ERROR,
		DiagnosticSignWarn = vim.diagnostic.severity.WARN,
		DiagnosticSignInfo = vim.diagnostic.severity.INFO,
		DiagnosticSignHint = vim.diagnostic.severity.HINT,
	}) do
		local c = #vim.diagnostic.get(0, { severity = kind })
		local sign = vim.fn.sign_getdefined(sign_key)[1]
		local marker = sign.text
		if c ~= 0 then
			d = d .. " " .. marker .. tostring(c)
		end
	end
	return d
end
local statusline = " %0.45f%m%h%w%r%= %y%{v:lua.dotfiles.sl_wc()} %l:%c %p%%%{v:lua.dotfiles.sl_dg()} "
local statusline_nc = " %0.45f%m%h%w%r%="
function dotfiles.sl_stl()
	return vim.g.statusline_winid == vim.fn.win_getid() and statusline or statusline_nc
end
vim.opt.statusline = "%!v:lua.dotfiles.sl_stl()"
