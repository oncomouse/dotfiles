_dotfiles = _dotfiles or {}
function _dotfiles.sl_wc()
	return vim.tbl_contains({
		"markdown",
		"txt",
		"vimwiki",
	}, vim.opt.filetype:get()) and " W:" .. vim.fn.wordcount().words or ""
end
function _dotfiles.sl_dg()
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
local statusline = "%=%y%{v:lua._dotfiles.sl_wc()} %l:%c %p%%%{v:lua._dotfiles.sl_dg()} "
local statusline_nc = ""
local winbar = "%=%0.45f%m%h%w%r"
local function active()
	return vim.g.statusline_winid == vim.fn.win_getid()
end
function _dotfiles.sl_stl()
	return active() and statusline or statusline_nc
end

-- This is from heirline.nvim
local function pattern_list_match(str, pattern_list)
    for _, pattern in ipairs(pattern_list) do
        if str:find(pattern) then
            return true
        end
    end
    return false
end

local buf_matchers = {
    filetype = function(pattern_list)
        local ft = vim.bo.filetype
        return pattern_list_match(ft, pattern_list)
    end,
    buftype = function(pattern_list)
        local bt = vim.bo.buftype
        return pattern_list_match(bt, pattern_list)
    end,
    bufname = function(pattern_list)
        local bn = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t")
        return pattern_list_match(bn, pattern_list)
    end,
}

local function buffer_matches(patterns)
    for kind, pattern_list in pairs(patterns) do
        if buf_matchers[kind](pattern_list) then
            return true
        end
    end
    return false
end
-- End heirline.nvim copy
function _dotfiles.sl_winbar()
	local hide_winbar = buffer_matches({
		buftype = { "nofile", "prompt", "help", "quickfix" },
		filetype = { "^gina.*", "fugitive", "fzf" },
		bufname = { "^gina.*", ".*commit$", "^$" },
	})
	if hide_winbar then
		return ""
	end
	return winbar
end
vim.opt.statusline = "%!v:lua._dotfiles.sl_stl()"
-- vim.opt.winbar = "%!v:lua._dotfiles.sl_winbar()"
