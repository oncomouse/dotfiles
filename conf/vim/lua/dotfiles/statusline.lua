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

-- From heirline.nvim
local function width_percent_below(n, thresh, is_winbar)
    local winwidth
    if vim.o.laststatus == 3 and not is_winbar then
        winwidth = vim.o.columns
    else
        winwidth = vim.api.nvim_win_get_width(0)
    end

    return n / winwidth <= thresh
end
-- End From heirline.nvim

function _dotfiles.sl_fn()
	local filename = vim.api.nvim_buf_get_name(0)
	-- first, trim the pattern relative to the current directory. For other
	-- options, see :h filename-modifers
	filename = vim.fn.fnamemodify(filename, ":.")
	if filename == "" then
		return "[No Name]"
	end
	if vim.bo.buftype == "help" then
		return vim.fn.fnamemodify(filename, ":t")
	end
	-- now, if the filename would occupy more than 1/4th of the available
	-- space, we trim the file path to its initials
	if width_percent_below(#filename, 0.25) then
		filename = vim.fn.pathshorten(filename)
	end
	return filename
end

function _dotfiles.sl_fg()
	local flags = ""
	if vim.bo.modified then
		flags = flags .. "[+]"
	end
	if vim.bo.buftype == "help" then
		flags = flags .. "[?]"
	end
	if not vim.bo.modifiable or vim.bo.readonly then
		flags = flags .. "[]"
	end
	return flags
end

local file_name = "%{v:lua._dotfiles.sl_fn()}%{v:lua._dotfiles.sl_fg()}"
local statusline = file_name .. "%=%y%{v:lua._dotfiles.sl_wc()} %l:%c %p%%%{v:lua._dotfiles.sl_dg()} "
local statusline_nc = file_name

local function active()
	return vim.g.statusline_winid == vim.fn.win_getid()
end

function _dotfiles.sl_stl()
	return active() and statusline or statusline_nc
end
vim.opt.statusline = "%!v:lua._dotfiles.sl_stl()"
