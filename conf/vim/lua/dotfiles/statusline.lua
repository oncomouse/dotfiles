-- We don't load LuaSnip until InsertEnter, so we don't check for it until it could possibly be loaded.
local luasnip_might_have_loaded = false
vim.api.nvim_create_autocmd("InsertEnter", {
	group = vim.api.nvim_create_augroup("dotfiles-statusline-check-insert", {}),
	once = true,
	callback = function()
		luasnip_might_have_loaded = true
	end,
})

local H = {}

function H.hl(group, output)
	return string.format("%%#%s#%s%%*", group, output)
end

function H.is_disabled()
	return vim.g.ministatusline_disable == true or vim.b.ministatusline_disable == true
end

function H.get_config(config)
	return vim.tbl_deep_extend("force", require("mini.statusline").config, vim.b.ministatusline_config or {}, config or {})
end
H.diagnostic_levels = nil
function H.isnt_normal_buffer()
	-- For more information see ":h buftype"
	return vim.bo.buftype ~= ""
end

function H.get_filesize()
	local size = vim.fn.getfsize(vim.fn.getreg("%"))
	if size < 1024 then
		return string.format("%dB", size)
	elseif size < 1048576 then
		return string.format("%.2fKiB", size / 1024)
	else
		return string.format("%.2fMiB", size / 1048576)
	end
end

function H.get_filetype_icon()
	-- Skip if NerdFonts is disabled
	if not H.get_config().use_icons then
		return ""
	end
	-- Have this `require()` here to not depend on plugin initialization order
	local has_devicons, devicons = pcall(require, "nvim-web-devicons")
	if not has_devicons then
		return ""
	end

	local file_name, file_ext = vim.fn.expand("%:t"), vim.fn.expand("%:e")
	return devicons.get_icon(file_name, file_ext, { default = true })
end

function H.get_diagnostic_count(id)
	return #vim.diagnostic.get(0, { severity = id })
end

local M = {}

M.section_icon = function(args)
	if require("mini.statusline").is_truncated(args.trunc_width) then
		return ""
	end
	local icon = H.get_filetype_icon()
	if vim.bo.buftype == "help" then
		icon = ""
	end
	if vim.bo.buftype == "terminal" then
		icon = ""
	end
	if vim.bo.buftype == "quickfix" then
		icon = ""
	end
	if icon == "" then
		return ""
	end
	return string.format(" %s", icon)
end

M.section_filename = function(args)
	if args.inactive then
		return "%t"
	end

	local buftype = vim.bo.buftype
	-- In terminal always use plain name
	if buftype == "terminal" then
		return "%t"
	elseif buftype == "quickfix" then
		local name = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "Location List" or "Quickfix List"
		local title = vim.w.quickfix_title
		title = title and " " .. title or ""
		return string.format("[%s]%s", name, title)
	else
		local filename = (require("mini.statusline").is_truncated(args.trunc_width) and "%t" or "%f")
		local flags = "%m%r"
		if buftype == "help" then
			flags = ""
		end

		-- Use fullpath if not truncated
		return string.format("%s%s", filename, flags)
	end
end
M.section_fileinfo = function(args)
	local filetype = vim.bo.filetype

	-- Don't show anything if can't detect file type or not inside a "normal
	-- buffer"
	if (filetype == "") or H.isnt_normal_buffer() then
		return ""
	end

	-- Construct output string if truncated
	if require("mini.statusline").is_truncated(args.trunc_width) then
		return filetype
	end

	-- Construct output string with extra file info
	local encoding = vim.bo.fileencoding or vim.bo.encoding
	local format = vim.bo.fileformat
	local size = H.get_filesize()

	return string.format("%s %s[%s] %s", filetype, encoding, format, size)
end

M.section_diagnostics = function(args)
	-- Assumption: there are no attached clients if table
	-- `vim.lsp.buf_get_clients()` is empty
	if H.diagnostic_levels == nil then
		H.diagnostic_levels = {
			{
				id = vim.diagnostic.severity.ERROR,
				sign = vim.fn.sign_getdefined("DiagnosticSignError")[1].text,
				hl = "MiniStatuslineDiagnosticError",
			},
			{
				id = vim.diagnostic.severity.WARN,
				sign = vim.fn.sign_getdefined("DiagnosticSignWarn")[1].text,
				hl = "MiniStatuslineDiagnosticWarn",
			},
			{
				id = vim.diagnostic.severity.INFO,
				sign = vim.fn.sign_getdefined("DiagnosticSignInfo")[1].text,
				hl = "MiniStatuslineDiagnosticInfo",
			},
			{
				id = vim.diagnostic.severity.HINT,
				sign = vim.fn.sign_getdefined("DiagnosticSignHint")[1].text,
				hl = "MiniStatsulineDiagnosticHint",
			},
		}
	end
	local hasnt_attached_client = next(vim.lsp.get_clients({ bufnr = 0 })) == nil
	local dont_show_lsp = require("mini.statusline").is_truncated(args.trunc_width)
		or H.isnt_normal_buffer()
		or hasnt_attached_client
	if dont_show_lsp then
		return ""
	end

	-- Construct diagnostic info using predefined order
	local t = {}
	for _, level in ipairs(H.diagnostic_levels) do
		local n = H.get_diagnostic_count(level.id)
		-- Add level info only if diagnostic is present
		if n > 0 then
			table.insert(t, H.hl(level.hl, string.format("%s%s", level.sign, n)))
		end
	end

	if vim.tbl_count(t) == 0 then
		return ""
	end
	return string.format(" %s", table.concat(t, ""))
end
M.section_location = function(args)
	-- Use virtual column number to allow update when past last column
	if require("mini.statusline").is_truncated(args.trunc_width) then
		return H.hl("MiniStatuslineFileinfo", "%l:%v")
	end

	if vim.o.buftype == "terminal" or vim.o.buftype == "quickfix" then
		return ""
	end

	return table.concat(
		vim.tbl_map(function(component)
			if type(component) == "string" then
				return H.hl("MiniStatuslineFileinfo", component)
			end
			return H.hl(component.hl, table.concat(component.strings, ""))
		end, {
			{ hl = "MiniStatuslineLocationRow", strings = { "%l" } },
			":",
			{ hl = "MiniStatuslineLocationColumn", strings = { "%v" } },
			" (",
			{ hl = "MiniStatuslineLocationPercentage", strings = { "%p" } },
			"%% %LL)",
		}),
		""
	)
end

function M.section_macro()
	local macro = vim.fn.reg_recording()
	if macro ~= "" then
		macro = string.format(" @%s ", macro)
	end
	return macro
end

-- function M.section_search(args)
-- 	if not require("czs").display_results() then
-- 		return ""
-- 	end
--
-- 	local target, current, total = require("czs").output()
--
-- 	local output = ""
-- 	if not require("mini.statusline").is_truncated(args.trunc_width) then
-- 		output = string.format("%s", target)
-- 	end
-- 	output = output .. string.format("[%s]/%s ", current, total)
-- 	return output
-- end

function M.section_showcmd()
	return "%S "
end

function M.section_luasnip(args)
	local has_luasnip, ls
	if luasnip_might_have_loaded then
		has_luasnip, ls = pcall(require, "luasnip")
	else
		return ""
	end
	if require("mini.statusline").is_truncated(args.trunc_width) then
		return ""
	end

	if vim.fn.mode() ~= "i" then
		return ""
	end

	local forward, backward, choice
	if has_luasnip and ls.expand_or_locally_jumpable() then
		forward = ls.jumpable(1) and "→" or ""
		backward = ls.jumpable(-1) and "←" or ""
		choice = ls.choice_active() and "?" or ""
		if #forward + #backward + #choice > 0 then
			return " " .. backward .. forward .. choice
		end
	end
	return ""
end

function M.section_wordcount()
	if vim.tbl_contains({
		"markdown",
		"org",
		"txt",
		"vimiwiki"
	}, vim.o.filetype) then
		return string.format("W%d ", vim.fn.wordcount().words)
	end
	return ""
end

function M.section_orgheadline()
	if orgmode ~= nil then
		return string.gsub(orgmode.statusline(), "%(Org%) ", "") .. " "
	end
	return ""
end
return M
