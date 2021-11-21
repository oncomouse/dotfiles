-- luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
-- Lua port of https://stackoverflow.com/a/39307414
local function next_normal_window()
	for _, i in ipairs(vim.api.nvim_list_wins()) do
		local buf = vim.fn.winbufnr(i)
		if
			vim.fn.buflisted(buf)
			and vim.api.nvim_buf_get_option(buf, "buftype") ~= ""
			and not vim.api.nvim_win_get_option(i, "previewwindow")
			and vim.fn.win_getid() ~= i
		then
			return i
		end
	end
	return -1
end

function dotfiles.quit_if_only_window()
	local buftype = vim.api.nvim_buf_get_option(
		vim.fn.winbufnr(vim.api.nvim_win_get_number(0)),
		"buftype"
	)
	if
		buftype ~= "quickfix"
		and buftype ~= "help"
	then
		return
	end
	if next_normal_window() == -1 then
		if vim.fn.tabpagenr("$") == 1 then
			for _,i in ipairs(vim.api.nvim_list_wins()) do
				vim.api.nvim_buf_delete(i, { force = true })
			end
		end
		vim.cmd([[quit]])
	else
		if vim.fn.tabpagewinnr(vim.fn.tabpagenr(), "#") ~= 0 then
			local last_window = 0
			if vim.fn.winnr("$") == 1 then
				last_window = 1
			end
			vim.cmd([[close]])
			if last_window == 1 then
				if vim.fn.exists(":AirlineRefresh") then
					vim.cmd([[AirlineRefresh]])
				end
			end
		end
	end
end

vim.cmd([[autocmd WinEnter * lua dotfiles.quit_if_only_window()]])
