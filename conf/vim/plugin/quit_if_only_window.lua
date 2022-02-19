_dotfiles = _dotfiles or {}
_dotfiles.quit_if_only_window = {}
-- https://stackoverflow.com/a/39307414
local function next_normal_window()
	for i=1, vim.fn.winnr('$') do
		local buf = vim.fn.winbufnr(i)

		-- skip unlisted buffers
		if vim.fn.buflisted(buf) == 1 and
		-- skip temporary buffers with buftype set
		vim.fn.getbufvar(buf, '&buftype') ~= '' and
		-- skip the preview window
		vim.fn.getwinvar(i, '&previewwindow') == 0 and
		-- skip current window
		i ~= vim.fn.winnr() then
			return i
		end
	end

	return -1
end

_dotfiles.quit_if_only_window.quit_if_only_window = function()
	local buftype = vim.fn.getbufvar(vim.fn.winbufnr(vim.fn.winnr()), '&buftype')
	if buftype ~= 'quickfix' and buftype ~= 'help' then
		return
	end

	-- Check if there is more than one window
	if next_normal_window() == -1 then
		-- Check if there is more than one tab page
		if vim.fn.tabpagenr('$') == 1 then
			-- Before quitting Vim, delete the special buffer so that
			-- the '0 mark is correctly set to the previous buffer.
			-- Also disable autocmd on this command to avoid unnecessary
			-- autocmd nesting.
			if vim.fn.winnr('$') == 1 then
				if vim.fn.has('autocmd') == 1 then
					vim.cmd[[noautocmd bdelete]]
				end
				vim.cmd("quit")
			end
		else
			-- Note: workaround for the fact that in new tab the buftype is set
			-- too late (and sticks during this WinEntry autocmd to the old -
			-- potentially quickfix/help buftype - that would automatically
			-- close the new tab and open the buffer in copen window instead
			-- New tabpage has previous window set to 0
			if vim.fn.tabpagewinnr(vim.fn.tabpagenr(), '#') ~= 0 then
				local last_window = 0
				if vim.fn.winnr('$') == 1 then
					last_window = 1
				end
				vim.cmd("close")
				if last_window == 1 then
					-- Note: workaround for the same bug, but w.r.t. Airline
					-- plugin (it needs to refresh buftype and status line after
					-- last special window autocmd close on a tab page
					if vim.fn.exists(':AirlineRefresh') == 1 then
						vim.cmd[[execute 'AirlineRefresh']]
					end
				end
			end
		end
	end
end

-- autoclose last open location/quickfix/help windows on a tab
if vim.fn.has('autocmd') == 1 then
	vim.cmd[[aug AutoCloseAllQF
	au!
	autocmd WinEnter * nested lua _dotfiles.quit_if_only_window.quit_if_only_window()
	aug END]]
end

