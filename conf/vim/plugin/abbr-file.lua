-- Look for files named .abbr that contain a call to :abbr in each line.
vim.api.nvim_create_autocmd("FileType", {
	callback = function(ev)
		if not vim.api.nvim_buf_is_valid(ev.buf) then
			return
		end
		local path = vim.fs.normalize(vim.api.nvim_buf_get_name(ev.buf))
		for parent in vim.fs.parents(path) do
			local f = io.open(parent .. "/.abbr")
			if f then
				for line in f:lines() do
					vim.cmd(string.format("abbr %s", line))
				end
				f:close()
				break
			end
		end
	end,
})
