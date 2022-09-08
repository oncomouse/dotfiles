local M = {}

function M.get_buffer_lines(bufnum)
	bufnum = bufnum or 0
	return vim.api.nvim_buf_get_lines(bufnum, 0, -1, false)
end

return M
