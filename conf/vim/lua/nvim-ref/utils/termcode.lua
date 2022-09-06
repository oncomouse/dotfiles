-- Replace Termcodes:
local function t(str)
	-- Adjust boolean arguments as needed
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

return t
