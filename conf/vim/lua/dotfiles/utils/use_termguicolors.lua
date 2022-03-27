local function use_termguicolors()
	return vim.fn.has("mac") == 1 or vim.fn.exists("$DISPLAY") == 1 or vim.fn.exists("$WAYLAND_DISPLAY") == 1
end

return use_termguicolors
