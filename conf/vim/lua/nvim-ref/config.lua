local default_config = {
	bibfiles = {},
	commands = {
		"insert",
		"capture",
		"edit",
	},
	filetypes = {
		"latex",
		"markdown",
		"org",
	},
}
local function config(opts)
	if opts.bibfiles and type(opts.bibfiles) == "string" then
		opts.bibfiles = { opts.bibfiles }
	end
	return vim.tbl_deep_extend("keep", opts, default_config)
end

return config
