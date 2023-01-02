-- Turn off all the bindings in the plugin:
local ok, autolist = pcall(require, "autolist")
if ok then
	autolist.setup({
		colon = {
			preferred = "",
			indent = false,
			indent_raw = false,
		},
		invert = {
			ul_marker = "*",
		},
	})
end
