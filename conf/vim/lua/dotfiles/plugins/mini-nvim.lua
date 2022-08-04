local function config_mini()
	require("mini.ai").setup({
		mappings = {
			around_last = "aN",
			inside_last = "iN",
		},
	})
	require("mini.comment").setup({})
	require("mini.indentscope").setup({
		options = {
			indent_at_cursor = false,
		},
	})
	vim.g.miniindentscope_disable = true
	require("mini.jump").setup({})
end

return config_mini
