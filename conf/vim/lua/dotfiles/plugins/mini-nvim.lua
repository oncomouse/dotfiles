local function config_mini()
	require("mini.ai").setup({
		custom_textobjects = {
			e = function()
				local from = { line = 1, col = 1 }
				local to = {
					line = vim.fn.line("$"),
					col = vim.fn.getline("$"):len(),
				}
				return { from = from, to = to }
			end,
		},
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
