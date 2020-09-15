require"nvim-treesitter.configs".setup{
	ensure_installed = {
		"markdown",
		"json",
		"python",
		"html",
		"css",
		"ruby",
		"javascript",
		"lua",
		"yaml",
	}, -- one of "all", "language", or a list of languages
	highlight = { enable = true },
}
