return {
	{
		"oncomouse/nvim-ref",
		dev = false,
		ft = {
			"latex",
			"markdown",
			"org",
		},
		cmd = "NvimRef",
		opts = {
			bibfiles = {
				vim.fs.joinpath(vim.g.seadrive_path, "My Library/Documents/Academic Stuff/library.bib"),
			},
		},
		dependencies = {
			"plenary.nvim",
		},
	}, -- For BibTeX sources
}
