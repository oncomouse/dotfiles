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
				"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library.bib",
			},
		},
		dependencies = {
			"plenary.nvim",
		},
	}, -- For BibTeX sources
}
