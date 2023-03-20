return {
	{
		"NvChad/nvim-colorizer.lua",
		event = { "BufRead", "BufWinEnter", "BufNewFile" },
		opts = {
			filetypes = {
				"*", -- Load everywhere
				"!lazy", -- Except packer buffers
				"!help", -- And help
				html = { names = true, RRGGBBAA = false },
				css = { css = true, RRGGBBAA = false },
				scss = {
					css = true,
					RRGGBBAA = false,
					sass = {
						enable = true,
					},
				},
				sass = {
					css = true,
					RRGGBBAA = false,
					sass = {
						enable = true,
					},
				},
			},
			user_default_options = {
				names = false, -- Turn off highlighting color words in non-HTML/CSS settings
				RRGGBBAA = true,
				mode = "background", -- Could be background, foreground, or virtualtext
			},
		},
	}, -- Highlight colors in files

}
