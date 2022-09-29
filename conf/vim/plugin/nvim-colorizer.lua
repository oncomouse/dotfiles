if vim.opt.termguicolors:get() then
	require("colorizer").setup({
		filetypes = {
			"*", -- Load everywhere
			"!packer", -- Except packer buffers
			"!gina*", -- And commit buffers
			html = { names = true, RRGGBBAA = false },
			css = { css = true, RRGGBBAA = false },
			scss = {
				css = true,
				RRGGBBAA = false,
				-- custom_matcher = require("colorizer/sass").variable_matcher,
			},
		},
		user_default_options = {
			names = false, -- Turn off highlighting color words in non-HTML/CSS settings
			RRGGBBAA = true,
			mode = "background", -- Could be background, foreground, or virtualtext
			sass = {
				enable = true,
				parsers = {
					css = true,
				},
			},
		},
	})
end
