return {
	{
		window_frame = {
			{
				active_titlebar_bg = "{background}",

				-- The overall background color of the tab bar when
				-- the window is not focused
				inactive_titlebar_bg = "{background}",

				-- The color of the inactive tab bar edge/divider
				-- inactive_tab_edge = "{background}",
			},
		},
		colors = {
			{
				foreground = "{foreground}",
				background = "{background}",
				cursor_bg = "{cursor}",
				cursor_border = "{cursor}",
				cursor_fg = "{background}",
				selection_bg = "{color1}",
				selection_fg = "{background}",

				ansi = { { "{color0}", "{color1}", "{color2}", "{color3}", "{color4}", "{color5}", "{color6}", "{color7}" } },
				brights = {
					{ "{color8}", "{color9}", "{color10}", "{color11}", "{color12}", "{color13}", "{color14}", "{color15}" },
				},
				tab_bar = {
					{
						background = "{background}",
						inactive_tab = {
							{
								bg_color = "{background}",
								fg_color = "{foreground}",
								intensity = "Normal",
								underline = "None",
								italic = false,
								strikethrough = false,
							},
						},
						active_tab = {
							{
								bg_color = "{color8}",
								fg_color = "{color7}",
								intensity = "Normal",
								underline = "None",
								italic = false,
								strikethrough = false,
							},
						},
						inactive_tab_hover = {
							{
								bg_color = "{color8}",
								fg_color = "{color7}",
								intensity = "Normal",
								underline = "Single",
								italic = false,
								strikethrough = false,
							},
						},
						new_tab = {
							{
								bg_color = "{color8}",
								fg_color = "{color7}",
								intensity = "Normal",
								underline = "None",
								italic = false,
								strikethrough = false,
							},
						},
						new_tab_hover = {
							{
								bg_color = "{background}",
								fg_color = "{color6}",
								intensity = "Normal",
								underline = "Single",
								italic = false,
								strikethrough = false,
							},
						},
					},
				},
			},
		},
	},
}
