conky.config = { 
	out_to_x = false,
	out_to_console = true,
	update_interval = 1,
}

conky.text = [[
 ${execi 5 xbacklight | cut -d . -f 1}% ${if_match ${battery_percent} > 75}$else${if_match ${battery_percent} > 50}$else${if_match ${battery_percent} > 25}$else${endif}${endif}${endif}  ${battery_percent}%
]]

-- vim:ft=lua

