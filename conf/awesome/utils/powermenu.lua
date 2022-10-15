local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local commands = {
	Lock = "xscreensaver-command -lock",
	Suspend = "xscreensaver-command -lock && xset dpms force off",
	Logoff = "echo 'awesome.quit()' | awesome-client",
	Reboot = "reboot",
	Shutdown = "poweroff",
}
local powermenu_cmd = 'echo "'
	.. table.concat(gears.table.keys(commands), "\n")
	.. '" | '
	.. 'rofi -dmenu -theme themes/bar-menu.rasi -match fuzzy -auto-select -i -p Powermenu -font "'
	.. beautiful.font
	.. '"'
local function powermenu()
	awful.spawn.easy_async_with_shell(powermenu_cmd, function(stdout)
		stdout = stdout:gsub("^%s*(.-)%s*$", "%1")
		local cmd = commands[stdout] or ""
		awful.spawn.with_shell(cmd)
	end)
end
return powermenu
