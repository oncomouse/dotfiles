--[[
rofi.lua - rofi-powered menus for awesome wm
Usage:
rofi = require("rofi")
change width (optional, default 500px):
rofi.width=300
bind to key:
	awful.key({ modkey }, "f", function (c) rofi.client_flags(c) end)
--]]
local spawn = require("awful.spawn")
local beautiful = require("beautiful")

rofi = {width = 300}

function rofi.drun()
	spawn.with_shell(
		-- "rofi -sidebar-mode -show drun -match fuzzy -show-icons"
		string.format(
			"rofi -theme %s/dotfiles/conf/rofi/barmenu.rasi " ..
			"-match fuzzy -auto-select " ..
			"-show drun -show-icons -yoffset %d -drun-display-format '{name}'",
			os.getenv("HOME"), beautiful.bar_height
		)
	)
end

function rofi.window()
	spawn.with_shell(
		string.format(
			"rofi -theme %s/dotfiles/conf/rofi/barmenu.rasi " ..
			"-show window -show-icons " ..
			"-yoffset %d -window-format '{w} {c} {t:25}'",
			os.getenv("HOME"), beautiful.bar_height
		)
	)
end

function rofi.powermenu()
	spawn.with_shell(
		"~/dotfiles/scripts/rofi/powermenu/powermenu.sh"
	)
end

-- Source: https://gist.github.com/RobSis/7251f8eec0c63b32a1b0a7e37abd32d2
function rofi.client_flags(c)
	local opts = (c.maximized and "✓" or " ") .. " maximize\n" ..
		(c.floating and "✓" or " ") .. " floating\n" ..
		(c.fullscreen and "✓" or " ") .. " fullscreen\n" ..
		(c.sticky and "✓" or " ") .. " sticky\n" ..
		(c.ontop and "✓" or " ") .. " ontop\n" .. "  minimize\n"

	local w = math.min(rofi.width, c.width)
	local x = c.x
	local y = c.y

	local cmd = "echo -e \"" .. opts .. "\" | " ..
		" rofi -dmenu -matching fuzzy -auto-select" ..
		" -p flags: -location 1 -width " .. w .. " -xoffset " .. x ..
		" -yoffset " .. y
	spawn.easy_async({"bash", "-c", cmd}, function(output)
		for k in string.gmatch(output, ". (%S+)") do
			if k == "sticky" then
				c.sticky = not c.sticky
			elseif k == "ontop" then
				c.ontop = not c.ontop
			elseif k == "floating" then
				c.floating = not c.floating
			elseif k == "maximize" then
				c.maximized = not c.maximized
				c:raise()
			elseif k == "fullscreen" then
				c.fullscreen = not c.fullscreen
				c:raise()
			elseif k == "minimize" then
				c.minimized = true
			end
		end
	end)
end

return rofi
