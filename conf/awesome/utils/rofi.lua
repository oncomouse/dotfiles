-- luacheck: globals screen
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
local gears = require("gears")
local awful = require("awful")
local trim = require("utils.trim")

rofi = {
	width = 300
}

function rofi.drun()
	spawn.with_shell(
		-- "rofi -sidebar-mode -show drun -match fuzzy -show-icons"
		string.format(
			"rofi -theme %s/dotfiles/conf/rofi/barmenu.rasi " ..
			"-match fuzzy -auto-select " ..
			"-show drun -show-icons -yoffset %d -drun-display-format '{name}'",
			os.getenv("HOME"), screen[1].mywibox.visible and beautiful.bar_height or 0
		)
	)
end

function rofi.window()
	spawn.with_shell(
		string.format(
			"rofi -theme %s/dotfiles/conf/rofi/barmenu.rasi " ..
			"-show window -show-icons " ..
			"-yoffset %d -window-format '{w} {c} {t:25}'",
			os.getenv("HOME"), screen[1].mywibox.visible and beautiful.bar_height or 0
		)
	)
end

function rofi.powermenu()

	-- Rofi Setup:
	local theme = "full_square"
	local dir = os.getenv('HOME') .. "/dotfiles/conf/rofi/powermenu"
	-- Commands:
	local rofi_command = "rofi -theme " .. dir .. "/" .. theme
	local pause_command = "playerctl pause"
	local mute_command = "ponymix mute"
	local shutdown_command = "systemctl shutdown"
	local reboot_command = "systemctl reboot"
	local lock_command = string.format([[%s &&
		%s &&
		xscreensaver-command -lock]], pause_command, mute_command)
	local suspend_command = lock_command .. "&& xset dpms force off"
	local logout_command = "echo 'awesome.quit() | awesome-command'"
	-- Icons:
	local shutdown_icon="襤"
	local reboot_icon="勒"
	local lock_icon=""
	local suspend_icon="鈴"
	local logout_icon=""

	local function msg()
		awful.spawn('rofi -theme "' .. dir .. '/message.rasi" -e "Available Options  -  yes / y / no / n"')
	end

	local function confirm_executation(cmd)
		awful.spawn.easy_async_with_shell(string.format([[ rofi -dmenu\
		-i \
		-no-fixed-num-lines \
		-p "Are You Sure? : " \
		-theme %s/confirm.rasi \
		-font "FiraCode Nerd Font 16"
		]], dir), function(answer)
			answer = trim(answer)
			if answer == "yes" or answer == "YES" or answer == "y" or answer == "Y" then
				awful.spawn.with_shell(cmd)
				return true
			elseif answer == "no" or answer == "NO" or answer == "n" or answer == "NO" then
				return false
			else
				msg()
				return false
			end
		end)
	end

	local options = shutdown_icon .. "\n" .. reboot_icon .. "\n" .. lock_icon .. "\n" .. suspend_icon .. "\n" .. logout_icon
	-- Grab uptime for the prompt:
	awful.spawn.easy_async_with_shell("uptime -p | sed -s 's/up //g'", function(uptime)
		-- Run the main rofi menu:
		awful.spawn.easy_async('bash -c \'echo -e "'..options..'" | '..rofi_command .. ' -p "Uptime :' .. uptime .. '" -dmenu -selected-row 2\'', function(chosen)
			-- Parse output:
			if gears.string.startswith(chosen, shutdown_icon) then
				confirm_executation(shutdown_command)
			elseif gears.string.startswith(chosen, reboot_icon) then
				confirm_executation(reboot_command)
			elseif gears.string.startswith(chosen, lock_icon) then
				awful.spawn.with_shell(lock_command)
			elseif gears.string.startswith(chosen, suspend_icon) then
				confirm_executation(suspend_command)
			elseif gears.string.startswith(chosen, logout_icon) then
				confirm_executation(logout_command)
			end
		end)
	end)
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

	local cmd = "printf '%s' \"" .. opts .. "\" | " ..
		" rofi -dmenu -matching fuzzy -auto-select" ..
		" -p flags: -location 1 -width " .. w .. " -xoffset " .. x ..
		" -yoffset " .. y
	spawn.easy_async_with_shell(cmd, function(output)
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

function rofi.networkmanager()
	awful.spawn("networkmanager_dmenu")
end

return rofi
