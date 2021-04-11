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
local naughty = require("naughty")
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
	-- local r = coroutine.create(function()
	-- 	local theme = "full_square"
	-- 	local dir = os.getenv('HOME') .. "/dotfiles/conf/rofi/powermenu"
	-- 	local uptime = io.popen("uptime -p | sed -s 's/up //g'"):read("*all")
	-- 	local rofi_command = "rofi -theme " .. dir .. "/" .. theme
	-- 	local pause_command = "playerctl pause"
	-- 	local mute_command = "ponymix mute"
	-- 	local function base_command(command)
	-- 		return io.popen("basename " .. gears.string.split(command, " ")):read("*all")
	-- 	end
	-- 	local function run_command_if_found(command)
	-- 		cmd = base_command(command)
	-- 		if #io.popen("command -v " .. cmd):read("*all") then
	-- 			awful.spawn(command)
	-- 		end
	-- 	end
	-- 	local function confirm_exit()
	-- 		return trim(io.popen(string.format([[ rofi -dmenu\
	-- 		-i \
	-- 		-no-fixed-num-lines\
	-- 		-p "Are You Sure? : "\
	-- 		-theme %s/confirm.rasi \
	-- 		-font "FiraCode Nerd Font 16"
	-- 		]], dir)):read("*all"))
	-- 	end
	-- 	local function msg()
	-- 		awful.spawn('rofi -theme "' .. dir .. '/message.rasi" -e "Available Options  -  yes / y / no / n"')
	-- 	end
	-- 	local function confirm_executation(cmd)
	-- 		local answer = confirm_exit()
	-- 		if answer == "yes" or answer == "YES" or answer == "y" or answer == "Y" then
	-- 			awful.spawn(cmd)
	-- 			return true
	-- 		elseif answer == "no" or answer == "NO" or answer == "n" or answer == "NO" then
	-- 			return false
	-- 		else
	-- 			msg()
	-- 			return false
	-- 		end
	-- 	end

	-- 	local shutdown="襤"
	-- 	local reboot="勒"
	-- 	local lock=""
	-- 	local suspend="鈴"
	-- 	local logout=""
	-- 	local options = shutdown .. "\n" .. reboot .. "\n" .. lock .. "\n" .. suspend .. "\n" .. logout
	-- 	local chosen = io.popen('echo -e "'..options..'" | '..rofi_command .. ' -p "Uptime :' .. uptime .. '" -dmenu -selected-row 2'):read("*all")
	-- 	if gears.string.startswith(chosen, shutdown) then
	-- 		confirm_executation("systemctl poweroff")
	-- 	elseif gears.string.startswith(chosen, reboot) then
	-- 		confirm_executation("systemctl reboot")
	-- 	elseif gears.string.startswith(chosen, lock) then
	-- 		naughty.notify({text=chosen .. tostring(gears.string.startswith(chosen, lock))})
	-- 		-- run_command_if_found(pause_command)
	-- 		-- run_command_if_found(mute_command)
	-- 		awful.spawn("xscreensaver-command -lock")
	-- 	elseif gears.string.startswith(chosen, suspend) then
	-- 		if confirm_executation("xset dpms force off") then
	-- 			run_command_if_found(pause_command)
	-- 			run_command_if_found(mute_command)
	-- 			awful.spawn("xscreensaver-command -lock")
	-- 		end
	-- 	elseif gears.string.startswith(chosen, logout) then
	-- 		confirm_executation('echo "awesome.quit()" | awesome-command')
	-- 	end
	-- end)
	-- coroutine.resume(r)

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

return rofi
