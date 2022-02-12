-- luacheck: globals awesome
local notifications = require("notifications")
local awful = require("awful")

-- Source: https://github.com/blitmap/lua-snippets/blob/d02813ef60e956a249ee7dfb8d576981f8ba1cf4/string-trim.lua
local rtrim = function(s)
	local res = s
	local tmp = string.find(res, "%S%s*$")

	if not tmp then
		res = ""
	elseif tmp ~= #res then
		res = string.sub(res, 1, tmp)
	end

	return res, res ~= s
end

notifications.date = {}

local notif
local timeout = 2

awesome.connect_signal("dotfiles::date", function()
	awful.spawn.easy_async_with_shell("date +'%a %m/%d %-I:%M %p'", function(time)
		-- menubar.utils.rtrim(time)
		notif = notifications.notify_dwim({
			title = "Current Time:",
			message = "<b>" .. rtrim(time) .. "</b>",
			timeout = timeout,
			app_name = "clock",
		}, notif)
	end)
end)
