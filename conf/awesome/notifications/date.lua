-- luacheck: globals awesome
local notifications = require("notifications")
local awful = require("awful")
local icons = require("icons")
local rtrim = require("utils.rtrim")

notifications.date = {}

local notif
local timeout = 2

awesome.connect_signal("dotfiles::date", function()
	awful.spawn.easy_async_with_shell("date +'%a %m/%d %-I:%M %p'", function(time)
		notif = notifications.notify_dwim({
			title = "Current Time:",
			message = "<b>" .. rtrim(time) .. "</b>",
			timeout = timeout,
			icon = icons.image.date,
			app_name = "date",
		}, notif)
	end)
end)
