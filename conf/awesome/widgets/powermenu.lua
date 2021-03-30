-- luacheck: globals awesome gears screen x
local awful = require('awful')
local gears = require('gears')
local wibox = require('wibox')
local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi
-- local default_apps = require('configurations.default-apps')
local clickable_container = require('widgets.clickable-container')
-- local widget_icon_dir = beautiful.theme_path .. 'icons/system/'

local default_apps = {
	lock_screen = 'xscreensaver-command -lock',
}

local profile_name = wibox.widget {
	markup = 'user@hostname',
	font = 'FiraCode Nerd Font 16',
	align = 'center',
	valign = 'center',
	widget = wibox.widget.textbox
}

local uptimebox = wibox.widget {
	resize = true,
	text = '',
	font = 'FiraCode Nerd Font 16',
	widget = wibox.widget.textbox
}

local update_uptime = function()
	awful.spawn.easy_async_with_shell(
		"uptime -p | sed -e 's/up //g'",
		function(stdout)
			stdout = stdout:gsub('%\n','')
			uptimebox:set_markup_silently('<u>System Uptime</u>: ' .. stdout)
			uptimebox:emit_signal('widget::redraw_needed')
		end
	)
end

update_uptime()

-- local update_profile_pic = function()
-- 	awful.spawn.easy_async_with_shell(
-- 		"/home/devops/.config/awesome/utilities/profile-image",
-- 		function(stdout)
-- 			stdout = stdout:gsub('%\n','')
-- 			if not stdout:match('default') then
-- 				profile_imagebox:set_image(stdout)
-- 			else
-- 				profile_imagebox:set_image(widget_icon_dir .. 'defult.png')
-- 			end
-- 			profile_imagebox:emit_signal('widget::redraw_needed')
-- 		end
-- 	)
-- end

-- update_profile_pic()

local update_user_name = function()
	awful.spawn.easy_async_with_shell(
		[[
		fullname="$(getent passwd `whoami` | cut -d ':' -f 5 | cut -d ',' -f 1 | tr -d "\n")"
		if [ -z "$fullname" ];
		then
				printf "$(whoami)@$(hostname)"
		else
			printf "$fullname"
		fi
		]],
		function(stdout)
			profile_name:set_markup(stdout)
			profile_name:emit_signal('widget::redraw_needed')
		end
	)
end

update_user_name()

local build_power_button = function(name, icon, callback)
	local power_button_label= wibox.widget {
		text = name,
		font = 'FiraCode Nerd Font 16',
		align = 'center',
		valign = 'center',
		widget = wibox.widget.textbox
	}

	local power_button = wibox.widget {
		{
			{
				{
					{
						markup = icon,
						font = 'FiraCode Nerd Font 36',
						widget = wibox.widget.textbox
					},
					margins = dpi(8),
					widget = wibox.container.margin
				},
				bg = x.color8,
				fg = x.color7,
				widget = wibox.container.background
			},
			shape = gears.shape.circle,
			forced_width = dpi(64),
			forced_height = dpi(64),
			widget = clickable_container
		},
		left = dpi(24),
		right = dpi(24),
		widget = wibox.container.margin
	}

	local exit_screen_item = wibox.widget {
		layout = wibox.layout.fixed.vertical,
		spacing = dpi(5),
		power_button,
		power_button_label
	}

	exit_screen_item:connect_signal(
		'button::release',
		function()
			callback()
		end
	)
	return exit_screen_item
end


local cancel_button = wibox.widget{
	widget = clickable_container,
	bg = x.color8,
	fg = x.color7,
	border_width = dpi(1),
	border_color = beautiful.border_button,
	shape = function (cr, height, width)
		gears.shape.rounded_rect(cr, height, width, 8)
	end,
	{
		top = dpi(5),
		bottom = dpi(5),
		left = dpi(8),
		right = dpi(8),
		widget = wibox.container.margin,
		{
			text = "Cancel",
			font = "FiraCode Nerd Font 12",
			widget = wibox.widget.textbox
		}
	}
}

cancel_button:connect_signal("button::press", function (_,_,_, button)
	if button == 1 then
		awesome.emit_signal('module::exit_screen:hide')
	end
end)


local suspend_command = function()
	awesome.emit_signal('module::exit_screen:hide')
	awful.spawn.with_shell(default_apps.lock_screen .. ' & systemctl suspend')
end

local logout_command = function()
	awesome.quit()
end

local lock_command = function()
	awesome.emit_signal('module::exit_screen:hide')
	awful.spawn.with_shell(default_apps.lock_screen)
end

local poweroff_command = function()
	awful.spawn.with_shell('poweroff')
	awesome.emit_signal('module::exit_screen:hide')
end

local reboot_command = function()
	awful.spawn.with_shell('reboot')
	awesome.emit_signal('module::exit_screen:hide')
end

local poweroff = build_power_button('Shutdown', '襤', poweroff_command)
local reboot = build_power_button('Restart', '勒', reboot_command)
local suspend = build_power_button('Sleep', '鈴', suspend_command)
local logout = build_power_button('Logout', '', logout_command)
local lock = build_power_button('Lock', '', lock_command)

local create_exit_screen = function(s)
	s.exit_screen = wibox
	{
		widget = {},
		type = 'splash',
		visible = false,
		ontop = true,
		bg = x.color0,
		fg = x.color7,
		height = s.geometry.height,
		width = s.geometry.width,
		maximum_width = s.geometry.width,
		maximum_height = s.geometry.height,
		minimum_width = s.geometry.width,
		minimum_height = s.geometry.height,
		placement = awful.placement.center,
		x = s.geometry.x,
		y = s.geometry.y
	}

	s.exit_screen : setup {
		layout = wibox.layout.align.vertical,
		expand = 'none',
		nil,
		{
			layout = wibox.layout.align.vertical,
			{
				nil,
				{
					layout = wibox.layout.fixed.vertical,
					spacing = dpi(5),
					{
						layout = wibox.layout.align.vertical,
						expand = 'none',
						nil,
						{
							layout = wibox.layout.align.horizontal,
							expand = 'none',
							nil,
							{
								uptimebox,
								fg = x.color8,
								bg = x.color7,
								widget = wibox.container.background,
							},
							nil
						},
						nil
					},
					profile_name
				},
				nil,
				expand = 'none',
				layout = wibox.layout.align.horizontal
			},
			{
				layout = wibox.layout.align.horizontal,
				expand = 'none',
				nil,
				{
					{
						{
							poweroff,
							reboot,
							lock,
							suspend,
							logout,
							layout = wibox.layout.fixed.horizontal
						},
						spacing = dpi(30),
						layout = wibox.layout.fixed.vertical
					},
					widget = wibox.container.margin,
					margins = dpi(30)
				},
			},
			{
				layout = wibox.layout.align.horizontal,
				expand = 'none',
				nil,
				{
					widget = wibox.container.margin,
					margins = dpi(5),
					cancel_button
				},
				nil
			},
		},
		nil
	}

	s.exit_screen:buttons(
		gears.table.join(
			awful.button(
				{},
				2,
				function()
					awesome.emit_signal('module::exit_screen:hide')
				end
				),
			awful.button(
				{},
				3,
				function()
					awesome.emit_signal('module::exit_screen:hide')
				end
			)
		)
	)
end


screen.connect_signal(
	'request::desktop_decoration',
	function(s)
		create_exit_screen(s)
	end
)

screen.connect_signal(
	'removed',
	function(s)
		create_exit_screen(s)
	end
)

local exit_screen_grabber = awful.keygrabber {
	auto_start = true,
	stop_event = 'release',
	keypressed_callback = function(_, _, key, _) -- self, mod, key, command
		if key == 's' then
			suspend_command()

		elseif key == 'e' then
			logout_command()

		elseif key == 'l' then
			lock_command()

		elseif key == 'p' then
			poweroff_command()

		elseif key == 'r' then
			reboot_command()

		elseif key == 'Escape' or key == 'q' or key == 'x' then
			awesome.emit_signal('module::exit_screen:hide')
		end
	end
}

awesome.connect_signal(
	'module::exit_screen:show',
	function()
		for s in screen do
			s.exit_screen.visible = false
		end
		awful.screen.focused().exit_screen.visible = true
		exit_screen_grabber:start()
	end
)

awesome.connect_signal(
	'module::exit_screen:hide',
	function()
		exit_screen_grabber:stop()
		for s in screen do
			s.exit_screen.visible = false
		end
	end
)

