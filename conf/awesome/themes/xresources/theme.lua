---------------------------------------------
-- Awesome theme which follows xrdb config --
--   by Yauhen Kirylau                     --
--   modified by Andrew Pilsch             --
---------------------------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local rnotification = require("ruled.notification")
local dpi = xresources.apply_dpi
local xrdb = xresources.get_current_theme()
local themes_path = require("gears.filesystem").get_themes_dir()

-- inherit default theme
local theme = dofile(themes_path.."default/theme.lua")
-- load vector assets' generators for this theme

theme.font          = "sans 8"

-- Attach XRDB to the theme instead of having to keep looking it up:
theme.background = xrdb.background
theme.foreground = xrdb.foreground
theme.color0     = xrdb.color0
theme.color1     = xrdb.color1
theme.color2     = xrdb.color2
theme.color3     = xrdb.color3
theme.color4     = xrdb.color4
theme.color5     = xrdb.color5
theme.color6     = xrdb.color6
theme.color7     = xrdb.color7
theme.color8     = xrdb.color8
theme.color9     = xrdb.color9
theme.color10    = xrdb.color10
theme.color11    = xrdb.color11
theme.color12    = xrdb.color12
theme.color13    = xrdb.color13
theme.color14    = xrdb.color14
theme.color15    = xrdb.color15

theme.bg_normal     = theme.background
theme.bg_focus      = theme.color14
theme.bg_urgent     = theme.color1
theme.bg_minimize   = theme.color8
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = theme.foreground
theme.fg_focus      = theme.bg_normal
theme.fg_urgent     = theme.bg_normal
theme.fg_minimize   = theme.bg_normal
theme.fg_icon       = theme.color7

theme.useless_gap   = 0
theme.border_width  = 2
theme.border_color_normal = theme.color0
theme.border_color_active = theme.bg_focus
theme.border_color_floating_normal = theme.color0
theme.border_color_floating_active = theme.border_color_active
theme.border_color_marked = theme.color10

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
-- Taglist Formatting:
theme.taglist_bg_occupied = theme.background
theme.taglist_fg_occupied = theme.color7
-- Tasklist formatting:
theme.tasklist_bg_focus = theme.background
theme.tasklist_fg_focus = theme.color7
-- Hotkey formatting:
theme.hotkeys_modifiers_fg = theme.color4

theme.titlebar_bg_normal = theme.background
theme.titlebar_fg_normal = theme.color7
theme.titlebar_bg_focus = theme.color8
theme.titlebar_fg_focus = theme.foreground

theme.tooltip_fg = theme.fg_normal
theme.tooltip_bg = theme.bg_normal

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(16)
theme.menu_width  = dpi(100)

theme.icon_size              = dpi(16)
theme.notification_icon_size = dpi(16)

-- Recolor Layout icons:
theme = theme_assets.recolor_layout(theme, theme.color7)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Generate taglist squares:
-- local taglist_square_size = dpi(4)
theme.taglist_squares_sel = nil
theme.taglist_squares_unsel = nil

-- Set different colors for urgent notifications.
rnotification.connect_signal('request::rules', function()
    rnotification.append_rule {
        rule       = { urgency = 'critical' },
        properties = { bg = '#ff0000', fg = '#ffffff' }
    }
end)

-- Custom Notification stuff
theme.notification_position = "top_right"
theme.notification_border_width = 1
theme.notification_border_radius = 0
theme.notification_border_color = theme.color10
-- theme.notification_bg = theme.color7
-- theme.notification_fg = theme.color0
theme.notification_crit_bg = theme.color11
theme.notification_crit_fg = theme.color0
theme.notification_margin = dpi(15)
theme.notification_icon_size = dpi(50)
--theme.notification_height = dpi(80)
--theme.notification_width = dpi(300)
--theme.notification_opacity = 0.7
theme.notification_font = theme.font
-- theme.notification_padding = theme.screen_margin * 2
-- theme.notification_spacing = theme.screen_margin * 2

return theme
