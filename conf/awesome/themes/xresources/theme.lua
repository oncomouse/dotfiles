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

theme.bg_normal     = xrdb.background
theme.bg_focus      = xrdb.color14
theme.bg_urgent     = xrdb.color1
theme.bg_minimize   = xrdb.color8
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = xrdb.foreground
theme.fg_focus      = theme.bg_normal
theme.fg_urgent     = theme.bg_normal
theme.fg_minimize   = theme.bg_normal
theme.fg_icon       = xrdb.color7

theme.useless_gap   = 0
theme.border_width  = 2
theme.border_color_normal = xrdb.color0
theme.border_color_active = theme.bg_focus
theme.border_color_floating_normal = xrdb.color0
theme.border_color_floating_active = theme.border_color_active
theme.border_color_marked = xrdb.color10

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
theme.taglist_bg_occupied = xrdb.background
theme.taglist_fg_occupied = xrdb.color7
-- Tasklist formatting:
theme.tasklist_bg_focus = xrdb.background
theme.tasklist_fg_focus = xrdb.color7
-- Hotkey formatting:
theme.hotkeys_modifiers_fg = xrdb.color4

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
theme = theme_assets.recolor_layout(theme, xrdb.color7)

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

return theme
