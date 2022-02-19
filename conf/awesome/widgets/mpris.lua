local awful = require("awful")
local Block = require("widgets.block")

local icons = {
	PLAYING = "契",
	PAUSED = "",
	STOPPED = "栗",
}
local function escape_xml(str)
	str = string.gsub(str, "&", "&amp;")
	str = string.gsub(str, "<", "&lt;")
	str = string.gsub(str, ">", "&gt;")
	str = string.gsub(str, "'", "&apos;")
	str = string.gsub(str, '"', "&quot;")

	return str
end

return Block({
	buttons = {
		[awful.button.names.LEFT] = function()
			awesome.emit_signal("dotfiles::mpris::action", "play_pause")
		end,
		[awful.button.names.MIDDLE] = function()
			awesome.emit_signal("dotfiles::mpris::action", "previous")
		end,
		[awful.button.names.RIGHT] = function()
			awesome.emit_signal("dotfiles::mpris::action", "next")
		end,
	},
	callback = "dotfiles::mpris::request",
	signals = {
		["dotfiles::mpris::update"] = function(update, status, metadata)
			if status == "STOPPED" then
				update("")
				return
			end
			update(escape_xml(icons[status] .. " " .. metadata.artist .. " - " .. metadata.title))
		end,
	},
})
