-- luacheck: globals awesome
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
		[1] = function()
			awesome.emit_signal("dotfiles::mpris::action", "play_pause")
		end,
		[2] = function()
			awesome.emit_signal("dotfiles::mpris::action", "previous")
		end,
		[3] = function()
			awesome.emit_signal("dotfiles::mpris::action", "next")
		end,
	},
	cb = "dotfiles::mpris::request",
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
