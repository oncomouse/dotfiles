-- luacheck: globals awesome
local Block = require("widgets.block")

local charge_icons = {
	charging = "ﮣ",
	discharging = "ﮤ",
}

local icons = {
	charging = "",
	discharging = "",
}

return Block({
	signals = {
		["dotfiles::battery::status"] = function(update, level, charging)
			local icon = charging and icons.charging or icons.discharging
			local charge_icon = charging and charge_icons.charging or charge_icons.discharging
			update(icon .. " " .. tostring(level) .. " " .. charge_icon)
		end,
	},
	cb = "dotfiles::battery::request"
})
