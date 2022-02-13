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

local function make_output(update)
	local icon = update.state.charging and icons.charging or icons.discharging
	local charge_icon = update.state.charging and charge_icons.charging or charge_icons.discharging
	update(icon .. " " .. tostring(update.state.level) .. "%" .. charge_icon)
end

return Block({
	signals = {
		["dotfiles::battery::update"] = function(update)
			make_output(update)
		end,
		["dotfiles::battery::charger"] = function(update, charging)
			update.state.charging = charging
			make_output(update)
		end,
		["dotfiles::battery::level"] = function(update, level)
			update.state.level = level
			make_output(update)
		end,
	},
	cb = "dotfiles::battery::request",
})
