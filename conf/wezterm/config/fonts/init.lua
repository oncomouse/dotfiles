local DOTFILES_TARGET = require("config.env").DOTFILES_TARGET

local function get_fonts()
	local ok, config = pcall(require, "config.fonts." .. DOTFILES_TARGET)
	if ok then
		return config
	end
	return {}
end

return get_fonts()
