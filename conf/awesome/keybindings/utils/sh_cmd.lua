local awful = require("awful")
local function sh_cmd(cmd)
	return function()
		awful.spawn({ awful.util.shell, "-c", cmd })
	end
end

return sh_cmd
