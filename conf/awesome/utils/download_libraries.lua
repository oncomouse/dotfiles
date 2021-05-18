-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local function download_libraries(libraries)
	local run_count = 0
	local rc_tracker = 0
	local cb = function()
		rc_tracker = rc_tracker + 1
		if rc_tracker >= run_count then
			awesome.restart()
		end
	end
	local tail = function(x)
		local parts = gears.string.split(x, "/")
		return parts[#parts]
	end
	for _,gh_url in ipairs(libraries) do
		local dir = os.getenv("HOME") .. "/.config/awesome/" .. string.gsub(tail(gh_url), "%.", "_")
		if not gears.filesystem.is_dir(dir) then
			run_count = run_count + 1
			awful.spawn.with_line_callback("git clone https://github.com/" .. gh_url .. " " .. dir, {
				exit=cb
			})
		end
	end
end

return download_libraries
