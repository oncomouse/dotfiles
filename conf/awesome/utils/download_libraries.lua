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
		local opts = {}
		if type(gh_url) == "table" then
			local u = gh_url[1]
			table.remove(gh_url, 1)
			opts = gears.table.clone(gh_url)
			gh_url = u
		end
		local is_url = string.find(gh_url, 'http%l*:')
		if is_url == nil then
			local base_dir = opts.as == nil and string.gsub(tail(gh_url), "%.", "_") or opts.as
			local dir = os.getenv("HOME") .. "/.config/awesome/" .. base_dir
			if not gears.filesystem.is_dir(dir) then
				run_count = run_count + 1
				awful.spawn.with_line_callback("git clone https://github.com/" .. gh_url .. " " .. dir, {
					exit=cb
				})
			end
		else
			local file = os.getenv("HOME") .. "/.config/awesome/" .. tail(gh_url)
			if not gears.filesystem.file_readable(file) then
				run_count = run_count + 1
				awful.spawn.with_line_callback("curl -so " .. file .. " " ..gh_url, {
					exit=cb
				})
			end
		end
	end
end

return download_libraries
