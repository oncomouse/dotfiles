local awful = require("awful")

local function emit_player_status()
	local status_cmd = "playerctl status -F"

	-- Follow status
	awful.spawn.easy_async({
		"pkill", "--full", "--uid", os.getenv("USER"), "^playerctl status"
	}, function()
		awful.spawn.with_line_callback(status_cmd, {
			stdout = function(line)
				local status = false
				if line:find("Playing") then
					status = "play"
				elseif line:find("Paused") then
					status = "pause"
				else
					status = "stop"
				end
				awesome.emit_signal("dotfiles::playerctl::status", status)
			end
		})
	end)
end

local function emit_player_info()
		-- Command that lists artist and title in a format to find and follow
	local song_follow_cmd =
		"playerctl metadata --format 'artist_{{artist}}title_{{title}}' -F"

	-- Progress Cmds
	local prog_cmd = "playerctl position"
	local length_cmd = "playerctl metadata mpris:length"

	awful.widget.watch(prog_cmd, interval, function(_, interval)
		awful.spawn.easy_async_with_shell(length_cmd, function(length)
			local length_sec = tonumber(length) -- in microseconds
			local interval_sec = tonumber(interval) -- in seconds
			if length_sec and interval_sec then
				if interval_sec >= 0 and length_sec > 0 then
					awesome.emit_signal("bling::playerctl::position",
										interval_sec, length_sec / 1000000)
				end
			end
		end)
	end)

	-- Follow title
	awful.spawn.easy_async({
		"pkill", "--full", "--uid", os.getenv("USER"), "^playerctl metadata"
	}, function()
		awful.spawn.with_line_callback(song_follow_cmd, {
			stdout = function(line)
				local album_path = ""
				awful.spawn.easy_async_with_shell(art_script, function(out)
					-- Get album path
					album_path = out:gsub('%\n', '')
					-- Get title and artist
					local artist = line:match('artist_(.*)title_')
					local title = line:match('title_(.*)')
					-- If the title is nil or empty then the players stopped
					if title and title ~= "" then
						awesome.emit_signal(
							"dotfiles::playerctl::title_artist_album", title,
							artist, album_path)
					else
						awesome.emit_signal("dotfiles::playerctl::player_stopped")
					end
				end)
			end
		})
	end)
end

local play = function()
	awful.spawn("playerctl play-pause")
end
local stop = function()
	awful.spawn("playerctl stop")
end
local previous_track = function()
	awful.spawn("playerctl previous")
end
local next_track = function()
	awful.spawn("playerctl next")
end


local enable = function()
	emit_player_status()
	emit_player_info()
end

return {
	enable = enable,
	play = play,
	stop = stop,
	next_track = next_track,
	previous_track = previous_track
}
