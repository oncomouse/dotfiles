-- luacheck: globals hs
local string_literal = require("utils/string_literal")
function show_spotify_song()
	hs.alert.show(
		string_literal([[$artist - $song

*$album*]], {
			artist = hs.spotify.getCurrentArtist(),
			album = hs.spotify.getCurrentAlbum(),
			song = hs.spotify.getCurrentTrack(),
		}),
		{
			textStyle = {
				paragraphStyle = { alignment = "center" },
			},
		}
	)
end

return show_spotify_song
