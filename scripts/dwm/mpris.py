import dbus


def truncate(data, length=35):
    return data[:length] + "…" if len(data) > length else data


session_bus = dbus.SessionBus()
players = []
for name in session_bus.list_names():
    if name.startswith("org.mpris.MediaPlayer2"):
        players.append(name)
output = "栗"
if len(players) != 0:
    active_player = ""
    for player in players:
        player_bus = session_bus.get_object(player, "/org/mpris/MediaPlayer2")
        player_properties = dbus.Interface(
            player_bus, "org.freedesktop.DBus.Properties"
        )
        s = player_properties.Get("org.mpris.MediaPlayer2.Player", "PlaybackStatus")
        if s == "Playing" or (s == "Paused" and active_player == ""):
            player_properties = dbus.Interface(
                player_bus, "org.freedesktop.DBus.Properties"
            )
            metadata = player_properties.Get(
                "org.mpris.MediaPlayer2.Player", "Metadata"
            )
            output = "契" if s == "Playing" else ""
            artist = (
                "{} - ".format(metadata["xesam:artist"][0])
                if len(metadata["xesam:artist"][0]) > 0
                else ""
            )
            output += " {}{}".format(artist, metadata["xesam:title"])
print(truncate(output) + " ⁞ ")
