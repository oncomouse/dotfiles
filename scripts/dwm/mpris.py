import dbus


def truncate(data, length=35):
    return data[:length] + "…" if len(data) > length else data


session_bus = dbus.SessionBus()
players = []
for name in session_bus.list_names():
    if name.startswith("org.mpris.MediaPlayer2"):
        players.append(name)
if len(players) == 0:
    print("")
else:
    output = ""
    for player in players:
        player_bus = session_bus.get_object(player, "/org/mpris/MediaPlayer2")
        player_properties = dbus.Interface(
            player_bus, "org.freedesktop.DBus.Properties"
        )
        status = player_properties.Get(
            "org.mpris.MediaPlayer2.Player", "PlaybackStatus"
        )
        if status not in ("Playing", "Paused"):
            output = "栗"
            continue
        metadata = player_properties.Get("org.mpris.MediaPlayer2.Player", "Metadata")
        if status == "Playing":
            output = "ﱘ"
        else:
            output = ""
        # To just print the title
        output += truncate(
            " {} - {}".format(metadata["xesam:artist"][0], metadata["xesam:title"])
        )
    print(output + " ⁞ ")
