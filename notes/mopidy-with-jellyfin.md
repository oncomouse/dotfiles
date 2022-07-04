# Installing MPD + Jellyfin

1. Install `mopidy`, `mopidy-mpd`, and `mopidy-jellyfin`.
1. Run `stow mopidy`.
1. Copy `~/.config/mopidy/mopidy.conf.template` to `~/.config/mopidy/mopidy.conf`
1. Replace `<USERNAME>`, `<PASSWORD>`, and `<URL>` with the server
1. Run `systemctl --user daemon-reload` and `systemctl --user enable --now mopidy.service`
