# Setting Up Owntone on Arch

1. Add user `owntone` to groups `audio` and `media` (or whoever is the group owner of your media directory):
	1. `sudo usermod -a -G audio owntone`
	1. `sudo usermod -a -G media owntone`
1. Start/enable Avahi
	1. `sudo systemctl enable --now avahi-daemon.service`
1. UFW Settings:
	1. `sudo ufw allow 3689`
	1. `sudo ufw allow mdns comment 'mDNS'`
