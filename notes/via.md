# To Configure [via](https://usevia.app)

* `touch /etc/udev/rules.d/99-vial.rules`
* Add: `KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0660", TAG+="uaccess", TAG+="udev-acl", GROUP="1000"`
* Run: `sudo udevadm control --reload-rules && sudo udevadm trigger`
* Access [via](https://usevia.app) in Brave
