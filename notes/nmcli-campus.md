```
# nmcli con add type wifi ifname wlp3s0 con-name tamu ssid TAMU_WiFi
# nmcli con edit id TAMU_WiFi
nmcli> set ipv4.method auto
nmcli> set 802-1x.eap peap
nmcli> set 802-1x.phase2-auth mschapv2
nmcli> set 802-1x.identity myusername
nmcli> set 802-1x.password mypassword
nmcli> set wifi-sec.key-mgmt wpa-eap
nmcli> set connection.autoconnect yes
nmcli> save
nmcli> activate
```
