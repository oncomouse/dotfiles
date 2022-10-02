1. Install smbclient (`sudo pacman -S smbclient`)
2. Create a mount file in `/etc/systemd/system/`
3. Create a credentials file in `/etc/samba/credentials`
        1. `sudo chmod 600 <file>`
4. `sudo systemctl enable --now <file>.mount`

## Mount file format:

```
[Unit]
Description=Mount Share at boot

[Mount]
What=//<ip addr>/<share>
Where=<local mount>
Options=_netdev,credentials=<credentials file>,iocharset=utf8,rw,uid=1000,gid=1000
Type=cifs
TimeoutSec=30

[Install]
WantedBy=multi-user.target
```

## Credentials File

```
username=<USERNAME>
password=<PASSWORD>
```
