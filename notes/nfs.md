1. `sudo pacman -S nfs-utils`
2. `echo N > /sys/module/nfs/parameters/nfs4_disable_idmapping`
3. Create a mount file in `/etc/systemd/system/mnt-<mountpoint>.mount`
        * This would be for `/mnt/<mountpoint>`; filename must match path
4. `sudo systemctl enable --now network-online.service`
5. `sudo systemctl enable --now <file>.mount`

## `/etc/modprobe.d/nfs.conf`

```
options nfs nfs4_disable_idmapping=0
options nfsd nfs4_disable_idmapping=0
```

## `/etc/systemd/system/mnt-<mountpoint>.mount`

```
[Unit]
Description=Mount Share at boot
Requires=network-online.service
After=network-online.service

[Mount]
What=<ip addr>:/<share>
Where=/mnt/<mountpoint>
Options=vers=4
Type=nfs
TimeoutSec=30

[Install]
WantedBy=multi-user.target
```

## Make the service wait until NetworkManager is online:

```
[Unit]
Description=Wait until NM actually online
Requires=NetworkManager-wait-online.service
After=NetworkManager-wait-online.service

[Service]
Type=oneshot
ExecStart=/usr/bin/nm-online -q --timeout=120
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
```
