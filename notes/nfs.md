1. `sudo pacman -S nfs-utils`
2. `echo N > /sys/module/nfs/parameters/nfs4_disable_idmapping`
3. Create a mount file in `/etc/systemd/system/mnt-<mountpoint>.mount`
        * This would be for `/mnt/<mountpoint>`; filename must match path
4. `sudo systemctl enable --now <file>.mount`

## `/etc/modprobe.d/nfs.conf`

```
options nfs nfs4_disable_idmapping=0
options nfsd nfs4_disable_idmapping=0
```

## `/etc/systemd/system/mnt-<mountpoint>.mount`

```
[Unit]
Description=Mount Share at boot

[Mount]
What=<ip addr>:/<share>
Where=/mnt/<mountpoint>
Options=vers=4
Type=nfs
TimeoutSec=30

[Install]
WantedBy=multi-user.target
```
