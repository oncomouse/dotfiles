[Alternate Install](https://wiki.archlinux.org/index.php/User:Altercation/Bullet_Proof_Arch_Install#Objectives)

Notes:
1. Don't use systemd-start stuff, just use `arch-chroot /mnt`
1. maximevince's note is correct
1. In example `refind` config, image name should be `initramfs-linux-zen.img` (No slash)

Additional Packages to install, before reboot:

`pacman -S refind base-devel vim git curl fish`

1. Configure [sudo](https://wiki.archlinux.org/index.php/Sudo)

# User

useradd -m -G wheel -s /usr/bin/fish andrew

# Network Configuration

/etc/systemd/network/20-wired.network:

> [Match]
> Name=en*
> Name=eth*
>  
> [Network]
> DHCP=yes

/etc/systemd/network/20-wireless.network:

> [Match]
> Name=wlp*
> Name=wlan*
> 
> [Network]
> DHCP=yes

1. systemctl enable systemd-networkd.service
1. systemctl enable systemd-resolved.service
