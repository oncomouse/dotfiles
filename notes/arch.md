Additional Packages to install, before reboot:

`pacman -S grub base-devel vim git curl fish`

1. Configure [grub](https://wiki.archlinux.org/index.php/GRUB)
1. Configure [sudo](https://wiki.archlinux.org/index.php/Sudo)

# User

useradd -m andrew

# Network Configuration

/etc/systemd/network/20-wired.network:

> [Match]
> Name=en*
>  
> [Network]
> DHCP=yes

1. systemctl enable systemd-networkd.service
1. systemctl enable systemd-resolved.service
