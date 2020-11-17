Additional Packages to install, before reboot:

`pacman -S grub base-devel vi git curl fish`

1. Configure [grub](https://wiki.archlinux.org/index.php/GRUB)
1. Configure [sudo](https://wiki.archlinux.org/index.php/Sudo)

# Network Configuration

/etc/systemd/network/20-wired.network:

> [Match]
> Name=enp0s3
>  
> [Network]
> DHCP=yes

1. systemctl enable dhcpcd@enp0s3.service
1. systemctl enable systemd-networkd.service
1. systemctl enable systemd-resolved.service
1. systemctl start systemd-resolved.service
1. systemctl start systemd-networkd.service
