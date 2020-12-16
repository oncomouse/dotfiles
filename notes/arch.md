[Alternate Install](https://wiki.archlinux.org/index.php/User:Altercation/Bullet_Proof_Arch_Install#Objectives)
[IWD Wifi instructions](https://sudaraka.org/how-to/archlinux-installation-guide-2019/)

Notes:
1. Don't use systemd-start stuff, just use `arch-chroot /mnt`
1. `mkinitcpio -p linux` should be `mkinitcpio -p linux-zen`
1. Always use `/dev/mapper/cryptswap`
1. In example `refind` config, image name should be `initramfs-linux-zen.img`

Additional Packages to install, before reboot:

`pacman -S refind base-devel vim git curl fish dhcpcd btrfs-progs iw gptfdisk iwd terminus-font`

Set `/etc/vconsole.conf` to:

> FONT=ter-v18n

1. Configure [sudo](https://wiki.archlinux.org/index.php/Sudo)

# User

useradd -m -G wheel -s /usr/bin/fish andrew

# IWD Wifi Configuration

~~~sh
# systemctl start iwd
# systemctl start rfkill-unblock@wifi
# iwctl device wlp1s0 set-property Powered on
# iwctl
[iwd]# station wlp1s0 connect ESSID
Type the network passphrase for ESSID psk.
Passphrase: secret
[iwd]# exit
# sed '/^Passphrase=/d' /var/lib/iwd/ESSID.psk > /mnt/var/lib/iwd/ESSID.psk
# chmod 0600 /mnt/var/lib/iwd/ESSID.psk
~~~

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
1. systemctl enable dhcpcd.service
