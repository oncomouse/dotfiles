* [Alternate Install](https://wiki.archlinux.org/index.php/User:Altercation/Bullet_Proof_Arch_Install#Objectives)
* [IWD Wifi instructions](https://sudaraka.org/how-to/archlinux-installation-guide-2019/)
* [Hardening Advice](https://vez.mrsk.me/linux-hardening.html#kern)
* [Macbook Wifi](https://sylvaindurand.org/installing-arch-linux-on-macbook-pro-late-2013/)
* [Macbook Trackpad](https://howchoo.com/linux/the-perfect-almost-touchpad-settings-on-linux-2)

Notes:
1. Don't use systemd-start stuff, just use `arch-chroot /mnt`
1. `mkinitcpio -p linux` should be `mkinitcpio -p linux-zen`
1. Always use `/dev/mapper/cryptswap`
1. In example `refind` config, image name should be `initramfs-linux-zen.img`
	* Add `initrd=boot\<amd|intel>-ucode.img` before `initramfs` line to load ucode
	* Add " apparmor=1" after `initramfs`

# Partition Drive using `sgdisk`

Clear the drive:

```
# sgdisk --zap-all $DRIVE
```

Format the drive:
```
# sgdisk --clear \
         --new=1:0:+300MiB --typecode=1:ef00 --change-name=1:EFI \
         --new=2:0:+16GiB  --typecode=2:8200 --change-name=2:swap \
         --new=3:0:0       --typecode=3:8300 --change-name=3:system \
         $DRIVE
```

# Install these

Additional Packages to install, before reboot:

`pacman -S linux linux-firmware refind base base-devel neovim git curl fish iw sudo networkmanager gnupg`

Install AMD or Intel Microcode: `pacman -S amd-ucode` or `pacman -S intel-ucode`

# User

`useradd -m -G wheel -s /usr/bin/fish andrew`

# Configure Sudo for `wheel`

`visudo`

Add `Defaults editor=/usr/bin/nvim`
Uncomment line about `wheel`

# Network Configuration

`systemctl enable NetworkManager.service`

You may need to install `linux-firmware` and any DKMS modules.

To connect to wifi, `nmcli device wifi --ask connect <SSID>`

# AMD GPU Stuff

[AMDGPU on Arch Wiki](https://wiki.archlinux.org/index.php/AMDGPU)
 
# Keyring

1. [Keyring PAM Configuration](https://wiki.archlinux.org/title/GNOME/Keyring#PAM_method)

# Refind Configuration that works

```
"Boot with standard options"  "root=/dev/disk/by-partlabel/system rw initrd=\amd-ucode.img initrd=\initramfs-linux.img"
"Boot to single-user mode"    "root=/dev/disk/by-partlabel/system rw initrd=\amd-ucode.img initrd=\initramfs-linux-fallback.img"
"Boot with minimal options"   "ro root=/dev/disk/by-partlabel/system"
```
