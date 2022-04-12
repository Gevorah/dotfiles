---------------------------------------------------------------------------------
# Arch installation

## Pre-Installation

### Keyboard
```sh
ls /usr/share/kbd/keymaps/**/*.map.gz
loadkeys es
```

### Make partitions 
Verify the disk 'lsblk'
Make linux swap (4G), root (30G), home (60G), efi (550M)
```sh
cfdisk /dev/<disk>
```

### Format partitions
```sh
mkfs.fat -F32 /dev/<boot>
mkfs.ext4 /dev/<root>
mkfs.ext4 /dev/<home>
mkswap /dev/<swap>
```

### Mount partitions
```sh
mount /dev/<root> /mnt
swapon /dev/<swap>
mkdir -p /mnt/efi
mkdir -p /mnt/home
mount /dev/<boot> /mnt/efi
mount /dev/<home> /mnt/home
```

## Installation
pacstrap /mnt base linux linux-firmware nano # intel or amd ucode

## Configure
fstab
```sh
genfstab -U /mnt >> /mnt/etc/fstab
cat /mnt/etc/fstab # to check
```

### chroot
```sh
arch-chroot /mnt
```

### time zone
```sh
ln -sf /usr/share/zoneinfo/Region/City /etc/localtime
```

### locales
```sh
# uncomment en_US.UTF-8 UTF-8
nano /etc/locale.gen 
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
```

### network
```sh
echo "<hostname>" > /etc/hostname
echo "127.0.0.1    localhost" >> /etc/hosts
echo "::1          localhost" >> /etc/hosts
echo "127.0.0.1    <hostname>.localdomain    <hostname>" >> /etc/hosts

pacman -S netctl dialog dhcpcd elinks ifplugd # laptop: wpa_supplicant iw
systemctl enable dhcpcd
```

### Root password
```sh
passwd
```

### grub
```sh
pacman -S grub efibootmgr os-prober
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB
grub-mkconfig -o /boot/grub/grub.cfg
echo "OS_PROBER_DISABLE=false" >> /etc/default/grub
```

### user
```sh
useradd -m <username>
passwd <username>
usermod -aG wheel,video,audio,storage <username>
```
sudo privileges
```sh
pacman -S sudo

# Uncomment '%wheel ALL=(ALL) ALL' to allow members of group wheel to execute any command 
nano /etc/sudoers
```

### Reboot
```sh
exit
reboot
```

---------------------------------------------------------------------------------
# AUR helper
```sh
sudo pacman -Syu base-devel git curl
cd /opt/
sudo git clone https://aur.archlinux.org/yay-git.git
sudo chown -R username:users ./yay-git
cd yay-git/
makepkg -si
```

---------------------------------------------------------------------------------
# Desktop Environment
```sh
sudo pacman -Syu xorg xorg-server xorg-xinit lightdm lightdm-webkit2-greeter networkmanager nm-connection-editor network-manager-applet alacritty 
systemctl enable lightdm
```

## Sound
```sh
pacman -S pavucontrol pulseaudio alsa-utils
```

## Utility
```sh
sudo pacman -S ranger dunst rofi maim xclip flameshot

# picom-git next updates would include more of this fork
yay -S polybar picom-ibhagwan-git 
```

## Archiving and Compression
```sh
sudo pacman -S zip unzip p7zip
```

---------------------------------------------------------------------------------
# XMonad: https://github.com/xmonad/xmonad/blob/master/INSTALL.md

---------------------------------------------------------------------------------
# Lightdm theming
glorious theme: https://github.com/manilarome/lightdm-webkit2-theme-glorious
```sh
yay -S lightdm-webkit2-theme-glorious
```

`/etc/lightdm/lightdm.conf`
```sh
[Seat:*]
# Uncomment this line and set this value
greeter-session = lightdm-webkit2-greeter
```

`/etc/lightdm/lightdm-webkit2-greeter.conf`
```sh
[greeter]
debug_mode = true
webkit_theme = glorious
```

---------------------------------------------------------------------------------
# GTK Theming
```sh
sudo pacman -S lxappearance
```

Directories `/usr/share/themes` and `/usr/share/icons`

## Fonts
```sh
yay -S nerd-fonts-ubuntu-mono nerd-fonts-mononoki nerd-fonts-jetbrains-mono
```

## Cursor Theme: https://github.com/keeferrourke/capitaine-cursors
```sh
sudo pacman -S xcb-util-cursor capitaine-cursors
```
Edit `~/.gtkrc-2.0`
```sh
gtk-cursor-theme-name = "capitaine-cursors"
```
Again, edit `~/.config/gtk-3.0/settings.ini`
```sh
[Settings]
gtk-cursor-theme-name = capitaine-cursors
```
Finally, edit `/usr/share/icons/default/index.theme`
```sh
[Icon Theme]
Inherits = capitaine-cursors
```

## Icons Theme: https://github.com/PapirusDevelopmentTeam/papirus-icon-theme
```sh
sudo pacman -S papirus-icon-theme
```

---------------------------------------------------------------------------------
# Fish
```sh
sudo pacman -S fish
```

## (Optional) Setting fish as default shell
```sh
# list installed shells
chsh -l

# set fish as default for your user
chsh -s /path/to/fish

# disabele fish greeting message at startup
set -U fish_greeting
```

## Plugins
OMF: https://github.com/oh-my-fish/oh-my-fish
Fisher: https://github.com/jorgebucaran/fisher
Theme: https://github.com/hastinbe/theme-kawasaki

```sh
omf update
fisher update

omf install sudope
set -U fish_escape_delay_ms 300
```

---------------------------------------------------------------------------------
# Neovim
```sh
sudo pacman -S neovim
```

## Neovim dependences
```sh
sudo pacman -Syu python python-pip
pip install pynvim

# NVM: https://github.com/nvm-sh/nvm#installing-and-updating
omf install nvm
nvm install node
sudo pacman -S npm

# edit .npmrc
npm set prefix="$HOME/.local"

sudo npm install -g neovim

neovim +:checkhealth
```

## Plugins
### Vim Plug: https://github.com/junegunn/vim-plug
`:PlugInstall`

### Conqueror of completion
`:CocInstall <package>`
- coc-spell-checker # self-explanatory
- coc-tsserver # javascript and typescript server
- coc-css # css, scss and less server
- coc-pyright # Pyright extension
- coc-java # use Eclipse JDT Language Server
- coc-omnisharp # csharp and visual basic server
- coc-sql # sql server
- coc-xml # use XML Language Server

---------------------------------------------------------------------------------
# Git credentials
fisher install danhper/fish-ssh-agent

---------------------------------------------------------------------------------
# Troubleshooting
pacman -S --overwrite \* <x>

# lightdm troubleshooting:
# to avoid not input screen at start
nano /etc/mkinitcpio.conf # add video driver name to MODULES: https://wiki.archlinux.org/title/kernel_mode_setting#Early_KMS_start

# grub recovery:
# if Windows deletes it
mount /dev/<root> /mnt/
mount /dev/<boot> /mnt/efi
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB
grub-mkconfig -o /boot/grub/grub.cfg

