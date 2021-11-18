from libqtile.config import Key
from libqtile.command import lazy

modm = 'mod4'

rofi = 'rofi -no-lazy-grab -show drun'
maimcopy = 'maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot'
maimsave = 'maim -s ~/Pictures/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Pictures\" -i flameshot'

keys = [

    # ---------   Windows   ---------
    # close focused window
    Key([modm, 'shift'], 'c', lazy.window.kill()),

    # switch between windows
    Key([modm], 'Tab',  lazy.layout.down()),
    Key([modm], 'j',    lazy.layout.down()),
    Key([modm], 'k',    lazy.layout.up()),
    Key([modm], 'h',    lazy.layout.left()),
    Key([modm], 'l',    lazy.layout.right()),

    # move windows
    Key([modm], 'j',    lazy.layout.shuffle_down()),
    Key([modm], 'k',    lazy.layout.shuffle_up()),

    # toggle floating
    Key([modm, 'shift'], 't', lazy.window.toggle_floating()),

    # change windows size
    Key([modm, 'shift'], 'h', lazy.layout.grow()),
    Key([modm, 'shift'], 'l', lazy.layout.shrink()),

    # switch between layouts
    Key([modm], 'space', lazy.next_layout()),

    # qtile opt
    Key([modm, 'control'], 'r', lazy.restart()),
    Key([modm, 'control'], 'q', lazy.shutdown()),
    Key([modm], 'r', lazy.spawncmd()),

    # ---------   Apps   ---------
    # launch a terminal
    Key([modm],    'Return',   lazy.spawn('alacrity')),

    # rofi menu
    Key([modm], 'm', lazy.spawn(rofi)),

    # screenshot
    Key([modm, 'shift'], 's', lazy.spawn(maimcopy)),
    Key([modm], 's', lazy.spawn(maimsave)),

    # audio control
    Key([], 'XF86AudioPlay', lazy.spawn('playerctl play-pause')),
    Key([], 'XF86AudioPrev', lazy.spawn('playerctl previous')),
    Key([], 'XF86AudioNext', lazy.spawn('playerctl next')),

    # volume
    Key([], 'XF86AudioRaiseVolume', lazy.spawn('pactl set-sink-volume 0 +5%')),
    Key([], 'XF86AudioLowerVolume', lazy.spawn('pactl set-sink-volume 0 -5%')),
    Key([], 'XF86AudioMute', lazy.spawn('pactl set-sink-mute 0 toggle')),

    # brightness
    #Key([], 'XF86MonBrightnessUp', lazy.spawn('brightnessctl set +10%')),
    #Key([], 'XF86MonBrightnessDown', lazy.spawn('brightnessctl set 10%-')),

]

