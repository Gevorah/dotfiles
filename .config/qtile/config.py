from settings.groups import groups
from settings.keys import modm, keys
from settings.layouts import layouts, floating_layout
from settings.mouse import mouse

from libqtile import hook
from os import path
import subprocess

@hook.subscribe.startup
def startup():
    pass

@hook.subscribe.startup_once
def startup_once():
    home = path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

main = None
auto_fullscreen = True
bring_front_click = False
cursor_warp = True
dgroups_key_binder = None
dgroups_app_rules = []
focus_on_window_activation = 'urgent'
follow_mouse_focus = True
wmname = 'LG3D'
auto_minimize = True
