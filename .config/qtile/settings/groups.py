from libqtile.config import Key, Group
from libqtile.command import lazy
from .keys import modm, keys

workspaces = ['1','2','3','4','5','6','7','8','9']
#[' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']

groups = [Group(w) for w in workspaces]

for i, group in enumerate(groups):
    current_key = str(i+1);
    keys.extend([
        Key([modm], current_key, lazy.group[group.name].toscreen()),
        Key([modm, 'shift'], current_key, lazy.window.togroup(group.name))
    ])
    
