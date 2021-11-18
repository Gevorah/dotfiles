from libqtile.config import Click, Drag
from libqtile.command import lazy
from .keys import modm

mouse = [
    Drag([modm], 'Button1', lazy.window.set_position_floating(),
                            start=lazy.window.get_position()),
    Drag([modm], 'Button3', lazy.window.set_size_floating(),
                            start=lazy.window.get_size()),
    Click([modm], 'Button2', lazy.window.bring_to_front())
]
