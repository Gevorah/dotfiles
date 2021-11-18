from libqtile import layout
from libqtile.config import Match

config = {
        'border_focus': '#d65d0e',
        'border_width': 1,
        'margin': 9
}

floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules
    ]
)

layouts = [
    layout.Bsp(**config),
    #layout.Columns(),
    #layout.Matrix(columns=2,**config),
    #layout.Max(),
    layout.MonadTall(**config),
    layout.MonadWide(**config),
    #layout.RatioTile(**config),
    #layout.Slice(), # cuts piece of screen
    #layout.Stack(**config), # set of stacks dividing the screen
    #layout.Tile(**config), # two stacks dividing the screen
    #layout.TreeTab(),
    #layout.VerticalTile(**config), # vertical monitors
    #layout.Zoomy(),
]
