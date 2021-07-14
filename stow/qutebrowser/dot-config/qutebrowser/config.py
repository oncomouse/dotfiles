import json
from os import environ
from pathlib import Path

config.load_autoconfig()

try:
    cache = environ["XDG_CACHE_HOME"]
except KeyError:
    cache = "{}/.cache".format(Path.home())

colors = json.load(open("{}/wal/colors.json".format(cache)))

# Set dark them for website default
c.colors.webpage.preferred_color_scheme = "dark"
c.colors.webpage.darkmode.enabled = True
# Tabs
# Tab Selection
c.colors.tabs.even.bg = colors["colors"]["base03"]
c.colors.tabs.odd.bg = colors["colors"]["base03"]
c.colors.tabs.selected.odd.bg = colors["colors"]["base01"]
c.colors.tabs.selected.even.bg = colors["colors"]["base01"]
# Tab State
c.colors.tabs.indicator.error = colors["colors"]["color9"]
c.colors.tabs.indicator.start = colors["colors"]["base04"]
c.colors.tabs.indicator.stop = colors["colors"]["base05"]
# Downloads bar
c.colors.downloads.bar.bg = colors["colors"]["color6"]
c.colors.downloads.start.bg = colors["colors"]["color14"]
c.colors.downloads.stop.bg = colors["colors"]["color12"]
# Bottom Bar
# Weblink
c.colors.statusbar.url.success.http.fg = colors["colors"]["color2"]
c.colors.statusbar.url.success.https.fg = colors["colors"]["color2"]
# Different Modes
c.colors.statusbar.insert.bg = colors["colors"]["color13"]
c.colors.statusbar.insert.fg = colors["colors"]["color0"]
c.colors.statusbar.normal.bg = colors["colors"]["color0"]
c.colors.statusbar.normal.fg = colors["colors"]["color15"]
c.colors.statusbar.command.bg = colors["colors"]["color8"]
c.colors.statusbar.command.fg = colors["colors"]["color15"]
c.colors.statusbar.caret.fg = colors["colors"]["base09"]
c.colors.statusbar.caret.bg = colors["colors"]["color0"]
c.colors.statusbar.caret.selection.fg = colors["colors"]["color0"]
c.colors.statusbar.caret.selection.bg = colors["colors"]["base09"]
