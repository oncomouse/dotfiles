config.load_autoconfig()
config.bind(";m", "hint links spawn mpv {hint-url}")
config.bind(";M", "spawn --detach mpv {url}")
config.bind("<Ctrl+Shift+u>", "spawn --userscript qute-bitwarden")
config.bind("J", "tab-prev")
config.bind("K", "tab-next")
c.editor.command = ["st", "-e", "nvim", "-f", "{}"]
