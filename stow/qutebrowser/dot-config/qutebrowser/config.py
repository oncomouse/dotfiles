config.load_autoconfig()
config.bind(";m", "hint links spawn mpv {hint-url}")
config.bind(";M", "spawn --detach mpv {url}")
config.bind("<Ctrl+Shift+u>", "spawn --userscript qute-bitwarden")
config.bind("J", "tab-prev")
config.bind("K", "tab-next")
config.bind(
    "<Ctrl-p>",
    "jseval document.location='https://pinboard.in/add?next=same&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)",
    mode="normal",
)
c.editor.command = ["st", "-e", "nvim", "-f", "{}"]
