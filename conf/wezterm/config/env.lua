local HOME = os.getenv("HOME")
local XDG_CACHE_HOME = os.getenv("XDG_CACHE_HOME") or HOME .. "/.cache"
local XDG_CONFIG_HOME = os.getenv("XDG_CONFIG_HOME") or HOME .. "/.config"
local DOTFILES_TARGET = os.getenv("DOTFILES_TARGET") or "laptop"

return {
	HOME=HOME,
	XDG_CACHE_HOME=XDG_CACHE_HOME,
	XDG_CONFIG_HOME=XDG_CONFIG_HOME,
	DOTFILES_TARGET=DOTFILES_TARGET,
}
