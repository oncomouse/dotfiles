local dotfiles_target = os.getenv("DOTFILES_TARGET") or "desktop"
local is_laptop = (dotfiles_target == "laptop")
return is_laptop
