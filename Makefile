desktop:
	bash ./bootstrap/init.sh
server:
	env SERVER=true bash ./bootstrap/init.sh

# Suckless Targets:
sl_apps := dwm dmenu dwmblocks st

$(sl_apps:%=%-rebuild):
	scripts/sl_build.sh $(subst -rebuild,,${@}) rebuild

$(sl_apps):
	scripts/sl_build.sh ${@}
