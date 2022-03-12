desktop:
	bash ./bootstrap/init.sh
server:
	env SERVER=true bash ./bootstrap/init.sh

# Suckless Targets:
sl_apps := 2bwm dwm dmenu dwmblocks st neatvi aslstatus slstatus tabbed nextvi

$(sl_apps:%=%-rebuild):
	scripts/sl_build.sh $(subst -rebuild,,${@}) rebuild

$(sl_apps):
	scripts/sl_build.sh ${@}
