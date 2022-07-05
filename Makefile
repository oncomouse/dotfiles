desktop:
	bash ./bootstrap/init.sh
server:
	env SERVER=true bash ./bootstrap/init.sh

# Suckless Targets:
sl_apps := 2bwm berry dwl dwm dmenu dwmblocks st neatvi aslstatus slstatus tabbed nextvi shod lemonaid

$(sl_apps:%=%-rebuild):
	scripts/sl_build.sh $(subst -rebuild,,${@}) rebuild

$(sl_apps):
	scripts/sl_build.sh ${@}

ratpoison:
	scripts/rp_build.sh

sdorfehs:
	scripts/sf_build.sh

luastatus:
	conf/luastatus/build.sh
