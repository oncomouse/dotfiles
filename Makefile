desktop:
	bash ./bootstrap/init.sh
server:
	env SERVER=true bash ./bootstrap/init.sh

# Suckless Targets:
compiled_apps := 2bwm berry dwl dwm dmenu dwmblocks st neatvi aslstatus slstatus tabbed nextvi shod lemonaid luastatus ratpoison sdorfehs

$(compiled_apps:%=%-rebuild):
	scripts/build-compiled.sh $(subst -rebuild,,${@}) rebuild

$(compiled_apps):
	scripts/build-compiled.sh ${@}
