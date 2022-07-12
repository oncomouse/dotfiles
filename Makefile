desktop:
	bash ./bootstrap/init.sh
server:
	env SERVER=true bash ./bootstrap/init.sh

# Compiled Apps:
compiled_apps := dwl dwm dmenu dwmblocks st neatvi aslstatus slstatus tabbed nextvi sdorfehs

$(compiled_apps:%=%-rebuild):
	scripts/build-compiled.sh $(subst -rebuild,,${@}) rebuild

$(compiled_apps):
	scripts/build-compiled.sh ${@}
