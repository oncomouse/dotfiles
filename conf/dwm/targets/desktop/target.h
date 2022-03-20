static const char *fonts[]         = {
	"Fira Mono:size=10",
	"JoyPixels:size=10",
	"FiraCode Nerd Font:size=10"
};
static const char dmenufont[]      = "FiraCode Nerd Font:size=10";

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
	{ "=[]",      rtile },
};
