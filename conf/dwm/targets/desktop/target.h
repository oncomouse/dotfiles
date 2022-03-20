static const char *fonts[]         = {
	"Fira Mono:size=10",
	"JoyPixels:size=10",
	"FiraCode Nerd Font:size=10"
};
static const char dmenufont[]      = "FiraCode Nerd Font:size=10";
static const char rofifont[]       = "FiraCode Nerd Font Regular 10";

/* layout(s) */
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
	{ "=[]",      rtile },
};
static const int tileidx = 0;
static const int floatidx = 1;
