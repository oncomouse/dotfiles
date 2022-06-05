static const char *fonts[]         = {
	"JetBrainsMono Nerd Font:size=11",
	"JoyPixels:size=10"
};
static const char dmenufont[]      = "JetBrainsMono Nerd Font:size=11";
static const char rofifont[]       = "JetBrainsMono Nerd Font 11";

/* layout(s) */
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ " ",        NULL },    /* no layout function means floating behavior */
	{ "[]=",      tile },    /* first entry is default */
	{ "[M]",      monocle },
	{ "=[]",      rtile },
};
static const int tileidx = 1;
static const int floatidx = 0;
