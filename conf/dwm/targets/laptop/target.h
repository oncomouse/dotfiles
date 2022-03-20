static const char *fonts[]         = {
	"Hack-Regular:size=9",
	"JoyPixels:size=9",
	"FiraCode Nerd Font:size=9"
};
static const char dmenufont[]      = "Hack-Regular:size=9";
static const char rofifont[]       = "Hack 9";

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
