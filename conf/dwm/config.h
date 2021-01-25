/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const float cm_scale         = 0.70;     /* Centered Monocle scale */
static const int focusonwheel       = 0;
static const char *fonts[]          = { "FantasqueSansMono Nerd Font:size=16" };
static const char dmenufont[]       = "monospace:size=10";
static char normbgcolor[]          = "#222222";
static char normbordercolor[]      = "#444444";
static char normfgcolor[]          = "#bbbbbb";
static char selfgcolor[]           = "#eeeeee";
static char selbordercolor[]       = "#005577";
static char selbgcolor[]           = "#005577";
static char titlenormfgcolor[]     = "#bbbbbb";
static char titlenormbgcolor[]     = "#222222";
static char titlenormbordercolor[] = "#444444";
static char titleselfgcolor[]      = "#eeeeee";
static char titleselbgcolor[]      = "#005577";
static char titleselbordercolor[]  = "#005577";
static char tagsnormfgcolor[]      = "#bbbbbb";
static char tagsnormbgcolor[]      = "#222222";
static char tagsnormbordercolor[]  = "#444444";
static char tagsselfgcolor[]       = "#eeeeee";
static char tagsselbgcolor[]       = "#005577";
static char tagsselbordercolor[]   = "#005577";
static char statusfgcolor[]        = "#bbbbbb";
static char statusbgcolor[]        = "#222222";
static char statusbordercolor[]    = "#444444";
static char *colors[][3] = {
	/*               fg           bg           border   */
	[SchemeNorm] = { normfgcolor, normbgcolor, normbordercolor },
	[SchemeSel]  = { selfgcolor,  selbgcolor,  selbordercolor  },
	[SchemeStatus]  = { statusfgcolor, statusbgcolor, statusbordercolor  }, // Statusbar right {text,background,not used but cannot be empty}
	[SchemeTagsSel]  = { tagsselfgcolor, tagsselbgcolor,  tagsselbordercolor  }, // Tagbar left selected {text,background,not used but cannot be empty}
	[SchemeTagsNorm]  = { tagsnormfgcolor, tagsnormbgcolor,  tagsnormbordercolor  }, // Tagbar left unselected {text,background,not used but cannot be empty}
	[SchemeInfoSel]  = { titleselfgcolor, titleselbgcolor,  titleselbordercolor  }, // infobar middle  selected {text,background,not used but cannot be empty}
	[SchemeInfoNorm]  = { titlenormfgcolor, titlenormbgcolor,  titlenormbordercolor  }, // infobar middle  unselected {text,background,not used but cannot be
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp"    , NULL , NULL     , 0      , 1 , -1 } ,
	{ "Thunar"  , NULL , NULL     , 0      , 1 , -1 } ,
	{ "feh"     , NULL , NULL     , 0      , 1 , -1 } ,
	{ "Zotero"  , NULL , NULL     , 1 << 7 , 0 , -1 } ,
	{ "kitty"   , NULL , "ncspot" , 1 << 8 , 1 , -1 } ,
	{ "Spotify" , NULL , NULL     , 1 << 8 , 0 , -1 } ,
	{ "zoom"    , NULL , NULL     , 1 << 6 , 0 , -1 } ,
};

/* layout(s) */
static const float mfact     = 0.5; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "", tile}, 
	{ "", NULL}, 
	{ "", centeredmonocle},
	{ "", monocle},
};

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "rofi","-show","combi","-show-icons", NULL };
static const char *termcmd[]  = { "kitty", NULL };

static const char *statuscmds[] = { "~/dotfiles/scripts/dwm/status/volume.sh", "~/dotfiles/scripts/dwm/status/mpris.sh", "~/dotfiles/scripts/dwm/status/clock.sh" };
static char *statuscmd[] = { "/bin/sh", "-c", NULL, NULL };

#define MODKEY Mod4Mask
/* hotkeys are handled by sxhkd and dwm-ipc */
static Key keys[] = {NULL};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button1,        spawn,          {.v = statuscmd } },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = statuscmd } },
	{ ClkStatusText,        0,              Button3,        spawn,          {.v = statuscmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

static const char *ipcsockpath = "/tmp/dwm.sock";
static IPCCommand ipccommands[] = {
	IPCCOMMAND(  view,                1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  toggleview,          1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  tag,                 1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  toggletag,           1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  tagmon,              1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  focusmon,            1,      {ARG_TYPE_SINT}   ),
	IPCCOMMAND(  focusstack,          1,      {ARG_TYPE_SINT}   ),
	IPCCOMMAND(  zoom,                1,      {ARG_TYPE_NONE}   ),
	IPCCOMMAND(  incnmaster,          1,      {ARG_TYPE_SINT}   ),
	IPCCOMMAND(  killclient,          1,      {ARG_TYPE_SINT}   ),
	IPCCOMMAND(  togglefloating,      1,      {ARG_TYPE_NONE}   ),
	IPCCOMMAND(  setmfact,            1,      {ARG_TYPE_FLOAT}  ),
	IPCCOMMAND(  setcfact,            1,      {ARG_TYPE_FLOAT}  ),
	IPCCOMMAND(  setlayoutsafe,       1,      {ARG_TYPE_PTR}    ),
	IPCCOMMAND(  setlayout,           1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  quit,                1,      {ARG_TYPE_UINT}   ),
	IPCCOMMAND(  xrdb,                1,      {ARG_TYPE_NONE}   ),
};

