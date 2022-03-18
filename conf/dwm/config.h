/* See LICENSE file for copyright and license details. */
#include <X11/XF86keysym.h>
/* appearance */
static const unsigned int borderpx = 2;        /* border pixel of windows */
static const unsigned int snap     = 32;       /* snap pixel */
static const int showbar           = 1;        /* 0 means no bar */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayonleft = 1;   	/* 0: systray in the right corner, >0: systray on left of status text */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;     /* 0 means no systray */
static const int topbar            = 1;        /* 0 means bottom bar */
static const int focusedontop      = 1;
# include "target.h"
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
	/*                    fg                bg                 border */
	[SchemeNorm]      = { normfgcolor,      normbgcolor,       normbordercolor },
	[SchemeSel]       = { selfgcolor,       selbgcolor,        selbordercolor  },
	[SchemeStatus]    = { statusfgcolor,    statusbgcolor,     statusbordercolor  },    // Statusbar right {text, background, not used but cannot be empty}
	[SchemeTagsSel]   = { tagsselfgcolor,   tagsselbgcolor,    tagsselbordercolor  },   // Tagbar left selected {text, background, not used but cannot be empty}
	[SchemeTagsNorm]  = { tagsnormfgcolor,  tagsnormbgcolor,   tagsnormbordercolor  },  // Tagbar left unselected {text, background, not used but cannot be empty}
	[SchemeInfoSel]   = { titleselfgcolor,  titleselbgcolor,   titleselbordercolor  },  // infobar middle  selected {text, background, not used but cannot be empty}
	[SchemeInfoNorm]  = { titlenormfgcolor, titlenormbgcolor,  titlenormbordercolor  }, // infobar middle unselected {text,background, not used but cannot be empty}
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

#define WTYPE "_NET_WM_WINDOW_TYPE_"
static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class        role        instance  title  wintype,          tags mask  isfloating  alwaysontop monitor */

	{ NULL,         NULL,       NULL,     NULL,  WTYPE "DIALOG",   0,         1,          1,          -1 },
	{ NULL,         NULL,       NULL,     NULL,  WTYPE "UTILITY",  0,         1,          1,          -1 },
	{ NULL,         NULL,       NULL,     NULL,  WTYPE "TOOLBAR",  0,         1,          1,          -1 },
	{ NULL,         NULL,       NULL,     NULL,  WTYPE "SPLASH",   0,         1,          1,          -1 },
	{ "Gimp",       NULL,       NULL,     NULL,  NULL,             0,         1,          0,          -1 },
	{ "Thunar",     NULL,       NULL,     NULL,  NULL,             0,         1,          0,          -1 },
	{ "zoom",       NULL,       NULL,     NULL,  NULL,             0,         1,          0,          -1 },
	{ "mpv",        NULL,       NULL,     NULL,  NULL,             0,         1,          0,          -1 },
	{ NULL,         "pop-up",   NULL,     NULL,  NULL,             0,         1,          1,          -1 },
	{ "Firefox",    NULL,       NULL,     NULL,  NULL,             1 << 8,    0,          0,          -1 },

};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
	{ "=[]",      rtile },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }
#define STATUSBAR "dwmblocks"

static const char *termcmd[]     = { "dotfiles-term", NULL };

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = {
	"rofi",
	"-theme",
	"~/dotfiles/conf/rofi/barmenu.rasi",
	"-match",
	"fuzzy",
	"-auto-select",
	"-font",
	rofifont,
	"-show",
	"drun",
	"-show-icons",
	"-drun-display-format",
	"{name}",
	NULL
};
static const char *rofiwincmd[] = {
	"rofi",
	"-theme",
	"~/dotfiles/conf/rofi/barmenu.rasi",
	"-match",
	"fuzzy",
	"-auto-select",
	"-font",
	rofifont,
	"-show",
	"window",
	"-show-icons",
	"-window-format",
	"{w} {c} {t:25}",
	NULL
};
static const char *rofiemojicmd[] = {
	"rofi",
	"-show",
	"emoji",
	"-modi",
	"emoji",
	"-location",
	"1",
	"-theme-str",
	"window { width: 100%; }",
	"-font",
	rofifont,
	NULL
};
static const char *rofinetworkcmd[] = {
	"networkmanager_dmenu",
	"-location",
	"1",
	"-theme-str",
	"window { width: 100%; }",
	"-font",
	rofifont, NULL
};
static const char *rofimusiccmd[] = {
	"rofimusic.sh",
	rofifont,
	NULL
};

/* commands spawned when clicking statusbar, the mouse button pressed is exported as BUTTON */
/* static char *statuscmds[] = { "notify-send $BUTTON click" }; */
/* static char *statuscmd[] = { "/bin/sh", "-c", NULL, NULL }; */
/* modifier                     key        function        argument */

#include "focusurgent.c"

static Key keys[] = {
	{ MODKEY|Mod1Mask,              XK_r,                       spawn,          {.v = dmenucmd} },
	{ MODKEY,                       XK_p,                       spawn,          {.v = dmenucmd} },
	{ MODKEY|ShiftMask,             XK_p,                       spawn,          SHCMD("dwm-powermenu.sh") },
	{ MODKEY|ShiftMask,             XK_n,                       spawn,          {.v = rofinetworkcmd} },
	{ MODKEY|ShiftMask,             XK_w,                       spawn,          {.v = rofiwincmd} },
	{ MODKEY|ControlMask,           XK_space,                   spawn,          {.v = rofiemojicmd} },
	{ MODKEY|Mod1Mask,              XK_p,                       spawn,          {.v = rofimusiccmd} },
	{ MODKEY|ShiftMask,             XK_Return,                  spawn,          {.v = termcmd} },
	{ MODKEY,                       XK_e,                       spawn,          SHCMD("thunar") },
	{ MODKEY|ShiftMask,             XK_b,                       spawn,          SHCMD("dwm-brightness.sh default")},
	{ MODKEY,                       XK_b,                       togglebar,      {0} },
	{ MODKEY,                       XK_u,                       focusurgent,    {0} },
	{ MODKEY,                       XK_j,                       focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,                       focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_Tab,                     focusstack,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_Tab,                     focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,                       incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,                       incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,                       setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,                       setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return,                  zoom,           {0} },
	{ MODKEY,                       XK_w,                       killclient,     {0} },
	{ MODKEY,                       XK_t,                       setlayout,      {.v = &layouts[0]} },
	{ MODKEY|ShiftMask,             XK_t,                       setlayout,      {.v = &layouts[3]} },
	{ MODKEY,                       XK_f,                       setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,                       setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,                   togglefloating, {0} },
	{ MODKEY,                       XK_0,                       view,           {.ui = ~0} },
	{ MODKEY|ShiftMask,             XK_0,                       tag,            {.ui = ~0} },
	{ MODKEY,                       XK_comma,                   focusmon,       {.i = -1} },
	{ MODKEY,                       XK_period,                  focusmon,       {.i = +1} },
	{ MODKEY|ShiftMask,             XK_comma,                   tagmon,         {.i = -1} },
	{ MODKEY|ShiftMask,             XK_period,                  tagmon,         {.i = +1} },
	{ MODKEY,                       XK_F5,                      xrdb,           {.v = NULL} },

	TAGKEYS(                        XK_1,                                       0)
	TAGKEYS(                        XK_2,                                       1)
	TAGKEYS(                        XK_3,                                       2)
	TAGKEYS(                        XK_4,                                       3)
	TAGKEYS(                        XK_5,                                       4)
	TAGKEYS(                        XK_6,                                       5)
	TAGKEYS(                        XK_7,                                       6)
	TAGKEYS(                        XK_8,                                       7)
	TAGKEYS(                        XK_9,                                       8)

	{ MODKEY,                       XK_Down,                    moveresize,     {.v = "0x 25y 0w 0h" } },
	{ MODKEY,                       XK_Up,                      moveresize,     {.v = "0x -25y 0w 0h" } },
	{ MODKEY,                       XK_Right,                   moveresize,     {.v = "25x 0y 0w 0h" } },
	{ MODKEY,                       XK_Left,                    moveresize,     {.v = "-25x 0y 0w 0h" } },
	{ MODKEY|ShiftMask,             XK_Down,                    moveresize,     {.v = "0x 0y 0w 25h" } },
	{ MODKEY|ShiftMask,             XK_Up,                      moveresize,     {.v = "0x 0y 0w -25h" } },
	{ MODKEY|ShiftMask,             XK_Right,                   moveresize,     {.v = "0x 0y 25w 0h" } },
	{ MODKEY|ShiftMask,             XK_Left,                    moveresize,     {.v = "0x 0y -25w 0h" } },
	{ MODKEY|ControlMask,           XK_Up,                      moveresizeedge, {.v = "t"} },
	{ MODKEY|ControlMask,           XK_Down,                    moveresizeedge, {.v = "b"} },
	{ MODKEY|ControlMask,           XK_Left,                    moveresizeedge, {.v = "l"} },
	{ MODKEY|ControlMask,           XK_Right,                   moveresizeedge, {.v = "r"} },
	{ MODKEY|ControlMask|ShiftMask, XK_Up,                      moveresizeedge, {.v = "T"} },
	{ MODKEY|ControlMask|ShiftMask, XK_Down,                    moveresizeedge, {.v = "B"} },
	{ MODKEY|ControlMask|ShiftMask, XK_Left,                    moveresizeedge, {.v = "L"} },
	{ MODKEY|ControlMask|ShiftMask, XK_Right,                   moveresizeedge, {.v = "R"} },
	{ MODKEY|ShiftMask,             XK_q,                       quit,           {0} },
	{ 0,                            XF86XK_KbdBrightnessDown,   spawn,          SHCMD("sudo /usr/local/bin/keyboard-backlight down") },
	{ 0,                            XF86XK_KbdBrightnessUp,     spawn,          SHCMD("sudo /usr/local/bin/keyboard-backlight up") },
	{ 0,                            XF86XK_MonBrightnessUp,     spawn,          SHCMD("dwm-brightness.sh up") },
	{ 0,                            XF86XK_MonBrightnessDown,   spawn,          SHCMD("dwm-brightness.sh down") },
	{ 0,                            XF86XK_AudioMute,           spawn,          SHCMD("liskin-media mute") },
	{ 0,                            XF86XK_AudioLowerVolume,    spawn,          SHCMD("liskin-media volume down") },
	{ 0,                            XF86XK_AudioRaiseVolume,    spawn,          SHCMD("liskin-media volume up") },
	{ 0,                            XF86XK_AudioPlay,           spawn,          SHCMD("liskin-media play") },
	{ 0,                            XF86XK_AudioPrev,           spawn,          SHCMD("liskin-media prev") },
	{ 0,                            XF86XK_AudioNext,           spawn,          SHCMD("liskin-media next") },
	{ 0,                            XF86XK_AudioStop,           spawn,          SHCMD("liskin-media stop") }

};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkWinTitle,          0,              Button3,        spawn,          {.v = rofiwincmd} },
	{ ClkStatusText,        0,              Button1,        sigstatusbar,   {.i = 1} },
	{ ClkStatusText,        0,              Button2,        sigstatusbar,   {.i = 2} },
	{ ClkStatusText,        0,              Button3,        sigstatusbar,   {.i = 3} },
	{ ClkStatusText,        0,              Button4,        sigstatusbar,   {.i = 4} },
	{ ClkStatusText,        0,              Button5,        sigstatusbar,   {.i = 5} },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

