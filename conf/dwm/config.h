/* See LICENSE file for copyright and license details. */

#include <X11/XF86keysym.h>
/* Add somewhere in your constants definition section */


/* Add to keys[] array. With 0 as modifier, you are able to use the keys directly. */
/* static Key keys[] = { */
/* }; */
/* appearance */
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int gappih    = 10;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 10;       /* vert inner gap between windows */
static const unsigned int gappoh    = 15;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 5;       /* vert outer gap between windows and screen edge */
static const int smartgaps          = 0;        /* 1 means no outer gap when there is only one window */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const int usealtbar          = 1;        /* 1 means use non-dwm status bar */
static const char *altbarclass      = "Polybar"; /* Alternate bar class name */
static const char *altbarcmd        = "$HOME/dotfiles/scripts/polybar/launch"; /* Alternate bar launch command */
static const int focusonwheel       = 0;
static const char *fonts[]          = { "monospace:size=10" };
static const char dmenufont[]       = "monospace:size=10";
static char normbgcolor[]           = "#222222";
static char normbordercolor[]       = "#444444";
static char normfgcolor[]           = "#bbbbbb";
static char selfgcolor[]            = "#eeeeee";
static char selbordercolor[]        = "#005577";
static char selbgcolor[]            = "#005577";
static char *colors[][3] = {
	   /*               fg           bg           border   */
	   [SchemeNorm] = { normfgcolor, normbgcolor, normbordercolor },
	   [SchemeSel]  = { selfgcolor,  selbgcolor,  selbordercolor  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            1,           -1 },
	{ "Firefox",  NULL,       NULL,       1 << 8,       0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "", tile}, 
	{ "", NULL}, 
	{ "", monocle},
	/* { "[]=",      tile },   first entry is default */
	/*{ "><>",      NULL },    no layout function means floating behavior */
	/* { "[M]",      monocle }, */
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

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "rofi","-show","combi","-show-icons" };
/* static const char *termcmd[]  = { "kitty", NULL }; */
/* static const char *polybar_restartcmd[] = { "polybar-msg", "cmd", "restart", NULL }; */
/* static const char *upvol[]   = { "ponymix", "increase", "5%",     NULL }; */
/* static const char *downvol[]   = { "ponymix", "decrease", "5%",     NULL }; */
/* static const char *mutevol[] = { "ponymix", "toggle",  NULL }; */
/* static const char *playpausencspot[] = { "dbus-send","--print-reply","--dest=org.mpris.MediaPlayer2.ncspot","/org/mpris/MediaPlayer2","org.mpris.MediaPlayer2.Player.PlayPause", NULL}; */
/* /1* static const char *stopncspot[] = { "dbus-send","--print-reply","--dest=org.mpris.MediaPlayer2.ncspot","/org/mpris/MediaPlayer2","org.mpris.MediaPlayer2.Player.Stop", NULL} *1/ */
/* static const char *previousncspot[] = { "dbus-send","--print-reply","--dest=org.mpris.MediaPlayer2.ncspot","/org/mpris/MediaPlayer2","org.mpris.MediaPlayer2.Player.Previous", NULL}; */
/* static const char *nextncspot[] = { "dbus-send","--print-reply","--dest=org.mpris.MediaPlayer2.ncspot","/org/mpris/MediaPlayer2","org.mpris.MediaPlayer2.Player.Next", NULL}; */

static Key keys[] = {
	/* modifier                     key        function        argument */
	/* { MODKEY,                       XK_b,      togglebar,      {0} }, */
	/* { MODKEY,                       XK_j,      focusstack,     {.i = +1 } }, */
	/* { MODKEY,                       XK_k,      focusstack,     {.i = -1 } }, */
	/* { MODKEY,                       XK_i,      incnmaster,     {.i = +1 } }, */
	/* { MODKEY,                       XK_d,      incnmaster,     {.i = -1 } }, */
	/* { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} }, */
	/* { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} }, */
	/* { MODKEY,                       XK_Return, zoom,           {0} }, */
	/* { MODKEY,                       XK_Tab,    view,           {0} }, */
	/* { MODKEY|ShiftMask,             XK_c,      killclient,     {0} }, */
	/* { MODKEY|ShiftMask,             XK_space,  togglefloating, {0} }, */
	/* { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } }, */
	/* { MODKEY,                       XK_period, focusmon,       {.i = +1 } }, */
	/* { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } }, */
	/* { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } }, */
	/* TAGKEYS(                        XK_1,                      0) */
	/* TAGKEYS(                        XK_2,                      1) */
	/* TAGKEYS(                        XK_3,                      2) */
	/* TAGKEYS(                        XK_4,                      3) */
	/* TAGKEYS(                        XK_5,                      4) */
	/* TAGKEYS(                        XK_6,                      5) */
	/* TAGKEYS(                        XK_7,                      6) */
	/* TAGKEYS(                        XK_8,                      7) */
	/* TAGKEYS(                        XK_9,                      8) */
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_F5,     xrdb,           {.v = NULL } },
	/* { MODKEY|ShiftMask,             XK_q,      quit,           {0} }, */
	/* { 0,                       XF86XK_AudioLowerVolume, spawn, {.v = downvol } }, */
	/* { 0,                       XF86XK_AudioMute, spawn, {.v = mutevol } }, */
	/* { 0,                       XF86XK_AudioRaiseVolume, spawn, {.v = upvol   } }, */
	/* { 0, XF86XK_AudioPlay, spawn, {.v = playpausencspot}}, */
	/* { 0, XF86XK_AudioPrev, spawn, {.v = previousncspot}}, */
	/* { 0, XF86XK_AudioNext, spawn, {.v = nextncspot}}, */
	/* { MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } }, */
	/* { MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } }, */
	/* { MODKEY|Mod4Mask,              XK_h,      incrgaps,       {.i = +1 } }, */
	/* { MODKEY|Mod4Mask,              XK_l,      incrgaps,       {.i = -1 } }, */
	/* { MODKEY|Mod4Mask|ShiftMask,    XK_h,      incrogaps,      {.i = +1 } }, */
	/* { MODKEY|Mod4Mask|ShiftMask,    XK_l,      incrogaps,      {.i = -1 } }, */
	/* { MODKEY|Mod4Mask|ControlMask,  XK_h,      incrigaps,      {.i = +1 } }, */
	/* { MODKEY|Mod4Mask|ControlMask,  XK_l,      incrigaps,      {.i = -1 } }, */
	/* { MODKEY|Mod4Mask,              XK_0,      togglegaps,     {0} }, */
	/* { MODKEY|Mod4Mask|ShiftMask,    XK_0,      defaultgaps,    {0} }, */
	/* { MODKEY,                       XK_y,      incrihgaps,     {.i = +1 } }, */
	/* { MODKEY,                       XK_o,      incrihgaps,     {.i = -1 } }, */
	/* { MODKEY|ControlMask,           XK_y,      incrivgaps,     {.i = +1 } }, */
	/* { MODKEY|ControlMask,           XK_o,      incrivgaps,     {.i = -1 } }, */
	/* { MODKEY|Mod4Mask,              XK_y,      incrohgaps,     {.i = +1 } }, */
	/* { MODKEY|Mod4Mask,              XK_o,      incrohgaps,     {.i = -1 } }, */
	/* { MODKEY|ShiftMask,             XK_y,      incrovgaps,     {.i = +1 } }, */
	/* { MODKEY|ShiftMask,             XK_o,      incrovgaps,     {.i = -1 } }, */
	/* { MODKEY, XK_q, spawn, {.v = polybar_restartcmd }}, */
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	/* { ClkLtSymbol,          0,              Button1,        setlayout,      {0} }, */
	/* { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} }, */
	/* { ClkWinTitle,          0,              Button2,        zoom,           {0} }, */
	/* { ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } }, */
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	/* { ClkTagBar,            0,              Button1,        view,           {0} }, */
	/* { ClkTagBar,            0,              Button3,        toggleview,     {0} }, */
	/* { ClkTagBar,            MODKEY,         Button1,        tag,            {0} }, */
	/* { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} }, */
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
  IPCCOMMAND(  setlayoutsafe,       1,      {ARG_TYPE_PTR}    ),
  IPCCOMMAND(  quit,                1,      {ARG_TYPE_NONE}   )
};

