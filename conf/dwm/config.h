/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int gappih    = 5;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 5;       /* vert inner gap between windows */
static const unsigned int gappoh    = 5;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 5;       /* vert outer gap between windows and screen edge */
static const int smartgaps          = 0;        /* 1 means no outer gap when there is only one window */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const int vertpad            = 5;       /* vertical padding of bar */
static const int sidepad            = 5;       /* horizontal padding of bar */
static const int horizpadbar        = 5;        /* horizontal padding for statusbar */
static const int vertpadbar         = 0;        /* vertical padding for statusbar */
static const char *fonts[]               = { "FiraCode Nerd Font Mono:style=Bold:size=14" };
static const char dmenufont[]       = "monospace:size=10";
static char normfgcolor[]                = "#bbbbbb";
static char normbgcolor[]                = "#222222";
static char normbordercolor[]            = "#444444";

static char selfgcolor[]                 = "#eeeeee";
static char selbgcolor[]                 = "#005577";
static char selbordercolor[]             = "#005577";

static char titlenormfgcolor[]           = "#bbbbbb";
static char titlenormbgcolor[]           = "#222222";
static char titlenormbordercolor[]       = "#444444";

static char titleselfgcolor[]            = "#eeeeee";
static char titleselbgcolor[]            = "#005577";
static char titleselbordercolor[]        = "#005577";

static char tagsnormfgcolor[]            = "#bbbbbb";
static char tagsnormbgcolor[]            = "#222222";
static char tagsnormbordercolor[]        = "#444444";

static char tagsselfgcolor[]             = "#eeeeee";
static char tagsselbgcolor[]             = "#005577";
static char tagsselbordercolor[]         = "#005577";

static char statusfgcolor[]              = "#bbbbbb";
static char statusbgcolor[]              = "#222222";
static char statusbordercolor[]        = "#444444";

static char *colors[][3] = {
        /*               fg           bg           border   */
        [SchemeNorm]     = { normfgcolor, normbgcolor, normbordercolor },
        [SchemeSel]      = { selfgcolor,  selbgcolor,  selbordercolor  },
        [SchemeStatus]   = { statusfgcolor, statusbgcolor,  statusbordercolor  }, // Statusbar right {text,background,not used but cannot be empty}
        [SchemeTagsSel]  = { tagsselfgcolor, tagsselbgcolor, tagsnormbordercolor }, // Tagbar left selected {text,background,not used but cannot be empty}
        [SchemeTagsNorm] = { tagsnormfgcolor, tagsnormbgcolor, tagsnormbordercolor }, // Tagbar left unselected {text,background,not used but cannot be empty}
        [SchemeInfoSel]  = { titleselfgcolor, titleselbgcolor, titleselbordercolor }, // infobar middle  selected {text,background,not used but cannot be empty}
        [SchemeInfoNorm] = { titlenormfgcolor, titlenormbgcolor, titlenormbordercolor }, // infobar middle  unselected {text,background,not used but cannot be empty}
};

/* tagging: refer to https://github.com/bakkeby/patches/wiki/tagicons */
static const char *tags[NUMTAGS] = { NULL };  /* left for compatibility reasons, i.e. code that checks LENGTH(tags) */
static char *tagicons[][NUMTAGS] = {
        [IconsDefault]        = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
        [IconsVacant]         = { NULL },
        [IconsOccupied]       = { NULL },
};

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
        { "[]=",      tile },    /* first entry is default */
        { "><>",      NULL },    /* no layout function means floating behavior */
        { "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod1Mask
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
static const char *termcmd[]  = { "kitty", NULL };

static Key keys[] = {
        /* modifier                     key        function        argument */
        { MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
        { MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
        { MODKEY,                       XK_b,      togglebar,      {0} },
        { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
        { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
        { MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
        { MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
        { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
        { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
        { MODKEY|Mod4Mask,              XK_h,      incrgaps,       {.i = +1 } },
        { MODKEY|Mod4Mask,              XK_l,      incrgaps,       {.i = -1 } },
        { MODKEY|Mod4Mask|ShiftMask,    XK_h,      incrogaps,      {.i = +1 } },
        { MODKEY|Mod4Mask|ShiftMask,    XK_l,      incrogaps,      {.i = -1 } },
        { MODKEY|Mod4Mask|ControlMask,  XK_h,      incrigaps,      {.i = +1 } },
        { MODKEY|Mod4Mask|ControlMask,  XK_l,      incrigaps,      {.i = -1 } },
        { MODKEY|Mod4Mask,              XK_0,      togglegaps,     {0} },
        { MODKEY|Mod4Mask|ShiftMask,    XK_0,      defaultgaps,    {0} },
        { MODKEY,                       XK_y,      incrihgaps,     {.i = +1 } },
        { MODKEY,                       XK_o,      incrihgaps,     {.i = -1 } },
        { MODKEY|ControlMask,           XK_y,      incrivgaps,     {.i = +1 } },
        { MODKEY|ControlMask,           XK_o,      incrivgaps,     {.i = -1 } },
        { MODKEY|Mod4Mask,              XK_y,      incrohgaps,     {.i = +1 } },
        { MODKEY|Mod4Mask,              XK_o,      incrohgaps,     {.i = -1 } },
        { MODKEY|ShiftMask,             XK_y,      incrovgaps,     {.i = +1 } },
        { MODKEY|ShiftMask,             XK_o,      incrovgaps,     {.i = -1 } },
        { MODKEY,                       XK_Return, zoom,           {0} },
        { MODKEY,                       XK_Tab,    view,           {0} },
        { MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
        { MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
        { MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
        { MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
        { MODKEY,                       XK_space,  setlayout,      {0} },
        { MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
        { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
        { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
        { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
        { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
        { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
        { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
        { MODKEY,                       XK_F5,     xrdb,           {.v = NULL } },
        //	{ MODKEY|ShiftMask,             XK_a,      seticonset,     {.i = 0 } },
        //	{ MODKEY|ShiftMask,             XK_b,      seticonset,     {.i = 1 } },
        TAGKEYS(                        XK_1,                      0)
        TAGKEYS(                        XK_2,                      1)
        TAGKEYS(                        XK_3,                      2)
        TAGKEYS(                        XK_4,                      3)
        TAGKEYS(                        XK_5,                      4)
        TAGKEYS(                        XK_6,                      5)
        TAGKEYS(                        XK_7,                      6)
        TAGKEYS(                        XK_8,                      7)
        TAGKEYS(                        XK_9,                      8)
        { MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
        /* click                event mask      button          function        argument */
        { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
        { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
        { ClkWinTitle,          0,              Button2,        zoom,           {0} },
        { ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
        { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
        { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
        { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
        { ClkTagBar,            0,              Button1,        view,           {0} },
        { ClkTagBar,            0,              Button3,        toggleview,     {0} },
        { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
        { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
        { ClkTagBar,            0,              Button4,        cycleiconset,   {.i = +1 } },
        { ClkTagBar,            0,              Button5,        cycleiconset,   {.i = -1 } },
};

