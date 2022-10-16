/* appearance */
static const int sloppyfocus        = 1;  /* focus follows mouse */
static const unsigned int borderpx  = 2;  /* border pixel of windows */
static const int lockfullscreen     = 1;  /* 1 will force focus on the fullscreen window */
static const float rootcolor[]      = {0.3, 0.3, 0.3, 1.0};
static const float bordercolor[]    = {0.1294117647, 0.15294117647, 0.2, 1.0};
static const float focuscolor[]     = {0.58431372549, 0.90196078431, 0.79607843137, 1.0};
static const int smartborders       = 1;

static const char *const autostart[] = {
		"sh", "-c", "convert ~/.cache/wal/background.svg ~/.cache/wal/background.png", NULL,
		"sh", "-c", "swaybg -m tile -i ~/.cache/wal/background.png", NULL,
		"sh", "-c", "waybar&", NULL,
		"sh", "-c", "pkill dunst; dunst&", NULL,
        NULL /* terminate */
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* app_id     title       tags mask     isfloating   monitor */
	/* examples:
	{ "Gimp",     NULL,       0,            1,           -1 },
	{ "firefox",  NULL,       1 << 8,       0,           -1 },
	*/
};

/* layout(s) */
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* monitors
 * The order in which monitors are defined determines their position.
 * Non-configured monitors are always added to the left. */
static const MonitorRule monrules[] = {
	/* name       mfact nmaster scale layout       rotate/reflect x y */
	/* example of a HiDPI laptop monitor:
	{ "eDP-1",    0.5,  1,      2,    &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL, 0, 0 },
	*/
	/* defaults */
	{ NULL,       0.55, 1,      1,    &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL, 0, 0 },
};

/* keyboard */
static const struct xkb_rule_names xkb_rules = {
	/* can specify fields: rules, model, layout, variant, options */
	/* example:
	.options = "ctrl:nocaps",
	*/
};

static const int repeat_rate = 25;
static const int repeat_delay = 600;

/* Trackpad */
static const int tap_to_click = 1;
static const int natural_scrolling = 0;

static const int cursor_timeout = 5;

/* If you want to use the windows key change this to WLR_MODIFIER_LOGO */
#define MODKEY WLR_MODIFIER_LOGO
#define TAGKEYS(KEY,SKEY,TAG) \
	{ MODKEY,                    KEY,            view,            {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_CTRL,  KEY,            toggleview,      {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_SHIFT, SKEY,           tag,             {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_CTRL|WLR_MODIFIER_SHIFT,SKEY,toggletag, {.ui = 1 << TAG} }

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *termcmd[] = { "foot", NULL };
static const char rofifont[]       = "FiraCode Nerd Font Regular 10";
static const char *menucmd[] = {
	"rofi",
	"-theme",
	"themes/bar-menu.rasi",
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
	"themes/bar-menu.rasi",
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
	"-font",
	rofifont,
	NULL
};
static const char *rofinetworkcmd[] = {
	"networkmanager_dmenu",
	"-location",
	"1",
	"-font",
	rofifont, NULL
};
static const char *rofimusiccmd[] = {
	"rofimusic.sh",
	rofifont,
	NULL
};

static const Key keys[] = {
	/* Note that Shift changes certain key codes: c -> C, 2 -> at, etc. */
	/* modifier                  key                 function        argument */

	{ MODKEY|WLR_MODIFIER_ALT,               XKB_KEY_r,                          spawn,               {.v = menucmd} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_Return,                     spawn,               {.v = termcmd} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_p,                          spawn,               SHCMD("dotfiles-powermenu") },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_n,                          spawn,               {.v = rofinetworkcmd} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_w,                          spawn,               {.v = rofiwincmd} },
	{ MODKEY|WLR_MODIFIER_CTRL,              XKB_KEY_space,                      spawn,               {.v = rofiemojicmd} },
	{ MODKEY|WLR_MODIFIER_ALT,               XKB_KEY_p,                          spawn,               {.v = rofimusiccmd} },
	{ MODKEY,                                XKB_KEY_e,                          spawn,               SHCMD("dotfiles-fm") },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_b,                          spawn,               SHCMD("dotfiles-brightness default")},
	{ MODKEY|WLR_MODIFIER_ALT,               XKB_KEY_c,                          spawn,               SHCMD("dunstify -i alarm-clock-panel -h string:x-dunst-stack-tag:date \"$(date +'%a %m/%d %I:%M %p')\" -t 1500") },
	{ MODKEY,                                XKB_KEY_j,                          focusstack,          {.i = +1} },
	{ MODKEY,                                XKB_KEY_k,                          focusstack,          {.i = -1} },
	{ MODKEY,                                XKB_KEY_i,                          incnmaster,          {.i = +1} },
	{ MODKEY,                                XKB_KEY_d,                          incnmaster,          {.i = -1} },
	{ MODKEY,                                XKB_KEY_h,                          setmfact,            {.f = -0.05} },
	{ MODKEY,                                XKB_KEY_l,                          setmfact,            {.f = +0.05} },
	{ MODKEY,                                XKB_KEY_Return,                     zoom,                {0} },
	{ MODKEY,                                XKB_KEY_Tab,                        view,                {0} },
	{ MODKEY,                                XKB_KEY_w,                          killclient,          {0} },
	{ MODKEY,                                XKB_KEY_t,                          setlayout,           {.v = &layouts[0]} },
	{ MODKEY,                                XKB_KEY_f,                          setlayout,           {.v = &layouts[1]} },
	{ MODKEY,                                XKB_KEY_m,                          setlayout,           {.v = &layouts[2]} },
	{ MODKEY,                                XKB_KEY_space,                      setlayout,           {0} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_space,                      togglefloating,      {0} },
	{ MODKEY,                                XKB_KEY_F11,                        togglefullscreen,    {0} },
	{ MODKEY,                                XKB_KEY_0,                          view,                {.ui = ~0} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_parenright,                 tag,                 {.ui = ~0} },
	{ MODKEY,                                XKB_KEY_comma,                      focusmon,            {.i = WLR_DIRECTION_LEFT} },
	{ MODKEY,                                XKB_KEY_period,                     focusmon,            {.i = WLR_DIRECTION_RIGHT} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_less,                       tagmon,              {.i = WLR_DIRECTION_LEFT} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_greater,                    tagmon,              {.i = WLR_DIRECTION_RIGHT} },
	{ MODKEY|WLR_MODIFIER_SHIFT,             XKB_KEY_Q,                          quit,                {0} },
	{ 0,                                     XKB_KEY_XF86MonBrightnessUp,        spawn,               SHCMD("dotfiles-brightness up") },
	{ 0,                                     XKB_KEY_XF86MonBrightnessDown,      spawn,               SHCMD("dotfiles-brightness down") },
	{ 0,                                     XKB_KEY_XF86AudioMute,              spawn,               SHCMD("dotfiles-volume mute") },
	{ 0,                                     XKB_KEY_XF86AudioLowerVolume,       spawn,               SHCMD("dotfiles-volume down") },
	{ 0,                                     XKB_KEY_XF86AudioRaiseVolume,       spawn,               SHCMD("dotfiles-volume up") },
	{ 0,                                     XKB_KEY_XF86AudioPlay,              spawn,               SHCMD("dotfiles-media play") },
	{ 0,                                     XKB_KEY_XF86AudioPrev,              spawn,               SHCMD("dotfiles-media prev") },
	{ 0,                                     XKB_KEY_XF86AudioNext,              spawn,               SHCMD("dotfiles-media next") },
	{ 0,                                     XKB_KEY_XF86AudioStop,              spawn,               SHCMD("dotfiles-media stop") },
	TAGKEYS(          XKB_KEY_1,             XKB_KEY_exclam,                     0),
	TAGKEYS(          XKB_KEY_2,             XKB_KEY_at,                         1),
	TAGKEYS(          XKB_KEY_3,             XKB_KEY_numbersign,                 2),
	TAGKEYS(          XKB_KEY_4,             XKB_KEY_dollar,                     3),
	TAGKEYS(          XKB_KEY_5,             XKB_KEY_percent,                    4),
	TAGKEYS(          XKB_KEY_6,             XKB_KEY_asciicircum,                5),
	TAGKEYS(          XKB_KEY_7,             XKB_KEY_ampersand,                  6),
	TAGKEYS(          XKB_KEY_8,             XKB_KEY_asterisk,                   7),
	TAGKEYS(          XKB_KEY_9,             XKB_KEY_parenleft,                  8),

	/* Ctrl-Alt-Backspace and Ctrl-Alt-Fx used to be handled by X server */
	{ WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_Terminate_Server, quit, {0} },
#define CHVT(n) { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_XF86Switch_VT_##n, chvt, {.ui = (n)} }
	CHVT(1), CHVT(2), CHVT(3), CHVT(4), CHVT(5), CHVT(6),
	CHVT(7), CHVT(8), CHVT(9), CHVT(10), CHVT(11), CHVT(12),
};

static const Button buttons[] = {
	{ MODKEY, BTN_LEFT,   moveresize,     {.ui = CurMove} },
	{ MODKEY, BTN_MIDDLE, togglefloating, {0} },
	{ MODKEY, BTN_RIGHT,  moveresize,     {.ui = CurResize} },
};
