/* See LICENSE file for copyright and license details. */

/* appearance */
#include "target.h"
static char* normbgcolor        = "#222222";
static char* normfgcolor        = "#cccccc";
static char* selbgcolor         = "#555555";
static char* selfgcolor         = "#ffffff";
static char* urgbgcolor         = "#111111";
static char* urgfgcolor         = "#cc0000";
static const char before[]      = "";
static const char after[]       = "";
static const char titletrim[]   = "…";
static const int  tabwidth      = 200;
static const Bool foreground    = True;
static       Bool urgentswitch  = False;

/*
 * Where to place a new tab when it is opened. When npisrelative is True,
 * then the current position is changed + newposition. If npisrelative
 * is False, then newposition is an absolute position.
 */
static int  newposition   = 0;
static Bool npisrelative  = False;

#define SETPROP(p) { \
		.v = (char *[]){ "/bin/sh", "-c", \
				"prop=\"`xwininfo -children -id $1 | grep '^     0x' |" \
				"sed -e's@^ *\\(0x[0-9a-f]*\\) \"\\([^\"]*\\)\".*@\\1 \\2@' |" \
				"xargs -0 printf %b | dmenu -l 10 -w $1`\" &&" \
				"xprop -id $1 -f $0 8s -set $0 \"$prop\"", \
				p, winid, NULL \
		} \
}

/*
 * Xresources preferences to load at startup
 */
ResourcePref resources[] = {
		{ "font",         STRING,  &font },
		{ "color0",       STRING,  &normbgcolor },
		{ "color7",       STRING,  &normfgcolor },
		{ "color8",       STRING,  &selbgcolor },
		{ "color15",      STRING,  &selfgcolor },
		{ "color0",       STRING,  &urgbgcolor },
		{ "color6",       STRING,  &urgfgcolor },
};

#define MODKEY ControlMask
static Key keys[] = {
	/* modifier                        key        function     argument */
	{ MODKEY|ShiftMask,                XK_Return, focusonce,   { 0 } },
	{ MODKEY|ShiftMask,                XK_t,      spawn,       { 0 } },

	{ MODKEY,                          XK_Next,   rotate,      { .i = +1 } },
	{ MODKEY,                          XK_Prior,  rotate,      { .i = -1 } },
	{ MODKEY|ShiftMask,                XK_Next,   movetab,     { .i = -1 } },
	{ MODKEY|ShiftMask,                XK_Prior,  movetab,     { .i = +1 } },
	{ MODKEY|ShiftMask,                XK_Tab,    rotate,      { .i = 0 } },

	{ MODKEY|ShiftMask,                XK_grave,  spawn,       SETPROP("_TABBED_SELECT_TAB") },
	{ MODKEY|ShiftMask,                XK_1,      move,        { .i = 0 } },
	{ MODKEY|ShiftMask,                XK_2,      move,        { .i = 1 } },
	{ MODKEY|ShiftMask,                XK_3,      move,        { .i = 2 } },
	{ MODKEY|ShiftMask,                XK_4,      move,        { .i = 3 } },
	{ MODKEY|ShiftMask,                XK_5,      move,        { .i = 4 } },
	{ MODKEY|ShiftMask,                XK_6,      move,        { .i = 5 } },
	{ MODKEY|ShiftMask,                XK_7,      move,        { .i = 6 } },
	{ MODKEY|ShiftMask,                XK_8,      move,        { .i = 7 } },
	{ MODKEY|ShiftMask,                XK_9,      move,        { .i = 8 } },
	{ MODKEY|ShiftMask,                XK_0,      move,        { .i = 9 } },

	{ MODKEY|ShiftMask,                XK_w,      killclient,  { 0 } },

	{ MODKEY|ShiftMask,                XK_u,      focusurgent, { 0 } },
	{ MODKEY|ShiftMask|ControlMask,    XK_u,      toggle,      { .v = (void*) &urgentswitch } },

	{ 0,                               XK_F11,    fullscreen,  { 0 } },
};
