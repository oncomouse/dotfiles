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
static const int barHeight	    = 26;

/*
 * Where to place a new tab when it is opened. When npisrelative is True,
 * then the current position is changed + newposition. If npisrelative
 * is False, then newposition is an absolute position.
 */
static int  newposition   = -1;
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

#define MODKEY ControlMask|ShiftMask
static Key keys[] = {
	/* modifier                        key        function     argument */
	{ MODKEY,          XK_Return, focusonce,   { 0 } },
	{ MODKEY,          XK_t,      spawn,       { 0 } },

	{ ControlMask,     XK_Prior,  rotate,      { .i = -1 } },
	{ ControlMask,     XK_Next,   rotate,      { .i = +1 } },
	{ MODKEY,          XK_h,      rotate,      { .i = -1 } },
	{ MODKEY,          XK_l,      rotate,      { .i = +1 } },
	{ MODKEY,          XK_Next,   movetab,     { .i = -1 } },
	{ MODKEY,          XK_Prior,  movetab,     { .i = +1 } },
	{ MODKEY,          XK_Tab,    rotate,      { .i = 0 } },

	{ MODKEY,          XK_grave,  spawn,       SETPROP("_TABBED_SELECT_TAB") },
	{ MODKEY,          XK_1,      move,        { .i = 0 } },
	{ MODKEY,          XK_2,      move,        { .i = 1 } },
	{ MODKEY,          XK_3,      move,        { .i = 2 } },
	{ MODKEY,          XK_4,      move,        { .i = 3 } },
	{ MODKEY,          XK_5,      move,        { .i = 4 } },
	{ MODKEY,          XK_6,      move,        { .i = 5 } },
	{ MODKEY,          XK_7,      move,        { .i = 6 } },
	{ MODKEY,          XK_8,      move,        { .i = 7 } },
	{ MODKEY,          XK_9,      move,        { .i = 8 } },
	{ MODKEY,          XK_0,      move,        { .i = 9 } },

	{ MODKEY,          XK_w,      killclient,  { 0 } },

	{ MODKEY,          XK_u,      focusurgent, { 0 } },
	{ MODKEY|Mod1Mask, XK_u,      toggle,      { .v = (void*) &urgentswitch } },

	{ 0,               XK_F11,    fullscreen,  { 0 } },
};
