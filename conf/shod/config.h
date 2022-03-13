struct Config config = {
	/*
	 * except for the foreground, colors fields are strings
	 * containing three elements delimited by colon:
	 * the body color, the color of the light 3D shadow,
	 * and the color of the dark 3D shadow.
	 */

	/* 0-or-1 flags */
	.sloppyfocus    = 1,            /* set to 1 to use sloppy focus */
	.honorconfig    = 0,            /* set to 1 to honor configure requests */

	/* general configuration */
	.modifier       = Mod4Mask,     /* Modifier button */
	.snap           = 8,            /* proximity of container edges to perform snap attraction */
	.font           = "fixed",      /* font for titles in titlebars */
	.ndesktops      = 9,           /* number of desktops per monitor */

	/* dock configuration */
	.dockwidth      = 64,           /* width of the dock (or its height, if it is horizontal) */
	.dockspace      = 64,           /* size of each dockapp (64 for windowmaker dockapps) */
	.dockgravity    = "S",          /* placement of the dock */
	.dockcolors     = {"#212733", "#3b4857", "#212733"},

	/* notification configuration */
	.notifgap       = 3,            /* gap, in pixels, between notifications */
	.notifgravity   = "NE",         /* placement of notifications */
	.notifcolors    = {"#3465A4", "#729FCF", "#204A87"},

	/* prompt configuration */
	.promptcolors   = {"#3465A4", "#729FCF", "#204A87"},

	/* title bar */
	.titlewidth = 17,
	.foreground = {
		[FOCUSED]   = "#d9d7ce",
		[UNFOCUSED] = "#d9d7ce",
		[URGENT]    = "#d9d7ce",
	},

	/* border */
	.borderwidth = 4,
	.bordercolors = {
		[FOCUSED]   = {"#2f3a46", "#596069", "#3b4857"},
		[UNFOCUSED] = {"#212733", "#212733", "#212733"},
		[URGENT]    = {"#CC0000", "#EF2929", "#A40000"},
	},

	/* size of 3D shadow effect, must be less than borderwidth */
	.shadowthickness = 2,
};

