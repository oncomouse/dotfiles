/* Lemonaid does not do lemonbar specific formatting for you
 * This means you yourself have to add the format specifiers for bg, fg,
 * underline, overline, etc.
 * This gives the user much more control over how they want to setup their bar.
 * Lemonaid only handles the signals and runs your commands after the specified
 * time interval.
 * This makes lemonaid more akin to the unix philosophy and helps make the
 * codebase lean and clean
 */

static const unsigned short int left = 1;     // no. of left aligned blocks
static const unsigned short int centre = 0;   // no. of centre aligned blocks
static const unsigned short int right = 0;    // no. of right aligned blocks

// Blocks must be in the order of their alignment
static const Block blocks[] = {
	// command                  update interval(in s)   update signal
	{ "lemonbar_desktops",        3600,                   1 },
	/* { "lemonbar_cpu",           10,                     2 }, */
	/* { "lemonbar_mem",           60,                     3 }, */
	/* { "lemonbar_tempt",         10,                     4 }, */
	/* { "lemonbar_vol",           0,                      5 }, */
	/* { "lemonbar_brightness",    0,                      6 }, */
	/* { "lemonbar_battery",       60,                     7 }, */
	/* { "lemonbar_wifi",          0,                      8 }, */
	/* { "lemonbar_clock",         60,                     9 }, */
};

// sets delimeter between status commands. NULL character ('\0') means no delimeter.
static const char *delim = "  ";
static const unsigned int delim_length = 3;

