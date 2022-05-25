/* Lemonaid does not do lemonbar specific formatting for you
 * This means you yourself have to add the format specifiers for bg, fg,
 * underline, overline, etc.
 * This gives the user much more control over how they want to setup their bar.
 * Lemonaid only handles the signals and runs your commands after the specified
 * time interval.
 * This makes lemonaid more akin to the unix philosophy and helps make the
 * codebase lean and clean
 */

static const unsigned short int left = 2;     // no. of left aligned blocks
static const unsigned short int centre = 0;   // no. of centre aligned blocks
static const unsigned short int right = 4;    // no. of right aligned blocks

// Blocks must be in the order of their alignment
static const Block blocks[] = {
	// command                  update interval(in s)   update signal

	{ "rpbar-desktop.sh",       0,                      1 },
	{ "rpbar-curwin.sh",        0,                      2 },
	{ "rpbar-volume.sh",        0,                      3 },
	{ "rpbar-brightness.sh",    0,                      4 },
	{ "rpbar-battery.sh",       30,                     5 },
	{ "rpbar-clock.sh",         60,                     6 },
	/* 
	 * dotfiles-media runs pkill -44 for status updates and lemonaid (which doesn't use
	 * mpris) dies if it receives an undefined signal. So we define a dummy signal.
	 * */
	{ "echo ''",                0,                      10 },

};

// sets delimeter between status commands. NULL character ('\0') means no delimeter.
static const char *delim = "\0";
static const unsigned int delim_length = 1;

