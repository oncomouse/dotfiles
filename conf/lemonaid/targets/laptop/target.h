static const unsigned short int left = 2;     // no. of left aligned blocks
static const unsigned short int centre = 0;   // no. of centre aligned blocks
static const unsigned short int right = 5;    // no. of right aligned blocks

// Blocks must be in the order of their alignment
static const Block blocks[] = {
	// command                  update interval(in s)   update signal

	{ "rpbar-desktop.sh",       120,                    1 },
	{ "rpbar-curwin.sh",        30,                     2 },
	{ "rpbar-mpris.sh",         0,                      10 },
	{ "rpbar-volume.sh",        120,                    3 },
	{ "rpbar-brightness.sh",    120,                    4 },
	{ "rpbar-battery.sh",       30,                     5 },
	{ "rpbar-clock.sh",         60,                     6 },
	/* 
	 * dotfiles-media runs pkill -44 for status updates and lemonaid (which doesn't use
	 * mpris) dies if it receives an undefined signal. So we define a dummy signal.
	 * */

};
