/* Lemonaid does not do lemonbar specific formatting for you
 * This means you yourself have to add the format specifiers for bg, fg,
 * underline, overline, etc.
 * This gives the user much more control over how they want to setup their bar.
 * Lemonaid only handles the signals and runs your commands after the specified
 * time interval.
 * This makes lemonaid more akin to the unix philosophy and helps make the
 * codebase lean and clean
 */

#include "target.h"

// sets delimeter between status commands. NULL character ('\0') means no delimeter.
static const char *delim = "\0";
static const unsigned int delim_length = 1;

