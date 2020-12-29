/* user and group to drop privileges to */
static const char *user  = "nobody";
static const char *group = "nobody";

static const char *colorname[NUMCOLS] = {
	[INIT] =   "black",     /* after initialization */
	[INPUT] =  "#005577",   /* during input */
	[FAILED] = "#CC3333",   /* wrong password */
	/* [CAPS] = "#FF0000", */
};

/*
 * Xresources preferences to load at startup
 */
/* ResourcePref resources[] = { */
/* 		{ "color0",       STRING,  &colorname[INIT] }, */
/* 		{ "color4",       STRING,  &colorname[INPUT] }, */
/* 		{ "color1",       STRING,  &colorname[FAILED] }, */
/* 		{ "color3",       STRING,  &colorname[CAPS] }, */
/* }; */
/* treat a cleared input like a wrong password (color) */
static const int failonclear = 1;
/*Enable blur*/
#define BLUR
/*Set blur radius*/
static const int blurRadius=5;
/*Enable Pixelation*/
//#define PIXELATION
/*Set pixelation radius*/
static const int pixelSize=0;
