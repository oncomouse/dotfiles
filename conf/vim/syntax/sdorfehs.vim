" Vim syntax file
" Language:	Ratpoison configuration/commands file ( /etc/sdorfehsrc ~/.sdorfehsrc )
" Maintainer:	Magnus Woldrich <m@japh.se>
" URL:		http://github.com/trapd00r/vim-syntax-sdorfehs
" Last Change:	2021-04-12 13:46:04
" Previous Maintainer:	Doug Kearns <djkea2@gus.gscit.monash.edu.au>

if exists("b:current_syntax")
  finish
endif

syn match   sdorfehsComment	"^\s*#.*$"		contains=sdorfehsTodo

syn keyword sdorfehsTodo	TODO NOTE FIXME XXX	contained

syn case ignore
syn keyword sdorfehsBooleanArg	on off			contained
syn case match

syn keyword sdorfehsCommandArg abort addhook alias banish chdir		contained
syn keyword sdorfehsCommandArg clrunmanaged cnext colon compat cother		contained
syn keyword sdorfehsCommandArg cprev curframe dedicate definekey delete	contained
syn keyword sdorfehsCommandArg delkmap describekey echo escape exec		contained
syn keyword sdorfehsCommandArg fdump focus focusdown focuslast focusleft	contained
syn keyword sdorfehsCommandArg focusprev focusright focusup frestore fselect	contained
syn keyword sdorfehsCommandArg gdelete getenv getsel gmerge gmove		contained
syn keyword sdorfehsCommandArg gnew gnewbg gnext gprev gravity			contained
syn keyword sdorfehsCommandArg groups gselect help hsplit inext		contained
syn keyword sdorfehsCommandArg info iother iprev kill lastmsg			contained
syn keyword sdorfehsCommandArg license link listhook meta msgwait		contained
syn keyword sdorfehsCommandArg newkmap newwm next nextscreen number		contained
syn keyword sdorfehsCommandArg only other prev prevscreen prompt		contained
syn keyword sdorfehsCommandArg putsel quit ratclick rathold ratrelwarp		contained
syn keyword sdorfehsCommandArg ratwarp readkey redisplay redo remhook		contained
syn keyword sdorfehsCommandArg remove resize restart rudeness sdump		contained
syn keyword sdorfehsCommandArg select set setenv sfdump shrink			contained
syn keyword sdorfehsCommandArg source sselect startup_message time title	contained
syn keyword sdorfehsCommandArg tmpwm unalias undefinekey undo unmanage		contained
syn keyword sdorfehsCommandArg unsetenv verbexec version vsplit warp		contained
syn keyword sdorfehsCommandArg windows framefmt infofmt			contained

syn match   sdorfehsGravityArg "\<\(n\|north\)\>"	contained
syn match   sdorfehsGravityArg "\<\(nw\|northwest\)\>"	contained
syn match   sdorfehsGravityArg "\<\(ne\|northeast\)\>"	contained
syn match   sdorfehsGravityArg "\<\(w\|west\)\>"	contained
syn match   sdorfehsGravityArg "\<\(c\|center\)\>"	contained
syn match   sdorfehsGravityArg "\<\(e\|east\)\>"	contained
syn match   sdorfehsGravityArg "\<\(s\|south\)\>"	contained
syn match   sdorfehsGravityArg "\<\(sw\|southwest\)\>"	contained
syn match   sdorfehsGravityArg "\<\(se\|southeast\)\>"	contained
syn case match

syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(F[1-9][0-9]\=\|\(\a\|\d\)\)\>" contained nextgroup=sdorfehsCommandArg skipwhite

syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(space\|exclam\|quotedbl\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(numbersign\|dollar\|percent\|ampersand\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(apostrophe\|quoteright\|parenleft\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(parenright\|asterisk\|plus\|comma\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(minus\|period\|slash\|colon\|semicolon\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(less\|equal\|greater\|question\|at\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(bracketleft\|backslash\|bracketright\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(asciicircum\|underscore\|grave\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(quoteleft\|braceleft\|bar\|braceright\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(asciitilde\)\>" contained nextgroup=sdorfehsCommandArg skipwhite

syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(BackSpace\|Tab\|Linefeed\|Clear\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Return\|Pause\|Scroll_Lock\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Sys_Req\|Escape\|Delete\)\>" contained nextgroup=sdorfehsCommandArg skipwhite

syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Home\|Left\|Up\|Right\|Down\|Prior\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Page_Up\|Next\|Page_Down\|End\|Begin\)\>" contained nextgroup=sdorfehsCommandArg skipwhite

syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Select\|Print\|Execute\|Insert\|Undo\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Redo\|Menu\|Find\|Cancel\|Help\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=\(Break\|Mode_switch\|script_switch\|Num_Lock\)\>" contained nextgroup=sdorfehsCommandArg skipwhite

syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=KP_\(Space\|Tab\|Enter\|F[1234]\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=KP_\(Home\|Left\|Up\|Right\|Down\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=KP_\(Prior\|Page_Up\|Next\|Page_Down\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=KP_\(End\|Begin\|Insert\|Delete\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=KP_\(Equal\|Multiply\|Add\|Separator\)\>" contained nextgroup=sdorfehsCommandArg skipwhite
syn match   sdorfehsKeySeqArg  "\<\([CMASH]\(-[CMASH]\)\{,4}-\)\=KP_\(Subtract\|Decimal\|Divide\|\d\)\>" contained nextgroup=sdorfehsCommandArg skipwhite

syn match   sdorfehsHookArg    "\<\(key\|switchwin\|switchframe\|switchgroup\|quit\|restart|newwindow|switchvscreen|switchscreen|deletewindow\)\>" contained

syn match   sdorfehsNumberArg  "\<\d\+\>"	contained nextgroup=sdorfehsNumberArg skipwhite

syn keyword sdorfehsSetArg	barborder	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	bargravity	contained nextgroup=sdorfehsGravityArg
syn keyword sdorfehsSetArg	barpadding	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	bgcolor
syn keyword sdorfehsSetArg	border		contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	fgcolor
syn keyword sdorfehsSetArg	framefmt	contained nextgroup=sdorfehsWinFmtArg
syn keyword sdorfehsSetArg	fwcolor
syn keyword sdorfehsSetArg	framemsgwait	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	gravity 	contained nextgroup=sdorfehsGravityArg
syn keyword sdorfehsSetArg	bwcolor
syn keyword sdorfehsSetArg	gravity		contained nextgroup=sdorfehsGravityArg
syn keyword sdorfehsSetArg	historysize
syn keyword sdorfehsSetArg	historycompaction
syn keyword sdorfehsSetArg	historyexpansion
syn keyword sdorfehsSetArg	infofmt         contained nextgroup=sdorfehsWinFmtArg
syn keyword sdorfehsSetArg	topkmap
syn keyword sdorfehsSetArg	barinpadding
syn keyword sdorfehsSetArg	onlyborder
syn keyword sdorfehsSetArg	font
syn keyword sdorfehsSetArg	framesels
syn keyword sdorfehsSetArg	maxundos
syn keyword sdorfehsSetArg	inputwidth	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	maxsizegravity	contained nextgroup=sdorfehsGravityArg
syn keyword sdorfehsSetArg	msgwait	        contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	padding		contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	resizeunit	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	startupmessage
syn keyword sdorfehsSetArg	stickyfmt	contained nextgroup=sdorfehsWinFmtArg
syn keyword sdorfehsSetArg	transgravity	contained nextgroup=sdorfehsGravityArg
syn keyword sdorfehsSetArg	vscreens	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	waitcursor	contained nextgroup=sdorfehsNumberArg
syn keyword sdorfehsSetArg	winfmt		contained nextgroup=sdorfehsWinFmtArg
syn keyword sdorfehsSetArg	wingravity	contained nextgroup=sdorfehsGravityArg
syn keyword sdorfehsSetArg	winliststyle	contained nextgroup=sdorfehsWinListArg
syn keyword sdorfehsSetArg	winname		contained nextgroup=sdorfehsWinNameArg
syn keyword sdorfehsSetArg	gap		contained nextgroup=sdorfehsNumberArg

syn match   sdorfehsWinFmtArg  "%[nstacil]"			contained nextgroup=sdorfehsWinFmtArg skipwhite
syn match   sdorfehsFrameFmtArg  "%[nstacil]"			contained nextgroup=sdorfehsWinFmtArg skipwhite
syn match   sdorfehsInfoFmtArg  "%[nstacil]"			contained nextgroup=sdorfehsWinFmtArg skipwhite

syn match   sdorfehsWinListArg "\<\(row\|column\)\>"		contained

syn match   sdorfehsWinNameArg "\<\(name\|title\|class\)\>"	contained

syn match   sdorfehsDefCommand		"^\s*set\s*"			nextgroup=sdorfehsSetArg
syn match   sdorfehsDefCommand		"^\s*defbarborder\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*defbargravity\s*"		nextgroup=sdorfehsGravityArg
syn match   sdorfehsDefCommand		"^\s*defbarpadding\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*defbgcolor\s*"
syn match   sdorfehsDefCommand		"^\s*defborder\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*deffgcolor\s*"
syn match   sdorfehsDefCommand		"^\s*deffont\s*"
syn match   sdorfehsDefCommand		"^\s*defframefmt\s*"		nextgroup=sdorfehsWinFmtArg
syn match   sdorfehsDefCommand		"^\s*defframesels\s*"
syn match   sdorfehsDefCommand		"^\s*definputwidth\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*defmaxsizegravity\s*"	nextgroup=sdorfehsGravityArg
syn match   sdorfehsDefCommand		"^\s*defpadding\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*defresizeunit\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*deftransgravity\s*"	nextgroup=sdorfehsGravityArg
syn match   sdorfehsDefCommand		"^\s*defwaitcursor\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsDefCommand		"^\s*defwinfmt\s*"		nextgroup=sdorfehsWinFmtArg
syn match   sdorfehsDefCommand		"^\s*defwingravity\s*"		nextgroup=sdorfehsGravityArg
syn match   sdorfehsDefCommand		"^\s*defwinliststyle\s*"	nextgroup=sdorfehsWinListArg
syn match   sdorfehsDefCommand		"^\s*defwinname\s*"		nextgroup=sdorfehsWinNameArg
syn match   sdorfehsDefCommand		"^\s*msgwait\s*"		nextgroup=sdorfehsNumberArg

syn match   sdorfehsStringCommand	"^\s*\zsaddhook\ze\s*"		nextgroup=sdorfehsHookArg
syn match   sdorfehsStringCommand	"^\s*\zsalias\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsbind\ze\s*"		nextgroup=sdorfehsKeySeqArg
syn match   sdorfehsStringCommand	"^\s*\zschdir\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zscolon\ze\s*"		nextgroup=sdorfehsCommandArg
syn match   sdorfehsStringCommand	"^\s*\zsdedicate\ze\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsStringCommand	"^\s*\zsdefinekey\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsdelkmap\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsdescribekey\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsecho\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsescape\ze\s*"		nextgroup=sdorfehsKeySeqArg
syn match   sdorfehsStringCommand	"^\s*\zsexec\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsfdump\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsfrestore\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsgdelete\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsgetenv\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsgravity\ze\s*"		nextgroup=sdorfehsGravityArg
syn match   sdorfehsStringCommand	"^\s*\zsgselect\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zslink\ze\s*"		nextgroup=sdorfehsKeySeqArg
syn match   sdorfehsStringCommand	"^\s*\zslisthook\ze\s*"		nextgroup=sdorfehsHookArg
syn match   sdorfehsStringCommand	"^\s*\zsnewkmap\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsnewwm\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsnumber\ze\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsStringCommand	"^\s*\zsprompt\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsratwarp\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsratrelwarp\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsratclick\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsrathold\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsreadkey\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsremhook\ze\s*"		nextgroup=sdorfehsHookArg
syn match   sdorfehsStringCommand	"^\s*\zsresize\ze\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsStringCommand	"^\s*\zsrudeness\ze\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsStringCommand	"^\s*\zsselect\ze\s*"		nextgroup=sdorfehsNumberArg
syn match   sdorfehsStringCommand	"^\s*\zssetenv\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zssource\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zssselect\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsstartup_message\ze\s*"	nextgroup=sdorfehsBooleanArg
syn match   sdorfehsStringCommand	"^\s*\zstitle\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zstmpwm\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsunalias\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsunbind\ze\s*"		nextgroup=sdorfehsKeySeqArg
syn match   sdorfehsStringCommand	"^\s*\zsundefinekey\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsunmanage\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsunsetenv\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zsverbexec\ze\s*"
syn match   sdorfehsStringCommand	"^\s*\zswarp\ze\s*"		nextgroup=sdorfehsBooleanArg

syn match   sdorfehsVoidCommand	"^\s*\zsabort\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsbanish\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsclrunmanaged\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zscnext\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zscompat\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zscother\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zscprev\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zscurframe\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsdelete\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocusdown\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocuslast\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocusleft\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocusprev\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocusright\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocusup\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfocus\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsfselect\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgetsel\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgmerge\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgmove\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgnewbg\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgnew\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgnext\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgprev\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsgroups\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zshelp\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zshsplit\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsinext\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsinfo\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsiother\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsiprev\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zskill\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zslastmsg\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zslicense\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsmeta\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsnextscreen\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsnext\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsonly\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsother\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsprevscreen\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsprev\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsputsel\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsquit\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsredisplay\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsredo\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsremove\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsrestart\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zssdump\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zssfdump\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsshrink\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zssplit\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zstime\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsundo\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsversion\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zsvsplit\ze\s*$"
syn match   sdorfehsVoidCommand	"^\s*\zswindows\ze\s*$"

hi def link sdorfehsBooleanArg	Boolean
hi def link sdorfehsCommandArg	Keyword
hi def link sdorfehsComment	Comment
hi def link sdorfehsDefCommand	Identifier
hi def link sdorfehsFrameFmtArg	Special
hi def link sdorfehsGravityArg	Constant
hi def link sdorfehsInfoFmtArg    Special
hi def link sdorfehsKeySeqArg	Special
hi def link sdorfehsNumberArg	Number
hi def link sdorfehsSetArg	Keyword
hi def link sdorfehsStringCommand	Identifier
hi def link sdorfehsTodo		Todo
hi def link sdorfehsVoidCommand	Identifier
hi def link sdorfehsWinFmtArg	Special
hi def link sdorfehsWinNameArg	Constant
hi def link sdorfehsWinListArg	Constant

let b:current_syntax = 'sdorfehs'

" vim: ts=8
