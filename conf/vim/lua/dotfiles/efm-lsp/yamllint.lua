return {
	lintCommand = 'yamllint -f auto -',
	lintStdin = true,
	lintIgnoreExitCode = true,
	lintFormats = {
		'  %l:%c	   %tarning  %m',
		'  %l:%c	   %trror	%m',
	},
}
