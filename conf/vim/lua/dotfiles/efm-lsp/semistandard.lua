return {
	lintCommand = "semistandard --stdin",
	lintStdin = true,
	lintOffset = 2,
	lintFormats = {" %f:%l:%c: %m"},
	lintIgnoreExitCode = true,
	formatCommand = "semistandard --fix --stdin",
	formatStdin = true
}
