return {
	lintCommand = "htmlhint --format=unix",
	lintStdin = true,
	lintFormats = {"%f:%l:%c: %m"},
	lintIgnoreExitCode = true,
}

