return {
	lintCommand = "htmlhint --format=unix ${INPUT}",
	lintStdin = true,
	lintFormats = {"%f:%l:%c: %m"},
	lintIgnoreExitCode = true,
}

