return {
	lintCommand = [[luacheck --formatter visual_studio - --globals vim --read-globals after_each before_each it packer_plugins vimp]],
	lintStdin = true,
	lintIgnoreExitCode = true,
	lintFormats = {
		'%f(%l,%c) : %tarning %m',
		'%f(%l,%c) : %trror %m',
	}
}
