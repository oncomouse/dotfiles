--luacheck: globals vim
return {
	"airblade/vim-rooter",
	event = "VimEnter",
	setup = function()
		vim.opt.path = ",,"
		vim.g.rooter_patterns = {
			"Rakefile",
			"package.json",
			".git/",
			"Gemfile",
			"pyproject.toml",
			"setup.py",
			"Makefile",
		}
	end,
}
