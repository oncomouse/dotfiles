return {
	"itchyny/calendar.vim",
	init = function()
		pcall(require, "dotfiles.credentials")
		vim.g.calendar_google_calendar = 1
		vim.g.calendar_google_task = 0
	end,
	cmd = "Calendar",
}
