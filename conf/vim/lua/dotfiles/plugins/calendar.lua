return {
	"itchyny/calendar.vim",
	init = function()
		require("dotfiles.credentials")
		vim.g.calendar_google_calendar = 1
		vim.g.calendar_google_task = 1
	end
}
